{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, finally, try)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Char (isControl, ord)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Numeric (showHex)
import System.Directory
  ( doesDirectoryExist
  , getCurrentDirectory
  , setCurrentDirectory
  )
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath
  ( isAbsolute
  , makeRelative
  , normalise
  , takeFileName
  , (</>)
  )
import System.IO (hFlush, stdout)

import Paths_remax (version)
import Remax.Git (ReviewMode (..), branchRoot, childRepos, refExists, repoRoot)
import Remax.Store
import Remax.UI (runReview)
import System.Posix.Process (getParentProcessID)

data ModeArg = ArgHead | ArgBase Text | ArgBranch | ArgUnstaged | ArgUntracked

data Cmd
  = Review ModeArg
  | -- Nothing scopes to the current directory; a path need not still
    -- exist, which is the point — you forget a directory *because* it
    -- is gone.
    Forget (Maybe FilePath)
  | List ListFormat ListWhen (Maybe FilePath)
  | Reply Text Text
  | Attend
  | ShowVersion

-- How a listing is printed, and when.
data ListFormat = ListText | ListJson
  deriving (Eq)

-- ListNow prints what is recorded and exits. ListOnChange prints nothing
-- until the punch list differs from what it was when the command
-- started, which is what lets an agent wait on a reviewer rather than
-- ask again on a timer.
data ListWhen = ListNow | ListOnChange
  deriving (Eq)

-- A file remax keeps the last listing's fingerprint in. With one, a wait
-- is measured from what the caller last saw rather than from the moment
-- it started — so a change that lands while the caller is busy is still
-- waiting for it when it gets back, instead of having been absorbed into
-- the new baseline and lost.

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> die err
    Right (mDir, cmd) -> do
      forM_ mDir $ \dir -> do
        there <- doesDirectoryExist dir
        unless there $
          die
            ( "No such directory: "
                ++ dir
                ++ "\nTo forget verdicts for a directory that is gone, run"
                ++ " remax forget "
                ++ dir
                ++ " from somewhere that still exists."
            )
        setCurrentDirectory dir
      case cmd of
        Review marg -> review marg
        Forget mPath -> forget mPath
        List fmt while stateFile -> list fmt while stateFile
        Reply key text -> reply key text
        Attend -> attend
        ShowVersion -> putStrLn ("remax " ++ showVersion version)

-- list's flags, in either order and each at most once.
listArgs :: [String] -> Maybe Cmd
listArgs =
  let
    go fmt while stateFile args =
      case args of
        [] -> Just (List fmt while stateFile)
        ("--json" : rest) | fmt == ListText -> go ListJson while stateFile rest
        ("--wait" : rest) | while == ListNow -> go fmt ListOnChange stateFile rest
        ("--state" : path : rest)
          | stateFile == Nothing -> go fmt while (Just path) rest
        _ -> Nothing
  in
    go ListText ListNow Nothing

usage :: String
usage =
  "usage: remax [-C DIR] [--base REF | --branch | --unstaged | --untracked]\n\
  \       remax [-C DIR] forget [PATH]  forget verdicts under PATH (default: the\n\
  \                                     current directory); PATH need not still exist\n\
  \       remax [-C DIR] list [--json] [--wait] [--state FILE]\n\
  \                                     show flagged and questioned lines;\n\
  \                                     --wait prints nothing until the list\n\
  \                                     changes, so an agent can wait on a\n\
  \                                     reviewer instead of asking on a timer;\n\
  \                                     --state FILE measures that from what\n\
  \                                     you last saw rather than from now, so\n\
  \                                     a change while you were busy is not\n\
  \                                     missed\n\
  \       remax [-C DIR] reply KEY TEXT  answer a note; shows in the review session\n\
  \       remax [-C DIR] watching       say an agent is on this review, until\n\
  \                                     stopped; the session shows it in its\n\
  \                                     header, working or waiting\n\
  \\n\
  \  -C DIR       run as if started in DIR\n\
  \  --base REF   review the working tree against REF (default: HEAD)\n\
  \  --branch     review against the merge-base with the mainline\n\
  \  --unstaged   review the working tree against the index\n\
  \  --untracked  review new files as pure additions"

parseArgs :: [String] -> Either String (Maybe FilePath, Cmd)
parseArgs args0 =
  let
    go mDir args =
      case args of
        ("-C" : dir : rest) -> go (Just dir) rest
        ["-C"] -> Left usage
        [] -> Right (mDir, Review ArgHead)
        ["--unstaged"] -> Right (mDir, Review ArgUnstaged)
        ["--untracked"] -> Right (mDir, Review ArgUntracked)
        ["--branch"] -> Right (mDir, Review ArgBranch)
        ["--base", ref] -> Right (mDir, Review (ArgBase (T.pack ref)))
        ["forget"] -> Right (mDir, Forget Nothing)
        ["forget", path] -> Right (mDir, Forget (Just path))
        ("list" : rest)
          | Just cmd <- listArgs rest -> Right (mDir, cmd)
        ["watching"] -> Right (mDir, Attend)
        ["--version"] -> Right (mDir, ShowVersion)
        ("reply" : key : rest)
          | not (null rest) ->
              Right (mDir, Reply (T.pack key) (T.pack (unwords rest)))
        _ -> Left usage
  in
    go Nothing args0

review :: ModeArg -> IO ()
review marg = do
  (root, roots, story) <- reviewStart
  mode <- case marg of
    ArgHead -> pure (VsRef "HEAD")
    ArgUnstaged -> pure VsIndex
    ArgUntracked -> pure Untracked
    ArgBase ref -> do
      ok <- refExists root ref
      unless ok (die ("No such ref: " ++ T.unpack ref))
      pure (VsRef ref)
    ArgBranch -> do
      got <- branchRoot root
      case got of
        Left e -> die (T.unpack e)
        Right sha -> pure (VsRef (T.take 10 sha))
  runReview mode root roots story

-- Every repository a command should act on: the one we are inside, or —
-- for a directory that merely holds repositories, which is what a story
-- directory is — all of them.
--
-- This matters most for agents. An agent gets pointed at whatever
-- directory the human names, and when the human is reviewing a story
-- that is the folder of checkouts rather than any one checkout. Refusing
-- there would make the first command an agent runs fail.
targetRepos :: IO [FilePath]
targetRepos = do
  got <- repoRoot
  here <- getCurrentDirectory
  case got of
    Just root -> pure [root]
    Nothing -> do
      children <- childRepos here
      if null children
        then
          die
            ( "Not inside a git repository, and no repositories directly under "
                ++ here
                ++ "."
            )
        else pure children

-- Where a review session starts: the first repository in reach, with the
-- rest reachable by :repo once the session is up.
-- Also where the checkouts sit, when they are a story rather than one
-- repository: the session watches that folder so a clone arriving
-- partway through a review is noticed.
reviewStart :: IO (FilePath, [FilePath], Maybe FilePath)
reviewStart = do
  inRepo <- repoRoot
  roots <- targetRepos
  story <- case inRepo of
    Just _ -> pure Nothing
    Nothing -> fmap Just getCurrentDirectory
  case roots of
    (first : _) -> pure (first, roots, story)
    [] -> die "No repository to review."

-- Forget the verdicts recorded for files under the current directory, so
-- those changes come up for review from scratch. At the top of the
-- repository that is everything recorded for it.
-- Verdicts outlive the files they were recorded against — that is the
-- whole design, since they are keyed by line content — so forgetting has
-- to be able to name a directory that is no longer there. The scope is
-- resolved by path arithmetic alone, never touching the filesystem, so a
-- deleted path scopes exactly as well as a live one.
forget :: Maybe FilePath -> IO ()
forget mPath = do
  -- every repository the command could act on, not just the one we are
  -- standing in: from a story directory this is all of the checkouts,
  -- and refusing there is what `list` and `reply` long ago stopped doing
  roots <- targetRepos
  here <- getCurrentDirectory
  conn <- openStore
  let
    target = case mPath of
      Nothing -> here
      Just path
        | isAbsolute path -> normalise path
        | otherwise -> normalise (here </> path)
    -- Three ways a target can sit against a repository, and
    -- makeRelative tells them apart: it hands back an absolute path when
    -- it cannot express one under the other.
    --
    -- The story case is the second one — standing in the folder and
    -- forgetting means every checkout under it, not nothing, which is
    -- what asking only the first question gave.
    scopeIn root =
      let
        inward = makeRelative root target
        outward = makeRelative target root
      in
        if not (isAbsolute inward)
          then -- target is inside this repository
            Just (if inward == "." then "" else T.pack inward)
          else
            if not (isAbsolute outward)
              then Just "" -- the repository is inside the target
              else Nothing -- unrelated
  found <- forM roots $ \root ->
    case scopeIn root of
      Nothing -> pure (root, "", [])
      Just scope -> do
        rows <- repoRows conn (T.pack root)
        let
          -- by path component, as :accept matches: "gen" covers gen/a.txt
          -- but not generated.hs, and naming one file exactly forgets
          -- just it
          underScope (_, f) =
            T.null scope || f == scope || (scope <> "/") `T.isPrefixOf` f
        pure (root, scope, filter underScope rows)
  let
    hits = filter (\(_, _, rows) -> not (null rows)) found
    doomedCount = sum (map (\(_, _, rows) -> length rows) hits)
    fileCount = sum (map (\(_, _, rows) -> length (nub (map snd rows))) hits)
    -- named as it was asked for: a path relativised against each root in
    -- turn has no single answer to report, and "." is not what anybody
    -- typed
    described = case mPath of
      Nothing -> if length roots > 1 then "here" else "the repository"
      Just path -> path
  if null hits
    then putStrLn ("Nothing is recorded for the files under " ++ described ++ ".")
    else do
      putStrLn
        ( show doomedCount
            ++ " line verdict(s) across "
            ++ show fileCount
            ++ " file(s) under "
            ++ described
            ++ "."
        )
      -- named when there is more than one, so a story-wide forget says
      -- what it is about to reach into
      when (length hits > 1) $
        forM_ hits $ \(root, _, rows) ->
          putStrLn ("    " ++ takeFileName root ++ ": " ++ show (length rows))
      putStrLn "  Forgetting them means reviewing those changes again from scratch."
      putStr "  Forget them? [y/n]: "
      hFlush stdout
      answer <- getLine
      if answer == "y"
        then do
          forM_ hits $ \(root, _, rows) ->
            deleteFiles conn (T.pack root) (nub (map snd rows))
          putStrLn ("  \10003 forgot " ++ show doomedCount ++ " line verdict(s)")
        else putStrLn "  left alone"

-- Hold a review open in the reviewer's header for as long as this runs.
--
-- `list --wait` says somebody is here only while it is blocked, which
-- goes quiet for the whole of the agent's turn — the mark went out
-- exactly when there was most going on. This is the other half: it says
-- somebody is attending, through the reading and the changing and the
-- replying, and stops saying it the moment it is stopped or killed.
attend :: IO ()
attend = do
  roots <- targetRepos
  -- Whoever started this is the agent, near enough. When they go, this
  -- goes: a heartbeat that outlives the agent it speaks for is worse
  -- than none, because it says somebody is attending a review that
  -- nobody is, and nothing else will ever contradict it.
  --
  -- Orphaned rather than merely detached: a process started with no
  -- parent to lose has one already, and refusing to run for it would
  -- take a legitimate way of starting this away.
  started <- getParentProcessID
  let
    beat = do
      parent <- getParentProcessID
      when (parent == started) $ do
        markPresence Working (map T.pack roots)
        threadDelay attendBeatUs
        beat
  beat `finally` clearWaiting

attendBeatUs :: Int
attendBeatUs = 1000000

-- How often the punch list is re-read while waiting. A listing is two
-- small queries against a local database, so the cost of asking is far
-- below the cost of a watcher — and a watcher would spend an inotify
-- instance per waiting agent, on a resource a desktop runs out of.
waitPollUs :: Int
waitPollUs = 400000

list :: ListFormat -> ListWhen -> Maybe FilePath -> IO ()
list fmt while stateFile = do
  roots <- targetRepos
  conn <- openStore
  let
    -- what the caller would be shown, as one comparable value: the
    -- listing has changed exactly when what they would read has
    digest = do
      rows <- mapM (\r -> fmap ((,) r) (punchRows conn (T.pack r))) roots
      pure (punchJson "" rows)
    -- which review the fingerprint below is of
    watching = T.intercalate "\t" (map T.pack roots)
    remembered = case stateFile of
      Nothing -> pure Nothing
      Just path -> do
        got <- try (TIO.readFile path) :: IO (Either IOException Text)
        pure $ case got of
          Left _ -> Nothing
          Right raw -> case T.breakOn "\n" raw of
            (recorded, rest)
              | recorded == watching -> Just (T.drop 1 rest)
            -- a fingerprint of somewhere else says nothing about here,
            -- and believing one is worse than having none: this
            -- repository's listing will not match it, so the wait
            -- returns at once with nothing to report. Measure from now
            -- instead. A file written before the scope was recorded
            -- reads as somewhere else, and costs one such measurement.
            _ -> Nothing
  case while of
    ListNow -> pure ()
    ListOnChange -> do
      -- the baseline is what the caller last saw when it has told us,
      -- and only otherwise the moment we started
      seen <- remembered
      before <- maybe digest pure seen
      let
        settle = do
          now <- digest
          -- a flag whose note is still being typed is not news yet: the
          -- verdict is in the store seconds before the sentence saying
          -- what is wrong with it, and returning on it means answering
          -- before being told
          busy <- composingNow conn (map T.pack roots)
          unless (now /= before && not busy) $ do
            -- re-announced rather than announced: the timestamp is what
            -- tells a session somebody is still here, so an agent that
            -- dies stops saying so without having to say anything
            markPresence Waiting (map T.pack roots)
            threadDelay waitPollUs
            settle
      settle `finally` clearWaiting
  forM_ stateFile $ \path -> do
    now <- digest
    void
      ( try (TIO.writeFile path (watching <> "\n" <> now)) ::
          IO (Either IOException ())
      )
  here <- getCurrentDirectory
  found <- mapM (\r -> fmap ((,) r) (punchRows conn (T.pack r))) roots
  let
    asJson = fmt == ListJson
    nonEmpty = filter (not . null . snd) found
    scope = case roots of
      [only] -> only
      _ -> here
  if asJson
    then TIO.putStrLn (punchJson (T.pack scope) found)
    else
      if null nonEmpty
        then putStrLn "No flagged or questioned lines recorded for this repository."
        else forM_ nonEmpty $ \(repo, rows) -> do
          -- name the repository only when there is more than one in
          -- play, so a single-repo listing reads exactly as before
          when (length roots > 1) $
            putStrLn ("── " ++ takeFileName repo ++ " ──")
          printPunchRows rows

printPunchRows :: [PunchRow] -> IO ()
printPunchRows rows =
  forM_ rows $ \row -> do
    let
      position = case punchLineNo row of
        Just n -> ":" <> T.pack (show n)
        Nothing -> ""
    TIO.putStrLn
      ( punchVerdict row
          <> "  "
          <> punchFile row
          <> position
          <> "  ("
          <> punchAt row
          <> ")"
      )
    TIO.putStrLn ("    " <> punchLine row)
    forM_ (punchThread row) $ \post ->
      case T.lines (postBody post) of
        [] -> pure ()
        (firstLine : more) -> do
          let
            label = postAuthor post <> ": "
            indent = T.replicate (T.length label) " "
          TIO.putStrLn ("    " <> label <> firstLine)
          forM_ more (\l -> TIO.putStrLn ("    " <> indent <> l))
    forM_ (punchCommitSha row) $ \sha ->
      TIO.putStrLn
        ( "    commit: "
            <> sha
            <> " "
            <> fromMaybe "" (punchCommitSubject row)
        )

-- An agent's post in a note conversation: KEY is the item's key from
-- `list --json`; the post appends to the whole thread's group and
-- renders in order, with its author, in a running session.
-- A key belongs to exactly one repository — it hashes the repo path
-- along with the line — so from a directory of checkouts we can simply
-- offer the key to each and let the one that owns it answer.
reply :: Text -> Text -> IO ()
reply key text = do
  roots <- targetRepos
  conn <- openStore
  attached <-
    mapM (\r -> fmap ((,) r) (appendAgentPost conn (T.pack r) "agent" key text)) roots
  case filter ((> 0) . snd) attached of
    [] -> die "No such key recorded for this repository."
    hits ->
      forM_ hits $ \(repo, n) ->
        putStrLn
          ( "reply attached to "
              ++ show n
              ++ " line(s)"
              ++ (if length roots > 1 then " in " ++ takeFileName repo else "")
          )

-- JSON output ------------------------------------------------------------------

-- The punch list for machines (agents acting on review feedback). The
-- line text carries its +/- marker; line numbers are advisory positions
-- from when the verdict was recorded.
-- Items carry their own repo, because a listing can now span several:
-- an agent aggregating a story's feedback needs to know which checkout
-- each item lives in before it can act on any of it.
punchJson :: Text -> [(FilePath, [PunchRow])] -> Text
punchJson scope found =
  let
    item repo row =
      jsonObject
        [ ("key", jsonString (punchKey row))
        , ("repo", jsonString (T.pack repo))
        , ("file", jsonString (punchFile row))
        , ("verdict", jsonString (punchVerdict row))
        , ("line", jsonString (punchLine row))
        , ("line_no", maybe "null" (T.pack . show) (punchLineNo row))
        , ("note", maybe "null" jsonString (openingNote row))
        , ("reply", maybe "null" jsonString (latestAnswer row))
        , ("thread", threadJson (punchThread row))
        , ("commit_sha", maybe "null" jsonString (punchCommitSha row))
        , ("commit_subject", maybe "null" jsonString (punchCommitSubject row))
        , ("updated_at", jsonString (punchAt row))
        ]
    items =
      concatMap (\(repo, rows) -> map (item repo) rows) found
  in
    jsonObject
      [ ("repo", jsonString scope)
      , ("items", "[" <> T.intercalate ", " items <> "]")
      ]

-- Compatibility views of the thread: the opening reviewer post is the
-- "note", the newest non-reviewer post the "reply".
openingNote :: PunchRow -> Maybe Text
openingNote row =
  case punchThread row of
    (post : _)
      | postAuthor post == "reviewer" -> Just (postBody post)
    _ -> Nothing

latestAnswer :: PunchRow -> Maybe Text
latestAnswer row =
  case filter (\p -> postAuthor p /= "reviewer") (reverse (punchThread row)) of
    (post : _) -> Just (postBody post)
    [] -> Nothing

threadJson :: [Post] -> Text
threadJson posts =
  let
    one post =
      jsonObject
        [ ("author", jsonString (postAuthor post))
        , ("body", jsonString (postBody post))
        ]
  in
    "[" <> T.intercalate ", " (map one posts) <> "]"

jsonObject :: [(Text, Text)] -> Text
jsonObject fields =
  let
    field (k, v) = jsonString k <> ": " <> v
  in
    "{" <> T.intercalate ", " (map field fields) <> "}"

jsonString :: Text -> Text
jsonString t =
  let
    escape c = case c of
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\t' -> "\\t"
      '\r' -> "\\r"
      _
        | isControl c ->
            let
              hex = showHex (ord c) ""
            in
              T.pack ("\\u" ++ replicate (4 - length hex) '0' ++ hex)
        | otherwise -> T.singleton c
  in
    "\"" <> T.concatMap escape t <> "\""
