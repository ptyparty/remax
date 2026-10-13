{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent qualified as Concurrent
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Data.Char qualified as Char
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Version qualified as Version
import Numeric qualified as Numeric
import System.Directory qualified as Directory
import System.Environment qualified as Env
import System.Exit qualified as Exit
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import System.IO qualified as SysIO

import Paths_remax qualified as Paths
import Remax.Git qualified as Git
import Remax.Punch qualified as Punch
import Remax.Store qualified as Store
import Remax.UI qualified as UI
import System.Posix.Process qualified as Posix

data ModeArg = ArgHead | ArgBase T.Text | ArgBranch | ArgUnstaged | ArgUntracked

data Cmd
  = Review ModeArg
  | -- Nothing scopes to the current directory; a path need not still
    -- exist, which is the point — you forget a directory *because* it
    -- is gone.
    Forget (Maybe FilePath)
  | List ListOptions
  | Reply T.Text T.Text
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

data ListOptions = ListOptions
  { listFormat :: ListFormat
  , listWhen :: ListWhen
  , listStateFile :: Maybe FilePath
  , listAwaiting :: Maybe Store.Awaiting
  }

-- A file remax keeps the last listing's fingerprint in. With one, a wait
-- is measured from what the caller last saw rather than from the moment
-- it started — so a change that lands while the caller is busy is still
-- waiting for it when it gets back, instead of having been absorbed into
-- the new baseline and lost.

main :: IO ()
main = do
  args <- Env.getArgs
  case parseArgs args of
    Left err -> Exit.die err
    Right (mDir, cmd) -> do
      Monad.forM_ mDir $ \dir -> do
        there <- Directory.doesDirectoryExist dir
        Monad.unless there $
          Exit.die
            ( "No such directory: "
                ++ dir
                ++ "\nTo forget verdicts for a directory that is gone, run"
                ++ " remax forget "
                ++ dir
                ++ " from somewhere that still exists."
            )
        Directory.setCurrentDirectory dir
      case cmd of
        Review marg -> review marg
        Forget mPath -> forget mPath
        List options -> list options
        Reply target text -> reply target text
        Attend -> attend
        ShowVersion -> putStrLn ("remax " ++ Version.showVersion Paths.version)

-- list's flags, in either order and each at most once.
listArgs :: [String] -> Maybe Cmd
listArgs =
  let
    go options args =
      case args of
        [] -> Just (List options)
        ("--json" : rest)
          | listFormat options == ListText ->
              go options {listFormat = ListJson} rest
        ("--wait" : rest)
          | listWhen options == ListNow ->
              go options {listWhen = ListOnChange} rest
        ("--state" : path : rest)
          | listStateFile options == Nothing ->
              go options {listStateFile = Just path} rest
        ("--awaiting" : who : rest)
          | listAwaiting options == Nothing
          , Just a <- Store.parseAwaiting (T.pack who) ->
              go options {listAwaiting = Just a} rest
        _ -> Nothing
  in
    go
      ListOptions
        { listFormat = ListText
        , listWhen = ListNow
        , listStateFile = Nothing
        , listAwaiting = Nothing
        }

usage :: String
usage =
  "usage: remax [-C DIR] [--base REF | --branch | --unstaged | --untracked]\n\
  \       remax [-C DIR] forget [PATH]  forget verdicts under PATH (default: the\n\
  \                                     current directory); PATH need not still exist\n\
  \       remax [-C DIR] list [--json] [--wait] [--state FILE]\n\
  \                      [--awaiting note|agent|reviewer]\n\
  \                                     show flagged and questioned lines;\n\
  \                                     --wait prints nothing until the list\n\
  \                                     changes, so an agent can wait on a\n\
  \                                     reviewer instead of asking on a timer;\n\
  \                                     --state FILE measures that from what\n\
  \                                     you last saw rather than from now, so\n\
  \                                     a change while you were busy is not\n\
  \                                     missed; your own replies and edits\n\
  \                                     do not count as a change; --awaiting\n\
  \                                     shows only conversations waiting on\n\
  \                                     that party, and a wait wakes only\n\
  \                                     when one is new or changed\n\
  \       remax [-C DIR] reply ID TEXT   answer a note; shows in the review session\n\
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
        ("reply" : target : rest)
          | not (null rest) ->
              Right (mDir, Reply (T.pack target) (T.pack (unwords rest)))
        _ -> Left usage
  in
    go Nothing args0

review :: ModeArg -> IO ()
review marg = do
  (root, roots, story) <- reviewStart
  mode <- case marg of
    ArgHead -> pure (Git.VsRef "HEAD")
    ArgUnstaged -> pure Git.VsIndex
    ArgUntracked -> pure Git.Untracked
    ArgBase ref -> do
      ok <- Git.refExists root ref
      Monad.unless ok (Exit.die ("No such ref: " ++ T.unpack ref))
      pure (Git.VsRef ref)
    ArgBranch -> do
      got <- Git.branchRoot root
      case got of
        Left e -> Exit.die (T.unpack e)
        Right sha -> pure (Git.VsRef (T.take 10 sha))
  UI.runReview mode root roots story

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
  found <- reposInReach
  here <- Directory.getCurrentDirectory
  if null found
    then Exit.die (noRepoMessage here ++ ".")
    else pure found

reposInReach :: IO [FilePath]
reposInReach = do
  got <- Git.repoRoot
  here <- Directory.getCurrentDirectory
  case got of
    Just root -> pure [root]
    Nothing -> Git.childRepos here

noRepoMessage :: FilePath -> String
noRepoMessage here =
  "Not inside a git repository, and no repositories directly under " ++ here

waitForRepos :: IO [FilePath]
waitForRepos = do
  found <- reposInReach
  if not (null found)
    then pure found
    else do
      here <- Directory.getCurrentDirectory
      SysIO.hPutStrLn SysIO.stderr (noRepoMessage here ++ "; waiting for one to appear. Ctrl-C quits.")
      let
        poll = do
          Concurrent.threadDelay repoPollUs
          again <- reposInReach
          if null again then poll else pure again
      poll

repoPollUs :: Int
repoPollUs = 2000000

-- Where a review session starts: the first repository in reach, with the
-- rest reachable by :repo once the session is up.
-- Also where the checkouts sit, when they are a story rather than one
-- repository: the session watches that folder so a clone arriving
-- partway through a review is noticed.
reviewStart :: IO (FilePath, [FilePath], Maybe FilePath)
reviewStart = do
  roots <- waitForRepos
  inRepo <- Git.repoRoot
  story <- case inRepo of
    Just _ -> pure Nothing
    Nothing -> fmap Just Directory.getCurrentDirectory
  case roots of
    (first : _) -> do
      paused <- Monad.filterM (fmap Maybe.isJust . Git.rebaseStatus) roots
      pure (Maybe.fromMaybe first (Maybe.listToMaybe paused), roots, story)
    [] -> Exit.die "No repository to review."

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
  here <- Directory.getCurrentDirectory
  conn <- Store.openStore
  let
    target = case mPath of
      Nothing -> here
      Just path
        | FilePath.isAbsolute path -> FilePath.normalise path
        | otherwise -> FilePath.normalise (here </> path)
    -- Three ways a target can sit against a repository, and
    -- makeRelative tells them apart: it hands back an absolute path when
    -- it cannot express one under the other.
    --
    -- The story case is the second one — standing in the folder and
    -- forgetting means every checkout under it, not nothing, which is
    -- what asking only the first question gave.
    scopeIn root =
      let
        inward = FilePath.makeRelative root target
        outward = FilePath.makeRelative target root
      in
        if not (FilePath.isAbsolute inward)
          then -- target is inside this repository
            Just (if inward == "." then "" else T.pack inward)
          else
            if not (FilePath.isAbsolute outward)
              then Just "" -- the repository is inside the target
              else Nothing -- unrelated
  found <- Monad.forM roots $ \root ->
    case scopeIn root of
      Nothing -> pure (root, "", [])
      Just scope -> do
        rows <- Store.repoRows conn (T.pack root)
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
    fileCount = sum (map (\(_, _, rows) -> length (List.nub (map snd rows))) hits)
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
      Monad.when (length hits > 1) $
        Monad.forM_ hits $ \(root, _, rows) ->
          putStrLn ("    " ++ FilePath.takeFileName root ++ ": " ++ show (length rows))
      putStrLn "  Forgetting them means reviewing those changes again from scratch."
      putStr "  Forget them? [y/n]: "
      SysIO.hFlush SysIO.stdout
      answer <- getLine
      if answer == "y"
        then do
          Monad.forM_ hits $ \(root, _, rows) ->
            Store.deleteFiles conn (T.pack root) (List.nub (map snd rows))
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
  started <- Posix.getParentProcessID
  let
    beat = do
      parent <- Posix.getParentProcessID
      Monad.when (parent == started) $ do
        Store.markPresence Store.Working (map T.pack roots)
        Concurrent.threadDelay attendBeatUs
        beat
  beat `Exception.finally` Store.clearWaiting

attendBeatUs :: Int
attendBeatUs = 1000000

-- How often the punch list is re-read while waiting. A listing is two
-- small queries against a local database, so the cost of asking is far
-- below the cost of a watcher — and a watcher would spend an inotify
-- instance per waiting agent, on a resource a desktop runs out of.
waitPollUs :: Int
waitPollUs = 400000

list :: ListOptions -> IO ()
list options = do
  let
    fmt = listFormat options
    while = listWhen options
    stateFile = listStateFile options
  roots <- targetRepos
  conn <- Store.openStore
  let
    digest = do
      rows <- mapM (Store.punchRows conn . T.pack) roots
      pure (reviewerDigest (listAwaiting options) (concat rows))
    news before now =
      case listAwaiting options of
        Nothing -> now /= before
        Just _ -> not (now `Set.isSubsetOf` before)
    -- which review the fingerprint below is of
    watching =
      T.intercalate
        "\t"
        ( "digest/2"
            : maybe "all" Store.awaitingText (listAwaiting options)
            : map T.pack roots
        )
    remembered = case stateFile of
      Nothing -> pure Nothing
      Just path -> do
        got <- Exception.try (TIO.readFile path) :: IO (Either Exception.IOException T.Text)
        pure $ case got of
          Left _ -> Nothing
          Right raw -> case T.breakOn "\n" raw of
            (recorded, rest)
              | recorded == watching -> Just (Set.fromList (T.lines (T.drop 1 rest)))
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
          busy <- Store.composingNow conn (map T.pack roots)
          Monad.unless (news before now && not busy) $ do
            -- re-announced rather than announced: the timestamp is what
            -- tells a session somebody is still here, so an agent that
            -- dies stops saying so without having to say anything
            Store.markPresence Store.Waiting (map T.pack roots)
            Concurrent.threadDelay waitPollUs
            settle
      settle `Exception.finally` Store.clearWaiting
  Monad.forM_ stateFile $ \path -> do
    now <- digest
    Monad.void
      ( Exception.try (TIO.writeFile path (T.unlines (watching : Set.toList now))) ::
          IO (Either Exception.IOException ())
      )
  here <- Directory.getCurrentDirectory
  found <- mapM (\r -> fmap ((,) r) (Store.punchRows conn (T.pack r))) roots
  listed <-
    mapM
      ( \(repo, rows) -> do
          groups <- Store.loadNoteGroups conn (T.pack repo)
          let
            wanted conversation =
              maybe True (== Punch.conversationAwaiting conversation) (listAwaiting options)
            shown = filter wanted (Punch.conversations repo groups rows)
            files = List.nub (map Punch.conversationFile shown)
          contents <- mapM (Git.readFileOr "" . (repo </>) . T.unpack) files
          let
            fileLines = Map.fromList (zip files (map T.lines contents))
          pure (repo, map (describe fileLines) shown)
      )
      found
  let
    asJson = fmt == ListJson
    subjects = Punch.commitSubjects (concatMap snd found)
    nonEmpty = filter (not . null . snd) listed
    scope = case roots of
      [only] -> only
      _ -> here
  if asJson
    then TIO.putStrLn (punchJson (T.pack scope) subjects listed)
    else
      if null nonEmpty
        then putStrLn $
          case listAwaiting options of
            Nothing -> "No flagged or questioned lines recorded for this repository."
            Just party -> "No conversations awaiting " ++ T.unpack (Store.awaitingText party) ++ "."
        else Monad.forM_ nonEmpty $ \(repo, described) -> do
          -- name the repository only when there is more than one in
          -- play, so a single-repo listing reads exactly as before
          Monad.when (length roots > 1) $
            putStrLn ("── " ++ FilePath.takeFileName repo ++ " ──")
          printConversations subjects described

data Described = Described
  { describedConversation :: Punch.Conversation
  , describedLinesInFile :: Maybe Punch.LinesInFile
  }

describe :: Map.Map T.Text [T.Text] -> Punch.Conversation -> Described
describe fileLines conversation =
  Described
    { describedConversation = conversation
    , describedLinesInFile =
        Punch.linesInFile
          (Map.findWithDefault [] (Punch.conversationFile conversation) fileLines)
          (Punch.conversationLines conversation)
    }

printConversations :: Map.Map T.Text T.Text -> [Described] -> IO ()
printConversations subjects described =
  Monad.forM_ described $ \(Described conversation inFile) -> do
    TIO.putStrLn
      ( Punch.conversationFile conversation
          <> "  ("
          <> Punch.conversationUpdatedAt conversation
          <> ", awaiting "
          <> Store.awaitingText (Punch.conversationAwaiting conversation)
          <> maybe "" (\l -> ", lines in file: " <> Punch.linesInFileText l) inFile
          <> ")"
      )
    Monad.forM_ (Punch.conversationLines conversation) $ \line -> do
      let
        position = case Punch.punchLineNumber line of
          Just n -> " :" <> T.pack (show n)
          Nothing -> ""
      TIO.putStrLn
        ( "    "
            <> Store.verdictText (Punch.punchLineVerdict line)
            <> position
            <> "  "
            <> Punch.punchLineText line
        )
    Monad.forM_ (Punch.conversationThread conversation) $ \post ->
      case T.lines (Store.postBody post) of
        [] -> pure ()
        (firstLine : more) -> do
          let
            label = Store.postAuthor post <> ": "
            indent = T.replicate (T.length label) " "
          TIO.putStrLn ("    " <> label <> firstLine)
          Monad.forM_ more (\l -> TIO.putStrLn ("    " <> indent <> l))
    Monad.forM_ (Punch.conversationCommitSha conversation) $ \sha ->
      TIO.putStrLn
        ( "    commit: "
            <> sha
            <> maybe "" (" " <>) (Map.lookup sha subjects)
        )

-- An agent's post in a note conversation: ID is a conversation's id from
-- `list --json`; the post appends to the whole thread's group and
-- renders in order, with its author, in a running session.
-- An id belongs to exactly one repository — it hashes the repo path
-- along with the lines — so from a directory of checkouts we can simply
-- offer the id to each and let the one that owns it answer.
reply :: T.Text -> T.Text -> IO ()
reply target text = do
  roots <- targetRepos
  conn <- Store.openStore
  attached <-
    mapM (\r -> fmap ((,) r) (Store.appendAgentPost conn (T.pack r) "agent" target text)) roots
  case filter ((> 0) . snd) attached of
    [] -> Exit.die "No such conversation recorded for this repository."
    hits ->
      Monad.forM_ hits $ \(repo, n) ->
        putStrLn
          ( "reply attached to "
              ++ show n
              ++ " line(s)"
              ++ (if length roots > 1 then " in " ++ FilePath.takeFileName repo else "")
          )

-- JSON output ------------------------------------------------------------------

-- The punch list for machines (agents acting on review feedback). The
-- line text carries its +/- marker; line numbers are advisory positions
-- from when the verdict was recorded.
-- Conversations have their own repo, because a listing can now span
-- several: an agent aggregating a story's feedback needs to know which
-- checkout each conversation lives in before it can act on any of it.
punchJson :: T.Text -> Map.Map T.Text T.Text -> [(FilePath, [Described])] -> T.Text
punchJson scope subjects listed =
  let
    lineJson line =
      jsonObject
        [ ("line_no", maybe "null" (T.pack . show) (Punch.punchLineNumber line))
        , ("line", jsonString (Punch.punchLineText line))
        , ("verdict", jsonString (Store.verdictText (Punch.punchLineVerdict line)))
        ]
    conversationJson (Described conversation inFile) =
      jsonObject
        [ ("id", jsonString (Store.groupIdToText (Punch.conversationId conversation)))
        , ("repo", jsonString (T.pack (Punch.conversationRepo conversation)))
        , ("file", jsonString (Punch.conversationFile conversation))
        , ("commit_sha", maybe "null" jsonString (Punch.conversationCommitSha conversation))
        , ("lines", jsonArray (map lineJson (Foldable.toList (Punch.conversationLines conversation))))
        , ("thread", threadJson (Punch.conversationThread conversation))
        , ("awaiting", jsonString (Store.awaitingText (Punch.conversationAwaiting conversation)))
        , ("lines_in_file", maybe "null" (jsonString . Punch.linesInFileText) inFile)
        , ("updated_at", jsonString (Punch.conversationUpdatedAt conversation))
        ]
    shown = concatMap snd listed
    listedShas =
      Set.fromList
        (Maybe.mapMaybe (Punch.conversationCommitSha . describedConversation) shown)
    commits =
      jsonObject
        ( map
            (\(sha, subject) -> (sha, jsonString subject))
            (Map.toList (Map.restrictKeys subjects listedShas))
        )
  in
    jsonObject
      [ ("repo", jsonString scope)
      , ("commits", commits)
      ,
        ( "conversations"
        , jsonArray
            (map conversationJson shown)
        )
      ]

reviewerDigest :: Maybe Store.Awaiting -> [Store.PunchRow] -> Set.Set T.Text
reviewerDigest party rows =
  let
    wanted row = maybe True (== Store.awaiting (Store.punchThread row)) party
    rowJson row =
      jsonObject
        [ ("key", jsonString (Store.lineKeyToText (Store.punchKey row)))
        , ("file", jsonString (Store.punchFile row))
        , ("commit_sha", maybe "null" jsonString (Store.punchCommitSha row))
        , ("line", jsonString (Store.punchLine row))
        , ("verdict", jsonString (Store.verdictText (Store.punchVerdict row)))
        , ("thread", threadJson (filter Store.isReviewerPost (Store.punchThread row)))
        ]
  in
    Set.fromList (map rowJson (filter wanted rows))

threadJson :: [Store.Post] -> T.Text
threadJson posts =
  let
    one post =
      jsonObject
        [ ("author", jsonString (Store.postAuthor post))
        , ("body", jsonString (Store.postBody post))
        ]
  in
    jsonArray (map one posts)

jsonArray :: [T.Text] -> T.Text
jsonArray values = "[" <> T.intercalate ", " values <> "]"

jsonObject :: [(T.Text, T.Text)] -> T.Text
jsonObject fields =
  let
    field (k, v) = jsonString k <> ": " <> v
  in
    "{" <> T.intercalate ", " (map field fields) <> "}"

jsonString :: T.Text -> T.Text
jsonString t =
  let
    escape c = case c of
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\t' -> "\\t"
      '\r' -> "\\r"
      _
        | Char.isControl c ->
            let
              hex = Numeric.showHex (Char.ord c) ""
            in
              T.pack ("\\u" ++ replicate (4 - length hex) '0' ++ hex)
        | otherwise -> T.singleton c
  in
    "\"" <> T.concatMap escape t <> "\""
