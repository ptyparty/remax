{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent qualified as Concurrent
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Data.Char qualified as Char
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

import Database.SQLite.Simple qualified as SQLite
import Paths_remax qualified as Paths
import Remax.Config qualified as Config
import Remax.Conversation qualified as Conv
import Remax.Feedback qualified as Feedback
import Remax.FeedbackText qualified as FeedbackText
import Remax.Git qualified as Git
import Remax.GitHub qualified as GitHub
import Remax.Hunks qualified as Hunks
import Remax.Json qualified as Json
import Remax.Key qualified as Key
import Remax.LineRange qualified as LineRange
import Remax.Remote qualified as Remote
import Remax.RemoteCache qualified as RemoteCache
import Remax.Store qualified as Store
import Remax.UI qualified as UI
import Remax.WorkStore qualified as WorkStore
import System.Posix.Process qualified as Posix

data ModeArg
  = ArgHead
  | ArgBase T.Text
  | ArgBranch
  | ArgUnstaged
  | ArgUntracked
  | ArgRef T.Text (Maybe T.Text)
  | ArgPull Int

data Cmd
  = Review ModeArg
  | -- Nothing scopes to the current directory; a path need not still
    -- exist, which is the point — you forget a directory *because* it
    -- is gone.
    Forget (Maybe FilePath)
  | List ListOptions
  | ShowConversation T.Text
  | Reply T.Text T.Text
  | Comment CommentArgs
  | Note T.Text T.Text
  | Move T.Text Int
  | Attend
  | Hunks [String]
  | TourBegin [String]
  | TourStep [String]
  | TourClear [String]
  | RunInSession T.Text
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

data CommentArgs
  = Draft T.Text T.Text
  | Revise T.Text T.Text

data ListOptions = ListOptions
  { listFormat :: ListFormat
  , listWhen :: ListWhen
  , listStateFile :: Maybe FilePath
  , listAwaiting :: Maybe Feedback.Party
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
        ShowConversation target -> showConversation target
        Reply target text -> reply target text
        Comment commentArgs -> comment commentArgs
        Note place text -> openNote place text
        Move target pull -> move target pull
        Attend -> attend
        Hunks rest -> hunks rest
        TourBegin rest -> tourBegin rest
        TourStep rest -> tourStep rest
        TourClear rest -> tourBegin rest
        RunInSession command -> runInSession command
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
          , Just a <- Feedback.parseParty (T.pack who) ->
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
  \       remax [-C DIR] --ref REF [--base BASE | --branch]\n\
  \       remax [-C DIR] --pr N\n\
  \       remax [-C DIR] forget [PATH]  forget verdicts under PATH (default: the\n\
  \                                     current directory); PATH need not still exist\n\
  \       remax [-C DIR] list [--json] [--wait] [--state FILE]\n\
  \                      [--awaiting note|agent|reviewer]\n\
  \                                     show conversations, each under the\n\
  \                                     shortest id prefix only it has, with\n\
  \                                     its first lines and last two posts;\n\
  \                                     --json prints each one whole;\n\
  \                                     --wait prints nothing until the list\n\
  \                                     changes, so an agent can wait on a\n\
  \                                     reviewer instead of asking on a timer;\n\
  \                                     --state FILE measures that from what\n\
  \                                     you last saw rather than from now, so\n\
  \                                     a change while you were busy is not\n\
  \                                     missed; --awaiting shows only\n\
  \                                     conversations waiting on that party,\n\
  \                                     and a wait wakes only when one is new\n\
  \                                     or changed, so with --awaiting agent\n\
  \                                     your own replies and edits do not\n\
  \                                     wake it; --awaiting agent leaves out\n\
  \                                     PR comments, review summaries and\n\
  \                                     threads a bot spoke last on; against\n\
  \                                     the --state FILE or the wait's start,\n\
  \                                     each conversation is marked new or\n\
  \                                     changed\n\
  \       remax [-C DIR] show ID        print one conversation whole\n\
  \       remax [-C DIR] reply ID TEXT   answer a conversation; shows in the review\n\
  \                                     session, and never reaches GitHub\n\
  \       remax [-C DIR] note PATH:LINE[-LINE] TEXT\n\
  \                                     open a note to the reviewer on changed\n\
  \                                     lines; it waits for them until they\n\
  \                                     answer or resolve it, and never\n\
  \                                     reaches GitHub\n\
  \       remax [-C DIR] comment HUNK TEXT\n\
  \                                     propose a GitHub comment on a hunk; the\n\
  \                                     reviewer takes it with Enter\n\
  \       remax [-C DIR] comment --revise ID TEXT\n\
  \                                     propose text for a conversation; the\n\
  \                                     reviewer takes it with Enter\n\
  \                                     reply, comment --revise and move take\n\
  \                                     a conversation id or any prefix of it\n\
  \                                     that no other id shares\n\
  \       remax [-C DIR] move ID --pr N  file a draft comment and every post on\n\
  \                                     it under pull request N, such as after\n\
  \                                     its PR was reopened under a new number\n\
  \       remax [-C DIR] watching       say an agent is on this review, until\n\
  \                                     stopped; the session shows it in its\n\
  \                                     header, working or waiting\n\
  \       remax [-C DIR] hunks [--json] [--ref REF [--base BASE] | --base REF | --pr N]\n\
  \                                     list the change set as the session sees\n\
  \                                     it, one id per hunk; with no ref flags,\n\
  \                                     whatever the open session is viewing\n\
  \       remax [-C DIR] do COMMAND     run a : command in the remax session open\n\
  \                                     on this checkout, in this qmux tab, such\n\
  \                                     as do :go HUNK_ID; commands that only\n\
  \                                     look, move or change the view run, and\n\
  \                                     the rest are the reviewer's; prints what\n\
  \                                     the session said, or that it is held\n\
  \                                     until the reviewer is free\n\
  \       remax [-C DIR] tour begin [ref flags]\n\
  \       remax [-C DIR] tour step [ref flags] [--title WORDS] ID[,ID...] NOTE...\n\
  \       remax [-C DIR] tour clear [ref flags]\n\
  \                                     guide the reviewer through the change:\n\
  \                                     begin empties the tour, each step names\n\
  \                                     hunks and says something about them, and\n\
  \                                     the session walks them in that order\n\
  \\n\
  \  -C DIR       run as if started in DIR\n\
  \  --base REF   review the working tree against REF (default: HEAD)\n\
  \  --branch     review against the merge-base with the mainline\n\
  \  --unstaged   review the working tree against the index\n\
  \  --untracked  review new files as pure additions\n\
  \  --ref REF    review REF itself, read-only, without checking it out:\n\
  \               against BASE, or its merge-base with the mainline;\n\
  \               verdicts record the ref and its tip for the agent\n\
  \  --pr N       review pull request N of the GitHub repository this\n\
  \               checkout belongs to, read-only, through gh: its head and\n\
  \               base are fetched into refs/remax/pull/N, and no branch\n\
  \               of yours moves"

parseArgs :: [String] -> Either String (Maybe FilePath, Cmd)
parseArgs args0 =
  let
    go mDir args =
      case args of
        ("-C" : dir : rest) -> go (Just dir) rest
        ["-C"] -> Left usage
        ["forget"] -> Right (mDir, Forget Nothing)
        ["forget", path] -> Right (mDir, Forget (Just path))
        ("list" : rest)
          | Just cmd <- listArgs rest -> Right (mDir, cmd)
        ["watching"] -> Right (mDir, Attend)
        ["show", target] -> Right (mDir, ShowConversation (T.pack target))
        ("hunks" : rest) -> Right (mDir, Hunks rest)
        ("tour" : "begin" : rest) -> Right (mDir, TourBegin rest)
        ("tour" : "step" : rest) -> Right (mDir, TourStep rest)
        ("tour" : "clear" : rest) -> Right (mDir, TourClear rest)
        ["--version"] -> Right (mDir, ShowVersion)
        ("do" : rest)
          | not (null rest) -> Right (mDir, RunInSession (T.pack (unwords rest)))
        ("reply" : target : rest)
          | not (null rest) ->
              Right (mDir, Reply (T.pack target) (T.pack (unwords rest)))
        ("note" : place : rest)
          | not (null rest) -> Right (mDir, Note (T.pack place) (T.pack (unwords rest)))
        ("comment" : "--revise" : target : rest)
          | not (null rest) ->
              Right (mDir, Comment (Revise (T.pack target) (T.pack (unwords rest))))
        ["move", target, "--pr", n]
          | Just pull <- GitHub.pullNumber (T.pack n) ->
              Right (mDir, Move (T.pack target) pull)
        ("comment" : hunkId : rest)
          | not (null rest) ->
              Right (mDir, Comment (Draft (T.pack hunkId) (T.pack (unwords rest))))
        _
          | Just marg <- reviewArgs args -> Right (mDir, Review marg)
          | otherwise -> Left usage
  in
    go Nothing args0

reviewArgs :: [String] -> Maybe ModeArg
reviewArgs args =
  case args of
    [] -> Just ArgHead
    ["--unstaged"] -> Just ArgUnstaged
    ["--untracked"] -> Just ArgUntracked
    ["--branch"] -> Just ArgBranch
    ["--base", ref] -> Just (ArgBase (T.pack ref))
    ["--ref", ref] -> Just (ArgRef (T.pack ref) Nothing)
    ["--ref", ref, "--branch"] -> Just (ArgRef (T.pack ref) Nothing)
    ["--branch", "--ref", ref] -> Just (ArgRef (T.pack ref) Nothing)
    ["--ref", ref, "--base", base] -> Just (ArgRef (T.pack ref) (Just (T.pack base)))
    ["--base", base, "--ref", ref] -> Just (ArgRef (T.pack ref) (Just (T.pack base)))
    ["--pr", n] -> fmap ArgPull (GitHub.pullNumber (T.pack n))
    _ -> Nothing

review :: ModeArg -> IO ()
review marg = do
  (root, roots, story) <- reviewStart
  opening <- case marg of
    ArgHead -> pure (plainOpening (Git.VsRef "HEAD"))
    ArgUnstaged -> pure (plainOpening Git.VsIndex)
    ArgUntracked -> pure (plainOpening Git.Untracked)
    ArgBase ref -> do
      ok <- Git.refExists root ref
      Monad.unless ok (Exit.die ("No such ref: " ++ T.unpack ref))
      pure (plainOpening (Git.VsRef ref))
    ArgBranch -> do
      got <- Git.branchRoot root
      case got of
        Left e -> Exit.die (T.unpack e)
        Right sha -> pure (plainOpening (Git.VsRef (T.take 10 sha)))
    ArgRef ref mBase -> do
      ok <- Git.refExists root ref
      Monad.unless ok (Exit.die ("No such ref: " ++ T.unpack ref))
      base <- case mBase of
        Just given -> do
          okBase <- Git.refExists root given
          Monad.unless okBase (Exit.die ("No such ref: " ++ T.unpack given))
          pure given
        Nothing -> do
          got <- Git.branchRootOf root ref
          case got of
            Left e -> Exit.die (T.unpack e)
            Right sha -> pure (T.take 10 sha)
      pure (plainOpening (Git.AtRef (Git.RefReview {Git.rrTip = ref, Git.rrBase = base})))
    ArgPull n -> do
      conn <- Store.openStore
      opened <- openPullArg conn root n
      start <- GitHub.pullStart root opened
      pure
        Opening
          { openingMode = GitHub.startedMode start opened
          , openingPullMeta = Just (GitHub.openedMeta opened)
          }
  UI.runReview (openingMode opening) (openingPullMeta opening) root roots story

data Opening = Opening
  { openingMode :: Git.ReviewMode
  , openingPullMeta :: Maybe GitHub.PullMeta
  }

plainOpening :: Git.ReviewMode -> Opening
plainOpening mode = Opening {openingMode = mode, openingPullMeta = Nothing}

openPullArg :: SQLite.Connection -> FilePath -> Int -> IO GitHub.Opened
openPullArg conn root n = do
  known <- GitHub.cachedRepoName conn root
  SysIO.hPutStrLn SysIO.stderr ("fetching PR #" ++ show n ++ "\8230")
  got <- GitHub.openPull root (GitHub.repoCached known) n
  case got of
    Left failure -> Exit.die (T.unpack (GitHub.failureText failure))
    Right opened -> do
      GitHub.rememberRepoName conn known (GitHub.openedRepo opened)
      pure opened

-- Flags an agent command accepts anywhere in its arguments, pulled out so
-- the rest can be read positionally.
takeFlags :: [String] -> [String] -> Either String (Map.Map String String, [String])
takeFlags valued args =
  case args of
    [] -> Right (Map.empty, [])
    (flag : rest)
      | flag `elem` valued ->
          case rest of
            (val : more) -> fmap (\(m, ps) -> (Map.insert flag val m, ps)) (takeFlags valued more)
            [] -> Left (flag ++ " wants a value")
      | "--" `List.isPrefixOf` flag -> Left ("unknown flag " ++ flag)
      | otherwise -> fmap (\(m, ps) -> (m, flag : ps)) (takeFlags valued rest)

-- The view an agent command works on: the ref flags it was given, else
-- whatever the open session on this repository is looking at, else
-- the working tree against HEAD.
agentMode :: SQLite.Connection -> FilePath -> Map.Map String String -> IO Git.ReviewMode
agentMode conn root flags =
  case (Map.lookup "--pr" flags, Map.lookup "--ref" flags, Map.lookup "--base" flags) of
    (Just n, _, _) -> do
      k <- maybe (Exit.die ("--pr wants a pull request number, not " ++ n)) pure (GitHub.pullNumber (T.pack n))
      viewed <- Store.sessionView conn (T.pack root)
      case viewed >>= Git.modeFromText of
        Just mode@(Git.AtPull pr _) | Git.prNumber pr == k -> pure mode
        _ -> fmap GitHub.openedMode (openPullArg conn root k)
    (Nothing, Just ref, mBase) -> do
      ok <- Git.refExists root (T.pack ref)
      Monad.unless ok (Exit.die ("No such ref: " ++ ref))
      base <- case mBase of
        Just given -> pure (T.pack given)
        Nothing -> do
          got <- Git.branchRootOf root (T.pack ref)
          either (Exit.die . T.unpack) (pure . T.take 10) got
      pure (Git.AtRef (Git.RefReview {Git.rrTip = T.pack ref, Git.rrBase = base}))
    (Nothing, Nothing, Just base) -> pure (Git.VsRef (T.pack base))
    (Nothing, Nothing, Nothing) -> do
      viewed <- Store.sessionView conn (T.pack root)
      pure (Maybe.fromMaybe (Git.VsRef "HEAD") (viewed >>= Git.modeFromText))

refFlags :: [String]
refFlags = ["--ref", "--base", "--pr"]

hunks :: [String] -> IO ()
hunks args = do
  (flags, positional) <- either Exit.die pure (takeFlags refFlags (filter (/= "--json") args))
  let
    asJson = "--json" `elem` args
  Monad.unless (null positional) (Exit.die usage)
  roots <- targetRepos
  conn <- Store.openStore
  cfg <- Config.loadConfig
  listed <- Monad.forM roots $ \root -> do
    mode <- agentMode conn root flags
    found <- Hunks.changeSetHunks cfg mode root
    pure (root, mode, found)
  if asJson
    then TIO.putStrLn (jsonArray (map hunksJson listed))
    else Monad.forM_ listed $ \(root, mode, found) -> do
      Monad.when (length roots > 1) (putStrLn ("\9472\9472 " ++ FilePath.takeFileName root ++ " \9472\9472"))
      TIO.putStrLn ("viewing " <> Git.modeLabel mode)
      Monad.forM_ found $ \h -> do
        TIO.putStrLn (Hunks.hunkId h <> "  " <> T.pack (Hunks.hunkFile h) <> ":" <> T.pack (show (Hunks.hunkLine h)))
        Monad.forM_ (Hunks.hunkLines h) (\l -> TIO.putStrLn ("    " <> l))

hunksJson :: (FilePath, Git.ReviewMode, [Hunks.Hunk]) -> T.Text
hunksJson (root, mode, found) =
  let
    one h =
      jsonObject
        [ ("id", jsonString (Hunks.hunkId h))
        , ("file", jsonString (T.pack (Hunks.hunkFile h)))
        , ("line_no", T.pack (show (Hunks.hunkLine h)))
        , ("lines", jsonArray (map jsonString (Hunks.hunkLines h)))
        ]
  in
    jsonObject
      [ ("repo", jsonString (T.pack root))
      , ("subject", jsonString (Git.subjectText mode))
      , ("view", jsonString (Git.modeLabel mode))
      , ("hunks", jsonArray (map one found))
      ]

tourBegin :: [String] -> IO ()
tourBegin args = do
  (flags, positional) <- either Exit.die pure (takeFlags refFlags args)
  Monad.unless (null positional) (Exit.die usage)
  roots <- targetRepos
  conn <- Store.openStore
  Monad.forM_ roots $ \root -> do
    mode <- agentMode conn root flags
    known <- Store.tourSubjects conn (T.pack root)
    spellings <- Git.sameCommitSubjects root mode known
    Monad.forM_ spellings $ \subject -> do
      Store.clearTour conn (T.pack root) subject
      Store.clearTourRequest conn (T.pack root) subject
    putStrLn ("tour cleared for " ++ T.unpack (Git.modeLabel mode) ++ (if length roots > 1 then " in " ++ FilePath.takeFileName root else ""))

tourStep :: [String] -> IO ()
tourStep args = do
  (flags, positional) <- either Exit.die pure (takeFlags ("--title" : refFlags) args)
  (idsArg, noteWords) <- case positional of
    (ids : rest@(_ : _)) -> pure (ids, rest)
    _ -> Exit.die "tour step wants ID[,ID...] followed by the note"
  let
    ids = filter (not . T.null) (T.splitOn "," (T.pack idsArg))
    note = T.pack (unwords noteWords)
    title = fmap T.pack (Map.lookup "--title" flags)
  roots <- targetRepos
  conn <- Store.openStore
  cfg <- Config.loadConfig
  placed <- Monad.forM roots $ \root -> do
    mode <- agentMode conn root flags
    found <- Hunks.changeSetHunks cfg mode root
    let
      chosen = filter ((`elem` ids) . Hunks.hunkId) found
    pure (root, mode, chosen)
  case filter (\(_, _, chosen) -> not (null chosen)) placed of
    [] -> Exit.die "No hunk with that id in the view; run remax hunks to list them."
    ((root, mode, chosen) : _) -> do
      let
        missing = filter (`notElem` map Hunks.hunkId chosen) ids
      Monad.unless (null missing) (Exit.die ("No hunk with id " ++ T.unpack (T.intercalate ", " missing)))
      seq_ <- Store.appendTourStep conn (T.pack root) (Git.subjectText mode) "tour" title note (concatMap Hunks.hunkKeys chosen)
      putStrLn ("step " ++ show seq_ ++ ": " ++ show (length chosen) ++ " hunk(s)")

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

runInSession :: T.Text -> IO ()
runInSession command = do
  roots <- reposInReach
  delivered <- Remote.send command roots
  case delivered of
    Remote.NobodyOpen -> Exit.die "No remax is open on this checkout here."
    Remote.Answered answer -> do
      let
        said = Remote.replyText answer
      case answer of
        Remote.Ran _ -> Monad.unless (T.null said) (TIO.putStrLn said)
        Remote.Held -> TIO.putStrLn said
        Remote.Refused _ -> Exit.die (T.unpack said)
        Remote.NotHere -> Exit.die (T.unpack said)

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
          Monad.forM_ hits $ \(root, _, rows) -> do
            Store.deleteFiles conn (T.pack root) (List.nub (map snd rows))
            WorkStore.forgetWork conn (T.pack root)
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
    party = listAwaiting options
  roots <- targetRepos
  conn <- Store.openStore
  let
    digest = fmap (listDigest party) (fetchListing conn roots)
    watching =
      T.intercalate
        "\t"
        ( "digest/5"
            : maybe "all" Feedback.partyText party
            : map T.pack roots
        )
  seen <- maybe (pure Nothing) (readDigest watching) (listStateFile options)
  baseline <- case listWhen options of
    ListNow -> pure seen
    ListOnChange -> do
      -- the baseline is what the caller last saw when it has told us,
      -- and only otherwise the moment we started
      before <- maybe digest pure seen
      waitForNews conn roots party digest before
      pure (Just before)
  listing <- fetchListing conn roots
  let
    current = listDigest party listing
    listed = listingConversations listing
    moved = filter (const (moveWanted party)) (listingMoves listing)
  Monad.forM_ (listStateFile options) $ \path ->
    Monad.void
      ( Exception.try (TIO.writeFile path (T.unlines (watching : map (\(k, v) -> k <> "\t" <> v) (Map.toList current)))) ::
          IO (Either Exception.IOException ())
      )
  here <- Directory.getCurrentDirectory
  let
    shown = map (\(r, ls) -> (r, filter (Feedback.awaits party) ls)) listed
    requests = filter (requestWanted party . snd) (listingRequests listing)
    scope = case roots of
      [only] -> only
      _ -> here
  case listFormat options of
    ListJson -> TIO.putStrLn (listJson (T.pack scope) shown requests moved)
    ListText ->
      mapM_ TIO.putStrLn $
        listText
          ListPage
            { pageRoots = roots
            , pageParty = party
            , pageMoves = moved
            , pageRequests = requests
            , pageListed = shown
            , pageIds = Feedback.shortIds (map Feedback.listedId (concatMap snd listed))
            , pageChange = changeFor baseline current
            }

data Listing = Listing
  { listingConversations :: [(FilePath, [Feedback.Listed])]
  , listingRequests :: [(FilePath, Store.TourRequest)]
  , listingMoves :: [Store.ReviewMove]
  }

fetchListing :: SQLite.Connection -> [FilePath] -> IO Listing
fetchListing conn roots = do
  listed <- mapM (\r -> fmap ((,) r) (Feedback.listFor conn r)) roots
  asked <- mapM (\r -> fmap (map ((,) r)) (Store.tourRequests conn (T.pack r))) roots
  moved <- reviewMoves conn roots
  pure
    Listing
      { listingConversations = listed
      , listingRequests = concat asked
      , listingMoves = moved
      }

listDigest :: Maybe Feedback.Party -> Listing -> Map.Map T.Text T.Text
listDigest party listing =
  let
    convs =
      Map.fromListWith
        (++)
        (map (\(i, line) -> (conversationKey i, [line])) (concatMap (Feedback.digestOf party . snd) (listingConversations listing)))
    others =
      Set.toList (requestDigest party (listingRequests listing))
        ++ map moveJson (filter (const (moveWanted party)) (listingMoves listing))
  in
    Map.union
      (Map.map (Key.sha256Hex . T.intercalate "\n" . List.sort) convs)
      (Map.fromList (map (\t -> (Key.sha256Hex t, "")) others))

conversationKey :: T.Text -> T.Text
conversationKey i = Key.sha256Hex ("conversation\t" <> i)

readDigest :: T.Text -> FilePath -> IO (Maybe (Map.Map T.Text T.Text))
readDigest watching path = do
  got <- Exception.try (TIO.readFile path) :: IO (Either Exception.IOException T.Text)
  let
    entry line = case T.splitOn "\t" line of
      [k, v] -> Just (k, v)
      _ -> Nothing
  pure $ case got of
    Left _ -> Nothing
    Right raw -> case T.breakOn "\n" raw of
      (recorded, rest)
        | recorded == watching -> Just (Map.fromList (Maybe.mapMaybe entry (T.lines (T.drop 1 rest))))
      -- a fingerprint of somewhere else says nothing about here,
      -- and believing one is worse than having none: this
      -- repository's listing will not match it, so the wait
      -- returns at once with nothing to report. Measure from now
      -- instead. A file written before the scope was recorded
      -- reads as somewhere else, and costs one such measurement.
      _ -> Nothing

waitForNews :: SQLite.Connection -> [FilePath] -> Maybe Feedback.Party -> IO (Map.Map T.Text T.Text) -> Map.Map T.Text T.Text -> IO ()
waitForNews conn roots party digest before =
  let
    news now =
      case party of
        Nothing -> now /= before
        Just _ -> not (now `Map.isSubmapOf` before)
    settle = do
      now <- digest
      -- a flag whose note is still being typed is not news yet: the
      -- verdict is in the store seconds before the sentence saying
      -- what is wrong with it, and returning on it means answering
      -- before being told
      busy <- Store.composingNow conn (map T.pack roots)
      Monad.unless (news now && not busy) $ do
        -- re-announced rather than announced: the timestamp is what
        -- tells a session somebody is still here, so an agent that
        -- dies stops saying so without having to say anything
        Store.markPresence Store.Waiting (map T.pack roots)
        Concurrent.threadDelay waitPollUs
        settle
  in
    settle `Exception.finally` Store.clearWaiting

changeFor :: Maybe (Map.Map T.Text T.Text) -> Map.Map T.Text T.Text -> T.Text -> FeedbackText.Change
changeFor baseline current i =
  let
    key = conversationKey i
  in
    case baseline of
      Nothing -> FeedbackText.Unchanged
      Just before -> case Map.lookup key before of
        Nothing -> FeedbackText.New
        seen
          | seen == Map.lookup key current -> FeedbackText.Unchanged
          | otherwise -> FeedbackText.Changed

data ListPage = ListPage
  { pageRoots :: [FilePath]
  , pageParty :: Maybe Feedback.Party
  , pageMoves :: [Store.ReviewMove]
  , pageRequests :: [(FilePath, Store.TourRequest)]
  , pageListed :: [(FilePath, [Feedback.Listed])]
  , pageIds :: Map.Map T.Text T.Text
  , pageChange :: T.Text -> FeedbackText.Change
  }

listText :: ListPage -> [T.Text]
listText page =
  let
    roots = pageRoots page
    inRepo repo = if length roots > 1 then " in " <> T.pack (FilePath.takeFileName repo) else ""
    moveLine m =
      "moved  PR #"
        <> T.pack (show (Store.movePull m))
        <> " review is in "
        <> Store.moveTo m
        <> "  ("
        <> Store.moveAt m
        <> ")"
    requestLine (repo, request) =
      "tour   "
        <> maybe (Store.requestMode request) Git.modeLabel (Git.modeFromText (Store.requestMode request))
        <> inRepo repo
        <> (if T.null (Store.requestPrompt request) then "" else ": " <> Store.requestPrompt request)
        <> "  ("
        <> requestFlags request
        <> ")"
    nonEmpty = filter (not . null . snd) (pageListed page)
    conversation l =
      FeedbackText.listedText
        FeedbackText.Compact
        (Map.findWithDefault (Feedback.listedId l) (Feedback.listedId l) (pageIds page))
        (pageChange page (Feedback.listedId l))
        l
    repoBlock (repo, described) =
      (if length roots > 1 then ["\9472\9472 " <> T.pack (FilePath.takeFileName repo) <> " \9472\9472"] else [])
        ++ List.intercalate [""] (map conversation described)
    heading = map moveLine (pageMoves page) ++ map requestLine (pageRequests page)
    empty = case pageParty page of
      Nothing -> "No conversations recorded for this repository."
      Just who -> "No conversations awaiting " <> Feedback.partyText who <> "."
  in
    case List.intercalate [""] (filter (not . null) (heading : map repoBlock nonEmpty)) of
      [] -> [empty]
      rendered -> rendered

-- A tour request is the reviewer asking the agent for something, so it
-- is news for an agent and for anyone listing everything.
requestWanted :: Maybe Feedback.Party -> Store.TourRequest -> Bool
requestWanted party _ = moveWanted party

moveWanted :: Maybe Feedback.Party -> Bool
moveWanted party =
  case party of
    Nothing -> True
    Just Feedback.AwaitingAgent -> True
    Just _ -> False

-- The latest move of each review group among these checkouts: a group
-- is a checkout and the pull request worktrees made from it, and every
-- member reports the same move.
reviewMoves :: SQLite.Connection -> [FilePath] -> IO [Store.ReviewMove]
reviewMoves conn roots = do
  found <- mapM (Store.reviewMoveFor conn . T.pack) roots
  pure (List.nub (Maybe.catMaybes found))

moveJson :: Store.ReviewMove -> T.Text
moveJson m =
  jsonObject
    [ ("repo", jsonString (Store.moveRepo m))
    , ("pull", T.pack (show (Store.movePull m)))
    , ("to", jsonString (Store.moveTo m))
    , ("moved_at", jsonString (Store.moveAt m))
    ]

requestDigest :: Maybe Feedback.Party -> [(FilePath, Store.TourRequest)] -> Set.Set T.Text
requestDigest party asked =
  Set.fromList (map (uncurry requestJson) (filter (requestWanted party . snd) asked))

-- The ref flags that reproduce the requested view for hunks and tour.
requestFlags :: Store.TourRequest -> T.Text
requestFlags request =
  case Git.modeFromText (Store.requestMode request) of
    Just (Git.AtRef rr) -> "--ref " <> Git.rrTip rr <> " --base " <> Git.rrBase rr
    Just (Git.VsRef ref) -> "--base " <> ref
    Just (Git.AtPull pr _) -> "--pr " <> T.pack (show (Git.prNumber pr))
    _ -> "working tree"

requestJson :: FilePath -> Store.TourRequest -> T.Text
requestJson repo request =
  let
    (ref, base, pull) = case Git.modeFromText (Store.requestMode request) of
      Just (Git.AtRef rr) -> (jsonString (Git.rrTip rr), jsonString (Git.rrBase rr), "null")
      Just (Git.VsRef r) -> ("null", jsonString r, "null")
      Just (Git.AtPull pr _) -> ("null", "null", T.pack (show (Git.prNumber pr)))
      _ -> ("null", "null", "null")
  in
    jsonObject
      [ ("repo", jsonString (T.pack repo))
      , ("view", jsonString (maybe (Store.requestMode request) Git.modeLabel (Git.modeFromText (Store.requestMode request))))
      , ("ref", ref)
      , ("base", base)
      , ("pull", pull)
      , ("prompt", jsonString (Store.requestPrompt request))
      , ("asked_at", jsonString (Store.requestAskedAt request))
      ]

-- An agent's post on a conversation: ID is a conversation's id from
-- `list`, a GitHub thread's or a note's own. From a directory
-- of checkouts the id is offered to each, and the one that holds it
-- answers.
reply :: T.Text -> T.Text -> IO ()
reply given text = do
  roots <- targetRepos
  conn <- Store.openStore
  target <- resolveTarget conn roots given
  attached <- mapM (\r -> fmap ((,) r) (Feedback.replyTo conn r target text)) roots
  let
    replied (repo, outcome) = case outcome of
      Feedback.Replied n landing -> Just (repo, n, landing)
      Feedback.NoSuchConversation -> Nothing
  case Maybe.mapMaybe replied attached of
    [] -> Exit.die "No such conversation recorded for this repository."
    hits ->
      Monad.forM_ hits $ \(repo, n, landing) ->
        putStrLn
          ( "reply attached to "
              ++ show n
              ++ " line(s) "
              ++ scopeWords landing
              ++ (if length roots > 1 then " in " ++ FilePath.takeFileName repo else "")
          )

resolveTarget :: SQLite.Connection -> [FilePath] -> T.Text -> IO T.Text
resolveTarget conn roots given = do
  listed <- fmap concat (mapM (Feedback.listFor conn) roots)
  fmap (Maybe.fromMaybe given) (resolveIn listed given)

resolveIn :: [Feedback.Listed] -> T.Text -> IO (Maybe T.Text)
resolveIn listed given =
  case Feedback.resolveId (map Feedback.listedId listed) given of
    Feedback.ResolvedId full -> pure (Just full)
    Feedback.UnknownId -> pure Nothing
    Feedback.AmbiguousId ids ->
      Exit.die (T.unpack (T.unlines (("More than one conversation id starts with " <> given <> ":") : map ("  " <>) ids)))

showConversation :: T.Text -> IO ()
showConversation given = do
  roots <- targetRepos
  conn <- Store.openStore
  listed <- fmap concat (mapM (Feedback.listFor conn) roots)
  resolved <- resolveIn listed given
  case resolved of
    Nothing -> Exit.die "No such conversation recorded for this repository."
    Just full -> do
      let
        repoLine l = if length roots > 1 then ["repo " <> T.pack (Feedback.listedRepo l)] else []
        whole l = repoLine l ++ FeedbackText.listedText FeedbackText.Whole full FeedbackText.Unchanged l
      mapM_ TIO.putStrLn (List.intercalate [""] (map whole (filter ((== full) . Feedback.listedId) listed)))

scopeWords :: WorkStore.ConversationScope -> String
scopeWords = T.unpack . FeedbackText.scopeText

move :: T.Text -> Int -> IO ()
move given pull = do
  roots <- targetRepos
  conn <- Store.openStore
  target <- resolveTarget conn roots given
  outcomes <- mapM (\r -> WorkStore.moveDraft conn (T.pack r) (Conv.LocalId target) pull) roots
  if WorkStore.Moved `elem` outcomes
    then putStrLn ("draft moved to PR #" ++ show pull)
    else Exit.die "No such draft comment recorded for this repository; only drafts move."

openNote :: T.Text -> T.Text -> IO ()
openNote place text = do
  range <- maybe (Exit.die "note wants PATH:LINE or PATH:FIRST-LAST, such as src/Foo.hs:40-52") pure (LineRange.parseLineRange place)
  path <- maybe (Exit.die "note wants a path before the lines, such as src/Foo.hs:40-52") pure (LineRange.lrPath range)
  roots <- targetRepos
  conn <- Store.openStore
  cfg <- Config.loadConfig
  let
    attempt pending =
      case pending of
        [] -> Exit.die ("No changed lines at " ++ T.unpack (LineRange.lineRangeText range) ++ " in the view; a note sits on lines remax hunks lists.")
        (root : rest) -> do
          mode <- agentMode conn root Map.empty
          hs <- Hunks.changeSetHunks cfg mode root
          cached <- RemoteCache.readRemote (T.pack root)
          noted <- Feedback.noteOnLines conn root (fmap RemoteCache.cachedPull cached) hs path (LineRange.lrLines range) text
          case noted of
            Feedback.NoChangedLines -> attempt rest
            Feedback.Noted cid spanLines -> do
              let
                shown = T.unpack (Conv.conversationIdText cid)
              putStrLn
                ( "note "
                    ++ shown
                    ++ " on "
                    ++ show (length spanLines)
                    ++ " changed line(s) of "
                    ++ T.unpack path
                    ++ "; remax do :go "
                    ++ shown
                    ++ " takes the reviewer there"
                )
  attempt roots

-- A comment the agent drafts for the reviewer on a hunk, or new text it
-- proposes for a conversation; neither reaches GitHub until the
-- reviewer takes it and submits.
comment :: CommentArgs -> IO ()
comment args = do
  roots <- targetRepos
  conn <- Store.openStore
  case args of
    Revise given text -> do
      target <- resolveTarget conn roots given
      outcomes <- mapM (\r -> Feedback.proposeOn conn r target text) roots
      let
        proposed outcome = case outcome of
          Feedback.Replied _ landing -> Just landing
          Feedback.NoSuchConversation -> Nothing
      case Maybe.mapMaybe proposed outcomes of
        [] -> Exit.die "No such conversation recorded for this repository."
        (landing : _) -> putStrLn ("proposal recorded " ++ scopeWords landing ++ "; the reviewer takes it with Enter")
    Draft hunkId text -> do
      cfg <- Config.loadConfig
      found <- Monad.forM roots $ \root -> do
        mode <- agentMode conn root Map.empty
        hs <- Hunks.changeSetHunks cfg mode root
        pure (root, List.find ((== hunkId) . Hunks.hunkId) hs)
      case Maybe.mapMaybe (\(root, h) -> fmap ((,) root) h) found of
        [] -> Exit.die "No such hunk in the view; remax hunks lists them."
        ((root, h) : _) -> do
          cached <- RemoteCache.readRemote (T.pack root)
          drafted <- Feedback.commentOnHunk conn root (fmap RemoteCache.cachedPull cached) h text
          case drafted of
            Feedback.EmptyHunk -> Exit.die "That hunk has no lines to comment on."
            Feedback.Drafted lid landing ->
              putStrLn
                ( "comment "
                    ++ T.unpack (Conv.localIdToText lid)
                    ++ " proposed on "
                    ++ Hunks.hunkFile h
                    ++ ":"
                    ++ show (Hunks.hunkLine h)
                    ++ " "
                    ++ scopeWords landing
                    ++ "; the reviewer takes it with Enter"
                )

-- JSON output ------------------------------------------------------------------

listJson :: T.Text -> [(FilePath, [Feedback.Listed])] -> [(FilePath, Store.TourRequest)] -> [Store.ReviewMove] -> T.Text
listJson scope listed requests moved =
  jsonObject
    [ ("repo", jsonString scope)
    , ("review_moves", jsonArray (map moveJson moved))
    , ("tour_requests", jsonArray (map (uncurry requestJson) requests))
    , ("conversations", jsonArray (map (Json.encode . Feedback.listedJson) (concatMap snd listed)))
    ]

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
