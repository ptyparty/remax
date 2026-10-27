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

import Database.SQLite.Simple qualified as SQLite
import Paths_remax qualified as Paths
import Remax.Config qualified as Config
import Remax.Git qualified as Git
import Remax.GitHub qualified as GitHub
import Remax.Hunks qualified as Hunks
import Remax.Punch qualified as Punch
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
  | Reply T.Text T.Text
  | Attend
  | Hunks [String]
  | TourBegin [String]
  | TourStep [String]
  | TourClear [String]
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
        Hunks rest -> hunks rest
        TourBegin rest -> tourBegin rest
        TourStep rest -> tourStep rest
        TourClear rest -> tourBegin rest
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
  \       remax [-C DIR] --ref REF [--base BASE | --branch]\n\
  \       remax [-C DIR] --pr N\n\
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
  \       remax [-C DIR] hunks [--json] [--ref REF [--base BASE] | --base REF | --pr N]\n\
  \                                     list the change set as the session sees\n\
  \                                     it, one id per hunk; with no ref flags,\n\
  \                                     whatever the open session is viewing\n\
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
        ("hunks" : rest) -> Right (mDir, Hunks rest)
        ("tour" : "begin" : rest) -> Right (mDir, TourBegin rest)
        ("tour" : "step" : rest) -> Right (mDir, TourStep rest)
        ("tour" : "clear" : rest) -> Right (mDir, TourClear rest)
        ["--version"] -> Right (mDir, ShowVersion)
        ("reply" : target : rest)
          | not (null rest) ->
              Right (mDir, Reply (T.pack target) (T.pack (unwords rest)))
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
      pure
        Opening
          { openingMode = GitHub.openedMode opened
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
    fmt = listFormat options
    while = listWhen options
    stateFile = listStateFile options
  roots <- targetRepos
  conn <- Store.openStore
  let
    digest = do
      rows <- mapM (Store.punchRows conn . T.pack) roots
      asked <- mapM (\r -> fmap (map ((,) r)) (Store.tourRequests conn (T.pack r))) roots
      pure (Set.union (reviewerDigest (listAwaiting options) (concat rows)) (requestDigest (listAwaiting options) (concat asked)))
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
            sources = List.nub (map (\c -> (Punch.conversationRef c, Punch.conversationFile c)) shown)
          contents <- mapM (sourceLines repo) sources
          let
            fileLines = Map.fromList (zip sources contents)
          pure (repo, map (describe fileLines) shown)
      )
      found
  asked <- mapM (\r -> fmap (map ((,) r)) (Store.tourRequests conn (T.pack r))) roots
  let
    asJson = fmt == ListJson
    subjects = Punch.commitSubjects (concatMap snd found)
    nonEmpty = filter (not . null . snd) listed
    requests = filter (requestWanted (listAwaiting options) . snd) (concat asked)
    scope = case roots of
      [only] -> only
      _ -> here
  if asJson
    then TIO.putStrLn (punchJson (T.pack scope) subjects listed requests)
    else do
      Monad.forM_ requests $ \(repo, request) ->
        TIO.putStrLn
          ( "tour requested for "
              <> maybe (Store.requestMode request) Git.modeLabel (Git.modeFromText (Store.requestMode request))
              <> (if length roots > 1 then " in " <> T.pack (FilePath.takeFileName repo) else "")
              <> (if T.null (Store.requestPrompt request) then "" else ": " <> Store.requestPrompt request)
              <> "  ("
              <> requestFlags request
              <> ")"
          )
      if null nonEmpty
        then Monad.when (null requests) . putStrLn $
          case listAwaiting options of
            Nothing -> "No flagged or questioned lines recorded for this repository."
            Just party -> "No conversations awaiting " ++ T.unpack (Store.awaitingText party) ++ "."
        else Monad.forM_ nonEmpty $ \(repo, described) -> do
          -- name the repository only when there is more than one in
          -- play, so a single-repo listing reads exactly as before
          Monad.when (length roots > 1) $
            putStrLn ("── " ++ FilePath.takeFileName repo ++ " ──")
          printConversations subjects described

-- A tour request is the reviewer asking the agent for something, so it
-- is news for an agent and for anyone listing everything.
requestWanted :: Maybe Store.Awaiting -> Store.TourRequest -> Bool
requestWanted party _ =
  case party of
    Nothing -> True
    Just Store.AwaitingAgent -> True
    Just _ -> False

requestDigest :: Maybe Store.Awaiting -> [(FilePath, Store.TourRequest)] -> Set.Set T.Text
requestDigest party asked =
  Set.fromList (map (uncurry requestJson) (filter (requestWanted party . snd) asked))

-- The ref flags that reproduce the requested view for hunks and tour.
requestFlags :: Store.TourRequest -> T.Text
requestFlags request =
  case Git.modeFromText (Store.requestMode request) of
    Just (Git.AtRef rr) -> "--ref " <> Git.rrTip rr <> " --base " <> Git.rrBase rr
    Just (Git.VsRef ref) -> "--base " <> ref
    _ -> "working tree"

requestJson :: FilePath -> Store.TourRequest -> T.Text
requestJson repo request =
  let
    (ref, base) = case Git.modeFromText (Store.requestMode request) of
      Just (Git.AtRef rr) -> (jsonString (Git.rrTip rr), jsonString (Git.rrBase rr))
      Just (Git.VsRef r) -> ("null", jsonString r)
      _ -> ("null", "null")
  in
    jsonObject
      [ ("repo", jsonString (T.pack repo))
      , ("view", jsonString (maybe (Store.requestMode request) Git.modeLabel (Git.modeFromText (Store.requestMode request))))
      , ("ref", ref)
      , ("base", base)
      , ("prompt", jsonString (Store.requestPrompt request))
      , ("asked_at", jsonString (Store.requestAskedAt request))
      ]

data Described = Described
  { describedConversation :: Punch.Conversation
  , describedLinesInFile :: Maybe Punch.LinesInFile
  }

describe :: Map.Map (Maybe T.Text, T.Text) [T.Text] -> Punch.Conversation -> Described
describe fileLines conversation =
  Described
    { describedConversation = conversation
    , describedLinesInFile =
        Punch.linesInFile
          ( Map.findWithDefault
              []
              (Punch.conversationRef conversation, Punch.conversationFile conversation)
              fileLines
          )
          (Punch.conversationLines conversation)
    }

sourceLines :: FilePath -> (Maybe T.Text, T.Text) -> IO [T.Text]
sourceLines repo (mRef, file) =
  case mRef of
    Nothing -> fmap T.lines (Git.readFileOr "" (repo </> T.unpack file))
    Just ref -> do
      got <- Git.refContent repo ref (T.unpack file)
      pure $ case got of
        Just (Git.RefText ls) -> ls
        _ -> []

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
    Monad.forM_ (Punch.conversationRef conversation) $ \ref ->
      TIO.putStrLn ("    ref: " <> ref)

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
punchJson :: T.Text -> Map.Map T.Text T.Text -> [(FilePath, [Described])] -> [(FilePath, Store.TourRequest)] -> T.Text
punchJson scope subjects listed requests =
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
        , ("ref", maybe "null" jsonString (Punch.conversationRef conversation))
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
      , ("tour_requests", jsonArray (map (uncurry requestJson) requests))
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
