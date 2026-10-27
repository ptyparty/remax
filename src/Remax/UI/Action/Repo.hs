{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Repo
  ( switchRepo
  , rescanRepos
  , describeRepo
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.Int qualified as DataInt
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Remax.Config qualified as Config
import Remax.Conversation qualified as Conv
import Remax.Git qualified as Git
import Remax.Store qualified as Store
import Remax.UI.Action.Pull qualified as PullAction
import Remax.UI.Action.Rebase qualified as Rebase
import Remax.UI.Core.Jobs qualified as Jobs
import Remax.UI.Core.Tour qualified as Tour
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Verdict qualified as Verdict
import Remax.UI.Model.Load qualified as Load
import Remax.UI.Model.Nav qualified as Nav
import Remax.UI.Model.Watch qualified as Watch
import Remax.UI.Model.Work qualified as WorkModel
import System.FilePath qualified as FilePath

-- Remember where we are before stepping away.
currentPlace :: Types.St -> Maybe (FilePath, Types.RepoPlace)
currentPlace st = do
  fv <- Types.currentFileMay st
  pure
    ( Types.stRoot st
    , Types.RepoPlace
        { Types.rpMode = Types.stMode st
        , Types.rpFile = Types.fvPath fv
        , Types.rpHunk = Types.stCurH st
        , Types.rpWalk = Types.stWalk st
        , Types.rpOpened = Types.stOpened st
        , Types.rpAnchor = Types.stAnchor st
        }
    )

-- Point the session at another repository, keeping the one we leave in
-- stPlaces so coming back lands where we left off. Everything else is
-- read fresh: the verdicts and threads are that repository's, the rebase
-- state comes back out of its .git, and the buffers are re-read from
-- disk because an agent may have been working here in the meantime.
switchRepo :: FilePath -> Brick.EventM Types.Name Types.St ()
switchRepo target = do
  st <- State.get
  if target == Types.stRoot st
    then Types.setMsg ("already in " <> T.pack (FilePath.takeFileName target))
    else
      if any ("opening " `T.isPrefixOf`) (Types.stBusy st)
        then Types.setMsg "still opening the last repository"
        else do
          let
            remembered = Map.lookup target (Types.stPlaces st)
            mode = maybe (Git.VsRef "HEAD") Types.rpMode remembered
            conn = Types.stConn st
            saved = case currentPlace st of
              Just (from, place) -> Map.insert from place (Types.stPlaces st)
              Nothing -> Types.stPlaces st
          let
            work = do
              verdicts <- Store.loadVerdicts conn (T.pack target)
              threads <- Store.loadThreads conn (T.pack target)
              own <- WorkModel.loadFor conn target mode
              groups <- Store.loadNoteGroups conn (T.pack target)
              rebase <- Git.rebaseStatus target
              (branch, headAt) <- Git.subjectPlace (Types.rebaseEffMode rebase mode) target
              Tour.recordView conn target (Types.rebaseEffMode rebase mode)
              tour <- Tour.loadTourFor conn target (Types.rebaseEffMode rebase mode)
              files <-
                fmap
                  (map (Verdict.recountFile verdicts threads (Conv.conversations Conv.emptyRemote own)))
                  (Load.loadAll (Types.stConfig st) (Types.stHls st) (Types.rebaseEffMode rebase mode) target)
              -- the watch manager is shared, so following a second repository
              -- costs watch descriptors rather than another inotify instance
              gitDirs <- Git.gitStateWatchDirs target
              mapM_ (Types.stWatch st) gitDirs
              mapM_ (Types.stWatch st . Types.fileDir target . Types.fvPath) files
              -- and the rest of it in the background, so the first change in a
              -- directory nothing is changed in yet is noticed here too
              Watch.watchTracked (Types.stWatch st) target
              pure (verdicts, threads, own, groups, rebase, branch, headAt, files, tour)
          Jobs.startJob ("opening " <> T.pack (FilePath.takeFileName target)) work $ \outcome ->
            case outcome of
              Left err -> Jobs.jobFailed "opening" err
              Right loaded -> do
                leaving <- State.get
                MonadIO.liftIO (Store.finishPass conn (Types.stPassId leaving))
                passId <- MonadIO.liftIO (Store.beginPass conn (T.pack target) (Git.modeLabel mode))
                arriveAt target remembered saved mode passId loaded

arriveAt ::
  FilePath ->
  Maybe Types.RepoPlace ->
  Map.Map FilePath Types.RepoPlace ->
  Git.ReviewMode ->
  DataInt.Int64 ->
  ( Map.Map T.Text Store.Verdict
  , Map.Map T.Text [Store.Post]
  , Conv.OwnWork
  , Map.Map T.Text T.Text
  , Maybe Git.RebaseStatus
  , Maybe T.Text
  , Maybe (T.Text, T.Text)
  , [Types.FileView]
  , [Store.TourStep]
  ) ->
  Brick.EventM Types.Name Types.St ()
arriveAt target remembered saved mode passId loaded = do
  let
    (verdicts, threads, own, groups, rebase, branch, headAt, files, tour) = loaded
  State.modify $ \s ->
    WorkModel.withConversations . Tour.installTour tour $
      s
        { Types.stRoot = target
        , Types.stEpoch = Types.stEpoch s + 1
        , Types.stRefreshing = False
        , Types.stGitQueued = False
        , Types.stReloading = Map.empty
        , Types.stHighlighting = Nothing
        , Types.stFiles = files
        , Types.stVerdicts = verdicts
        , Types.stThreads = threads
        , Types.stWork = own
        , Types.stCountedVerdicts = verdicts
        , Types.stCountedThreads = threads
        , Types.stNoteGroups = groups
        , Types.stRebase = rebase
        , Types.stBranch = branch
        , Types.stHead = headAt
        , Types.stPassId = passId
        , Types.stMode = mode
        , Types.stTreeMode = case Git.subjectOf mode of
            Git.WorkingTree -> mode
            _ -> Git.VsRef "HEAD"
        , Types.stPlaces = saved
        , -- it listed the repository we just left; bringing it back
          -- here would offer files that are not in front of us
          Types.stLastSelector = Nothing
        , Types.stCurF = 0
        , Types.stCurH = 0
        , Types.stWritten = Set.empty
        , Types.stAltFile = Nothing
        , Types.stAnchor = maybe Nothing Types.rpAnchor remembered
        , Types.stOpened = maybe Set.empty Types.rpOpened remembered
        , Types.stWalk = maybe (Types.stWalk s) Types.rpWalk remembered
        , Types.stGrepNav = Nothing
        , Types.stCmdFiles = []
        , Types.stCmdDirs = []
        , Types.stCmdRefs = []
        , Types.stPullMeta = Nothing
        , Types.stPullPoll = Types.noPullPoll
        , Types.stBand = Nothing
        , Types.stUnfolded = Set.empty
        , Types.stConfirm = Nothing
        , Types.stCiting = Nothing
        , Types.stBaseAt = Nothing
        }
  case remembered of
    Just place -> restorePlace place
    Nothing -> do
      arrived <- State.get
      case Verdict.firstUnrespondedOrPending arrived of
        Just (f, h) -> Nav.parkAnchor f h
        Nothing -> pure ()
  Types.setMsg (repoArrivalMsg target rebase files)
  PullAction.refreshPullMeta

-- Land back on the file and hunk we left, when they are still there;
-- otherwise fall back to wherever a fresh session would have opened.
restorePlace :: Types.RepoPlace -> Brick.EventM Types.Name Types.St ()
restorePlace place = do
  st <- State.get
  case List.findIndex ((== Types.rpFile place) . Types.fvPath) (Types.stFiles st) of
    Just f -> Nav.parkM f (Types.rpHunk place)
    Nothing ->
      case Verdict.firstUnrespondedOrPending st of
        Just (f, h) -> Nav.parkAnchor f h
        Nothing -> pure ()

repoArrivalMsg :: FilePath -> Maybe Git.RebaseStatus -> [Types.FileView] -> T.Text
repoArrivalMsg target rebase files =
  let
    name = T.pack (FilePath.takeFileName target)
    hunks = length (concatMap Types.fvRuns files)
    what
      | null files = "nothing to review"
      | otherwise = Types.tshow hunks <> " hunk(s) in " <> Types.tshow (length files) <> " file(s)"
    mid = case rebase of
      Just rb -> " · rebasing, " <> Rebase.rebaseStopMsg rb
      Nothing -> ""
  in
    name <> " · " <> what <> mid

-- The project tree ------------------------------------------------------------

-- Pick which repository the session is looking at. Each row costs two
-- git calls and a diff, so the picker opens at once with what was last
-- known about each checkout and the rows fill in as the answers land,
-- described in the background a few repositories at a time. Worth it:
-- choosing between five repos by name alone tells you nothing about
-- which one needs you.
-- Which checkouts the story holds, asked again. A story directory is a
-- working set rather than a fixed one — a repository gets cloned into it
-- partway through a review, or tidied away — and the session was going on
-- believing the list it was handed at startup.
--
-- Asked at the two moments the list is about to be used rather than kept
-- current in the background: a directory listing is cheap, it cannot go
-- stale between here and the answer, and nothing else in the session
-- consults the list unprompted.
rescanRepos :: Brick.EventM Types.Name Types.St ()
rescanRepos = do
  st <- State.get
  case Types.stStory st of
    Nothing -> pure ()
    Just dir -> do
      found <- MonadIO.liftIO (Git.childRepos dir)
      Monad.unless (found == Types.stRoots st) (State.modify (\s -> s {Types.stRoots = found}))

-- How much of a repository is left to read, for the switcher. Knowing a
-- repository has changes says nothing about whether they have been
-- looked at, which is the thing worth knowing before switching to one.
--
-- Verdicts are keyed by line content, so answering this for a repository
-- the session is not in means actually diffing it. That is affordable
-- for the handful of checkouts a story holds and not for something the
-- size of a vendored tree, so past a point the count is not attempted
-- and the file count stands in for it.
repoStatusMaxFiles :: Int
repoStatusMaxFiles = 40

repoHunkCount :: Types.St -> FilePath -> IO (Int, Int)
repoHunkCount st root
  -- the session already has this one loaded, down to its verdicts
  | root == Types.stRoot st =
      let
        runs = concatMap Types.fvRuns (Types.stFiles st)
      in
        pure
          ( length (filter (not . Verdict.runPending (Types.stVerdicts st)) runs)
          , length runs
          )
  | otherwise = do
      verdicts <- Store.loadVerdicts (Types.stConn st) (T.pack root)
      files <- Load.loadAll (Types.stConfig st) (Types.stHls st) (Git.VsRef "HEAD") root
      let
        runs = concatMap Types.fvRuns files
      pure (length (filter (not . Verdict.runPending verdicts) runs), length runs)

describeRepo :: Types.St -> FilePath -> IO (FilePath, [T.Text])
describeRepo st root = do
  branch <- Git.currentBranch root
  rebase <- Git.rebaseStatus root
  changed <- Git.reviewFiles (Git.VsRef "HEAD") (Config.cfgUntracked (Types.stConfig st)) root
  counted <-
    if null changed || length changed > repoStatusMaxFiles
      then pure Nothing
      else fmap Just (repoHunkCount st root)
  let
    onBranch = case branch of
      Just b -> b
      Nothing -> "detached"
    reviewed = case counted of
      Just (done, total)
        | total > 0 ->
            Types.tshow done <> "/" <> Types.tshow total <> " hunks"
      _ -> Types.tshow (length changed) <> " changed"
    state = case rebase of
      Just rb
        | Git.rebaseConflicted rb -> "CONFLICT"
        | otherwise -> "rebasing " <> Git.rebaseProgress rb
      Nothing
        | null changed -> "clean"
        | otherwise -> reviewed
  pure (root, [T.pack (FilePath.takeFileName root), onBranch, state])
