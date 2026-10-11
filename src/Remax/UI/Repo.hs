{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Repo
  ( switchRepo
  , rescanRepos
  , describeRepo
  ) where

import Brick
  ( EventM
  )
import Control.Monad
  ( unless
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
  ( get
  , modify
  )
import Data.Int (Int64)
import Data.List
  ( findIndex
  )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Remax.Config
import Remax.Git
  ( RebaseStatus (..)
  , ReviewMode (..)
  , childRepos
  , currentBranch
  , gitStateWatchDirs
  , headCommit
  , modeLabel
  , rebaseStatus
  , reviewFiles
  )
import Remax.Store
import Remax.UI.Jobs
import Remax.UI.Load
import Remax.UI.Nav
import Remax.UI.Rebase
import Remax.UI.Types
import Remax.UI.Verdict
import Remax.UI.Watch
import System.FilePath
  ( takeFileName
  )

-- Remember where we are before stepping away.
currentPlace :: St -> Maybe (FilePath, RepoPlace)
currentPlace st = do
  fv <- currentFileMay st
  pure
    ( stRoot st
    , RepoPlace
        { rpMode = stMode st
        , rpFile = fvPath fv
        , rpHunk = stCurH st
        , rpOnlyPending = stOnlyPending st
        , rpRunFilter = stRunFilter st
        , rpOpened = stOpened st
        , rpAnchor = stAnchor st
        }
    )

-- Point the session at another repository, keeping the one we leave in
-- stPlaces so coming back lands where we left off. Everything else is
-- read fresh: the verdicts and threads are that repository's, the rebase
-- state comes back out of its .git, and the buffers are re-read from
-- disk because an agent may have been working here in the meantime.
switchRepo :: FilePath -> EventM Name St ()
switchRepo target = do
  st <- get
  if target == stRoot st
    then setMsg ("already in " <> T.pack (takeFileName target))
    else
      if any ("opening " `T.isPrefixOf`) (stBusy st)
        then setMsg "still opening the last repository"
        else do
          let
            remembered = Map.lookup target (stPlaces st)
            mode = maybe (VsRef "HEAD") rpMode remembered
            conn = stConn st
            saved = case currentPlace st of
              Just (from, place) -> Map.insert from place (stPlaces st)
              Nothing -> stPlaces st
          let
            work = do
              verdicts <- loadVerdicts conn (T.pack target)
              threads <- loadThreads conn (T.pack target)
              groups <- loadNoteGroups conn (T.pack target)
              rebase <- rebaseStatus target
              branch <- currentBranch target
              headAt <- headCommit target
              files <-
                fmap
                  (map (recountFile verdicts threads))
                  (loadAll (stConfig st) (stHls st) (rebaseEffMode rebase mode) target)
              -- the watch manager is shared, so following a second repository
              -- costs watch descriptors rather than another inotify instance
              gitDirs <- gitStateWatchDirs target
              mapM_ (stWatch st) gitDirs
              mapM_ (stWatch st . fileDir target . fvPath) files
              -- and the rest of it in the background, so the first change in a
              -- directory nothing is changed in yet is noticed here too
              watchTracked (stWatch st) target
              pure (verdicts, threads, groups, rebase, branch, headAt, files)
          startJob ("opening " <> T.pack (takeFileName target)) work $ \outcome ->
            case outcome of
              Left err -> jobFailed "opening" err
              Right loaded -> do
                leaving <- get
                liftIO (finishPass conn (stPassId leaving))
                passId <- liftIO (beginPass conn (T.pack target) (modeLabel mode))
                arriveAt target remembered saved mode passId loaded

arriveAt ::
  FilePath ->
  Maybe RepoPlace ->
  Map.Map FilePath RepoPlace ->
  ReviewMode ->
  Int64 ->
  ( Map.Map Text Verdict
  , Map.Map Text [Post]
  , Map.Map Text Text
  , Maybe RebaseStatus
  , Maybe Text
  , Maybe (Text, Text)
  , [FileView]
  ) ->
  EventM Name St ()
arriveAt target remembered saved mode passId loaded = do
  let
    (verdicts, threads, groups, rebase, branch, headAt, files) = loaded
  modify $ \s ->
    s
      { stRoot = target
      , stEpoch = stEpoch s + 1
      , stRefreshing = False
      , stGitQueued = False
      , stReloading = Map.empty
      , stHighlighting = Nothing
      , stFiles = files
      , stVerdicts = verdicts
      , stThreads = threads
      , stCountedVerdicts = verdicts
      , stCountedThreads = threads
      , stNoteGroups = groups
      , stRebase = rebase
      , stBranch = branch
      , stHead = headAt
      , stPassId = passId
      , stMode = mode
      , stPlaces = saved
      , -- it listed the repository we just left; bringing it back
        -- here would offer files that are not in front of us
        stLastSelector = Nothing
      , stCurF = 0
      , stCurH = 0
      , stWritten = Set.empty
      , stAltFile = Nothing
      , stAnchor = maybe Nothing rpAnchor remembered
      , stOpened = maybe Set.empty rpOpened remembered
      , stOnlyPending = maybe (stOnlyPending s) rpOnlyPending remembered
      , stRunFilter = maybe (stRunFilter s) rpRunFilter remembered
      , stGrepNav = Nothing
      , stCmdFiles = []
      , stCmdDirs = []
      , stCmdRefs = []
      }
  case remembered of
    Just place -> restorePlace place
    Nothing -> do
      arrived <- get
      case firstUnrespondedOrPending arrived of
        Just (f, h) -> parkAnchor f h
        Nothing -> pure ()
  setMsg (repoArrivalMsg target rebase files)

-- Land back on the file and hunk we left, when they are still there;
-- otherwise fall back to wherever a fresh session would have opened.
restorePlace :: RepoPlace -> EventM Name St ()
restorePlace place = do
  st <- get
  case findIndex ((== rpFile place) . fvPath) (stFiles st) of
    Just f -> parkM f (rpHunk place)
    Nothing ->
      case firstUnrespondedOrPending st of
        Just (f, h) -> parkAnchor f h
        Nothing -> pure ()

repoArrivalMsg :: FilePath -> Maybe RebaseStatus -> [FileView] -> Text
repoArrivalMsg target rebase files =
  let
    name = T.pack (takeFileName target)
    hunks = length (concatMap fvRuns files)
    what
      | null files = "nothing to review"
      | otherwise = tshow hunks <> " hunk(s) in " <> tshow (length files) <> " file(s)"
    mid = case rebase of
      Just rb -> " · rebasing, " <> rebaseStopMsg rb
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
rescanRepos :: EventM Name St ()
rescanRepos = do
  st <- get
  case stStory st of
    Nothing -> pure ()
    Just dir -> do
      found <- liftIO (childRepos dir)
      unless (found == stRoots st) (modify (\s -> s {stRoots = found}))

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

repoHunkCount :: St -> FilePath -> IO (Int, Int)
repoHunkCount st root
  -- the session already has this one loaded, down to its verdicts
  | root == stRoot st =
      let
        runs = concatMap fvRuns (stFiles st)
      in
        pure
          ( length (filter (not . runPending (stVerdicts st)) runs)
          , length runs
          )
  | otherwise = do
      verdicts <- loadVerdicts (stConn st) (T.pack root)
      files <- loadAll (stConfig st) (stHls st) (VsRef "HEAD") root
      let
        runs = concatMap fvRuns files
      pure (length (filter (not . runPending verdicts) runs), length runs)

describeRepo :: St -> FilePath -> IO (FilePath, [Text])
describeRepo st root = do
  branch <- currentBranch root
  rebase <- rebaseStatus root
  changed <- reviewFiles (VsRef "HEAD") (cfgUntracked (stConfig st)) root
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
            tshow done <> "/" <> tshow total <> " hunks"
      _ -> tshow (length changed) <> " changed"
    state = case rebase of
      Just rb
        | rebaseConflicted rb -> "CONFLICT"
        | otherwise -> "rebasing " <> tshow (rebaseStep rb) <> "/" <> tshow (rebaseTotal rb)
      Nothing
        | null changed -> "clean"
        | otherwise -> reviewed
  pure (root, [T.pack (takeFileName root), onBranch, state])
