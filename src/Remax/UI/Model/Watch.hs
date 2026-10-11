{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Model.Watch
  ( withWatcher
  , installWatches
  , watchTracked
  , watchPollWarning
  , DirWatch (..)
  , dispatchFsEvent
  ) where

import Brick.BChan qualified as BChan
import Control.Concurrent qualified as Concurrent
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Data.IORef qualified as IORef
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.Git qualified as Git
import Remax.Store qualified as Store
import Remax.UI.Core.Types qualified as Types
import System.Exit qualified as Exit
import System.FSNotify qualified as FS
import System.FilePath qualified as FilePath

-- How often the fallback watcher re-reads the directories it is
-- watching, in microseconds. A review session cares about "did the file
-- I am looking at just change", so a second is plenty, and the watch
-- list is a handful of directories rather than the whole repository.
pollInterval :: Int
pollInterval = 1000000

-- The session's watcher, and whether it had to settle for polling.
--
-- inotify instances are a scarce per-user kernel resource: stock
-- fs.inotify.max_user_instances is 128, and a desktop with a few
-- editors, sync clients and language servers running can hold every one
-- of them. remax needs exactly one per session -- every directory it
-- follows is a watch descriptor on that single instance, and those are
-- capped orders of magnitude higher -- but one is still one more than
-- the kernel has to spare sometimes. Losing the native watcher is no
-- reason to refuse to review code, so fall back to polling and let the
-- caller say so on screen.
withWatcher :: (FS.WatchManager -> Types.WatchMode -> IO a) -> IO a
withWatcher use = do
  let
    pollConfig =
      FS.defaultConfig {FS.confWatchMode = FS.WatchModePoll pollInterval}
    run mgr mode = use mgr mode `Exception.finally` FS.stopManager mgr
  native <-
    Exception.try (FS.startManagerConf FS.defaultConfig) ::
      IO (Either Exception.IOException FS.WatchManager)
  case native of
    Right mgr -> run mgr Types.NativeWatch
    Left nativeErr -> do
      polled <-
        Exception.try (FS.startManagerConf pollConfig) ::
          IO (Either Exception.IOException FS.WatchManager)
      case polled of
        Right mgr -> run mgr Types.PollingWatch
        Left polledErr -> Exit.die (watcherFailure nativeErr polledErr)

-- Watch every directory the repository keeps a file in, not only the
-- ones a file is changed in today.
--
-- Watching where the changes are cannot notice the first change anywhere
-- else: an edit in a directory nothing had changed in reached nobody, so
-- no refresh ran, so the directory never got watched, so the next edit
-- there reached nobody either. The file simply sat there until something
-- else made the session look.
--
-- Taken from git rather than by walking the tree, so build directories
-- and everything else ignored stay out of it — that walk is what makes a
-- recursive watch cost more than it is worth. In the background, because
-- a large repository is thousands of directories and none of them is
-- worth making somebody wait for.
watchTracked :: (FilePath -> IO ()) -> FilePath -> IO ()
watchTracked watch root =
  Monad.void . Concurrent.forkIO $ do
    tracked <- Git.listFiles Git.VisibleFiles root
    mapM_ (watch . Types.fileDir root) tracked

-- Shown once, in the status line, when the native watcher was refused.
-- Polling follows git state and the file on screen; an edit to another
-- file is picked up when that file is visited, so the message leads
-- with the fix rather than the consequence.
watchPollWarning :: T.Text
watchPollWarning =
  "\9888 inotify full \8212 polling; sudo sysctl -w fs.inotify.max_user_instances=512"

-- Both watchers refused, which should not happen -- polling asks the
-- kernel for nothing. Say what was tried and what to do about it,
-- rather than letting an inotify errno reach the user on its own.
watcherFailure :: Exception.IOException -> Exception.IOException -> String
watcherFailure nativeErr polledErr =
  unlines
    [ "remax: no file watcher available."
    , ""
    , "  native : " ++ show nativeErr
    , "  polling: " ++ show polledErr
    , ""
    , "If the native error mentions inotify, the kernel is out of inotify"
    , "instances -- they are a per-user resource and a busy desktop can use"
    , "every one. Raise the cap:"
    , ""
    , "  sudo sysctl -w fs.inotify.max_user_instances=512"
    , ""
    , "and put it in /etc/sysctl.d/ so it survives a reboot."
    ]

-- Entry point ---------------------------------------------------------------

data DirWatch = DirWatch
  { dirWatchFollow :: FilePath -> IO ()
  , dirWatchForget :: FilePath -> IO ()
  }

noisyDirNames :: [String]
noisyDirNames =
  filter (/= ".git") (map FilePath.dropTrailingPathSeparator Git.buildOutputDirs)

followNewDirectory :: BChan.BChan Types.REvent -> FilePath -> DirWatch -> FilePath -> IO ()
followNewDirectory chan root dirWatch dir = do
  ignored <- Git.isIgnored root dir
  Monad.unless ignored $ do
    dirWatchFollow dirWatch dir
    files <- Git.listFilesUnder Git.VisibleFiles root [dir]
    mapM_ (dirWatchFollow dirWatch . Types.fileDir root) files
    Monad.void (BChan.writeBChanNonBlocking chan Types.GitStateChanged)

dispatchFsEvent :: BChan.BChan Types.REvent -> FilePath -> DirWatch -> FS.Event -> IO ()
dispatchFsEvent chan root dirWatch ev = do
  let
    absolute = FS.eventPath ev
    rel = FilePath.makeRelative root absolute
    parts = FilePath.splitDirectories rel
    noisy = any (`elem` noisyDirNames) parts
    underGit = ".git" `elem` parts
    isDir = FS.eventIsDirectory ev == FS.IsDirectory
  Monad.unless noisy $
    case ev of
      FS.WatchedDirectoryRemoved {} -> do
        dirWatchForget dirWatch absolute
        Monad.void (BChan.writeBChanNonBlocking chan Types.GitStateChanged)
      FS.Removed {}
        | isDir -> do
            dirWatchForget dirWatch absolute
            Monad.void (BChan.writeBChanNonBlocking chan Types.GitStateChanged)
      FS.Added {}
        | isDir && not underGit ->
            Monad.void (Concurrent.forkIO (followNewDirectory chan root dirWatch absolute))
      _
        | underGit ->
            -- git updates the index by writing index.lock and renaming it,
            -- so the lock file is the reliably-delivered signal
            Monad.when
              ( FilePath.takeFileName rel
                  `elem` ["index", "index.lock", "HEAD", "HEAD.lock", "COMMIT_EDITMSG"]
                  || "refs" `elem` parts
              )
              (Monad.void (BChan.writeBChanNonBlocking chan Types.GitStateChanged))
        | otherwise -> Monad.void (BChan.writeBChanNonBlocking chan (Types.FileChangedOnDisk absolute))

-- Loading and rebuilding -----------------------------------------------------

-- Watch only the directories that actually matter: where the
-- changed files live, plus .git itself for index/HEAD updates.
-- A recursive watch over the whole repository walks every build
-- artifact directory and dominates startup on real projects.
type Watched = Map.Map FilePath (Maybe FS.StopListening)

installWatches :: FS.WatchManager -> BChan.BChan Types.REvent -> FilePath -> Types.WatchMode -> IO DirWatch
installWatches mgr chan root mode = do
  watchedRef <- IORef.newIORef (Map.empty :: Watched)
  let
    watchDirOnce = watchOnce watchedRef mgr (dispatchFsEvent chan root dirWatch)
    followDir dir =
      if mode == Types.PollingWatch && ".git" `notElem` FilePath.splitDirectories dir
        then pure ()
        else watchDirOnce dir
    dirWatch =
      DirWatch
        { dirWatchFollow = followDir
        , dirWatchForget = forgetBeneath watchedRef
        }
  gitDirs <- Git.gitStateWatchDirs root
  Monad.forM_ gitDirs watchDirOnce
  -- the verdict store lives outside the repository; watching it makes
  -- an agent's `remax reply` show up in the session as it lands
  storeDir <- Store.storeDirectory
  Monad.void
    ( Exception.try
        ( FS.watchDir
            mgr
            storeDir
            (const True)
            (\_ -> Monad.void (BChan.writeBChanNonBlocking chan Types.StoreChanged))
        ) ::
        IO (Either Exception.IOException FS.StopListening)
    )
  watchTracked followDir root
  pure dirWatch

asRemembered :: FilePath -> FilePath
asRemembered = FilePath.normalise . FilePath.dropTrailingPathSeparator

watchOnce :: IORef.IORef Watched -> FS.WatchManager -> (FS.Event -> IO ()) -> FilePath -> IO ()
watchOnce watchedRef mgr handler given = do
  let
    dir = asRemembered given
    claim seen =
      if Map.member dir seen
        then (seen, False)
        else (Map.insert dir Nothing seen, True)
  claimed <- IORef.atomicModifyIORef' watchedRef claim
  Monad.when claimed $ do
    got <-
      Exception.try (FS.watchDir mgr dir (const True) handler) ::
        IO (Either Exception.IOException FS.StopListening)
    -- a directory whose watch was refused (inotify full, or gone
    -- between listing and watching) is worth trying again the next
    -- time it comes up, and remembering it regardless meant it
    -- never was
    case got of
      Right stop -> IORef.atomicModifyIORef' watchedRef (\seen -> (Map.insert dir (Just stop) seen, ()))
      Left _ -> IORef.atomicModifyIORef' watchedRef (\seen -> (Map.delete dir seen, ()))

forgetBeneath :: IORef.IORef Watched -> FilePath -> IO ()
forgetBeneath watchedRef given = do
  let
    dir = asRemembered given
    beneath k = k == dir || (dir ++ "/") `List.isPrefixOf` k
  dead <-
    IORef.atomicModifyIORef'
      watchedRef
      ( \seen ->
          let
            (gone, kept) = Map.partitionWithKey (\k _ -> beneath k) seen
          in
            (kept, Maybe.catMaybes (Map.elems gone))
      )
  Monad.forM_ dead (\stop -> Monad.void (Exception.try stop :: IO (Either Exception.IOException ())))
