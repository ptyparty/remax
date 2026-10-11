{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Core.Jobs
  ( forMaybeBounded
  , forBounded
  , startJob
  , jobFailed
  , withSlot
  ) where

import Brick qualified as Brick
import Brick.BChan qualified as BChan
import Control.Concurrent qualified as Concurrent
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.IORef qualified as IORef
import Data.Text qualified as T
import Remax.UI.Core.Types qualified as Types

-- Load files on a few workers at once: each one costs a git spawn for
-- its base, and a rebase-stop commit can easily touch a hundred files —
-- serially that is seconds of startup, in parallel it is I/O-bound.
-- A worker's IOException counts as "file not loadable", like loadOne's
-- own error handling.
forMaybeBounded :: (a -> IO (Maybe b)) -> [a] -> IO [Maybe b]
forMaybeBounded f =
  let
    swallow x = do
      got <- Exception.try (f x)
      pure $ case got of
        Left e -> let _ = (e :: Exception.IOException) in Nothing
        Right r -> r
  in
    forBounded 8 swallow

forBounded :: Int -> (a -> IO b) -> [a] -> IO [b]
forBounded workers f xs = do
  slots <- Concurrent.newQSem workers
  handles <- Monad.forM xs $ \x -> do
    slot <- Concurrent.newEmptyMVar
    _ <- Concurrent.forkIO $ do
      Concurrent.waitQSem slots
      got <- Exception.try (f x)
      Concurrent.signalQSem slots
      Concurrent.putMVar slot got
    pure slot
  Monad.forM handles $ \slot -> do
    got <- Concurrent.takeMVar slot
    case got of
      Left e -> Exception.throwIO (e :: Exception.IOException)
      Right r -> pure r

pulseUs :: Int
pulseUs = 120000

startJob :: T.Text -> IO a -> (Either T.Text a -> Brick.EventM Types.Name Types.St ()) -> Brick.EventM Types.Name Types.St ()
startJob label work finish = do
  st <- State.get
  let
    chan = Types.stChan st
    jobs = Types.stJobs st
    beat = do
      Concurrent.threadDelay pulseUs
      Monad.void (BChan.writeBChanNonBlocking chan Types.Pulse)
      running <- IORef.readIORef jobs
      Monad.when (running > 0) beat
  State.modify (\s -> s {Types.stBusy = Types.stBusy s ++ [label]})
  first <- MonadIO.liftIO (IORef.atomicModifyIORef' jobs (\n -> (n + 1, n == 0)))
  Monad.when first (Monad.void (MonadIO.liftIO (Concurrent.forkIO beat)))
  Monad.void . MonadIO.liftIO . Concurrent.forkIO $ do
    got <- Exception.try work
    IORef.atomicModifyIORef' jobs (\n -> (n - 1, ()))
    BChan.writeBChan chan . Types.JobDone label . finish $
      case got of
        Right result -> Right result
        Left e -> Left (T.pack (show (e :: Exception.SomeException)))

jobFailed :: T.Text -> T.Text -> Brick.EventM Types.Name Types.St ()
jobFailed label err = Types.setMsg ("! " <> label <> ": " <> err)

withSlot :: Concurrent.QSem -> IO a -> IO a
withSlot slots act = (Concurrent.waitQSem slots >> act) `Exception.finally` Concurrent.signalQSem slots
