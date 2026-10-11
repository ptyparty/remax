{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Core.Jobs
  ( forMaybeBounded
  , forBounded
  , startJob
  , jobFailed
  , withSlot
  ) where

import Brick
  ( EventM
  )
import Brick.BChan
  ( writeBChan
  , writeBChanNonBlocking
  )
import Control.Concurrent
  ( QSem
  , forkIO
  , newEmptyMVar
  , newQSem
  , putMVar
  , signalQSem
  , takeMVar
  , threadDelay
  , waitQSem
  )
import Control.Exception
  ( IOException
  , SomeException
  , finally
  , throwIO
  , try
  )
import Control.Monad
  ( forM
  , void
  , when
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
  ( get
  , modify
  )
import Data.IORef
  ( atomicModifyIORef'
  , readIORef
  )
import Data.Text (Text)
import qualified Data.Text as T
import Remax.UI.Core.Types

-- Load files on a few workers at once: each one costs a git spawn for
-- its base, and a rebase-stop commit can easily touch a hundred files —
-- serially that is seconds of startup, in parallel it is I/O-bound.
-- A worker's IOException counts as "file not loadable", like loadOne's
-- own error handling.
forMaybeBounded :: (a -> IO (Maybe b)) -> [a] -> IO [Maybe b]
forMaybeBounded f =
  let
    swallow x = do
      got <- try (f x)
      pure $ case got of
        Left e -> let _ = (e :: IOException) in Nothing
        Right r -> r
  in
    forBounded 8 swallow

forBounded :: Int -> (a -> IO b) -> [a] -> IO [b]
forBounded workers f xs = do
  slots <- newQSem workers
  handles <- forM xs $ \x -> do
    slot <- newEmptyMVar
    _ <- forkIO $ do
      waitQSem slots
      got <- try (f x)
      signalQSem slots
      putMVar slot got
    pure slot
  forM handles $ \slot -> do
    got <- takeMVar slot
    case got of
      Left e -> throwIO (e :: IOException)
      Right r -> pure r

pulseUs :: Int
pulseUs = 120000

startJob :: Text -> IO a -> (Either Text a -> EventM Name St ()) -> EventM Name St ()
startJob label work finish = do
  st <- get
  let
    chan = stChan st
    jobs = stJobs st
    beat = do
      threadDelay pulseUs
      void (writeBChanNonBlocking chan Pulse)
      running <- readIORef jobs
      when (running > 0) beat
  modify (\s -> s {stBusy = stBusy s ++ [label]})
  first <- liftIO (atomicModifyIORef' jobs (\n -> (n + 1, n == 0)))
  when first (void (liftIO (forkIO beat)))
  void . liftIO . forkIO $ do
    got <- try work
    atomicModifyIORef' jobs (\n -> (n - 1, ()))
    writeBChan chan . JobDone label . finish $
      case got of
        Right result -> Right result
        Left e -> Left (T.pack (show (e :: SomeException)))

jobFailed :: Text -> Text -> EventM Name St ()
jobFailed label err = setMsg ("! " <> label <> ": " <> err)

withSlot :: QSem -> IO a -> IO a
withSlot slots act = (waitQSem slots >> act) `finally` signalQSem slots
