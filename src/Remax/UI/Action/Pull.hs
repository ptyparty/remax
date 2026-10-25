{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Pull
  ( openPull
  , refreshPullMeta
  , pollPullIfDue
  , pollPullNow
  , PullFreshness (..)
  , pullFreshness
  , shortAge
  , staleText
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.Text qualified as T
import GHC.Clock qualified as GHCClock
import Remax.Git qualified as Git
import Remax.GitHub qualified as GitHub
import Remax.PullPoll qualified as PullPoll
import Remax.UI.Action.Settings qualified as Settings
import Remax.UI.Core.Jobs qualified as Jobs
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Model.Load qualified as Load

openPull :: Int -> Brick.EventM Types.Name Types.St ()
openPull n = do
  st <- State.get
  known <- MonadIO.liftIO (GitHub.cachedRepoName (Types.stConn st) (Types.stRoot st))
  let
    label = "opening PR #" <> Types.tshow n
  Jobs.startJob label (GitHub.openPull (Types.stRoot st) (GitHub.repoCached known) n) $ \outcome -> do
    latest <- State.get
    Monad.when (Types.stEpoch latest == Types.stEpoch st) $
      case outcome of
        Left err -> Jobs.jobFailed label err
        Right (Left failure) -> Types.setMsg ("! " <> GitHub.failureText failure)
        Right (Right opened) -> do
          MonadIO.liftIO (GitHub.rememberRepoName (Types.stConn latest) known (GitHub.openedRepo opened))
          State.modify (\s -> s {Types.stPullMeta = Just (GitHub.openedMeta opened), Types.stPullPoll = Types.noPullPoll})
          Settings.setMode (GitHub.openedMode opened)
          Types.setMsg ("PR #" <> Types.tshow n <> " \183 " <> GitHub.metaTitle (GitHub.openedMeta opened))
          pollPullNow

refreshPullMeta :: Brick.EventM Types.Name Types.St ()
refreshPullMeta = do
  st <- State.get
  case Git.subjectOf (Types.stMode st) of
    Git.Pull pr _ -> do
      let
        n = Git.prNumber pr
        label = "reading PR #" <> Types.tshow n
      Jobs.startJob label (GitHub.viewPull (Types.stRoot st) (Git.prRepo pr) n) $ \outcome -> do
        latest <- State.get
        let
          stillOpen = case Git.subjectOf (Types.stMode latest) of
            Git.Pull now _ -> Git.prNumber now == n
            _ -> False
        Monad.when (Types.stEpoch latest == Types.stEpoch st && stillOpen) $
          case outcome of
            Left err -> Jobs.jobFailed label err
            Right (Left failure) -> Types.setMsg ("! " <> GitHub.failureText failure)
            Right (Right meta) -> State.modify (\s -> s {Types.stPullMeta = Just meta})
    _ -> pure ()

pullPollSeconds :: Double
pullPollSeconds = 10

pollPullIfDue :: Brick.EventM Types.Name Types.St ()
pollPullIfDue = do
  now <- MonadIO.liftIO GHCClock.getMonotonicTime
  modifyPoll (\pp -> pp {Types.ppNow = now})
  pp <- State.gets Types.stPullPoll
  Monad.when (now - Types.ppStarted pp >= pullPollSeconds) pollPullNow

pollPullNow :: Brick.EventM Types.Name Types.St ()
pollPullNow = do
  st <- State.get
  case Git.subjectOf (Types.stMode st) of
    Git.Pull pr _ | Types.ppFlight (Types.stPullPoll st) == Types.PollIdle -> startPoll st pr
    _ -> pure ()

startPoll :: Types.St -> Git.PullRequest -> Brick.EventM Types.Name Types.St ()
startPoll st pr = do
  now <- MonadIO.liftIO GHCClock.getMonotonicTime
  let
    n = Git.prNumber pr
    cache = case Types.ppCache (Types.stPullPoll st) of
      Just c | PullPoll.pollCachePull c == n -> c
      _ -> PullPoll.emptyPollCache n
    work = PullPoll.pollPull (Types.stRoot st) pr cache
  modifyPoll (\pp -> pp {Types.ppFlight = Types.PollRunning, Types.ppStarted = now, Types.ppNow = now})
  Jobs.startQuietJob ("syncing PR #" <> Types.tshow n) work (finishPoll (Types.stEpoch st) n)

finishPoll :: Int -> Int -> Either T.Text (Either GitHub.GhFailure PullPoll.Polled) -> Brick.EventM Types.Name Types.St ()
finishPoll epoch n outcome = do
  latest <- State.get
  modifyPoll (\pp -> pp {Types.ppFlight = Types.PollIdle})
  let
    stillOpen = case Git.subjectOf (Types.stMode latest) of
      Git.Pull now _ -> Git.prNumber now == n
      _ -> False
  Monad.when (Types.stEpoch latest == epoch && stillOpen) $
    case outcome of
      Left err -> pollFailed err
      Right (Left failure) -> pollFailed (GitHub.failureText failure)
      Right (Right polled) -> applyPolled polled

pollFailed :: T.Text -> Brick.EventM Types.Name Types.St ()
pollFailed reason = modifyPoll (\pp -> pp {Types.ppFailure = Just reason})

applyPolled :: PullPoll.Polled -> Brick.EventM Types.Name Types.St ()
applyPolled polled = do
  now <- MonadIO.liftIO GHCClock.getMonotonicTime
  let
    snap = PullPoll.polledSnapshot polled
  modifyPoll $ \pp ->
    pp
      { Types.ppCache = Just (PullPoll.polledCache polled)
      , Types.ppSnapshot = Just snap
      , Types.ppLastOk = Just now
      , Types.ppFailure = Nothing
      , Types.ppNow = now
      }
  State.modify (\s -> s {Types.stPullMeta = Just (PullPoll.snapshotMeta snap)})
  case PullPoll.polledHead polled of
    GitHub.RefsFetched -> Load.refreshGit
    GitHub.RefsInPlace -> pure ()

modifyPoll :: (Types.PullPoll -> Types.PullPoll) -> Brick.EventM Types.Name Types.St ()
modifyPoll f = State.modify (\s -> s {Types.stPullPoll = f (Types.stPullPoll s)})

data PullFreshness
  = AwaitingFirstPoll
  | SyncedAgo Int
  | StaleSince (Maybe Int) T.Text

pullFreshness :: Types.St -> Maybe PullFreshness
pullFreshness st =
  case Git.subjectOf (Types.stMode st) of
    Git.Pull _ _ ->
      let
        pp = Types.stPullPoll st
        age at = max 0 (floor (Types.ppNow pp - at))
      in
        Just $ case (Types.ppFailure pp, Types.ppLastOk pp) of
          (Just reason, lastOk) -> StaleSince (fmap age lastOk) reason
          (Nothing, Just at) -> SyncedAgo (age at)
          (Nothing, Nothing) -> AwaitingFirstPoll
    _ -> Nothing

shortAge :: Int -> T.Text
shortAge secs
  | secs < 60 = Types.tshow secs <> "s"
  | secs < 3600 = Types.tshow (secs `div` 60) <> "m"
  | secs < 86400 = Types.tshow (secs `div` 3600) <> "h"
  | secs < 86400 * 60 = Types.tshow (secs `div` 86400) <> "d"
  | otherwise = Types.tshow (secs `div` (86400 * 30)) <> "mo"

staleText :: Maybe Int -> T.Text
staleText age = "\9888 stale" <> maybe "" (\secs -> " " <> shortAge secs) age
