{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Pull
  ( openPull
  , refreshPullMeta
  , cachedRepoName
  , rememberRepoName
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.Text qualified as T
import Remax.Git qualified as Git
import Remax.GitHub qualified as GitHub
import Remax.UI.Action.Settings qualified as Settings
import Remax.UI.Core.Jobs qualified as Jobs
import Remax.UI.Core.Types qualified as Types

openPull :: Int -> Brick.EventM Types.Name Types.St ()
openPull n = do
  st <- State.get
  known <- cachedRepoName st
  let
    label = "opening PR #" <> Types.tshow n
  Jobs.startJob label (GitHub.openPull (Types.stRoot st) (GitHub.repoCached known) n) $ \outcome -> do
    latest <- State.get
    Monad.when (Types.stEpoch latest == Types.stEpoch st) $
      case outcome of
        Left err -> Jobs.jobFailed label err
        Right (Left failure) -> Types.setMsg ("! " <> GitHub.failureText failure)
        Right (Right opened) -> do
          rememberRepoName known (GitHub.openedRepo opened)
          State.modify (\s -> s {Types.stPullMeta = Just (GitHub.openedMeta opened)})
          Settings.setMode (GitHub.openedMode opened)
          Types.setMsg ("PR #" <> Types.tshow n <> " \183 " <> GitHub.metaTitle (GitHub.openedMeta opened))

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

cachedRepoName :: Types.St -> Brick.EventM Types.Name Types.St GitHub.RepoName
cachedRepoName st = MonadIO.liftIO (GitHub.cachedRepoName (Types.stConn st) (Types.stRoot st))

rememberRepoName :: GitHub.RepoName -> T.Text -> Brick.EventM Types.Name Types.St ()
rememberRepoName known name = do
  st <- State.get
  MonadIO.liftIO (GitHub.rememberRepoName (Types.stConn st) known name)
