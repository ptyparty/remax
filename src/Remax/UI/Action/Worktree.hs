{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Worktree
  ( editPull
  , reviewPull
  , returnToTree
  , openPullHere
  , chooseWorktree
  , newWorktreeChoice
  , typedPathChoice
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Remax.Config qualified as Config
import Remax.Git qualified as Git
import Remax.GitHub qualified as GitHub
import Remax.Store qualified as Store
import Remax.UI.Action.Pull qualified as PullAction
import Remax.UI.Action.Repo qualified as Repo
import Remax.UI.Action.Settings qualified as Settings
import Remax.UI.Core.Jobs qualified as Jobs
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Model.Load qualified as Load
import System.Directory qualified as Directory
import System.FilePath qualified as FilePath

type OpenPicker = Types.EditEntry -> [FilePath] -> FilePath -> Brick.EventM Types.Name Types.St ()

-- Edit the open pull request: in place when this checkout is on its
-- branch or rebasing it, otherwise in the worktree remembered for it,
-- and failing that in one the picker chooses.
editPull :: OpenPicker -> Brick.EventM Types.Name Types.St ()
editPull openPicker = do
  st <- State.get
  case Types.stMode st of
    Git.AtPull pr Git.Checkout -> Types.setMsg (Git.pullLabel pr <> " is already in edit mode \8212 :pr review returns to the fetched head")
    Git.AtPull pr Git.ReadOnly -> do
      let
        walked = fmap (const (Git.prTip pr)) (Types.pullCommitWalk st)
        entry =
          Types.EditEntry
            { Types.eePull = Git.pullAtHead pr
            , Types.eeCommit = walked
            , Types.eeThen = maybe (editHere (Git.pullAtHead pr)) (editCommit (Git.pullAtHead pr)) walked
            }
      standing <- MonadIO.liftIO (Git.branchStanding (Types.stRoot st) pr)
      case standing of
        Git.OffBranch _ -> editElsewhere openPicker entry
        _ -> Types.eeThen entry
    _ -> Types.setMsg PullAction.noPullOpen

editHere :: Git.PullRequest -> Brick.EventM Types.Name Types.St ()
editHere pr = do
  st <- State.get
  standing <- MonadIO.liftIO (Git.branchStanding (Types.stRoot st) pr)
  case standing of
    Git.OnBranch
      | Types.stMode st == Git.AtPull pr Git.Checkout -> pure ()
      | otherwise -> PullAction.editInPlace pr
    Git.RebasingBranch rb -> PullAction.editAtRebaseStop pr rb
    Git.OffBranch _ -> Types.setMsg ("this checkout is not on " <> Git.prHeadBranch pr <> " \8212 :pr edit again")

editCommit :: Git.PullRequest -> T.Text -> Brick.EventM Types.Name Types.St ()
editCommit pr sha = do
  st <- State.get
  let
    root = Types.stRoot st
    short = T.take 7 sha
  standing <- MonadIO.liftIO (Git.branchStanding root pr)
  case standing of
    Git.OffBranch _ -> Types.setMsg ("this checkout is not on " <> Git.prHeadBranch pr <> " \8212 :pr edit again")
    Git.OnBranch -> do
      refused <- MonadIO.liftIO (rebaseRefusal root sha)
      case refused of
        Just why -> Types.setMsg why
        Nothing -> do
          PullAction.editInPlace pr
          Types.whenWritable $ \grant -> do
            res <- MonadIO.liftIO (Git.rebaseToEdit grant root sha)
            rb <- MonadIO.liftIO (Git.rebaseStatus root)
            State.modify (\s -> s {Types.stRebase = rb, Types.stWritten = Set.empty})
            Load.forceGitRefresh
            case (res, rb) of
              (_, Just stopped) -> PullAction.editAtRebaseStop pr stopped
              (Left e, Nothing) -> Types.setMsg e
              (Right (), Nothing) -> Types.setMsg ("the rebase ended without stopping at " <> short)
    Git.RebasingBranch rb -> do
      place <- MonadIO.liftIO (Git.rebasePlaceOf root sha)
      case place of
        Git.StoppedAt -> PullAction.editAtRebaseStop pr rb
        Git.EarlierThanStop -> Types.setMsg (beforeStopMsg rb sha)
        Git.NotInRebase -> Types.setMsg (notInRebaseMsg rb sha)
        Git.LaterThanStop -> do
          PullAction.editAtRebaseStop pr rb
          Types.whenWritable $ \grant -> do
            res <- MonadIO.liftIO (Git.markTodoEdit grant root sha)
            Types.setMsg $ case res of
              Left e -> e
              Right () -> "editing at stop " <> Git.rebaseProgress rb <> " \183 the rebase will also stop at " <> short

editElsewhere :: OpenPicker -> Types.EditEntry -> Brick.EventM Types.Name Types.St ()
editElsewhere openPicker entry = do
  st <- State.get
  let
    pr = Types.eePull entry
    n = Git.prNumber pr
    conn = Types.stConn st
  original <- MonadIO.liftIO (fmap T.unpack (Store.originalOf conn (Types.repoText st)))
  remembered <- MonadIO.liftIO (Store.pullWorktree conn (T.pack original) n)
  present <- MonadIO.liftIO (maybe (pure False) (Directory.doesDirectoryExist . T.unpack) remembered)
  case remembered of
    Just path
      | present -> do
          standing <- MonadIO.liftIO (Git.branchStanding (T.unpack path) pr)
          refused <- MonadIO.liftIO (walkRefusal (T.unpack path) standing (Types.eeCommit entry))
          case (standing, refused) of
            (_, Just why) -> Types.setMsg why
            (Git.OffBranch _, Nothing) -> checkoutInto entry (T.unpack path)
            (_, Nothing) -> enterWorktree entry (T.unpack path)
    _ -> do
      Monad.forM_ remembered (const (MonadIO.liftIO (Store.forgetWorktree conn (T.pack original) n)))
      existing <- MonadIO.liftIO (Git.worktreesOnBranch original pr)
      openPicker entry (filter (/= Types.stRoot st) existing) (defaultWorktreePath st original pr)

walkRefusal :: FilePath -> Git.BranchStanding -> Maybe T.Text -> IO (Maybe T.Text)
walkRefusal path standing walked =
  case (standing, walked) of
    (Git.RebasingBranch rb, Just sha) -> do
      place <- Git.rebasePlaceOf path sha
      pure $ case place of
        Git.EarlierThanStop -> Just (beforeStopMsg rb sha)
        Git.NotInRebase -> Just (notInRebaseMsg rb sha)
        _ -> Nothing
    (Git.OnBranch, Just sha) -> rebaseRefusal path sha
    _ -> pure Nothing

rebaseRefusal :: FilePath -> T.Text -> IO (Maybe T.Text)
rebaseRefusal root sha = do
  dirty <- Git.trackedChanges root
  onBranch <- Git.isAncestor root sha "HEAD"
  pure $ case (dirty, onBranch) of
    (True, _) -> Just ("tracked changes in the tree \8212 commit or stash before editing commit " <> T.take 7 sha)
    (_, False) -> Just ("commit " <> T.take 7 sha <> " is not on the branch here \8212 it was rewritten since the head was fetched; :pr review shows the fetched head")
    _ -> Nothing

beforeStopMsg :: Git.RebaseStatus -> T.Text -> T.Text
beforeStopMsg rb sha = "commit " <> T.take 7 sha <> " is before the stop at " <> Git.rebaseProgress rb <> " \8212 :continue to the end or :abort! first"

notInRebaseMsg :: Git.RebaseStatus -> T.Text -> T.Text
notInRebaseMsg rb sha = "commit " <> T.take 7 sha <> " is not in the rebase stopped at " <> Git.rebaseProgress rb <> " \8212 :continue to the end or :abort! first"

defaultWorktreePath :: Types.St -> FilePath -> Git.PullRequest -> FilePath
defaultWorktreePath st original pr =
  let
    repoName = snd (T.breakOnEnd "/" (Git.prRepo pr))
    filled =
      T.replace "<repo>" repoName
        . T.replace "<N>" (Types.tshow (Git.prNumber pr))
        $ Config.cfgPrWorktree (Types.stConfig st)
  in
    Git.resolveUnder original (T.unpack filled)

newWorktreeChoice :: FilePath -> T.Text
newWorktreeChoice path = "new:" <> T.pack path

typedPathChoice :: T.Text
typedPathChoice = "type:"

chooseWorktree :: Types.EditEntry -> T.Text -> Brick.EventM Types.Name Types.St ()
chooseWorktree entry choice =
  case T.stripPrefix "new:" choice of
    Just path -> createWorktree entry (T.unpack path)
    Nothing
      | choice == typedPathChoice -> do
          st <- State.get
          original <- MonadIO.liftIO (fmap T.unpack (Store.originalOf (Types.stConn st) (Types.repoText st)))
          let
            prefilled = T.pack (defaultWorktreePath st original (Types.eePull entry))
            submit typed = createWorktree entry (Git.resolveUnder original (T.unpack typed))
          State.modify (\s -> s {Types.stPrompt = Just (Types.PathPrompt "worktree at " prefilled submit)})
      | T.null choice -> pure ()
      | otherwise -> enterWorktree entry (T.unpack choice)

createWorktree :: Types.EditEntry -> FilePath -> Brick.EventM Types.Name Types.St ()
createWorktree entry path = do
  st <- State.get
  let
    pr = Types.eePull entry
    n = Git.prNumber pr
    conn = Types.stConn st
    label = "adding a worktree for " <> Git.pullLabel pr
    work = do
      original <- fmap T.unpack (Store.originalOf conn (Types.repoText st))
      added <- Git.addDetachedWorktree original path (Git.pullHeadRef n)
      case added of
        Left err -> pure (Left err)
        Right () -> do
          canonical <- Directory.canonicalizePath path
          Store.rememberWorktree conn (T.pack original) n (T.pack canonical)
          checked <- GitHub.checkoutPull canonical (Git.prRepo pr) n
          pure (Right (fmap (const canonical) checked))
  Jobs.startJob label work $ \outcome -> do
    latest <- State.get
    Monad.when (Types.stEpoch latest == Types.stEpoch st) $
      case outcome of
        Left err -> Jobs.jobFailed label err
        Right (Left err) -> Types.setMsg ("! git: " <> err)
        Right (Right (Left failure)) -> Types.setMsg ("! " <> GitHub.failureText failure <> " \8212 :pr edit tries the checkout again")
        Right (Right (Right canonical)) -> enterWorktree entry canonical

checkoutInto :: Types.EditEntry -> FilePath -> Brick.EventM Types.Name Types.St ()
checkoutInto entry path = do
  st <- State.get
  let
    pr = Types.eePull entry
    label = "checking out " <> Git.pullLabel pr <> " in " <> T.pack (FilePath.takeFileName path)
  Jobs.startJob label (GitHub.checkoutPull path (Git.prRepo pr) (Git.prNumber pr)) $ \outcome -> do
    latest <- State.get
    Monad.when (Types.stEpoch latest == Types.stEpoch st) $
      case outcome of
        Left err -> Jobs.jobFailed label err
        Right (Left failure) -> Types.setMsg ("! " <> GitHub.failureText failure)
        Right (Right ()) -> enterWorktree entry path

enterWorktree :: Types.EditEntry -> FilePath -> Brick.EventM Types.Name Types.St ()
enterWorktree entry path = do
  canonical <- MonadIO.liftIO (Directory.canonicalizePath path)
  let
    pr = Types.eePull entry
  State.modify $ \s ->
    s {Types.stWorktrees = if canonical `elem` Types.stWorktrees s then Types.stWorktrees s else Types.stWorktrees s ++ [canonical]}
  Repo.arrive
    Repo.Arrival
      { Repo.arTarget = canonical
      , Repo.arMode = Just (Git.AtPull (Git.pullAtHead pr) Git.Checkout)
      , Repo.arThen = do
          Types.setMsg ("editing " <> Git.pullLabel pr <> " in " <> T.pack (FilePath.takeFileName canonical))
          Types.eeThen entry
      }

-- The checkout this session came from, when it is in one of that
-- checkout's pull request worktrees.
cameFrom :: Types.St -> Brick.EventM Types.Name Types.St (Maybe FilePath)
cameFrom st = do
  original <- MonadIO.liftIO (fmap T.unpack (Store.originalOf (Types.stConn st) (Types.repoText st)))
  pure (if original /= Types.stRoot st && Map.member original (Types.stPlaces st) then Just original else Nothing)

reviewPull :: Brick.EventM Types.Name Types.St ()
reviewPull = do
  st <- State.get
  from <- cameFrom st
  case (Types.stMode st, from) of
    (Git.AtPull pr _, Just original) ->
      Repo.arrive
        Repo.Arrival
          { Repo.arTarget = original
          , Repo.arMode = Just (Git.AtPull (Git.pullAtHead pr) Git.ReadOnly)
          , Repo.arThen = Types.setMsg ("reviewing " <> Git.pullLabel pr <> " read-only in " <> T.pack (FilePath.takeFileName original))
          }
    _ -> PullAction.reviewPull

returnToTree :: Brick.EventM Types.Name Types.St ()
returnToTree = do
  st <- State.get
  from <- cameFrom st
  case from of
    Just original ->
      Repo.arrive
        Repo.Arrival
          { Repo.arTarget = original
          , Repo.arMode = Just (maybe (Git.VsRef "HEAD") Types.rpTreeMode (Map.lookup original (Types.stPlaces st)))
          , Repo.arThen = pure ()
          }
    Nothing -> Settings.returnToTree

openPullHere :: Int -> Brick.EventM Types.Name Types.St ()
openPullHere n = do
  st <- State.get
  from <- cameFrom st
  case from of
    Just original -> Repo.arrive Repo.Arrival {Repo.arTarget = original, Repo.arMode = Nothing, Repo.arThen = PullAction.openPull n}
    Nothing -> PullAction.openPull n
