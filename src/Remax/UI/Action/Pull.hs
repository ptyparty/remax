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
  , reviewHeadAgainst
  , reviewPushedHead
  , startCommitWalk
  , walkToCommit
  , walkRun
  , editPull
  , editInPlace
  , editAtRebaseStop
  , reviewPull
  , noPullOpen
  , noteEnded
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import GHC.Clock qualified as GHCClock
import Remax.Conversation qualified as Conv
import Remax.Git qualified as Git
import Remax.GitHub qualified as GitHub
import Remax.Highlight qualified as Highlight
import Remax.PullPoll qualified as PullPoll
import Remax.RemoteCache qualified as RemoteCache
import Remax.Store qualified as Store
import Remax.TreeSitter qualified as TS
import Remax.UI.Action.Settings qualified as Settings
import Remax.UI.Core.Jobs qualified as Jobs
import Remax.UI.Core.PullView qualified as PullView
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Model.Load qualified as Load
import Remax.UI.Model.Nav qualified as Nav
import Remax.UI.Model.Work qualified as WorkModel

openPull :: Int -> Brick.EventM Types.Name Types.St ()
openPull n = do
  st <- State.get
  known <- MonadIO.liftIO (GitHub.cachedRepoName (Types.stConn st) (Types.stRoot st))
  let
    label = "opening PR #" <> Types.tshow n
  Jobs.startJob label (openAndStart (Types.stRoot st) known n) $ \outcome -> do
    latest <- State.get
    Monad.when (Types.stEpoch latest == Types.stEpoch st) $
      case outcome of
        Left err -> Jobs.jobFailed label err
        Right (Left failure) -> Types.setMsg ("! " <> GitHub.failureText failure)
        Right (Right (opened, start)) -> do
          MonadIO.liftIO (GitHub.rememberRepoName (Types.stConn latest) known (GitHub.openedRepo opened))
          State.modify (\s -> s {Types.stPullMeta = Just (GitHub.openedMeta opened), Types.stPullPoll = Types.noPullPoll})
          Settings.setMode (GitHub.startedMode start opened)
          State.modify Types.showPullView
          Types.setMsg (openedMessage start opened)
          pollPullNow

openAndStart :: FilePath -> GitHub.RepoName -> Int -> IO (Either GitHub.GhFailure (GitHub.Opened, GitHub.PullStart))
openAndStart root known n = do
  got <- GitHub.openPull root (GitHub.repoCached known) n
  case got of
    Left failure -> pure (Left failure)
    Right opened -> fmap (\start -> Right (opened, start)) (GitHub.pullStart root opened)

openedMessage :: GitHub.PullStart -> GitHub.Opened -> T.Text
openedMessage start opened =
  let
    title = Git.pullLabel (GitHub.openedPull opened) <> " \183 " <> GitHub.metaTitle (GitHub.openedMeta opened)
  in
    case start of
      GitHub.EditYourPull -> withEndedNote (Just (GitHub.metaState (GitHub.openedMeta opened))) ("editing " <> title)
      GitHub.ReviewPull GitHub.YourPull -> title <> " \183 :pr edit opens it in a worktree"
      GitHub.ReviewPull GitHub.TheirPull -> title

withEndedNote :: Maybe GitHub.PullState -> T.Text -> T.Text
withEndedNote state msg =
  case state >>= GitHub.endedLabel of
    Nothing -> msg
    Just label
      | T.null msg -> "the pull request is " <> label
      | otherwise -> msg <> " \183 the pull request is " <> label

noteEnded :: Maybe GitHub.PullState -> Brick.EventM Types.Name Types.St ()
noteEnded state = State.modify (\s -> s {Types.stMsg = withEndedNote state (Types.stMsg s)})

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
        Monad.when (Types.stEpoch latest == Types.stEpoch st && onPull latest n) $
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
  Monad.when (Types.stEpoch latest == epoch && onPull latest n) $
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
  State.modify (\s -> WorkModel.withConversations s {Types.stPullMeta = Just (PullPoll.snapshotMeta snap), Types.stMode = retargeted snap (Types.stMode s)})
  WorkModel.placeOutdated
  st <- State.get
  toks <- MonadIO.liftIO (excerptTokens (Types.stHls st) (Types.ppExcerptToks (Types.stPullPoll st)) snap)
  modifyPoll (\pp -> pp {Types.ppExcerptToks = toks})
  Monad.when (PullPoll.polledFreshness polled == PullPoll.Changed) $
    MonadIO.liftIO (RemoteCache.writeRemote (Types.repoText st) (PullPoll.snapshotPull snap) (PullPoll.snapshotRemote snap))
  case PullPoll.polledHead polled of
    GitHub.RefsFetched -> Load.refreshGit
    GitHub.RefsInPlace -> pure ()

retargeted :: PullPoll.Snapshot -> Git.ReviewMode -> Git.ReviewMode
retargeted snap mode =
  case mode of
    Git.AtPull pr editing
      | Git.prNumber pr == PullPoll.snapshotPull snap ->
          Git.AtPull pr {Git.prBaseBranch = PullPoll.pullHeadBaseBranch (PullPoll.snapshotHead snap)} editing
    _ -> mode

excerptTokens :: TS.HighlighterTable -> Map.Map Store.RemoteThreadId [[Highlight.Seg]] -> PullPoll.Snapshot -> IO (Map.Map Store.RemoteThreadId [[Highlight.Seg]])
excerptTokens table known snap =
  let
    unseen thread =
      if Map.notMember (Conv.remoteThreadId thread) known
        then Just (thread, either Conv.outPath Conv.posPath (Conv.remoteThreadWhere thread))
        else Nothing
    highlight (thread, path) =
      case TS.highlighterFor table (T.unpack path) Nothing of
        Nothing -> pure Nothing
        Just hl -> do
          got <- TS.tsHighlight hl (map (T.drop 1) (PullView.hunkBody (Conv.remoteCommentDiffHunk (NEL.head (Conv.remoteThreadComments thread)))))
          pure (fmap (\scoped -> (Conv.remoteThreadId thread, map (map (\(scope, t) -> (Highlight.scopeSeg scope, t))) scoped)) got)
  in
    do
      fresh <- traverse highlight (Maybe.mapMaybe unseen (Conv.remoteThreads (PullPoll.snapshotRemote snap)))
      pure (Map.union known (Map.fromList (Maybe.catMaybes fresh)))

onPull :: Types.St -> Int -> Bool
onPull st n =
  case Git.subjectOf (Types.stMode st) of
    Git.Pull pr _ -> Git.prNumber pr == n
    _ -> False

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

noPullOpen :: T.Text
noPullOpen = "no pull request open \8212 :pr opens one"

reviewHeadAgainst :: T.Text -> Brick.EventM Types.Name Types.St ()
reviewHeadAgainst base = do
  st <- State.get
  case Git.subjectOf (Types.stMode st) of
    Git.Pull pr _ ->
      Settings.setMode (Git.AtPull (Git.pullAtHead pr) {Git.prBase = base} Git.ReadOnly)
    _ -> Types.setMsg noPullOpen

reviewPushedHead :: T.Text -> Brick.EventM Types.Name Types.St ()
reviewPushedHead sha = do
  st <- State.get
  case Git.subjectOf (Types.stMode st) of
    Git.Pull pr _ -> do
      let
        n = Git.prNumber pr
        label = "fetching " <> T.take 7 sha
      Jobs.startJob label (GitHub.keepPushedHead (Types.stRoot st) (Git.prRepo pr) n sha) $ \outcome -> do
        latest <- State.get
        Monad.when (Types.stEpoch latest == Types.stEpoch st && onPull latest n) $
          case outcome of
            Left err -> Jobs.jobFailed label err
            Right (Left failure) -> Types.setMsg ("! " <> T.take 7 sha <> " is neither here nor on GitHub: " <> GitHub.failureText failure)
            Right (Right ref) -> reviewHeadAgainst ref
    _ -> Types.setMsg noPullOpen

startCommitWalk :: Brick.EventM Types.Name Types.St ()
startCommitWalk = walkFrom (const (Just 0))

walkToCommit :: T.Text -> Brick.EventM Types.Name Types.St ()
walkToCommit sha = walkFrom (List.findIndex ((== sha) . PullPoll.pullCommitSha))

walkFrom :: ([PullPoll.PullCommit] -> Maybe Int) -> Brick.EventM Types.Name Types.St ()
walkFrom pick = do
  st <- State.get
  case (Types.stMode st, Types.ppSnapshot (Types.stPullPoll st)) of
    (Git.AtPull pr Git.ReadOnly, Just snap)
      | PullPoll.snapshotPull snap == Git.prNumber pr ->
          let
            commits = PullPoll.snapshotCommits snap
          in
            case pick commits of
              _ | null commits -> Types.setMsg ("PR #" <> Types.tshow (Git.prNumber pr) <> " has no commits")
              Nothing -> Types.setMsg ("that commit is no longer on PR #" <> Types.tshow (Git.prNumber pr))
              Just i -> do
                State.modify (\s -> s {Types.stPullView = Nothing, Types.stBand = Nothing})
                walkTo Types.CommitWalk {Types.cwPull = pr, Types.cwCommits = commits, Types.cwIndex = i}
    (Git.AtPull _ Git.ReadOnly, _) -> Types.setMsg "the PR's commits are still loading from GitHub \8212 :pr commits again in a moment"
    (Git.AtPull _ Git.Checkout, _) -> Types.setMsg "the commit walk is read-only \8212 :pr review returns to the fetched head"
    _ -> Types.setMsg noPullOpen

walkTo :: Types.CommitWalk -> Brick.EventM Types.Name Types.St ()
walkTo walk =
  case Types.safeIdx (Types.cwCommits walk) (Types.cwIndex walk) of
    Nothing -> pure ()
    Just commit -> do
      let
        sha = PullPoll.pullCommitSha commit
        pr = Types.cwPull walk
      Settings.setMode (Git.AtPull pr {Git.prTip = sha, Git.prBase = sha <> "^"} Git.ReadOnly)
      Types.setMsg
        ( "commit "
            <> Types.tshow (Types.cwIndex walk + 1)
            <> "/"
            <> Types.tshow (length (Types.cwCommits walk))
            <> " \183 "
            <> PullPoll.pullCommitSubject commit
        )

walkRun :: Int -> Brick.EventM Types.Name Types.St ()
walkRun d = do
  st <- State.get
  let
    step = Nav.planRun st d
    atEdge = case step of
      Nav.RunTo _ -> False
      Nav.RunWraps _ -> True
      Nav.RunEmpty _ -> True
  case Types.pullCommitWalk st of
    Just walk | atEdge -> stepCommit walk d
    _ -> Nav.takeRun step

stepCommit :: Types.CommitWalk -> Int -> Brick.EventM Types.Name Types.St ()
stepCommit walk d =
  let
    next = Types.cwIndex walk + signum d
    total = length (Types.cwCommits walk)
    position i = "commit " <> Types.tshow i <> "/" <> Types.tshow total
  in
    case (next < 0, next >= total) of
      (True, _) -> Types.setMsg (position (1 :: Int) <> " is the PR's first \8212 :base reviews a range")
      (_, True) -> Types.setMsg (position total <> " is the PR's last \8212 :base reviews a range")
      _ -> walkTo walk {Types.cwIndex = next}

editPull :: Brick.EventM Types.Name Types.St ()
editPull = do
  st <- State.get
  case Types.stMode st of
    Git.AtPull pr Git.Checkout -> Types.setMsg (Git.pullLabel pr <> " is already in edit mode \8212 :pr review returns to the fetched head")
    Git.AtPull pr Git.ReadOnly -> do
      standing <- MonadIO.liftIO (Git.branchStanding (Types.stRoot st) pr)
      case standing of
        Git.OnBranch -> editInPlace pr
        Git.RebasingBranch rb -> editAtRebaseStop pr rb
        Git.OffBranch (Just rb) ->
          Types.setMsg
            ( "this checkout is rebasing "
                <> Maybe.fromMaybe "a detached HEAD" (Git.rebaseBranch rb)
                <> ", not "
                <> Git.prHeadBranch pr
                <> " \8212 :continue or :abort! first"
            )
        Git.OffBranch Nothing -> checkoutPull st pr
    _ -> Types.setMsg noPullOpen

editInPlace :: Git.PullRequest -> Brick.EventM Types.Name Types.St ()
editInPlace pr = do
  Settings.setMode (Git.AtPull (Git.pullAtHead pr) Git.Checkout)
  Types.setMsg ("editing " <> Git.pullLabel pr <> " on " <> Git.prHeadBranch pr)

editAtRebaseStop :: Git.PullRequest -> Git.RebaseStatus -> Brick.EventM Types.Name Types.St ()
editAtRebaseStop pr rb = do
  Settings.setMode (Git.AtPull (Git.pullAtHead pr) Git.Checkout)
  Types.setMsg
    ( "editing "
        <> Git.pullLabel pr
        <> " at rebase stop "
        <> Git.rebaseProgress rb
        <> " \183 :continue folds the edits into "
        <> (if T.null (Git.rebaseSha rb) then "the stopped commit" else Git.rebaseSha rb)
    )

data CheckoutOutcome = CheckoutRefused GitHub.GhFailure | CheckedOut

checkoutUnlessOnHead :: FilePath -> Git.PullRequest -> IO CheckoutOutcome
checkoutUnlessOnHead root pr = do
  branch <- Git.currentBranch root
  if branch == Just (Git.prHeadBranch pr)
    then pure CheckedOut
    else fmap (either CheckoutRefused (const CheckedOut)) (GitHub.checkoutPull root (Git.prRepo pr) (Git.prNumber pr))

checkoutPull :: Types.St -> Git.PullRequest -> Brick.EventM Types.Name Types.St ()
checkoutPull st pr = do
  let
    label = "checking out " <> Git.pullLabel pr
  Jobs.startJob label (checkoutUnlessOnHead (Types.stRoot st) pr) $ \outcome -> do
    latest <- State.get
    Monad.when (Types.stEpoch latest == Types.stEpoch st && onPull latest (Git.prNumber pr)) $
      case outcome of
        Left err -> Jobs.jobFailed label err
        Right (CheckoutRefused failure) -> Types.setMsg ("! " <> GitHub.failureText failure)
        Right CheckedOut -> do
          Settings.setMode (Git.AtPull (Git.pullAtHead pr) Git.Checkout)
          Types.setMsg ("editing " <> Git.pullLabel pr <> " on " <> Git.prHeadBranch pr)

reviewPull :: Brick.EventM Types.Name Types.St ()
reviewPull = do
  st <- State.get
  case Types.stMode st of
    Git.AtPull pr Git.Checkout -> do
      Settings.setMode (Git.AtPull (Git.pullAtHead pr) Git.ReadOnly)
      pollPullNow
    Git.AtPull pr Git.ReadOnly -> Types.setMsg (Git.pullLabel pr <> " is already read-only \8212 :pr edit opens it in a worktree")
    _ -> Types.setMsg noPullOpen
