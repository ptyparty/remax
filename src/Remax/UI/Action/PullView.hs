{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.PullView
  ( openPullView
  , togglePullView
  , closePullView
  , handlePullViewKey
  , jumpAwaiting
  , commentOnPull
  , listChecks
  , viewWidth
  , markReady
  , markDraft
  , foldHere
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Remax.Config qualified as Config
import Remax.Conversation qualified as Conv
import Remax.Git qualified as Git
import Remax.GitHub qualified as GitHub
import Remax.PullPoll qualified as PullPoll
import Remax.UI.Action.Inbox qualified as InboxAction
import Remax.UI.Action.Pull qualified as PullAction
import Remax.UI.Action.Threads qualified as ThreadAction
import Remax.UI.Core.PullView qualified as PullView
import Remax.UI.Core.Threads qualified as Threads
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.View qualified as View

openPullView :: Brick.EventM Types.Name Types.St ()
openPullView = do
  st <- State.get
  case Git.subjectOf (Types.stMode st) of
    Git.Pull _ _ -> State.modify Types.showPullView
    _ -> Types.setMsg "no pull request open \8212 :pr opens one"

togglePullView :: Brick.EventM Types.Name Types.St ()
togglePullView = do
  st <- State.get
  case Types.stPullView st of
    Just _ -> closePullView
    Nothing -> openPullView

closePullView :: Brick.EventM Types.Name Types.St ()
closePullView = State.modify Types.hidePullView

{- | The timeline's text width: the terminal less the tray and the
gutter, the same number the draw wraps with.
-}
viewWidth :: Brick.EventM Types.Name Types.St Int
viewWidth = do
  st <- State.get
  columns <- View.terminalColumns
  pure (viewWidthFor st columns)

viewWidthFor :: Types.St -> Int -> Int
viewWidthFor st columns =
  let
    tray = if Types.stTray st && not (null (Types.stFiles st)) then 32 else 0
  in
    max 40 (columns - tray - 6)

handlePullViewKey :: V.Event -> Brick.EventM Types.Name Types.St ()
handlePullViewKey ev = do
  st <- State.get
  let
    keys = Config.cfgKeys (Types.stConfig st)
    plain = keys /= Config.KeysEmacs
    acted act = do
      handled <- act
      case handled of
        ThreadAction.Handled -> pure ()
        ThreadAction.Unhandled -> Types.setMsg "not on this post"
    deleting = maybe False Types.bandDeleting (Types.stBand st)
  State.modify (\s -> s {Types.stBand = fmap (\b -> b {Types.bandDeleting = False}) (Types.stBand s)})
  case ev of
    V.EvKey V.KEsc [] -> closePullView
    V.EvKey (V.KChar 'c') [V.MCtrl] | not plain -> State.modify (\s -> s {Types.stEmacsPrefix = Just 'c'})
    V.EvKey (V.KChar 'j') [] | plain -> moveStop 1
    V.EvKey V.KDown [] -> moveStop 1
    V.EvKey (V.KChar 'n') [V.MCtrl] -> moveStop 1
    V.EvKey (V.KChar 'k') [] | plain -> moveStop (-1)
    V.EvKey V.KUp [] -> moveStop (-1)
    V.EvKey (V.KChar 'p') [V.MCtrl] -> moveStop (-1)
    V.EvKey (V.KChar ']') [] | plain -> jumpAwaiting 1
    V.EvKey (V.KChar '[') [] | plain -> jumpAwaiting (-1)
    V.EvKey (V.KChar 'r') [] | plain -> acted ThreadAction.replyKey
    V.EvKey (V.KChar '?') [] | plain -> acted ThreadAction.askKey
    V.EvKey (V.KChar '!') [] | plain -> acted ThreadAction.flagKey
    V.EvKey (V.KChar '@') [] | plain -> commentOnPull
    V.EvKey (V.KChar 'e') [] | plain -> acted ThreadAction.editKey
    V.EvKey (V.KChar '+') [] | plain -> acted ThreadAction.reactKey
    V.EvKey (V.KChar 'y') [] | plain -> acted ThreadAction.copyKey
    V.EvKey (V.KChar 'c') [] | plain -> listChecks
    V.EvKey (V.KChar 'z') [] | plain -> State.modify (\s -> s {Types.stZPending = True})
    V.EvKey (V.KChar 'a') []
      | plain && Types.stZPending st -> do
          State.modify (\s -> s {Types.stZPending = False})
          foldHere
    V.EvKey (V.KChar 'd') []
      | plain && deleting -> acted ThreadAction.discardKey
      | plain -> State.modify (\s -> s {Types.stBand = fmap (\b -> b {Types.bandDeleting = True}) (Types.stBand s)})
    V.EvKey V.KEnter [] -> enterKey
    V.EvKey (V.KChar ' ') [] | plain -> State.modify (\s -> s {Types.stSpace = True})
    V.EvKey (V.KChar ':') [] | plain -> pure ()
    _ -> pure ()

moveStop :: Int -> Brick.EventM Types.Name Types.St ()
moveStop d = do
  width <- viewWidth
  st <- State.get
  let
    all_ = PullView.stops st width
    here = PullView.selectedStop st >>= (`List.elemIndex` all_)
    next = case here of
      Just i -> Types.safeIdx all_ (max 0 (min (length all_ - 1) (i + d)))
      Nothing -> if d > 0 then Types.safeIdx all_ 0 else Types.safeIdx all_ (length all_ - 1)
  State.modify (selectStop next)

selectStop :: Maybe PullView.Stop -> Types.St -> Types.St
selectStop stop s =
  let
    (band, commit) = case stop of
      Just (PullView.PostStop cid i) -> (Just (Types.Band cid i False), Nothing)
      Just (PullView.CommitStop sha) -> (Nothing, Just sha)
      Nothing -> (Nothing, Nothing)
  in
    s {Types.stBand = band, Types.stPullView = fmap (\pv -> pv {Types.pvCommit = commit}) (Types.stPullView s)}

stopConv :: PullView.Stop -> Maybe Conv.ConversationId
stopConv stop =
  case stop of
    PullView.PostStop cid _ -> Just cid
    PullView.CommitStop _ -> Nothing

{- | Moves to the newest post of the next thread awaiting your answer,
wrapping around the timeline.
-}
jumpAwaiting :: Int -> Brick.EventM Types.Name Types.St ()
jumpAwaiting d = do
  width <- viewWidth
  st <- State.get
  let
    all_ = PullView.stops st width
    awaits stop = maybe False (Threads.awaitsYou st) (stopConv stop >>= (`Conv.conversationFor` Types.stConversations st))
    newest = map NEL.last . NEL.groupBy (\a b -> stopConv a == stopConv b) . filter awaits $ all_
    current = PullView.selectedStop st
    here = current >>= (`List.elemIndex` all_)
    position b = Maybe.fromMaybe 0 (List.elemIndex b all_)
    elsewhere b = (current >>= stopConv) /= stopConv b
    candidates = filter elsewhere newest
    (before, after) = List.partition (\b -> maybe False (position b <) here) candidates
    target = case here of
      Nothing -> if d > 0 then Maybe.listToMaybe candidates else Maybe.listToMaybe (reverse candidates)
      Just _ -> if d > 0 then Maybe.listToMaybe (after ++ before) else Maybe.listToMaybe (reverse before ++ reverse after)
  case target of
    Just b -> State.modify (selectStop (Just b))
    Nothing ->
      if null newest
        then Types.setMsg "no thread awaits you"
        else Types.setMsg "no other thread awaits you"

markReady :: Brick.EventM Types.Name Types.St ()
markReady = changeDraftness PullPoll.ReadyForReview

markDraft :: Brick.EventM Types.Name Types.St ()
markDraft = changeDraftness PullPoll.Draft

changeDraftness :: PullPoll.Draftness -> Brick.EventM Types.Name Types.St ()
changeDraftness wanted = do
  st <- State.get
  let
    current = fmap (PullPoll.pullHeadDraft . PullPoll.snapshotHead) (Types.ppSnapshot (Types.stPullPoll st))
  case (Git.subjectOf (Types.stMode st), wanted) of
    (Git.Pull pr _, _) | current == Just wanted -> Types.setMsg ("#" <> Types.tshow (Git.prNumber pr) <> " is already " <> draftnessText wanted)
    (Git.Pull pr _, PullPoll.ReadyForReview) ->
      ThreadAction.githubJob "marking it ready" (GitHub.markReady (Types.stRoot st) (Git.prRepo pr) (Git.prNumber pr)) "marked ready for review"
    (Git.Pull pr _, PullPoll.Draft) ->
      ThreadAction.githubJob "converting to a draft" (GitHub.markDraft (Types.stRoot st) (Git.prRepo pr) (Git.prNumber pr)) "converted to a draft"
    _ -> Types.setMsg "no pull request open \8212 :pr opens one"

draftnessText :: PullPoll.Draftness -> T.Text
draftnessText d =
  case d of
    PullPoll.ReadyForReview -> "ready for review"
    PullPoll.Draft -> "a draft"

commentOnPull :: Brick.EventM Types.Name Types.St ()
commentOnPull = do
  st <- State.get
  Monad.void (ThreadAction.openWorkPopupAt st "PR-level" Nothing (Types.CommentOn Conv.OnPull) "")

listChecks :: Brick.EventM Types.Name Types.St ()
listChecks = do
  st <- State.get
  case Types.ppSnapshot (Types.stPullPoll st) of
    Just snap -> State.modify (\s -> s {Types.stInfoPopup = Just ("checks \183 " <> PullView.checksSummary snap, PullView.checksListing snap)})
    Nothing -> Types.setMsg "the checks are still loading"

foldHere :: Brick.EventM Types.Name Types.St ()
foldHere = do
  st <- State.get
  case (PullView.selectedStop st, ThreadAction.bandPost st) of
    (Just (PullView.CommitStop sha), _) -> toggleMessage st sha
    (_, Just _) -> ThreadAction.toggleFoldAtBand
    _ -> Types.setMsg "j onto a thread's post or a commit first"

toggleMessage :: Types.St -> T.Text -> Brick.EventM Types.Name Types.St ()
toggleMessage st sha =
  let
    commit = Types.ppSnapshot (Types.stPullPoll st) >>= List.find ((== sha) . PullPoll.pullCommitSha) . PullPoll.snapshotCommits
  in
    case commit of
      Just c
        | not (T.null (PullPoll.pullCommitBody c)) -> State.modify (\s -> s {Types.stOpenCommits = ThreadAction.toggleMember sha (Types.stOpenCommits s)})
      _ -> Types.setMsg "this commit has no message beyond its subject"

{- | Enter takes a proposal, on a thread's posted comment opens its
lines in the diff, and on a commit opens the commit.
-}
enterKey :: Brick.EventM Types.Name Types.St ()
enterKey = do
  st <- State.get
  case (PullView.selectedStop st, ThreadAction.bandPost st) of
    (Just (PullView.CommitStop sha), _) -> PullAction.walkToCommit sha
    (_, Just (conv, _, post)) ->
      case (Conv.postState post, Conv.convAnchor conv) of
        (Conv.Proposal _ _, _) -> Monad.void ThreadAction.takeKey
        (_, Conv.OnPull) -> Types.setMsg "a PR-level comment has no line"
        _ -> InboxAction.jumpTo conv
    _ -> Types.setMsg "j onto a post or a commit first"
