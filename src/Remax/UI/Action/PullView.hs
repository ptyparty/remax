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
import Remax.UI.Action.Open qualified as Open
import Remax.UI.Action.Threads qualified as ThreadAction
import Remax.UI.Core.PullView qualified as PullView
import Remax.UI.Core.Threads qualified as Threads
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.View qualified as View
import Remax.UI.Model.Nav qualified as Nav

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
closePullView = State.modify (\s -> s {Types.stPullView = Nothing, Types.stBand = Nothing})

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
    here = Types.stBand st >>= \b -> List.elemIndex b {Types.bandDeleting = False} all_
    next = case here of
      Just i -> Types.safeIdx all_ (max 0 (min (length all_ - 1) (i + d)))
      Nothing -> if d > 0 then Types.safeIdx all_ 0 else Types.safeIdx all_ (length all_ - 1)
  State.modify (\s -> s {Types.stBand = next})

{- | Moves to the newest post of the next thread awaiting your answer,
wrapping around the timeline.
-}
jumpAwaiting :: Int -> Brick.EventM Types.Name Types.St ()
jumpAwaiting d = do
  width <- viewWidth
  st <- State.get
  let
    all_ = PullView.stops st width
    awaits b = maybe False Threads.awaitsYou (Conv.conversationFor (Types.bandConv b) (Types.stConversations st))
    newest = map NEL.last . NEL.groupBy (\a b -> Types.bandConv a == Types.bandConv b) . filter awaits $ all_
    current = Types.stBand st
    here = current >>= \b -> List.elemIndex b {Types.bandDeleting = False} all_
    position b = Maybe.fromMaybe 0 (List.elemIndex b all_)
    elsewhere b = fmap Types.bandConv current /= Just (Types.bandConv b)
    candidates = filter elsewhere newest
    (before, after) = List.partition (\b -> maybe False (position b <) here) candidates
    target = case here of
      Nothing -> if d > 0 then Maybe.listToMaybe candidates else Maybe.listToMaybe (reverse candidates)
      Just _ -> if d > 0 then Maybe.listToMaybe (after ++ before) else Maybe.listToMaybe (reverse before ++ reverse after)
  case target of
    Just b -> State.modify (\s -> s {Types.stBand = Just b})
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
  case ThreadAction.bandPost st of
    Just _ -> ThreadAction.toggleFoldAtBand
    Nothing -> Types.setMsg "j onto a thread's post first"

{- | Enter takes a proposal, and on a thread's posted comment opens its
lines in the diff.
-}
enterKey :: Brick.EventM Types.Name Types.St ()
enterKey = do
  st <- State.get
  case ThreadAction.bandPost st of
    Just (conv, _, post) ->
      case (Conv.postState post, Conv.convAnchor conv) of
        (Conv.Proposal _, _) -> Monad.void ThreadAction.takeKey
        (_, Conv.OnPosition pos) -> do
          closePullView
          Open.openFilePath (Conv.posPath pos)
          Nav.gotoLine (Conv.posLine pos)
        _ -> Types.setMsg "a PR-level comment has no line"
    Nothing -> Types.setMsg "j onto a post first"
