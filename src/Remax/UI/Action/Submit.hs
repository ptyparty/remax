{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Submit
  ( openSubmit
  , reopenSubmit
  , handleSubmitKey
  , itemLabel
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Database.SQLite.Simple qualified as SQLite
import Graphics.Vty qualified as V
import Remax.Conversation qualified as Conv
import Remax.Git qualified as Git
import Remax.GitHub qualified as GitHub
import Remax.Modal qualified as Modal
import Remax.PullPoll qualified as PullPoll
import Remax.Submit qualified as Submit
import Remax.UI.Action.Inbox qualified as InboxAction
import Remax.UI.Action.Threads qualified as ThreadAction
import Remax.UI.Core.Jobs qualified as Jobs
import Remax.UI.Core.Markdown qualified as Markdown
import Remax.UI.Core.Threads qualified as Threads
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Model.Work qualified as WorkModel
import Remax.Vi qualified as Vi
import Remax.WorkStore qualified as WorkStore

openSubmit :: Brick.EventM Types.Name Types.St ()
openSubmit = do
  st <- State.get
  case (Git.subjectOf (Types.stMode st), Types.ppSnapshot (Types.stPullPoll st)) of
    (Git.Pull _ _, _) | Maybe.isJust (Types.stSending st) -> Types.setMsg "the review is still sending"
    (Git.Pull pr editing, snap) ->
      case Types.openPullState st of
        Just ended | Just label <- GitHub.endedLabel ended -> Types.setMsg ("the pull request is " <> label <> " \8212 nothing to submit to")
        _ -> do
          let
            items = Submit.outgoing (WorkModel.remoteOf st) (Types.stWork st)
          placed <- MonadIO.liftIO (Submit.placeItems (Types.stRoot st) pr items)
          let
            ed0 = Modal.newEdState 0 [""]
            own = do
              s <- snap
              viewer <- Conv.remoteViewer (PullPoll.snapshotRemote s)
              pure (Conv.loginToText viewer == PullPoll.pullHeadAuthor (PullPoll.snapshotHead s))
            events = case (editing, own) of
              (Git.Checkout, _) -> Types.CommentOnly
              (_, Just True) -> Types.CommentOnly
              _ -> Types.AnyEvent
            event = case events of
              Types.CommentOnly -> Just GitHub.EventComment
              Types.AnyEvent -> Nothing
          State.modify $ \s ->
            s
              { Types.stSubmit =
                  Just
                    Types.SubmitDialog
                      { Types.sdEvent = event
                      , Types.sdEvents = events
                      , Types.sdSummary = ed0 {Modal.edMode = Modal.Insert}
                      , Types.sdFocus = Types.SummaryFocus
                      , Types.sdSel = 0
                      , Types.sdDeleting = False
                      , Types.sdItems = placed
                      , Types.sdFailure = Nothing
                      }
              , Types.stVi = Vi.viIdle
              }
    _ -> Types.setMsg "no pull request open \8212 :pr opens one"

{- | After a popup opened from the list returns, the dialog comes back
with the list read afresh.
-}
reopenSubmit :: Brick.EventM Types.Name Types.St ()
reopenSubmit = do
  st <- State.get
  case (Git.subjectOf (Types.stMode st), Types.stSubmit st) of
    (Git.Pull pr _, Just dialog) -> do
      placed <- MonadIO.liftIO (Submit.placeItems (Types.stRoot st) pr (Submit.outgoing (WorkModel.remoteOf st) (Types.stWork st)))
      State.modify (\s -> s {Types.stSubmit = Just dialog {Types.sdItems = placed, Types.sdSel = min (Types.sdSel dialog) (max 0 (length placed - 1)), Types.sdFocus = Types.ListFocus}})
    _ -> pure ()

setDialog :: Types.SubmitDialog -> Brick.EventM Types.Name Types.St ()
setDialog dialog = State.modify (\s -> s {Types.stSubmit = Just dialog})

closeDialog :: Brick.EventM Types.Name Types.St ()
closeDialog = State.modify (\s -> s {Types.stSubmit = Nothing, Types.stVi = Vi.viIdle})

handleSubmitKey :: V.Event -> Types.SubmitDialog -> Brick.EventM Types.Name Types.St ()
handleSubmitKey ev dialog = do
  chord <- Types.readChord ev
  case chord of
    Types.CancelKey -> closeDialog
    Types.PrefixStarted -> pure ()
    Types.PrefixedKey _ -> Types.setMsg "C-c C-k cancels"
    Types.PlainKey _ ->
      case Types.sdFocus dialog of
        Types.SummaryFocus -> summaryKey ev dialog
        Types.ListFocus -> listKey ev dialog

summaryKey :: V.Event -> Types.SubmitDialog -> Brick.EventM Types.Name Types.St ()
summaryKey ev dialog =
  let
    ed = Types.sdSummary dialog
    editing = case Modal.handleInsert ev ed of
      Modal.InsertHandled next -> setDialog dialog {Types.sdSummary = next}
      Modal.InsertExit next -> setDialog dialog {Types.sdSummary = next}
      Modal.InsertUnhandled -> pure ()
  in
    case (Modal.edMode ed, ev) of
      (Modal.Insert, _) -> editing
      (Modal.Normal, V.EvKey V.KEsc []) -> closeDialog
      (Modal.Normal, V.EvKey V.KEnter []) -> post dialog
      (Modal.Normal, V.EvKey (V.KChar '\t') []) -> setDialog dialog {Types.sdFocus = Types.ListFocus}
      (Modal.Normal, V.EvKey (V.KChar 'h') []) -> setDialog dialog {Types.sdEvent = stepEvent (-1) dialog}
      (Modal.Normal, V.EvKey V.KLeft []) -> setDialog dialog {Types.sdEvent = stepEvent (-1) dialog}
      (Modal.Normal, V.EvKey (V.KChar 'l') []) -> setDialog dialog {Types.sdEvent = stepEvent 1 dialog}
      (Modal.Normal, V.EvKey V.KRight []) -> setDialog dialog {Types.sdEvent = stepEvent 1 dialog}
      (Modal.Normal, _) -> Monad.forM_ (Modal.handleNormal ev ed) (\next -> setDialog dialog {Types.sdSummary = next})

{- | h and l walk Comment, Approve and Request changes; your own pull
request offers Comment alone.
-}
stepEvent :: Int -> Types.SubmitDialog -> Maybe GitHub.ReviewEvent
stepEvent d dialog =
  let
    events = [GitHub.EventComment, GitHub.EventApprove, GitHub.EventRequestChanges]
    here = Types.sdEvent dialog >>= (`List.elemIndex` events)
  in
    case (Types.sdEvents dialog, here) of
      (Types.CommentOnly, _) -> Just GitHub.EventComment
      (_, Just i) -> Types.safeIdx events (max 0 (min 2 (i + d)))
      (_, Nothing) -> Just (if d < 0 then GitHub.EventRequestChanges else GitHub.EventComment)

listKey :: V.Event -> Types.SubmitDialog -> Brick.EventM Types.Name Types.St ()
listKey ev dialog0 =
  let
    dialog = dialog0 {Types.sdDeleting = False}
    items = Types.sdItems dialog
    sel = Types.sdSel dialog
    move d = setDialog dialog {Types.sdSel = max 0 (min (length items - 1) (sel + d))}
    chosen = Types.safeIdx items sel
  in
    case ev of
      V.EvKey V.KEsc [] -> setDialog dialog {Types.sdFocus = Types.SummaryFocus}
      V.EvKey (V.KChar '\t') [] -> setDialog dialog {Types.sdFocus = Types.SummaryFocus}
      V.EvKey (V.KChar 'j') [] -> move 1
      V.EvKey V.KDown [] -> move 1
      V.EvKey (V.KChar 'k') [] -> move (-1)
      V.EvKey V.KUp [] -> move (-1)
      V.EvKey (V.KChar 'e') [] -> Monad.forM_ chosen editItem
      V.EvKey (V.KChar 'd') []
        | Types.sdDeleting dialog0 -> Monad.forM_ chosen dropItem
        | otherwise -> setDialog dialog {Types.sdDeleting = True}
      V.EvKey V.KEnter [] -> Monad.forM_ chosen jumpToItem
      _ -> setDialog dialog

editItem :: Submit.Placed -> Brick.EventM Types.Name Types.St ()
editItem placed = do
  st <- State.get
  let
    item = Submit.placedItem placed
    conv = List.find ((== Submit.itemTarget item) . Conv.convTarget) (Types.stConversations st)
    origin = case Submit.itemKind item of
      Submit.ReplyUnder _ _ author -> Types.ReplyOrigin author
      Submit.ReplyUnknown -> Types.ReplyOrigin "the thread"
      _ -> Types.CommentOrigin
  case conv of
    Just c -> do
      Monad.void (ThreadAction.openWorkPopup c (Types.EditPending (Submit.itemWork item) origin) (Conv.markdownText (Submit.itemBody item)))
      State.modify $ \s ->
        s
          { Types.stNotePopup =
              fmap
                ( \np -> case Types.npPurpose np of
                    Types.WorkFor wp -> np {Types.npPurpose = Types.WorkFor wp {Types.wpReturnToSubmit = True}}
                    _ -> np
                )
                (Types.stNotePopup s)
          }
    Nothing -> Types.setMsg "that item's conversation is gone"

dropItem :: Submit.Placed -> Brick.EventM Types.Name Types.St ()
dropItem placed = do
  st <- State.get
  MonadIO.liftIO (WorkStore.discardWork (Types.stConn st) (Submit.itemWork (Submit.placedItem placed)))
  WorkModel.reloadWork
  reopenSubmit
  Types.setMsg "dropped"

{- | Enter on a list row lands the body on the item's line and closes the
dialog; a PR-level item has no line to go to.
-}
jumpToItem :: Submit.Placed -> Brick.EventM Types.Name Types.St ()
jumpToItem placed = do
  st <- State.get
  let
    item = Submit.placedItem placed
    convs = Types.stConversations st
    conv = case Submit.itemKind item of
      Submit.ReplyUnder tid _ _ -> Conv.conversationFor (Conv.ThreadConversation tid) convs
      _ -> List.find ((== Submit.itemTarget item) . Conv.convTarget) convs
  case fmap (\c -> (c, Conv.convAnchor c)) conv of
    Just (_, Conv.OnPull) -> Types.setMsg "a PR-level comment has no line"
    Just (c, _) -> do
      closeDialog
      InboxAction.jumpTo c
    Nothing -> Types.setMsg "that item's conversation is gone"

itemLabel :: Types.St -> Submit.Placed -> (T.Text, T.Text, T.Text, T.Text)
itemLabel st placed =
  let
    item = Submit.placedItem placed
    first = Markdown.firstLine (Submit.itemBody item)
    threadPlace tid =
      maybe "?" (Threads.anchorPlace . Conv.convAnchor) (List.find ((== Conv.ThreadConversation tid) . Conv.convId) (Types.stConversations st))
    (glyph, place, what) = case Submit.itemKind item of
      Submit.NewComment lineSpan ->
        ( "@"
        , Threads.anchorPlace (Conv.OnLines lineSpan)
        , case Submit.placedAs placed of
            Submit.Inline _ -> "new comment"
            _ -> "PR-level, off the diff"
        )
      Submit.NewPullComment -> ("@", "PR-level", "comment")
      Submit.ReplyUnder tid _ author -> ("\8618", threadPlace tid, "reply to " <> author)
      Submit.ReplyUnknown -> ("\8618", "?", "reply \183 thread not polled")
      Submit.QuoteReply _ -> ("@", "PR-level", "quote-reply")
  in
    (glyph, place, what, first)

{- | Send everything, then drop what GitHub took and ask for the next
poll, leaving what it refused in the dialog with its reason.
-}
post :: Types.SubmitDialog -> Brick.EventM Types.Name Types.St ()
post dialog = do
  st <- State.get
  case (Git.subjectOf (Types.stMode st), Types.sdEvent dialog) of
    (_, Nothing) -> Types.setMsg "pick an event with h and l first"
    (Git.Pull pr _, Just event) -> do
      let
        submission =
          Submit.Submission
            { Submit.submissionRoot = Types.stRoot st
            , Submit.submissionPull = pr
            , Submit.submissionEvent = event
            , Submit.submissionBody = T.strip (T.intercalate "\n" (Modal.bufferLines (Types.sdSummary dialog)))
            , Submit.submissionItems = filter sendable (Types.sdItems dialog)
            }
        sendable p = case Submit.placedAs p of
          Submit.Unplaceable _ -> False
          _ -> True
        epoch = Types.stEpoch st
        landed accepted = Jobs.fromJob (Types.stChan st) (MonadIO.liftIO (recordAccepted (Types.stConn st) (Types.repoText st) accepted))
      closeDialog
      State.modify (\s -> s {Types.stSending = Just dialog {Types.sdFailure = Nothing}})
      Jobs.startJob "submitting the review" (Submit.submit landed submission) $ \outcome -> do
        latest <- State.get
        State.modify (\s -> s {Types.stSending = Nothing})
        Monad.when (Types.stEpoch latest == epoch) $
          case outcome of
            Left err -> do
              restoreDialog (Types.stSending latest) err
              Jobs.jobFailed "submitting" err
            Right sent -> landSent (Types.stSending latest) sent
    _ -> Types.setMsg "no pull request open"

recordAccepted :: SQLite.Connection -> T.Text -> Submit.Accepted -> IO ()
recordAccepted conn repo accepted = do
  Monad.forM_ (Submit.acceptedAs accepted) (\(lid, cid) -> WorkStore.recordSubmitted conn repo lid cid)
  WorkStore.discardWork conn (Submit.acceptedWork accepted)

restoreDialog :: Maybe Types.SubmitDialog -> T.Text -> Brick.EventM Types.Name Types.St ()
restoreDialog sending why =
  Monad.forM_ sending $ \dialog -> do
    setDialog dialog {Types.sdFailure = Just why}
    reopenSubmit

landSent :: Maybe Types.SubmitDialog -> Submit.Sent -> Brick.EventM Types.Name Types.St ()
landSent sending sent = do
  WorkModel.reloadWork
  ThreadAction.pollSoon
  let
    took = length (Submit.sentAccepted sent)
    reviewed = Maybe.isJust (Submit.sentReview sent)
  case Submit.sentFailure sent of
    Nothing ->
      Types.setMsg
        ( if reviewed || took > 0
            then "posted " <> (if reviewed then "the review" else "") <> (if took > 0 then (if reviewed then " and " else "") <> Types.tshow took <> " comment(s)" else "")
            else "nothing to send"
        )
    Just failure -> do
      restoreDialog sending (GitHub.failureText failure)
      Types.setMsg ("GitHub refused: " <> GitHub.failureText failure)
