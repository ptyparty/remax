{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Notes
  ( markThenNote
  , unnoteCmd
  , setNotePopupEd
  , openNotePopup
  , handleNotePopupKey
  , popupBoxWidth
  , popupRowWidth
  , NoteAct (..)
  , noteAct
  , transcriptPosts
  , postRows
  ) where

import Brick qualified as Brick
import Control.Applicative ((<|>))
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Remax.Config qualified as Config
import Remax.Conversation qualified as Conv
import Remax.DiffModel qualified as DiffModel
import Remax.Modal qualified as Modal
import Remax.SoftWrap qualified as SoftWrap
import Remax.Store qualified as Store
import Remax.UI.Action.Cite qualified as CiteAction
import Remax.UI.Action.Conversation qualified as Conversation
import Remax.UI.Action.Rebase qualified as Rebase
import Remax.UI.Action.Threads qualified as ThreadAction
import Remax.UI.Action.Verdicts qualified as VerdictAction
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Threads qualified as Threads
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Verdict qualified as Verdict
import Remax.UI.Core.View qualified as View
import Remax.UI.Model.Nav qualified as Nav
import Remax.Vi qualified as Vi
import Remax.Zipper qualified as Z

-- A flag or question wants its reason on record: the note popup opens
-- right away (config: note-on-flag = 0 turns this off), targeting the
-- lines just marked. Discarding keeps the mark, only the note is
-- skipped.
markThenNote :: Store.Verdict -> Brick.EventM Types.Name Types.St ()
markThenNote v = do
  preSt <- State.get
  let
    noting = Config.cfgNoteOnFlag (Types.stConfig preSt)
  -- with the note popup coming, the post-mark jump waits until the
  -- popup closes — advancing first means writing the note while looking
  -- at the wrong hunk
  Monad.when noting (State.modify (\s -> s {Types.stDeferAdvance = True}))
  marked <- VerdictAction.applyVerdict v
  if marked && noting
    then do
      after <- State.get
      let
        undoing =
          Types.MarkUndo
            { Types.muRows = Types.stLastRows after
            , Types.muBefore =
                Map.fromList
                  [ (k, Map.lookup k (Types.stVerdicts preSt))
                  | k <- Types.stLastMarked after
                  ]
            , Types.muCounts = Types.stCounts preSt
            , Types.muPunch = Types.stPunch preSt
            }
      openNotePopup (Types.NoteFromMark undoing)
      opened <- State.get
      case Types.stNotePopup opened of
        Nothing -> VerdictAction.runDeferredAdvance
        Just _ -> pure ()
    else State.modify (\s -> s {Types.stDeferAdvance = False})

-- What a note applies to: an active selection wins; then the line under
-- the cursor, when it already carries a verdict or a note (sitting on a
-- noted line and typing :note must open THAT note — a noted line's
-- whole group comes along, so the note stays one note); then whatever
-- was just marked (marking auto-advances, so the cursor has already
-- moved on); with none of those, the current hunk.
noteTargetKeys :: Types.St -> [T.Text]
noteTargetKeys st =
  let
    selectionKeys = do
      fv <- Types.currentFileMay st
      (lo, hi) <- View.activeSelectionRows (Types.fvEd fv)
      pure (changeKeys fv lo hi)
    cursorKeys = Types.currentFileMay st >>= cursorNoteKeys st
    hunkKeys = do
      fv <- Types.currentFileMay st
      r <- Types.safeIdx (Types.fvRuns fv) (Types.clampH (Types.fvRuns fv) (Types.stCurH st))
      pure (DiffModel.runKeys r)
    lastKeys =
      if null (Types.stLastMarked st) then Nothing else Just (Types.stLastMarked st)
  in
    Maybe.fromMaybe [] (selectionKeys <|> cursorKeys <|> lastKeys <|> hunkKeys)

changeKeys :: Types.FileView -> Int -> Int -> [T.Text]
changeKeys fv lo hi = map (\(k, _, _) -> k) (Nav.selectedChanges True fv lo hi)

-- The keys a note under the cursor is about: the whole group of a noted
-- line or hunk, or the marked lines of the cursor's row.
cursorNoteKeys :: Types.St -> Types.FileView -> Maybe [T.Text]
cursorNoteKeys st fv = do
  let
    row = fst (Modal.cursorRC (Types.fvEd fv))
    ks = changeKeys fv row row
    relevant k = Map.member k (Types.stVerdicts st) || Map.member k (Types.stThreads st)
    groupAmong keys = Maybe.listToMaybe (Maybe.mapMaybe (\k -> Map.lookup k (Types.stNoteGroups st)) keys)
    -- a conversation anywhere in the hunk under the cursor: from any
    -- line of the hunk, :note continues that thread rather than
    -- opening a second one a few lines away
    hunkGroup = do
      h <- Nav.hunkUnderCursor st
      r <- Types.safeIdx (Types.fvRuns fv) h
      groupAmong (DiffModel.runKeys r)
  case groupAmong ks <|> hunkGroup of
    Just gid -> Just (groupKeys st fv gid)
    Nothing
      | any relevant ks -> Just (filter relevant ks)
      | otherwise -> Nothing

-- The whole group, found by its name, so editing keeps it a
-- single thread rather than forking per line.
--
-- By name and not by what the note says: two notes worded the
-- same — "typo", "why?" — are two notes, and matching on the
-- words made them one. Editing one then rewrote the other, and
-- an agent's reply to one appeared under both.
groupKeys :: Types.St -> Types.FileView -> T.Text -> [T.Text]
groupKeys st fv gid =
  let
    inGroup r =
      Maybe.listToMaybe (filter (\k -> Map.lookup k (Types.stNoteGroups st) == Just gid) (Verdict.rowFeedbackKeys r))
  in
    Maybe.mapMaybe inGroup (Types.fvRows fv)

-- Save the reviewer's side of the conversation (empty text clears the
-- whole thread instead), keep the in-memory view in step, and report
-- where it landed. While the reviewer's own post is the newest it stays
-- editable in place; once someone answered, saving appends the next
-- post.
applyNote :: T.Text -> [T.Text] -> Brick.EventM Types.Name Types.St ()
applyNote text keys = do
  st <- State.get
  if T.null text
    then do
      n <- MonadIO.liftIO (Store.clearThread (Types.stConn st) (Types.repoText st) keys)
      State.modify $ \s ->
        s {Types.stThreads = foldr Map.delete (Types.stThreads s) keys}
      Conversation.refreshNoteGroups
      Types.setMsg $
        if n == 0
          then "no notes here"
          else "removed the conversation from " <> Types.tshow n <> " line(s)"
    else do
      n <- MonadIO.liftIO (Store.saveReviewerPost (Types.stConn st) (Types.repoText st) text keys)
      let
        markedKeys = filter (`Map.member` Types.stVerdicts st) keys
        place thread = case reverse thread of
          (Store.Post "reviewer" _ : older) -> reverse (Store.Post "reviewer" text : older)
          _ -> thread ++ [Store.Post "reviewer" text]
        update k m = Map.insert k (place (Map.findWithDefault [] k m)) m
      State.modify $ \s ->
        s {Types.stThreads = foldr update (Types.stThreads s) markedKeys}
      Conversation.refreshNoteGroups
      Types.setMsg $
        if n == 0
          then "no verdicts here yet — mark with ! or ? first"
          else "note on " <> Types.tshow n <> " line(s)"

unnoteCmd :: Brick.EventM Types.Name Types.St ()
unnoteCmd = do
  st <- State.get
  let
    keys = noteTargetKeys st
  if null keys
    then Types.setMsg "nothing here to unnote"
    else applyNote "" keys

-- The note popup: a small modal buffer with the full editing keys.
-- Enter in its normal mode saves, Ctrl-c discards; an empty note clears.
-- Discarding one that ! or ? opened cancels that mark too.
openNotePopup :: Types.NoteOrigin -> Brick.EventM Types.Name Types.St ()
openNotePopup origin = do
  st <- State.get
  let
    -- A note that ! or ? opened is about what that keystroke just
    -- marked, all of it. Asking noteTargetKeys instead lands it on the
    -- line under the cursor alone, because marking collapses the
    -- selection on its way out and the cursor is left on whichever line
    -- the selection happened to end at — so a note written across four
    -- lines was recorded against one of them, and if that one was
    -- unchanged it belonged to no hunk and nothing could ever resolve it.
    keys = case origin of
      Types.NoteFromMark _ | not (null (Types.stLastMarked st)) -> Types.stLastMarked st
      _ -> noteTargetKeys st
  if null keys
    then Types.setMsg "nothing to note — mark lines with ! or ? first"
    else do
      let
        existing = case Maybe.mapMaybe (`Map.lookup` Types.stThreads st) keys of
          (thread : _) -> case reverse thread of
            (Store.Post "reviewer" body : _) -> body
            _ -> ""
          [] -> ""
        ls = if T.null existing then [""] else T.lines existing
        lastRow = length ls - 1
        lastCol = case reverse ls of
          (l : _) -> T.length l
          [] -> 0
        ed0 = Modal.newEdState lastRow ls
        ed1 =
          ed0
            { Modal.edZipper = Z.moveCursor (lastRow, lastCol) (Modal.edZipper ed0)
            , Modal.edMode = Modal.Insert
            }
      State.modify
        (\s -> s {Types.stNotePopup = Just (Types.NotePopup ed1 (Types.NoteFor keys origin)), Types.stVi = Vi.viIdle})

setNotePopupEd :: Modal.EdState -> Brick.EventM Types.Name Types.St ()
setNotePopupEd ed = do
  State.modify $ \s ->
    s
      { Types.stNotePopup =
          fmap (\np -> np {Types.npEd = ed {Modal.edCopied = False}}) (Types.stNotePopup s)
      , Types.stKill = Modal.edKill ed
      }
  -- the popup shares the session's yank slot, so a copy made in a note
  -- reaches the clipboard the same way one made in the buffer does
  Nav.publishCopy ed

handleNotePopupKey :: V.Event -> Types.NotePopup -> Brick.EventM Types.Name Types.St ()
handleNotePopupKey ev np0 = do
  st0 <- State.get
  columns <- View.terminalColumns
  let
    layout = SoftWrap.WrappedRows (popupRowWidth (popupBoxWidth columns))
    np =
      np0
        { Types.npEd =
            (Types.npEd np0) {Modal.edKill = Types.stKill st0, Modal.edRowLayout = layout}
        }
  case ev of
    -- discard works from either popup mode; nobody should need to leave
    -- insert just to bail out
    V.EvKey (V.KChar 'c') [V.MCtrl] -> discardNote np
    V.EvKey (V.KChar 'g') [V.MCtrl]
      | Types.WorkFor _ <- Types.npPurpose np -> CiteAction.openCitePicker
    _ ->
      case Modal.edMode (Types.npEd np) of
        Modal.Insert ->
          case Modal.handleInsert ev (Types.npEd np) of
            Modal.InsertHandled ed -> setNotePopupEd ed
            Modal.InsertExit ed -> setNotePopupEd ed
            Modal.InsertUnhandled -> pure ()
        -- a half-typed vi command owns its next key here too, so `f⏎`
        -- looks for a character instead of posting the note
        Modal.Normal
          | Config.cfgKeys (Types.stConfig st0) == Config.KeysVi
          , Vi.viBusy (Types.stVi st0) ->
              notePopupVi st0 np ev
        Modal.Normal -> notePopupNormalKey st0 np ev

popupBoxWidth :: Int -> Int
popupBoxWidth columns = min 90 (max 50 (columns - 20))

popupRowWidth :: Int -> Int
popupRowWidth boxWidth = max 1 (boxWidth - 1)

notePopupNormalKey :: Types.St -> Types.NotePopup -> V.Event -> Brick.EventM Types.Name Types.St ()
notePopupNormalKey st np ev =
  case ev of
    V.EvKey V.KEnter [] -> saveNote np
    -- Esc from insert lands here in the popup's normal mode; Esc
    -- again cancels the dialog outright
    V.EvKey V.KEsc [] -> discardNote np
    V.EvKey (V.KChar '?') []
      | Types.WorkFor wp <- Types.npPurpose np
      , Types.ReplyTo target _ <- Types.wpAction wp ->
          ThreadAction.switchAction wp (Types.AskOn target)
    V.EvKey (V.KChar 'r') []
      | Types.WorkFor wp <- Types.npPurpose np
      , Types.AskOn target <- Types.wpAction wp ->
          ThreadAction.switchAction wp (Types.ReplyTo target (repliedTo st wp))
    -- the popup's normal mode follows the session's keymap, so a
    -- vi user gets vi here too; emacs is modeless and never
    -- reaches this branch
    _ ->
      case Config.cfgKeys (Types.stConfig st) of
        Config.KeysVi -> notePopupVi st np ev
        _ -> Monad.forM_ (Modal.handleNormal ev (Types.npEd np)) setNotePopupEd

-- Backing out of a note that ! or ? opened takes the mark with it.
-- The verdict was made by that same keystroke and has not been
-- explained yet, so abandoning the sentence abandons what it was
-- going to be about — and the advance the mark would have made never
-- happens either, since there is nothing to move on from.
--
-- A note asked for by :note is the other case: that mark was already
-- there and had nothing to do with this popup.
repliedTo :: Types.St -> Types.WorkPopup -> T.Text
repliedTo st wp =
  let
    posted c = filter (not . Conv.isLocalPost) (Conv.convPosts c)
    newest = do
      cid <- Types.wpConv wp
      conv <- Conv.conversationFor cid (Types.stConversations st)
      Maybe.listToMaybe (reverse (posted conv))
  in
    maybe "the thread" (Threads.authorName . Conv.postAuthor) newest

discardNote :: Types.NotePopup -> Brick.EventM Types.Name Types.St ()
discardNote np = do
  let
    discardMsg = case Types.npPurpose np of
      Types.NoteFor _ Types.NoteAsked -> "note discarded"
      Types.NoteFor _ (Types.NoteFromMark _) -> "mark cancelled"
      Types.AmendMessage _ -> "amend cancelled"
      Types.WorkFor _ -> "discarded"
  State.modify (\s -> s {Types.stNotePopup = Nothing, Types.stMsg = discardMsg, Types.stVi = Vi.viIdle})
  case Types.npPurpose np of
    Types.NoteFor _ (Types.NoteFromMark undoing) -> do
      VerdictAction.cancelMark undoing
      State.modify (\s -> s {Types.stDeferAdvance = False})
    _ -> VerdictAction.runDeferredAdvance

saveNote :: Types.NotePopup -> Brick.EventM Types.Name Types.St ()
saveNote np = do
  State.modify (\s -> s {Types.stNotePopup = Nothing, Types.stVi = Vi.viIdle})
  let
    text = T.strip (T.intercalate "\n" (Modal.bufferLines (Types.npEd np)))
  case Types.npPurpose np of
    Types.NoteFor keys _ -> do
      applyNote text keys
      VerdictAction.runDeferredAdvance
    Types.AmendMessage target -> Rebase.applyAmendMessage target text
    Types.WorkFor wp -> ThreadAction.saveWork wp text

notePopupVi :: Types.St -> Types.NotePopup -> V.Event -> Brick.EventM Types.Name Types.St ()
notePopupVi st np ev =
  case Vi.handleVi (Types.stVi st) ev (Types.npEd np) of
    Vi.ViUnhandled -> pure ()
    Vi.ViPending keys -> State.modify (\s -> s {Types.stVi = keys})
    Vi.ViDone keys ed -> do
      State.modify (\s -> s {Types.stVi = keys})
      setNotePopupEd ed

-- What the popup is about to do. The old chrome never said: a fresh
-- note, a rewrite of your own latest post, and a follow-up to somebody
-- else's answer all looked exactly alike.
data NoteAct = NoteNew | NoteEdit | NoteReply T.Text

noteAct :: [Store.Post] -> NoteAct
noteAct thread =
  case reverse thread of
    [] -> NoteNew
    (Store.Post author _ : _)
      | author == "reviewer" -> NoteEdit
      | otherwise -> NoteReply author

-- The posts to show above the editor. The whole conversation, except
-- that when you are rewriting your own latest post that post is already
-- in the editor and would otherwise appear twice.
transcriptPosts :: [Store.Post] -> [Store.Post]
transcriptPosts thread =
  case noteAct thread of
    NoteEdit -> init thread
    _ -> thread

-- One post as rendered transcript rows: the same glyphs the inline
-- conversation uses, wrapped to the popup, continuations indented under
-- the text so the authorship stays scannable down the left edge.
postRows :: Int -> Store.Post -> [Brick.Widget Types.Name]
postRows width post =
  let
    mine = Store.postAuthor post == "reviewer"
    lead =
      if mine
        then "\8801 you \183 "
        else "\8618 " <> Store.postAuthor post <> " \183 "
    attr = if mine then Attrs.noteInlineAttr else Attrs.replyInlineAttr
    -- a stored post keeps the reviewer's own line breaks; the transcript
    -- is a quotation, so re-wrap it to whatever width the popup got
    packed =
      case Modal.greedyWrap
        (max 12 (width - T.length lead))
        (T.words (T.unwords (T.lines (Store.postBody post)))) of
        [] -> [""]
        wss -> map T.unwords wss
  in
    case packed of
      [] -> []
      (first : rest) ->
        Brick.withAttr attr (Brick.txt (lead <> first))
          : map (\l -> Brick.withAttr attr (Brick.txt ("  " <> l))) rest
