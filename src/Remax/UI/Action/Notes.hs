{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Notes
  ( markThenNote
  , noteCmd
  , unnoteCmd
  , openNotePopup
  , handleNotePopupKey
  , NoteAct (..)
  , noteAct
  , transcriptPosts
  , postRows
  ) where

import Brick
  ( EventM
  , Widget
  , txt
  , withAttr
  )
import Control.Applicative ((<|>))
import Control.Monad
  ( forM_
  , when
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
  ( get
  , modify
  )
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( fromMaybe
  , listToMaybe
  , mapMaybe
  )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Remax.Config
import Remax.DiffModel
import Remax.Modal
import Remax.Store
import Remax.UI.Action.Conversation
import Remax.UI.Action.Rebase
import Remax.UI.Action.Verdicts
import Remax.UI.Core.Attrs
import Remax.UI.Core.Types
import Remax.UI.Core.Verdict
import Remax.UI.Core.View
import Remax.UI.Model.Nav
import Remax.Vi
  ( ViResult (..)
  , handleVi
  , viBusy
  , viIdle
  )
import qualified Remax.Zipper as Z

-- A flag or question wants its reason on record: the note popup opens
-- right away (config: note-on-flag = 0 turns this off), targeting the
-- lines just marked. Discarding keeps the mark, only the note is
-- skipped.
markThenNote :: Verdict -> EventM Name St ()
markThenNote v = do
  preSt <- get
  let
    noting = cfgNoteOnFlag (stConfig preSt)
  -- with the note popup coming, the post-mark jump waits until the
  -- popup closes — advancing first means writing the note while looking
  -- at the wrong hunk
  when noting (modify (\s -> s {stDeferAdvance = True}))
  marked <- applyVerdict v
  if marked && noting
    then do
      after <- get
      let
        undoing =
          MarkUndo
            { muRows = stLastRows after
            , muBefore =
                Map.fromList
                  [ (k, Map.lookup k (stVerdicts preSt))
                  | k <- stLastMarked after
                  ]
            , muCounts = stCounts preSt
            , muPunch = stPunch preSt
            }
      openNotePopup (NoteFromMark undoing)
      opened <- get
      case stNotePopup opened of
        Nothing -> runDeferredAdvance
        Just _ -> pure ()
    else modify (\s -> s {stDeferAdvance = False})

-- What a note applies to: an active selection wins; then the line under
-- the cursor, when it already carries a verdict or a note (sitting on a
-- noted line and typing :note must open THAT note — a noted line's
-- whole group comes along, so the note stays one note); then whatever
-- was just marked (marking auto-advances, so the cursor has already
-- moved on); with none of those, the current hunk.
noteTargetKeys :: St -> [Text]
noteTargetKeys st =
  let
    selectionKeys = do
      fv <- currentFileMay st
      (lo, hi) <- activeSelectionRows (fvEd fv)
      pure (changeKeys fv lo hi)
    cursorKeys = currentFileMay st >>= cursorNoteKeys st
    hunkKeys = do
      fv <- currentFileMay st
      r <- safeIdx (fvRuns fv) (clampH (fvRuns fv) (stCurH st))
      pure (runKeys r)
    lastKeys =
      if null (stLastMarked st) then Nothing else Just (stLastMarked st)
  in
    fromMaybe [] (selectionKeys <|> cursorKeys <|> lastKeys <|> hunkKeys)

changeKeys :: FileView -> Int -> Int -> [Text]
changeKeys fv lo hi = map (\(k, _, _) -> k) (selectedChanges True fv lo hi)

-- The keys a note under the cursor is about: the whole group of a noted
-- line or hunk, or the marked lines of the cursor's row.
cursorNoteKeys :: St -> FileView -> Maybe [Text]
cursorNoteKeys st fv = do
  let
    row = fst (cursorRC (fvEd fv))
    ks = changeKeys fv row row
    relevant k = Map.member k (stVerdicts st) || Map.member k (stThreads st)
    groupAmong keys = listToMaybe (mapMaybe (\k -> Map.lookup k (stNoteGroups st)) keys)
    -- a conversation anywhere in the hunk under the cursor: from any
    -- line of the hunk, :note continues that thread rather than
    -- opening a second one a few lines away
    hunkGroup = do
      h <- hunkUnderCursor st
      r <- safeIdx (fvRuns fv) h
      groupAmong (runKeys r)
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
groupKeys :: St -> FileView -> Text -> [Text]
groupKeys st fv gid =
  let
    inGroup r =
      listToMaybe (filter (\k -> Map.lookup k (stNoteGroups st) == Just gid) (rowFeedbackKeys r))
  in
    mapMaybe inGroup (fvRows fv)

-- Save the reviewer's side of the conversation (empty text clears the
-- whole thread instead), keep the in-memory view in step, and report
-- where it landed. While the reviewer's own post is the newest it stays
-- editable in place; once someone answered, saving appends the next
-- post.
applyNote :: Text -> [Text] -> EventM Name St ()
applyNote text keys = do
  st <- get
  if T.null text
    then do
      n <- liftIO (clearThread (stConn st) (repoText st) keys)
      modify $ \s ->
        s {stThreads = foldr Map.delete (stThreads s) keys}
      refreshNoteGroups
      setMsg $
        if n == 0
          then "no notes here"
          else "removed the conversation from " <> tshow n <> " line(s)"
    else do
      n <- liftIO (saveReviewerPost (stConn st) (repoText st) text keys)
      let
        markedKeys = filter (`Map.member` stVerdicts st) keys
        place thread = case reverse thread of
          (Post "reviewer" _ : older) -> reverse (Post "reviewer" text : older)
          _ -> thread ++ [Post "reviewer" text]
        update k m = Map.insert k (place (Map.findWithDefault [] k m)) m
      modify $ \s ->
        s {stThreads = foldr update (stThreads s) markedKeys}
      refreshNoteGroups
      setMsg $
        if n == 0
          then "no verdicts here yet — mark with ! or ? first"
          else "note on " <> tshow n <> " line(s)"

noteCmd :: Text -> EventM Name St ()
noteCmd text = do
  st <- get
  let
    keys = noteTargetKeys st
  if null keys
    then setMsg "nothing to note — mark lines with ! or ? first"
    else applyNote (T.strip text) keys

unnoteCmd :: EventM Name St ()
unnoteCmd = do
  st <- get
  let
    keys = noteTargetKeys st
  if null keys
    then setMsg "nothing here to unnote"
    else applyNote "" keys

-- The note popup: a small modal buffer with the full editing keys.
-- Enter in its normal mode saves, Ctrl-c discards; an empty note clears.
-- Discarding one that ! or ? opened cancels that mark too.
openNotePopup :: NoteOrigin -> EventM Name St ()
openNotePopup origin = do
  st <- get
  let
    -- A note that ! or ? opened is about what that keystroke just
    -- marked, all of it. Asking noteTargetKeys instead lands it on the
    -- line under the cursor alone, because marking collapses the
    -- selection on its way out and the cursor is left on whichever line
    -- the selection happened to end at — so a note written across four
    -- lines was recorded against one of them, and if that one was
    -- unchanged it belonged to no hunk and nothing could ever resolve it.
    keys = case origin of
      NoteFromMark _ | not (null (stLastMarked st)) -> stLastMarked st
      _ -> noteTargetKeys st
  if null keys
    then setMsg "nothing to note — mark lines with ! or ? first"
    else do
      let
        existing = case mapMaybe (`Map.lookup` stThreads st) keys of
          (thread : _) -> case reverse thread of
            (Post "reviewer" body : _) -> body
            _ -> ""
          [] -> ""
        ls = if T.null existing then [""] else T.lines existing
        lastRow = length ls - 1
        lastCol = case reverse ls of
          (l : _) -> T.length l
          [] -> 0
        ed0 = newEdState lastRow ls
        ed1 =
          ed0
            { edZipper = Z.moveCursor (lastRow, lastCol) (edZipper ed0)
            , edMode = Insert
            }
      modify
        (\s -> s {stNotePopup = Just (NotePopup ed1 (NoteFor keys origin)), stVi = viIdle})

setNotePopupEd :: EdState -> EventM Name St ()
setNotePopupEd ed = do
  modify $ \s ->
    s
      { stNotePopup =
          fmap (\np -> np {npEd = ed {edCopied = False}}) (stNotePopup s)
      , stKill = edKill ed
      }
  -- the popup shares the session's yank slot, so a copy made in a note
  -- reaches the clipboard the same way one made in the buffer does
  publishCopy ed

handleNotePopupKey :: V.Event -> NotePopup -> EventM Name St ()
handleNotePopupKey ev np0 = do
  st0 <- get
  let
    np = np0 {npEd = (npEd np0) {edKill = stKill st0}}
  case ev of
    -- discard works from either popup mode; nobody should need to leave
    -- insert just to bail out
    V.EvKey (V.KChar 'c') [V.MCtrl] -> discardNote np
    _ ->
      case edMode (npEd np) of
        Insert ->
          case handleInsert ev (npEd np) of
            InsertHandled ed -> setNotePopupEd (wrapNoteLine (popupWrapCol st0 np) ed)
            InsertExit ed -> setNotePopupEd ed
            InsertUnhandled -> pure ()
        -- a half-typed vi command owns its next key here too, so `f⏎`
        -- looks for a character instead of posting the note
        Normal
          | cfgKeys (stConfig st0) == KeysVi
          , viBusy (stVi st0) ->
              notePopupVi st0 np ev
        Normal -> notePopupNormalKey st0 np ev

notePopupNormalKey :: St -> NotePopup -> V.Event -> EventM Name St ()
notePopupNormalKey st np ev =
  case ev of
    V.EvKey V.KEnter [] -> saveNote np
    -- Esc from insert lands here in the popup's normal mode; Esc
    -- again cancels the dialog outright
    V.EvKey V.KEsc [] -> discardNote np
    -- the popup's normal mode follows the session's keymap, so a
    -- vi user gets vi here too; emacs is modeless and never
    -- reaches this branch
    _ ->
      case cfgKeys (stConfig st) of
        KeysVi -> notePopupVi st np ev
        _ -> forM_ (handleNormal ev (npEd np)) setNotePopupEd

popupWrapCol :: St -> NotePopup -> Int
popupWrapCol st np =
  case npPurpose np of
    NoteFor _ _ -> cfgNoteWrap (stConfig st)
    AmendMessage _ -> 72

-- Backing out of a note that ! or ? opened takes the mark with it.
-- The verdict was made by that same keystroke and has not been
-- explained yet, so abandoning the sentence abandons what it was
-- going to be about — and the advance the mark would have made never
-- happens either, since there is nothing to move on from.
--
-- A note asked for by :note is the other case: that mark was already
-- there and had nothing to do with this popup.
discardNote :: NotePopup -> EventM Name St ()
discardNote np = do
  let
    discardMsg = case npPurpose np of
      NoteFor _ NoteAsked -> "note discarded"
      NoteFor _ (NoteFromMark _) -> "mark cancelled"
      AmendMessage _ -> "amend cancelled"
  modify (\s -> s {stNotePopup = Nothing, stMsg = discardMsg, stVi = viIdle})
  case npPurpose np of
    NoteFor _ (NoteFromMark undoing) -> do
      cancelMark undoing
      modify (\s -> s {stDeferAdvance = False})
    _ -> runDeferredAdvance

saveNote :: NotePopup -> EventM Name St ()
saveNote np = do
  modify (\s -> s {stNotePopup = Nothing, stVi = viIdle})
  let
    text = T.strip (T.intercalate "\n" (bufferLines (npEd np)))
  case npPurpose np of
    NoteFor keys _ -> do
      applyNote text keys
      runDeferredAdvance
    AmendMessage folding -> applyAmendMessage folding text

notePopupVi :: St -> NotePopup -> V.Event -> EventM Name St ()
notePopupVi st np ev =
  case handleVi (stVi st) ev (npEd np) of
    ViUnhandled -> pure ()
    ViPending keys -> modify (\s -> s {stVi = keys})
    ViDone keys ed -> do
      modify (\s -> s {stVi = keys})
      setNotePopupEd ed

-- Auto-fill for the note popup: typing past the popup width breaks the
-- line at the last space, and the cursor rides the remainder down.
-- A single overlong word is left alone.
wrapNoteLine :: Int -> EdState -> EdState
wrapNoteLine noteWrapCol ed =
  let
    (row, col) = cursorRC ed
    ls = bufferLines ed
    line = case drop row ls of
      (l : _) -> l
      [] -> ""
    breakAt =
      T.findIndex (== ' ') (T.reverse (T.take (noteWrapCol + 1) line))
  in
    if T.length line <= noteWrapCol
      then ed
      else case breakAt of
        Nothing -> ed
        Just fromEnd ->
          let
            at = noteWrapCol - fromEnd
            keep = T.take at line
            rest = T.drop (at + 1) line
            wrapped = take row ls ++ (keep : rest : drop (row + 1) ls)
            cursor =
              if col > at
                then (row + 1, col - at - 1)
                else (row, col)
          in
            ed {edZipper = Z.moveCursor cursor (Z.textZipper wrapped Nothing)}

-- What the popup is about to do. The old chrome never said: a fresh
-- note, a rewrite of your own latest post, and a follow-up to somebody
-- else's answer all looked exactly alike.
data NoteAct = NoteNew | NoteEdit | NoteReply Text

noteAct :: [Post] -> NoteAct
noteAct thread =
  case reverse thread of
    [] -> NoteNew
    (Post author _ : _)
      | author == "reviewer" -> NoteEdit
      | otherwise -> NoteReply author

-- The posts to show above the editor. The whole conversation, except
-- that when you are rewriting your own latest post that post is already
-- in the editor and would otherwise appear twice.
transcriptPosts :: [Post] -> [Post]
transcriptPosts thread =
  case noteAct thread of
    NoteEdit -> init thread
    _ -> thread

-- One post as rendered transcript rows: the same glyphs the inline
-- conversation uses, wrapped to the popup, continuations indented under
-- the text so the authorship stays scannable down the left edge.
postRows :: Int -> Post -> [Widget Name]
postRows width post =
  let
    mine = postAuthor post == "reviewer"
    lead =
      if mine
        then "\8801 you \183 "
        else "\8618 " <> postAuthor post <> " \183 "
    attr = if mine then noteInlineAttr else replyInlineAttr
    -- a stored post keeps the reviewer's own line breaks; the transcript
    -- is a quotation, so re-wrap it to whatever width the popup got
    packed =
      case greedyWrap
        (max 12 (width - T.length lead))
        (T.words (T.unwords (T.lines (postBody post)))) of
        [] -> [""]
        wss -> map T.unwords wss
  in
    case packed of
      [] -> []
      (first : rest) ->
        withAttr attr (txt (lead <> first))
          : map (\l -> withAttr attr (txt ("  " <> l))) rest
