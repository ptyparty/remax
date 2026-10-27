{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Notes
  ( markThenNote
  , unnoteCmd
  , handleNotePopupKey
  , popupBoxWidth
  , popupRowWidth
  , setNotePopupEd
  , choiceLabel
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.List qualified as List
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
import Remax.UI.Core.Anchor qualified as Anchor
import Remax.UI.Core.Markdown qualified as Markdown
import Remax.UI.Core.Threads qualified as Threads
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.View qualified as View
import Remax.UI.Model.Nav qualified as Nav
import Remax.UI.Model.Work qualified as WorkModel
import Remax.Vi qualified as Vi
import Remax.WorkStore qualified as WorkStore

-- A flag or question wants its reason on record: the note popup opens
-- right away (config: note-on-flag = 0 turns this off), targeting the
-- lines just marked. Discarding the note cancels the mark too.
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
            , Types.muBefore = Map.fromList (map (\k -> (k, Map.lookup k (Types.stVerdicts preSt))) (Types.stLastMarked after))
            , Types.muCounts = Types.stCounts preSt
            , Types.muPunch = Types.stPunch preSt
            }
      openNotePopup v undoing
      opened <- State.get
      case Types.stNotePopup opened of
        Nothing -> VerdictAction.runDeferredAdvance
        Just _ -> pure ()
    else State.modify (\s -> s {Types.stDeferAdvance = False})

-- The lines the mark just wrote, as the span a note covers, in file
-- order with the text and number each line has now.
markedSpan :: Types.St -> Maybe Conv.LineSpan
markedSpan st = do
  fv <- Types.currentFileMay st
  Anchor.lineSpanFor st fv (map (\(key, _, _, _) -> key) (Types.stLastRows st))

-- The notes the span touches, oldest first, each offering a reply and
-- a join.
choicesFor :: Types.St -> Conv.LineSpan -> [Types.NoteChoice]
choicesFor st lineSpan =
  case Types.currentFileMay st of
    Nothing -> [Types.NewNote]
    Just fv ->
      let
        path = T.pack (Types.fvPath fv)
        keys = map Store.lineKeyToText (Conv.spanKeys lineSpan)
        spanRows = filter (\r -> any (`elem` keys) (Maybe.catMaybes [DiffModel.rKey r, DiffModel.rRemarkKey r])) (Types.fvRows fv)
        touches conv = any (\r -> Anchor.anchorCovers path r (Conv.convAnchor conv)) spanRows
        spanRowIxs = Maybe.mapMaybe (\(i, r) -> if any (`elem` keys) (Maybe.catMaybes [DiffModel.rKey r, DiffModel.rRemarkKey r]) then Just i else Nothing) (zip [0 ..] (Types.fvRows fv))
        drawnHere = Anchor.conversationsIn st (concatMap (\i -> Map.findWithDefault [] i (Types.fvThreads fv)) spanRowIxs)
        outdated conv = Map.member (Conv.convId conv) (Types.stOutdated st)
        touched =
          List.sortOn Conv.conversationTime $
            List.nubBy (\a b -> Conv.convId a == Conv.convId b) (filter touches (Types.stConversations st) ++ filter outdated drawnHere)
        choices conv = case (Conv.convVenue conv, outdated conv) of
          (Conv.LocalOnly _, True) -> [Types.ReplyOutdated conv]
          (Conv.LocalOnly _, False) -> [Types.ReplyNote conv, Types.JoinNote conv]
          _ -> []
      in
        Types.NewNote : concatMap choices touched

openNotePopup :: Store.Verdict -> Types.MarkUndo -> Brick.EventM Types.Name Types.St ()
openNotePopup v undoing = do
  st <- State.get
  case markedSpan st of
    Nothing -> Types.setMsg "nothing to note — mark lines with ! or ? first"
    Just lineSpan -> do
      let
        ed0 = Modal.newEdState 0 [""]
        ed1 = ed0 {Modal.edMode = Modal.Insert}
        plan =
          Types.NotePlan
            { Types.planVerdict = v
            , Types.planSpan = lineSpan
            , Types.planUndo = undoing
            , Types.planChoices = choicesFor st lineSpan
            , Types.planChosen = 0
            }
      State.modify (\s -> s {Types.stNotePopup = Just (Types.NotePopup ed1 (Types.NoteOn plan)), Types.stVi = Vi.viIdle})

-- Space u on a plain line: retire the notes written on it.
unnoteCmd :: Brick.EventM Types.Name Types.St ()
unnoteCmd = do
  st <- State.get
  let
    notes = filter isNote (ThreadAction.cursorConversations st)
    isNote c = case Conv.convVenue c of
      Conv.LocalOnly _ -> True
      _ -> False
    keys = concatMap (noteKeys . Conv.convAnchor) notes
    noteKeys anchor = case anchor of
      Conv.OnLines lineSpan -> Conv.spanKeys lineSpan
      _ -> []
  if null notes
    then Types.setMsg "nothing here to resolve"
    else do
      ThreadAction.retireNotesOn keys
      Conversation.resolveNotesInMemory (map Store.lineKeyToText keys)
      Types.setMsg ("resolved " <> Types.tshow (length notes) <> " note(s)")

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
    emacs = Config.cfgKeys (Types.stConfig st0) == Config.KeysEmacs
  case ev of
    -- discard works from either popup mode; nobody should need to leave
    -- insert just to bail out
    V.EvKey (V.KChar 'c') [V.MCtrl] -> discardNote np
    V.EvKey (V.KChar 'g') [V.MCtrl]
      | Types.WorkFor _ <- Types.npPurpose np -> CiteAction.openCitePicker
    V.EvKey (V.KChar 'o') [V.MCtrl]
      | Types.WorkFor wp <- Types.npPurpose np -> toggleLog wp
    V.EvKey (V.KChar 'n') [V.MMeta] | emacs -> moveChoice 1 np
    V.EvKey (V.KChar 'p') [V.MMeta] | emacs -> moveChoice (-1) np
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

toggleLog :: Types.WorkPopup -> Brick.EventM Types.Name Types.St ()
toggleLog wp =
  let
    flipped = case Types.wpLog wp of
      Types.FoldedLog -> Types.UnfoldedLog
      Types.UnfoldedLog -> Types.FoldedLog
  in
    State.modify (\s -> s {Types.stNotePopup = fmap (\p -> p {Types.npPurpose = Types.WorkFor wp {Types.wpLog = flipped}}) (Types.stNotePopup s)})

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
    V.EvKey (V.KChar '\t') [] -> moveChoice 1 np
    V.EvKey V.KBackTab [] -> moveChoice (-1) np
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

-- Tab and S-Tab walk the `goes to` list of a note or a reply with
-- several threads to choose from.
moveChoice :: Int -> Types.NotePopup -> Brick.EventM Types.Name Types.St ()
moveChoice d np =
  let
    step n total = if total <= 1 then 0 else (n + d) `mod` total
    purpose = case Types.npPurpose np of
      Types.NoteOn plan -> Types.NoteOn plan {Types.planChosen = step (Types.planChosen plan) (length (Types.planChoices plan))}
      Types.WorkFor wp
        | not (null (Types.wpChoices wp)) ->
            let
              chosen = step (Types.wpChosen wp) (length (Types.wpChoices wp))
            in
              Types.WorkFor (maybe wp (ThreadAction.aimAt wp {Types.wpChosen = chosen}) (Types.safeIdx (Types.wpChoices wp) chosen))
      other -> other
  in
    State.modify (\s -> s {Types.stNotePopup = fmap (\p -> p {Types.npPurpose = purpose}) (Types.stNotePopup s)})

repliedTo :: Types.St -> Types.WorkPopup -> T.Text
repliedTo st wp =
  let
    posted c = filter isPosted (Conv.convPosts c)
    isPosted p = case Conv.postState p of
      Conv.Posted {} -> True
      _ -> False
    newest = do
      cid <- Types.wpConv wp
      conv <- Conv.conversationFor cid (Types.stConversations st)
      Maybe.listToMaybe (reverse (posted conv))
  in
    maybe "the thread" (Threads.authorName . Conv.postAuthor) newest

-- Backing out of a note that ! or ? opened takes the mark with it.
-- The verdict was made by that same keystroke and has not been
-- explained yet, so abandoning the sentence abandons what it was
-- going to be about — and the advance the mark would have made never
-- happens either, since there is nothing to move on from.
discardNote :: Types.NotePopup -> Brick.EventM Types.Name Types.St ()
discardNote np = do
  let
    discardMsg = case Types.npPurpose np of
      Types.NoteOn _ -> "mark cancelled"
      Types.AmendMessage _ -> "amend cancelled"
      Types.WorkFor _ -> "discarded"
  State.modify (\s -> s {Types.stNotePopup = Nothing, Types.stMsg = discardMsg, Types.stVi = Vi.viIdle})
  case Types.npPurpose np of
    Types.NoteOn plan -> do
      VerdictAction.cancelMark (Types.planUndo plan)
      State.modify (\s -> s {Types.stDeferAdvance = False})
    _ -> VerdictAction.runDeferredAdvance

saveNote :: Types.NotePopup -> Brick.EventM Types.Name Types.St ()
saveNote np = do
  State.modify (\s -> s {Types.stNotePopup = Nothing, Types.stVi = Vi.viIdle})
  let
    text = T.strip (T.intercalate "\n" (Modal.bufferLines (Types.npEd np)))
  case Types.npPurpose np of
    Types.NoteOn plan -> do
      savePlan plan text
      VerdictAction.runDeferredAdvance
    Types.AmendMessage target -> Rebase.applyAmendMessage target text
    Types.WorkFor wp -> ThreadAction.saveWork wp text

-- Where the note's text goes: a new note on its lines, a reply on a
-- note already there, or a join that grows that note.
savePlan :: Types.NotePlan -> T.Text -> Brick.EventM Types.Name Types.St ()
savePlan plan text = do
  st <- State.get
  let
    conn = Types.stConn st
    scope = WorkModel.scopeOf st
    body = Conv.Markdown text
    lineSpan = Types.planSpan plan
    chosen = Maybe.fromMaybe Types.NewNote (Types.safeIdx (Types.planChoices plan) (Types.planChosen plan))
    rangeText = Threads.anchorPlace . Conv.OnLines
  if T.null text
    then Types.setMsg "empty note \8212 the mark stays"
    else case chosen of
      Types.NewNote -> do
        lid <- MonadIO.liftIO (WorkStore.newLocalId (Types.repoText st) (Conv.spanKeys lineSpan))
        Monad.void (MonadIO.liftIO (WorkStore.addAside conn scope (Conv.OnLinesOnly lid lineSpan (Types.planVerdict plan)) Conv.Viewer body))
        WorkModel.reloadWork
        Types.setMsg ("note on " <> rangeText lineSpan)
      Types.ReplyNote conv -> do
        Monad.void (MonadIO.liftIO (WorkStore.addAside conn scope (Conv.convTarget conv) Conv.Viewer body))
        WorkModel.reloadWork
        Types.setMsg ("reply on " <> Threads.anchorPlace (Conv.convAnchor conv))
      Types.JoinNote conv ->
        case Conv.convTarget conv of
          Conv.OnLinesOnly lid old verdict -> do
            let
              grown = Conv.spanUnion old lineSpan
            Monad.void (MonadIO.liftIO (WorkStore.addAside conn scope (Conv.OnLinesOnly lid old verdict) Conv.Viewer body))
            MonadIO.liftIO (WorkStore.growNote conn (Types.repoText st) lid grown)
            WorkModel.reloadWork
            Types.setMsg ("on " <> rangeText lineSpan <> " \183 the note under " <> rangeText old <> " now covers " <> rangeText grown)
          _ -> Types.setMsg "that conversation cannot grow"
      Types.ReplyOutdated conv -> do
        Monad.void (MonadIO.liftIO (WorkStore.addAside conn scope (Conv.convTarget conv) Conv.Viewer body))
        WorkModel.reloadWork
        Types.setMsg ("reply to the outdated note \183 was " <> Threads.anchorPlace (Conv.convAnchor conv))

notePopupVi :: Types.St -> Types.NotePopup -> V.Event -> Brick.EventM Types.Name Types.St ()
notePopupVi st np ev =
  case Vi.handleVi (Types.stVi st) ev (Types.npEd np) of
    Vi.ViUnhandled -> pure ()
    Vi.ViPending keys -> State.modify (\s -> s {Types.stVi = keys})
    Vi.ViDone keys ed -> do
      State.modify (\s -> s {Types.stVi = keys})
      setNotePopupEd ed

-- One row of the `goes to` list.
choiceLabel :: Conv.LineSpan -> Types.NoteChoice -> T.Text
choiceLabel lineSpan choice =
  let
    range = Threads.anchorPlace . Conv.OnLines
    firstLine c = maybe "" (Markdown.firstLine . Conv.postBody) (Maybe.listToMaybe (Conv.convPosts c))
    place c = Threads.anchorPlace (Conv.convAnchor c)
  in
    case choice of
      Types.NewNote -> "new note on " <> range lineSpan
      Types.ReplyNote c -> "reply on " <> place c <> " \183 the note stays on " <> place c <> " \183 " <> firstLine c
      Types.JoinNote c ->
        case Conv.convAnchor c of
          Conv.OnLines old -> "join the note on " <> place c <> " \183 it grows to " <> range (Conv.spanUnion old lineSpan) <> " \183 " <> firstLine c
          _ -> "join the note on " <> place c
      Types.ReplyOutdated c -> "reply to the outdated note \183 was " <> place c <> " \183 " <> firstLine c
