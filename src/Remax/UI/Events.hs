{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Events
  ( syncComposing
  , rememberSelector
  , noteTrayView
  , reanchorBody
  , handleEvent
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Remax.Clipboard qualified as Clipboard
import Remax.Config qualified as Config
import Remax.Emacs qualified as Emacs
import Remax.Modal qualified as Modal
import Remax.Store qualified as Store
import Remax.UI.Action.Command qualified as Command
import Remax.UI.Action.Editor qualified as Editor
import Remax.UI.Action.Menus qualified as Menus
import Remax.UI.Action.Notes qualified as NoteAction
import Remax.UI.Action.Open qualified as Open
import Remax.UI.Action.Picker qualified as PickerAction
import Remax.UI.Action.Search qualified as Search
import Remax.UI.Action.Settings qualified as Settings
import Remax.UI.Action.Tree qualified as TreeAction
import Remax.UI.Action.TreeKeys qualified as TreeKeys
import Remax.UI.Action.Verdicts qualified as VerdictAction
import Remax.UI.Core.Tour qualified as Tour
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Verdict qualified as Verdict
import Remax.UI.Core.View qualified as View
import Remax.UI.Model.Load qualified as Load
import Remax.UI.Model.Nav qualified as Nav
import Remax.Vi qualified as Vi
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

-- Tell the store whether a note is being written into this repository.
--
-- A flag reaches the store the instant the key is pressed, which is what
-- makes the tray and the meters answer at once — and means an agent
-- waiting on the reviewer hears about it seconds before the sentence
-- saying what is wrong with it. It cannot act on that, and waiting again
-- for the note is how it misses the note. So the store carries the fact
-- that one is being written, and the wait holds until it is.
--
-- Driven off the state after every event rather than from the places a
-- popup opens and closes, so a path that dismisses one some other way
-- cannot leave the marker standing. Only transitions are written.
syncComposing :: Brick.EventM Types.Name Types.St ()
syncComposing = do
  st <- State.get
  let
    writing = case Types.stNotePopup st of
      Just np
        | Types.NoteFor _ _ <- Types.npPurpose np -> Just (Types.repoText st)
      _ -> Nothing
  Monad.unless (writing == Types.stComposing st) $ do
    -- cleared where it was set before being set anywhere else, so
    -- switching repositories cannot strand it on the old one
    Monad.forM_ (Types.stComposing st) (\was -> MonadIO.liftIO (Store.setComposing (Types.stConn st) was False))
    Monad.forM_ writing (\now -> MonadIO.liftIO (Store.setComposing (Types.stConn st) now True))
    State.modify (\s -> s {Types.stComposing = writing})

-- Keep whichever selector is on screen, so ␣' can bring it back.
--
-- Watched after every event rather than saved wherever one closes. They
-- close from several places — Escape, a choice taken, a repository
-- switched out from under them — and the one that forgot to save would
-- quietly reopen something older instead. What is open now is what
-- comes back, and the last event that had one open is the state it was
-- left in: Escape and Enter both close without disturbing the query or
-- the row, so what is remembered is what was on screen.
rememberSelector :: Brick.EventM Types.Name Types.St ()
rememberSelector = do
  st <- State.get
  let
    onScreen = case (Types.stTree st, Types.stPicker st) of
      (Just tv, _) -> Just (Types.LastTree tv)
      (_, Just pk) -> Just (Types.LastPicker pk)
      _ -> Nothing
  Monad.forM_ onScreen (\sel -> State.modify (\s -> s {Types.stLastSelector = Just sel}))

-- Note where the tray is sitting, so the next frame knows which of its
-- rows can be seen. Read after the event rather than during the draw
-- because that is where brick will tell us; it is therefore one frame
-- behind, which the margin in tray covers.
noteTrayView :: Brick.EventM Types.Name Types.St ()
noteTrayView = do
  mvp <- Brick.lookupViewport Types.TrayVP
  Monad.forM_ mvp $ \vp -> do
    let
      seen = (Brick._vpTop vp, snd (Brick._vpSize vp))
    State.modify (\s -> s {Types.stTraySeen = seen})

-- Write the window's origin back after an event, and move the viewport
-- by the same amount so nothing jumps.
--
-- The origin only changes when the cursor has drifted near an edge of
-- what is drawn; the rows on screen are the same rows either side of
-- that, so the view must not move. brick's scroll offset is in window
-- coordinates, and shifting the window by n rows moves everything on
-- screen by n unless the offset is shifted with it.
--
-- Deliberate jumps (a hunk, a reload) set the origin themselves before
-- placing the viewport, so by the time this runs there is nothing left
-- to reconcile and it does nothing.
reanchorBody :: Brick.EventM Types.Name Types.St ()
reanchorBody = do
  st <- State.get
  case Types.currentFileMay st of
    Nothing -> pure ()
    Just fv -> do
      screenRows <- View.terminalRows
      let
        (_, origin) = View.bodyWindow screenRows st fv
        moved = origin - Types.fvOrigin fv
      Monad.unless (moved == 0) $ do
        State.modify (Verdict.updateFile (Types.stCurF st) (\f -> f {Types.fvOrigin = origin}))
        mvp <- Brick.lookupViewport Types.BodyVP
        case mvp of
          Just vp -> Brick.setTop (Brick.viewportScroll Types.BodyVP) (max 0 (Brick._vpTop vp - moved))
          Nothing -> pure ()

startTour :: Brick.EventM Types.Name Types.St ()
startTour = do
  State.modify (\s -> s {Types.stWalk = Types.WalkTour})
  st <- State.get
  case Tour.tourOrder st of
    (first : _) -> do
      Nav.parkAnchor (fst first) (snd first)
      State.modify (\s -> s {Types.stTourLast = Just first})
      Types.setMsg ("tour started \8212 " <> Types.tshow (length (Types.stTour st)) <> " steps, ]/[ follow it")
    [] -> Types.setMsg "a tour arrived but none of its hunks are in this view"

handleEvent :: Brick.BrickEvent Types.Name Types.REvent -> Brick.EventM Types.Name Types.St ()
handleEvent brickEvent =
  case brickEvent of
    Brick.AppEvent (Types.FileChangedOnDisk absolute) -> do
      st <- State.get
      -- relativize here, against whichever repository is current now
      let
        rel = FilePath.makeRelative (Types.stRoot st) absolute
      Monad.unless (FilePath.isAbsolute rel) (Load.reloadFromDisk rel)
    Brick.AppEvent Types.GitStateChanged -> Load.refreshGit
    Brick.AppEvent Types.StoreChanged -> do
      st <- State.get
      threads <- MonadIO.liftIO (Store.loadThreads (Types.stConn st) (Types.repoText st))
      groups <- MonadIO.liftIO (Store.loadNoteGroups (Types.stConn st) (Types.repoText st))
      steps <- MonadIO.liftIO (Tour.loadTourFor (Types.stConn st) (Types.stRoot st) (Types.effectiveMode st))
      State.modify (\s -> Tour.installTour steps s {Types.stThreads = threads, Types.stNoteGroups = groups})
      Monad.when (null (Types.stTour st) && not (null steps)) startTour
    Brick.AppEvent (Types.Attending seen) -> do
      State.modify (\s -> s {Types.stAttending = seen, Types.stThrob = Types.stThrob s + 1})
      Load.pollSubject
    Brick.AppEvent Types.Tick -> Nav.scrollHunkIntoView
    Brick.AppEvent Types.Pulse -> State.modify (\s -> s {Types.stThrob = Types.stThrob s + 1})
    Brick.AppEvent (Types.JobDone label finish) -> do
      State.modify (\s -> s {Types.stBusy = List.delete label (Types.stBusy s)})
      finish
    Brick.VtyEvent ev -> do
      st <- State.get
      -- messages are transient: any keystroke clears the old one, and a
      -- handler that has something new to say sets it afresh
      State.modify (\s -> s {Types.stMsg = ""})
      case (Types.stNotePopup st, Types.stPicker st, Types.stPrompt st) of
        _
          | Types.stHelp st -> State.modify (\s -> s {Types.stHelp = False})
        _
          | Types.stInfoPopup st /= Nothing ->
              State.modify (\s -> s {Types.stInfoPopup = Nothing})
        (Just np, _, _) -> NoteAction.handleNotePopupKey ev np
        _
          | Just tv <- Types.stTree st -> TreeKeys.handleTreeKey ev tv
        (_, Just pk, _) -> PickerAction.handlePickerKey ev pk
        (_, _, Just p) -> handlePromptKey ev p
        _ ->
          if Types.stSpace st
            then handleSpaceKey ev
            else case Types.stEmacsPrefix st of
              Just pfx -> do
                State.modify (\s -> s {Types.stEmacsPrefix = Nothing})
                if null (Types.stFiles st)
                  then handleIdlePrefixed pfx ev
                  else handleEmacsPrefixed pfx ev
              Nothing
                | Types.stIdleGo st -> do
                    State.modify (\s -> s {Types.stIdleGo = False})
                    handleIdleGo ev
                | null (Types.stFiles st) -> handleIdleKey ev
                | otherwise -> handleModalKey ev
    _ -> pure ()

-- The helix-style space menu: Space, then one key; anything unlisted
-- just cancels. Dispatch reads spaceMenuEntries, which is also what the
-- infobox renders, so the menu and the bindings cannot disagree.
handleSpaceKey :: V.Event -> Brick.EventM Types.Name Types.St ()
handleSpaceKey ev = do
  State.modify (\s -> s {Types.stSpace = False})
  let
    -- the table is keyed by what the menu prints, so Enter has to become
    -- its own glyph to find its row
    typed = case ev of
      V.EvKey (V.KChar c) [] -> Just (T.singleton c)
      V.EvKey V.KEnter [] -> Just "\9166"
      _ -> Nothing
  case typed of
    Nothing -> pure ()
    Just key ->
      case List.find (\(k, _, _) -> k == key) Menus.spaceMenuEntries of
        Just (_, _, action) -> action
        Nothing -> pure ()

-- With nothing on screen the only meaningful keys are quitting and the
-- command line (to switch what the session is looking at).
handleIdleKey :: V.Event -> Brick.EventM Types.Name Types.St ()
handleIdleKey ev =
  case ev of
    V.EvKey (V.KChar ':') [] -> Command.openCmdPrompt
    V.EvKey (V.KChar 'p') [V.MCtrl] -> TreeAction.openFileFinder
    V.EvKey (V.KChar 'g') [] -> State.modify (\s -> s {Types.stIdleGo = True})
    V.EvKey (V.KChar ' ') [] -> State.modify (\s -> s {Types.stSpace = True})
    V.EvKey (V.KChar 'x') [V.MMeta] -> Command.openCmdPrompt
    V.EvKey (V.KChar 'x') [V.MCtrl] ->
      State.modify (\s -> s {Types.stEmacsPrefix = Just 'x'})
    _ -> pure ()

-- The g menu with nothing on screen. Only gr means anything here: there
-- is no file to move around in, and switching repository is most of what
-- an empty session is for — a story's first checkout being clean is
-- exactly when the next one is wanted.
handleIdleGo :: V.Event -> Brick.EventM Types.Name Types.St ()
handleIdleGo ev =
  case ev of
    V.EvKey (V.KChar 'r') [] -> PickerAction.openRepoPicker
    _ -> pure ()

-- With no files on screen, only the session chords mean anything.
handleIdlePrefixed :: Char -> V.Event -> Brick.EventM Types.Name Types.St ()
handleIdlePrefixed pfx ev =
  case (pfx, ev) of
    ('x', V.EvKey (V.KChar 'c') [V.MCtrl]) -> Brick.halt
    _ -> pure ()

handlePromptKey :: V.Event -> Types.Prompt -> Brick.EventM Types.Name Types.St ()
handlePromptKey ev prompt =
  let
    input = case prompt of
      Types.SearchPrompt t -> t
      Types.CmdPrompt t -> t
    rewrap t = case prompt of
      Types.SearchPrompt _ -> Types.SearchPrompt t
      Types.CmdPrompt _ -> Types.CmdPrompt t
    isCmd = case prompt of
      Types.CmdPrompt _ -> True
      Types.SearchPrompt _ -> False
  in
    case ev of
      V.EvKey V.KEsc [] ->
        State.modify (\s -> s {Types.stPrompt = Nothing, Types.stPromptCycle = Nothing})
      V.EvKey V.KEnter [] -> do
        State.modify (\s -> s {Types.stPrompt = Nothing, Types.stPromptCycle = Nothing})
        case prompt of
          Types.SearchPrompt t -> do
            State.modify (\s -> s {Types.stLastSearch = t, Types.stGrepNav = Nothing})
            st <- State.get
            Monad.unless (T.null t) (Search.searchStep (Types.stSearchDir st) t)
          Types.CmdPrompt t -> Command.runCommand t
      V.EvKey (V.KChar '\t') [] | isCmd -> Command.promptComplete 1 input
      V.EvKey V.KBackTab [] | isCmd -> Command.promptComplete (-1) input
      V.EvKey V.KBS [] ->
        State.modify
          ( \s ->
              s
                { Types.stPrompt = Just (rewrap (T.dropEnd 1 input))
                , Types.stPromptCycle = Nothing
                }
          )
      V.EvKey (V.KChar c) [] ->
        State.modify
          ( \s ->
              s
                { Types.stPrompt = Just (rewrap (T.snoc input c))
                , Types.stPromptCycle = Nothing
                }
          )
      _ -> pure ()

handleModalKey :: V.Event -> Brick.EventM Types.Name Types.St ()
handleModalKey ev = do
  st <- State.get
  let
    i = Types.stCurF st
    fv = Types.stFiles st !! i
    -- the kill slot is session state: whatever was yanked anywhere is
    -- pasteable here
    ed = (Types.fvEd fv) {Modal.edKill = Types.stKill st}
  case Config.cfgKeys (Types.stConfig st) of
    Config.KeysEmacs -> handleEmacsKey i ed ev
    Config.KeysVi -> handleViKey i ed ev
    Config.KeysHelix -> handleHelixKey i fv ed ev

-- Emacs keymap: editing chords go to Remax.Emacs; M-x and C-s open the
-- prompts; C-c prefixes the review verbs (same letters as the helix
-- keys), C-x the session chords (C-x C-c quits, C-x C-s saves).
handleEmacsKey :: Int -> Modal.EdState -> V.Event -> Brick.EventM Types.Name Types.St ()
handleEmacsKey i ed ev = do
  st <- State.get
  case Types.stEmacsPrefix st of
    Just pfx -> do
      State.modify (\s -> s {Types.stEmacsPrefix = Nothing})
      handleEmacsPrefixed pfx ev
    Nothing ->
      case ev of
        V.EvKey (V.KChar 'x') [V.MMeta] -> Command.openCmdPrompt
        V.EvKey (V.KChar 's') [V.MCtrl] -> Search.openSearchPrompt 1
        V.EvKey (V.KChar 'c') [V.MCtrl] ->
          State.modify (\s -> s {Types.stEmacsPrefix = Just 'c'})
        V.EvKey (V.KChar 'x') [V.MCtrl] ->
          State.modify (\s -> s {Types.stEmacsPrefix = Just 'x'})
        -- C-y is emacs's paste, so it consults the clipboard the way
        -- p does in the modal keymaps
        V.EvKey (V.KChar 'y') [V.MCtrl] -> do
          Nav.pullClipboard
          refreshed <- State.get
          emacsEdit i ed {Modal.edKill = Types.stKill refreshed} ev
        _ -> emacsEdit i ed ev

emacsEdit :: Int -> Modal.EdState -> V.Event -> Brick.EventM Types.Name Types.St ()
emacsEdit i ed ev =
  case Emacs.handleEmacs ev ed of
    Just edNext -> applyEdit i ed edNext
    Nothing -> pure ()

applyEdit :: Int -> Modal.EdState -> Modal.EdState -> Brick.EventM Types.Name Types.St ()
applyEdit i ed edNext = do
  let
    changed = Modal.bufferLines edNext /= Modal.bufferLines ed
    entering = Modal.edMode edNext == Modal.Insert && Modal.edMode ed /= Modal.Insert
  if not (changed || entering)
    then Nav.setEd i edNext
    else Types.whenWritable $ \grant ->
      if changed
        then do
          Load.rebuildWith i edNext
          Monad.when (Modal.edMode edNext == Modal.Normal) (VerdictAction.applyToDisk grant i)
        else Nav.setEd i edNext

handleEmacsPrefixed :: Char -> V.Event -> Brick.EventM Types.Name Types.St ()
handleEmacsPrefixed pfx ev = do
  st <- State.get
  case pfx of
    'x' ->
      case ev of
        V.EvKey (V.KChar 'c') [V.MCtrl] -> Brick.halt
        V.EvKey (V.KChar 's') [V.MCtrl] -> Types.whenWritable (\grant -> VerdictAction.applyToDisk grant (Types.stCurF st))
        _ -> pure ()
    _ ->
      case ev of
        V.EvKey V.KEnter [] -> VerdictAction.acceptSelection
        V.EvKey (V.KChar '!') [] -> NoteAction.markThenNote Store.Flagged
        V.EvKey (V.KChar '?') [] -> NoteAction.markThenNote Store.Questioned
        V.EvKey (V.KChar 'r') [] -> VerdictAction.clearVerdictBySelection
        V.EvKey (V.KChar 'R') [] -> VerdictAction.clearVerdictBySelection
        V.EvKey (V.KChar ']') [] -> Nav.switchRun 1
        V.EvKey (V.KChar '[') [] -> Nav.switchRun (-1)
        V.EvKey (V.KChar 'x') [] -> Nav.selectCurrentHunk
        V.EvKey (V.KChar 'p') [] -> VerdictAction.cycleWalkFilter
        V.EvKey (V.KChar 'w') [] -> Settings.toggleIgnoreWs
        V.EvKey (V.KChar 'u') [] -> NoteAction.unnoteCmd
        V.EvKey (V.KChar 'h') [] -> Nav.setCurrentHunkHere
        V.EvKey (V.KChar 'v') [] -> VerdictAction.revertTarget
        V.EvKey (V.KChar 'a') [] -> Open.gotoAltFile
        V.EvKey (V.KChar '/') [] -> PickerAction.openGrepPicker ""
        V.EvKey (V.KChar 'c') [] -> VerdictAction.showCommitPopup
        V.EvKey (V.KChar '\'') [] -> Nav.jumpToAnchor
        V.EvKey (V.KChar '*') [] -> Search.searchWordUnderCursor
        V.EvKey (V.KChar 'n') [] -> Search.searchRepeat 1
        V.EvKey (V.KChar 'N') [] -> Search.searchRepeat (-1)
        V.EvKey (V.KChar 'f') [] -> TreeAction.openFileFinder
        V.EvKey (V.KChar 'b') [] -> PickerAction.openBufferPicker
        V.EvKey (V.KChar 'e') [] -> Editor.editCurrent
        V.EvKey (V.KChar ')') [] -> Open.switchFile 1
        V.EvKey (V.KChar '(') [] -> Open.switchFile (-1)
        V.EvKey (V.KChar '\t') [] ->
          State.modify (\s -> s {Types.stTray = not (Types.stTray s)})
        _ -> pure ()

-- Insert mode is the same in every modal keymap: type, and land the
-- result back in the file view as cheaply as the edit allows — a
-- same-shape edit patches one line rather than rebuilding the diff.
handleInsertKey :: Int -> Modal.EdState -> V.Event -> Brick.EventM Types.Name Types.St ()
handleInsertKey i ed ev =
  case Modal.handleInsert ev ed of
    Modal.InsertHandled edNext -> do
      let
        oldLs = Modal.bufferLines ed
        newLs = Modal.bufferLines edNext
      if newLs == oldLs
        then Nav.setEd i edNext
        else
          if length newLs == length oldLs
            then
              State.modify
                ( \s ->
                    Verdict.updateFile
                      i
                      ( Load.patchLine
                          (Types.stConfig s)
                          (fst (Modal.cursorRC edNext))
                          (View.currentLineOf edNext)
                          . Nav.withEd edNext
                      )
                      s
                )
            else Load.rebuildWith i edNext
    Modal.InsertExit edNext -> do
      Load.rebuildWith i edNext
      Types.whenWritable (\grant -> VerdictAction.applyToDisk grant i)
    Modal.InsertUnhandled -> pure ()

handleHelixKey :: Int -> Types.FileView -> Modal.EdState -> V.Event -> Brick.EventM Types.Name Types.St ()
handleHelixKey i fv ed ev =
  case Modal.edMode ed of
    Modal.Insert -> handleInsertKey i ed ev
    -- a pending g/f/t prefix owns the next key outright; review keys
    -- must not steal its target character
    -- ga is review-level (goto alternate file), but the g prefix lives
    -- in the modal layer; catch it here and let the modal cancel its
    -- pending state
    Modal.Normal
      | Modal.pendingLabel ed == Just "g"
      , V.EvKey (V.KChar c) [] <- ev
      , Just go <- helixGoKey c -> do
          deferToModal i ed ev
          go
    Modal.Normal | Modal.hasPending ed -> deferToModal i ed ev
    Modal.Normal -> helixNormalKey i fv ed ev

-- The review-level g keys. gr: go to another repository — free in every
-- keymap remax has, and the g menu is where going somewhere already
-- lives; Space R is the same door for the keymaps that have no g of
-- their own. gn/gp: the next and previous file. helix spends these on
-- buffers, and a file under review is what remax has instead of one, so
-- they keep the meaning a reader arrives with. ) and ( are the same move
-- in one keystroke; these are for the hand that already knows helix.
helixGoKey :: Char -> Maybe (Brick.EventM Types.Name Types.St ())
helixGoKey c =
  case c of
    'a' -> Just Open.gotoAltFile
    'r' -> Just PickerAction.openRepoPicker
    'n' -> Just (Open.switchFile 1)
    'p' -> Just (Open.switchFile (-1))
    _ -> Nothing

helixNormalKey :: Int -> Types.FileView -> Modal.EdState -> V.Event -> Brick.EventM Types.Name Types.St ()
helixNormalKey i fv ed ev = do
  st <- State.get
  case ev of
    V.EvKey (V.KChar '\t') [] -> State.modify (\s -> s {Types.stTray = not (Types.stTray s)})
    V.EvKey V.KEnter [] -> VerdictAction.acceptSelection
    V.EvKey (V.KChar '!') [] -> NoteAction.markThenNote Store.Flagged
    V.EvKey (V.KChar '?') [] -> NoteAction.markThenNote Store.Questioned
    V.EvKey (V.KChar 'R') [] -> VerdictAction.clearVerdictBySelection
    V.EvKey (V.KChar ']') [] -> Nav.switchRun 1
    V.EvKey (V.KChar '[') [] -> Nav.switchRun (-1)
    V.EvKey (V.KChar '/') [] -> Search.openSearchPrompt 1
    V.EvKey (V.KChar ':') [] -> Command.openCmdPrompt
    V.EvKey (V.KChar '*') [] -> Search.searchWordUnderCursor
    V.EvKey (V.KChar 'n') [] -> Search.searchRepeat 1
    V.EvKey (V.KChar 'N') [] -> Search.searchRepeat (-1)
    V.EvKey (V.KChar '\'') [] -> Nav.jumpToAnchor
    V.EvKey (V.KChar 'X') [] -> Nav.selectCurrentHunk
    V.EvKey (V.KChar 'p') [] -> pasteKey deferToModal i ev
    V.EvKey (V.KChar 'P') [] -> pasteKey deferToModal i ev
    V.EvKey (V.KChar ' ') [] -> State.modify (\s -> s {Types.stSpace = True})
    V.EvKey (V.KChar 'E') [] ->
      Types.whenWritable (\grant -> Editor.launchEditor grant (Types.stRoot st </> Types.fvPath fv) (fst (Modal.cursorRC ed) + 1))
    V.EvKey (V.KChar ')') [] -> Open.switchFile 1
    V.EvKey (V.KChar '(') [] -> Open.switchFile (-1)
    V.EvKey (V.KChar 'p') [V.MCtrl] -> TreeAction.openFileFinder
    V.EvKey (V.KChar '{') [] -> Brick.vScrollBy (Brick.viewportScroll Types.TrayVP) (-8)
    V.EvKey (V.KChar '}') [] -> Brick.vScrollBy (Brick.viewportScroll Types.TrayVP) 8
    _ -> deferToModal i ed ev

-- Vi keymap: the buffer belongs to vi, and the review verbs live behind
-- the Space leader the way the emacs keymap puts them behind C-c. What
-- keeps its remax key is structural navigation — hunks, files, the tray,
-- the anchor back to what is under review — because that is the tool's
-- own vocabulary rather than the editor's, and vi's sentence and
-- paragraph motions are a small price for it. Everything else is vi's:
-- `?` searches backward, `X` deletes leftward, `E` is the WORD motion
-- (Space e still opens $EDITOR here).
handleViKey :: Int -> Modal.EdState -> V.Event -> Brick.EventM Types.Name Types.St ()
handleViKey i ed ev = do
  st <- State.get
  case Modal.edMode ed of
    Modal.Insert -> handleInsertKey i ed ev
    -- a half-typed vi command owns the next keystroke outright, so `f]`
    -- finds a bracket instead of walking to the next hunk, and `2` then
    -- `}` counts rather than scrolling the tray
    Modal.Normal | Vi.viBusy (Types.stVi st) -> deferToVi i ed ev
    Modal.Normal ->
      case ev of
        V.EvKey (V.KChar '\t') [] ->
          State.modify (\s -> s {Types.stTray = not (Types.stTray s)})
        V.EvKey (V.KChar ']') [] -> Nav.switchRun 1
        V.EvKey (V.KChar '[') [] -> Nav.switchRun (-1)
        V.EvKey (V.KChar ')') [] -> Open.switchFile 1
        V.EvKey (V.KChar '(') [] -> Open.switchFile (-1)
        V.EvKey (V.KChar '{') [] ->
          Brick.vScrollBy (Brick.viewportScroll Types.TrayVP) (-8)
        V.EvKey (V.KChar '}') [] ->
          Brick.vScrollBy (Brick.viewportScroll Types.TrayVP) 8
        V.EvKey (V.KChar '\'') [] -> Nav.jumpToAnchor
        V.EvKey (V.KChar ':') [] -> Command.openCmdPrompt
        V.EvKey (V.KChar '/') [] -> Search.openSearchPrompt 1
        V.EvKey (V.KChar '?') [] -> Search.openSearchPrompt (-1)
        V.EvKey (V.KChar 'n') [] -> Search.searchRepeat 1
        V.EvKey (V.KChar 'N') [] -> Search.searchRepeat (-1)
        V.EvKey (V.KChar '*') [] -> Search.searchWordUnderCursor
        V.EvKey (V.KChar '#') [] -> Search.searchWordBackward
        V.EvKey (V.KChar 'p') [V.MCtrl] -> TreeAction.openFileFinder
        V.EvKey (V.KChar ' ') [] ->
          State.modify (\s -> s {Types.stSpace = True})
        V.EvKey (V.KChar 'p') [] -> pasteKey deferToVi i ev
        V.EvKey (V.KChar 'P') [] -> pasteKey deferToVi i ev
        _ -> deferToVi i ed ev

-- The vi layer's own state lives in St, so the result carries both a new
-- key state and (when the command completed) a new buffer.
deferToVi :: Int -> Modal.EdState -> V.Event -> Brick.EventM Types.Name Types.St ()
deferToVi i ed ev = do
  st <- State.get
  case Vi.handleVi (Types.stVi st) ev ed of
    Vi.ViUnhandled -> pure ()
    Vi.ViPending keys -> State.modify (\s -> s {Types.stVi = keys})
    Vi.ViDone keys edNext -> do
      State.modify (\s -> s {Types.stVi = keys})
      applyEdit i ed edNext

-- A paste key, whichever keymap asked: consult the system clipboard,
-- then hand the key to the editor with the slot already refreshed. An
-- empty slot after that really is empty, and the message says which of
-- the two reasons applies.
pasteKey ::
  (Int -> Modal.EdState -> V.Event -> Brick.EventM Types.Name Types.St ()) ->
  Int ->
  V.Event ->
  Brick.EventM Types.Name Types.St ()
pasteKey deliver i ev = do
  Nav.pullClipboard
  st <- State.get
  let
    ed = (Types.fvEd (Types.stFiles st !! i)) {Modal.edKill = Types.stKill st}
  if T.null (Modal.edKill ed)
    then
      Types.setMsg
        ( if Clipboard.clipboardReadable (Types.stClip st)
            then "nothing to paste — y yanks (the walk filter is Space p)"
            else "nothing to paste — no clipboard helper here (wl-copy, xclip, xsel, pbcopy)"
        )
    else deliver i ed ev

deferToModal :: Int -> Modal.EdState -> V.Event -> Brick.EventM Types.Name Types.St ()
deferToModal i ed ev =
  case Modal.handleNormal ev ed of
    Just edNext -> applyEdit i ed edNext
    Nothing -> pure ()
