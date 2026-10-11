{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Events
  ( syncComposing
  , rememberSelector
  , noteTrayView
  , reanchorBody
  , handleEvent
  ) where

import Brick
  ( BrickEvent (..)
  , EventM
  , Viewport (..)
  , ViewportScroll (..)
  , halt
  , lookupViewport
  , viewportScroll
  )
import Control.Monad
  ( forM_
  , unless
  , when
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
  ( get
  , modify
  )
import Data.List
  ( delete
  , find
  )
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Remax.Clipboard
import Remax.Config
import Remax.Emacs (handleEmacs)
import Remax.Modal
import Remax.Store
import Remax.UI.Command
import Remax.UI.Editor
import Remax.UI.Load
import Remax.UI.Menus
import Remax.UI.Nav
import Remax.UI.Notes
import Remax.UI.Open
import Remax.UI.Picker
import Remax.UI.Search
import Remax.UI.Tree
import Remax.UI.TreeKeys
import Remax.UI.Types
import Remax.UI.Verdict
import Remax.UI.Verdicts
import Remax.UI.View
import Remax.Vi
  ( ViResult (..)
  , handleVi
  , viBusy
  )
import System.FilePath
  ( isAbsolute
  , makeRelative
  , (</>)
  )

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
syncComposing :: EventM Name St ()
syncComposing = do
  st <- get
  let
    writing = case stNotePopup st of
      Just np
        | NoteFor _ _ <- npPurpose np -> Just (repoText st)
      _ -> Nothing
  unless (writing == stComposing st) $ do
    -- cleared where it was set before being set anywhere else, so
    -- switching repositories cannot strand it on the old one
    forM_ (stComposing st) (\was -> liftIO (setComposing (stConn st) was False))
    forM_ writing (\now -> liftIO (setComposing (stConn st) now True))
    modify (\s -> s {stComposing = writing})

-- Keep whichever selector is on screen, so ␣' can bring it back.
--
-- Watched after every event rather than saved wherever one closes. They
-- close from several places — Escape, a choice taken, a repository
-- switched out from under them — and the one that forgot to save would
-- quietly reopen something older instead. What is open now is what
-- comes back, and the last event that had one open is the state it was
-- left in: Escape and Enter both close without disturbing the query or
-- the row, so what is remembered is what was on screen.
rememberSelector :: EventM Name St ()
rememberSelector = do
  st <- get
  let
    onScreen = case (stTree st, stPicker st) of
      (Just tv, _) -> Just (LastTree tv)
      (_, Just pk) -> Just (LastPicker pk)
      _ -> Nothing
  forM_ onScreen (\sel -> modify (\s -> s {stLastSelector = Just sel}))

-- Note where the tray is sitting, so the next frame knows which of its
-- rows can be seen. Read after the event rather than during the draw
-- because that is where brick will tell us; it is therefore one frame
-- behind, which the margin in tray covers.
noteTrayView :: EventM Name St ()
noteTrayView = do
  mvp <- lookupViewport TrayVP
  forM_ mvp $ \vp -> do
    let
      seen = (_vpTop vp, snd (_vpSize vp))
    modify (\s -> s {stTraySeen = seen})

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
reanchorBody :: EventM Name St ()
reanchorBody = do
  st <- get
  case currentFileMay st of
    Nothing -> pure ()
    Just fv -> do
      screenRows <- terminalRows
      let
        (_, origin) = bodyWindow screenRows st fv
        moved = origin - fvOrigin fv
      unless (moved == 0) $ do
        modify (updateFile (stCurF st) (\f -> f {fvOrigin = origin}))
        mvp <- lookupViewport BodyVP
        case mvp of
          Just vp -> setTop (viewportScroll BodyVP) (max 0 (_vpTop vp - moved))
          Nothing -> pure ()

handleEvent :: BrickEvent Name REvent -> EventM Name St ()
handleEvent brickEvent =
  case brickEvent of
    AppEvent (FileChangedOnDisk absolute) -> do
      st <- get
      -- relativize here, against whichever repository is current now
      let
        rel = makeRelative (stRoot st) absolute
      unless (isAbsolute rel) (reloadFromDisk rel)
    AppEvent GitStateChanged -> refreshGit
    AppEvent StoreChanged -> do
      st <- get
      threads <- liftIO (loadThreads (stConn st) (repoText st))
      groups <- liftIO (loadNoteGroups (stConn st) (repoText st))
      modify (\s -> s {stThreads = threads, stNoteGroups = groups})
    AppEvent (Attending seen) -> do
      modify (\s -> s {stAttending = seen, stThrob = stThrob s + 1})
      pollCurrentFile
    AppEvent Tick -> scrollHunkIntoView
    AppEvent Pulse -> modify (\s -> s {stThrob = stThrob s + 1})
    AppEvent (JobDone label finish) -> do
      modify (\s -> s {stBusy = delete label (stBusy s)})
      finish
    VtyEvent ev -> do
      st <- get
      -- messages are transient: any keystroke clears the old one, and a
      -- handler that has something new to say sets it afresh
      modify (\s -> s {stMsg = ""})
      case (stNotePopup st, stPicker st, stPrompt st) of
        _
          | stHelp st -> modify (\s -> s {stHelp = False})
        _
          | stInfoPopup st /= Nothing ->
              modify (\s -> s {stInfoPopup = Nothing})
        (Just np, _, _) -> handleNotePopupKey ev np
        _
          | Just tv <- stTree st -> handleTreeKey ev tv
        (_, Just pk, _) -> handlePickerKey ev pk
        (_, _, Just p) -> handlePromptKey ev p
        _ ->
          if stSpace st
            then handleSpaceKey ev
            else case stEmacsPrefix st of
              Just pfx -> do
                modify (\s -> s {stEmacsPrefix = Nothing})
                if null (stFiles st)
                  then handleIdlePrefixed pfx ev
                  else handleEmacsPrefixed pfx ev
              Nothing
                | stIdleGo st -> do
                    modify (\s -> s {stIdleGo = False})
                    handleIdleGo ev
                | null (stFiles st) -> handleIdleKey ev
                | otherwise -> handleModalKey ev
    _ -> pure ()

-- The helix-style space menu: Space, then one key; anything unlisted
-- just cancels. Dispatch reads spaceMenuEntries, which is also what the
-- infobox renders, so the menu and the bindings cannot disagree.
handleSpaceKey :: V.Event -> EventM Name St ()
handleSpaceKey ev = do
  modify (\s -> s {stSpace = False})
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
      case find (\(k, _, _) -> k == key) spaceMenuEntries of
        Just (_, _, action) -> action
        Nothing -> pure ()

-- With nothing on screen the only meaningful keys are quitting and the
-- command line (to switch what the session is looking at).
handleIdleKey :: V.Event -> EventM Name St ()
handleIdleKey ev =
  case ev of
    V.EvKey (V.KChar ':') [] -> openCmdPrompt
    V.EvKey (V.KChar 'p') [V.MCtrl] -> openFileFinder
    V.EvKey (V.KChar 'g') [] -> modify (\s -> s {stIdleGo = True})
    V.EvKey (V.KChar ' ') [] -> modify (\s -> s {stSpace = True})
    V.EvKey (V.KChar 'x') [V.MMeta] -> openCmdPrompt
    V.EvKey (V.KChar 'x') [V.MCtrl] ->
      modify (\s -> s {stEmacsPrefix = Just 'x'})
    _ -> pure ()

-- The g menu with nothing on screen. Only gr means anything here: there
-- is no file to move around in, and switching repository is most of what
-- an empty session is for — a story's first checkout being clean is
-- exactly when the next one is wanted.
handleIdleGo :: V.Event -> EventM Name St ()
handleIdleGo ev =
  case ev of
    V.EvKey (V.KChar 'r') [] -> openRepoPicker
    _ -> pure ()

-- With no files on screen, only the session chords mean anything.
handleIdlePrefixed :: Char -> V.Event -> EventM Name St ()
handleIdlePrefixed pfx ev =
  case (pfx, ev) of
    ('x', V.EvKey (V.KChar 'c') [V.MCtrl]) -> halt
    _ -> pure ()

handlePromptKey :: V.Event -> Prompt -> EventM Name St ()
handlePromptKey ev prompt =
  let
    input = case prompt of
      SearchPrompt t -> t
      CmdPrompt t -> t
    rewrap t = case prompt of
      SearchPrompt _ -> SearchPrompt t
      CmdPrompt _ -> CmdPrompt t
    isCmd = case prompt of
      CmdPrompt _ -> True
      SearchPrompt _ -> False
  in
    case ev of
      V.EvKey V.KEsc [] ->
        modify (\s -> s {stPrompt = Nothing, stPromptCycle = Nothing})
      V.EvKey V.KEnter [] -> do
        modify (\s -> s {stPrompt = Nothing, stPromptCycle = Nothing})
        case prompt of
          SearchPrompt t -> do
            modify (\s -> s {stLastSearch = t, stGrepNav = Nothing})
            st <- get
            unless (T.null t) (searchStep (stSearchDir st) t)
          CmdPrompt t -> runCommand t
      V.EvKey (V.KChar '\t') [] | isCmd -> promptComplete 1 input
      V.EvKey V.KBackTab [] | isCmd -> promptComplete (-1) input
      V.EvKey V.KBS [] ->
        modify
          ( \s ->
              s
                { stPrompt = Just (rewrap (T.dropEnd 1 input))
                , stPromptCycle = Nothing
                }
          )
      V.EvKey (V.KChar c) [] ->
        modify
          ( \s ->
              s
                { stPrompt = Just (rewrap (T.snoc input c))
                , stPromptCycle = Nothing
                }
          )
      _ -> pure ()

handleModalKey :: V.Event -> EventM Name St ()
handleModalKey ev = do
  st <- get
  let
    i = stCurF st
    fv = stFiles st !! i
    -- the kill slot is session state: whatever was yanked anywhere is
    -- pasteable here
    ed = (fvEd fv) {edKill = stKill st}
  case cfgKeys (stConfig st) of
    KeysEmacs -> handleEmacsKey i ed ev
    KeysVi -> handleViKey i ed ev
    KeysHelix -> handleHelixKey i fv ed ev

-- Emacs keymap: editing chords go to Remax.Emacs; M-x and C-s open the
-- prompts; C-c prefixes the review verbs (same letters as the helix
-- keys), C-x the session chords (C-x C-c quits, C-x C-s saves).
handleEmacsKey :: Int -> EdState -> V.Event -> EventM Name St ()
handleEmacsKey i ed ev = do
  st <- get
  case stEmacsPrefix st of
    Just pfx -> do
      modify (\s -> s {stEmacsPrefix = Nothing})
      handleEmacsPrefixed pfx ev
    Nothing ->
      case ev of
        V.EvKey (V.KChar 'x') [V.MMeta] -> openCmdPrompt
        V.EvKey (V.KChar 's') [V.MCtrl] -> openSearchPrompt 1
        V.EvKey (V.KChar 'c') [V.MCtrl] ->
          modify (\s -> s {stEmacsPrefix = Just 'c'})
        V.EvKey (V.KChar 'x') [V.MCtrl] ->
          modify (\s -> s {stEmacsPrefix = Just 'x'})
        -- C-y is emacs's paste, so it consults the clipboard the way
        -- p does in the modal keymaps
        V.EvKey (V.KChar 'y') [V.MCtrl] -> do
          pullClipboard
          refreshed <- get
          emacsEdit i ed {edKill = stKill refreshed} ev
        _ -> emacsEdit i ed ev

emacsEdit :: Int -> EdState -> V.Event -> EventM Name St ()
emacsEdit i ed ev =
  case handleEmacs ev ed of
    Just edNext ->
      if bufferLines edNext /= bufferLines ed
        then do
          rebuildWith i edNext
          applyToDisk i
        else setEd i edNext
    Nothing -> pure ()

handleEmacsPrefixed :: Char -> V.Event -> EventM Name St ()
handleEmacsPrefixed pfx ev = do
  st <- get
  case pfx of
    'x' ->
      case ev of
        V.EvKey (V.KChar 'c') [V.MCtrl] -> halt
        V.EvKey (V.KChar 's') [V.MCtrl] -> applyToDisk (stCurF st)
        _ -> pure ()
    _ ->
      case ev of
        V.EvKey V.KEnter [] -> acceptSelection
        V.EvKey (V.KChar '!') [] -> markThenNote Flagged
        V.EvKey (V.KChar '?') [] -> markThenNote Questioned
        V.EvKey (V.KChar 'r') [] -> clearVerdictBySelection
        V.EvKey (V.KChar 'R') [] -> clearVerdictBySelection
        V.EvKey (V.KChar ']') [] -> switchRun 1
        V.EvKey (V.KChar '[') [] -> switchRun (-1)
        V.EvKey (V.KChar 'x') [] -> selectCurrentHunk
        V.EvKey (V.KChar 'p') [] -> togglePendingOnly
        V.EvKey (V.KChar 'k') [] -> cycleRunFilter
        V.EvKey (V.KChar 'u') [] -> unnoteCmd
        V.EvKey (V.KChar 'h') [] -> setCurrentHunkHere
        V.EvKey (V.KChar 'v') [] -> revertTarget
        V.EvKey (V.KChar 'a') [] -> gotoAltFile
        V.EvKey (V.KChar '/') [] -> openGrepPicker ""
        V.EvKey (V.KChar 'c') [] -> showCommitPopup
        V.EvKey (V.KChar '\'') [] -> jumpToAnchor
        V.EvKey (V.KChar '*') [] -> searchWordUnderCursor
        V.EvKey (V.KChar 'n') [] -> searchRepeat 1
        V.EvKey (V.KChar 'N') [] -> searchRepeat (-1)
        V.EvKey (V.KChar 'f') [] -> openFileFinder
        V.EvKey (V.KChar 'b') [] -> openBufferPicker
        V.EvKey (V.KChar 'e') [] -> editCurrent
        V.EvKey (V.KChar ')') [] -> switchFile 1
        V.EvKey (V.KChar '(') [] -> switchFile (-1)
        V.EvKey (V.KChar '\t') [] ->
          modify (\s -> s {stTray = not (stTray s)})
        _ -> pure ()

-- Insert mode is the same in every modal keymap: type, and land the
-- result back in the file view as cheaply as the edit allows — a
-- same-shape edit patches one line rather than rebuilding the diff.
handleInsertKey :: Int -> EdState -> V.Event -> EventM Name St ()
handleInsertKey i ed ev =
  case handleInsert ev ed of
    InsertHandled edNext -> do
      let
        oldLs = bufferLines ed
        newLs = bufferLines edNext
      if newLs == oldLs
        then setEd i edNext
        else
          if length newLs == length oldLs
            then
              modify
                ( \s ->
                    updateFile
                      i
                      ( patchLine
                          (stConfig s)
                          (fst (cursorRC edNext))
                          (currentLineOf edNext)
                          . withEd edNext
                      )
                      s
                )
            else rebuildWith i edNext
    InsertExit edNext -> do
      rebuildWith i edNext
      applyToDisk i
    InsertUnhandled -> pure ()

handleHelixKey :: Int -> FileView -> EdState -> V.Event -> EventM Name St ()
handleHelixKey i fv ed ev =
  case edMode ed of
    Insert -> handleInsertKey i ed ev
    -- a pending g/f/t prefix owns the next key outright; review keys
    -- must not steal its target character
    -- ga is review-level (goto alternate file), but the g prefix lives
    -- in the modal layer; catch it here and let the modal cancel its
    -- pending state
    Normal
      | pendingLabel ed == Just "g"
      , V.EvKey (V.KChar c) [] <- ev
      , Just go <- helixGoKey c -> do
          deferToModal i ed ev
          go
    Normal | hasPending ed -> deferToModal i ed ev
    Normal -> helixNormalKey i fv ed ev

-- The review-level g keys. gr: go to another repository — free in every
-- keymap remax has, and the g menu is where going somewhere already
-- lives; Space R is the same door for the keymaps that have no g of
-- their own. gn/gp: the next and previous file. helix spends these on
-- buffers, and a file under review is what remax has instead of one, so
-- they keep the meaning a reader arrives with. ) and ( are the same move
-- in one keystroke; these are for the hand that already knows helix.
helixGoKey :: Char -> Maybe (EventM Name St ())
helixGoKey c =
  case c of
    'a' -> Just gotoAltFile
    'r' -> Just openRepoPicker
    'n' -> Just (switchFile 1)
    'p' -> Just (switchFile (-1))
    _ -> Nothing

helixNormalKey :: Int -> FileView -> EdState -> V.Event -> EventM Name St ()
helixNormalKey i fv ed ev = do
  st <- get
  case ev of
    V.EvKey (V.KChar '\t') [] -> modify (\s -> s {stTray = not (stTray s)})
    V.EvKey V.KEnter [] -> acceptSelection
    V.EvKey (V.KChar '!') [] -> markThenNote Flagged
    V.EvKey (V.KChar '?') [] -> markThenNote Questioned
    V.EvKey (V.KChar 'R') [] -> clearVerdictBySelection
    V.EvKey (V.KChar ']') [] -> switchRun 1
    V.EvKey (V.KChar '[') [] -> switchRun (-1)
    V.EvKey (V.KChar '/') [] -> openSearchPrompt 1
    V.EvKey (V.KChar ':') [] -> openCmdPrompt
    V.EvKey (V.KChar '*') [] -> searchWordUnderCursor
    V.EvKey (V.KChar 'n') [] -> searchRepeat 1
    V.EvKey (V.KChar 'N') [] -> searchRepeat (-1)
    V.EvKey (V.KChar '\'') [] -> jumpToAnchor
    V.EvKey (V.KChar 'X') [] -> selectCurrentHunk
    V.EvKey (V.KChar 'p') [] -> pasteKey deferToModal i ev
    V.EvKey (V.KChar 'P') [] -> pasteKey deferToModal i ev
    V.EvKey (V.KChar ' ') [] -> modify (\s -> s {stSpace = True})
    V.EvKey (V.KChar 'E') [] -> launchEditor (stRoot st </> fvPath fv) (fst (cursorRC ed) + 1)
    V.EvKey (V.KChar ')') [] -> switchFile 1
    V.EvKey (V.KChar '(') [] -> switchFile (-1)
    V.EvKey (V.KChar 'p') [V.MCtrl] -> openFileFinder
    V.EvKey (V.KChar '{') [] -> vScrollBy (viewportScroll TrayVP) (-8)
    V.EvKey (V.KChar '}') [] -> vScrollBy (viewportScroll TrayVP) 8
    _ -> deferToModal i ed ev

-- Vi keymap: the buffer belongs to vi, and the review verbs live behind
-- the Space leader the way the emacs keymap puts them behind C-c. What
-- keeps its remax key is structural navigation — hunks, files, the tray,
-- the anchor back to what is under review — because that is the tool's
-- own vocabulary rather than the editor's, and vi's sentence and
-- paragraph motions are a small price for it. Everything else is vi's:
-- `?` searches backward, `X` deletes leftward, `E` is the WORD motion
-- (Space e still opens $EDITOR here).
handleViKey :: Int -> EdState -> V.Event -> EventM Name St ()
handleViKey i ed ev = do
  st <- get
  case edMode ed of
    Insert -> handleInsertKey i ed ev
    -- a half-typed vi command owns the next keystroke outright, so `f]`
    -- finds a bracket instead of walking to the next hunk, and `2` then
    -- `}` counts rather than scrolling the tray
    Normal | viBusy (stVi st) -> deferToVi i ed ev
    Normal ->
      case ev of
        V.EvKey (V.KChar '\t') [] ->
          modify (\s -> s {stTray = not (stTray s)})
        V.EvKey (V.KChar ']') [] -> switchRun 1
        V.EvKey (V.KChar '[') [] -> switchRun (-1)
        V.EvKey (V.KChar ')') [] -> switchFile 1
        V.EvKey (V.KChar '(') [] -> switchFile (-1)
        V.EvKey (V.KChar '{') [] ->
          vScrollBy (viewportScroll TrayVP) (-8)
        V.EvKey (V.KChar '}') [] ->
          vScrollBy (viewportScroll TrayVP) 8
        V.EvKey (V.KChar '\'') [] -> jumpToAnchor
        V.EvKey (V.KChar ':') [] -> openCmdPrompt
        V.EvKey (V.KChar '/') [] -> openSearchPrompt 1
        V.EvKey (V.KChar '?') [] -> openSearchPrompt (-1)
        V.EvKey (V.KChar 'n') [] -> searchRepeat 1
        V.EvKey (V.KChar 'N') [] -> searchRepeat (-1)
        V.EvKey (V.KChar '*') [] -> searchWordUnderCursor
        V.EvKey (V.KChar '#') [] -> searchWordBackward
        V.EvKey (V.KChar 'p') [V.MCtrl] -> openFileFinder
        V.EvKey (V.KChar ' ') [] ->
          modify (\s -> s {stSpace = True})
        V.EvKey (V.KChar 'p') [] -> pasteKey deferToVi i ev
        V.EvKey (V.KChar 'P') [] -> pasteKey deferToVi i ev
        _ -> deferToVi i ed ev

-- The vi layer's own state lives in St, so the result carries both a new
-- key state and (when the command completed) a new buffer.
deferToVi :: Int -> EdState -> V.Event -> EventM Name St ()
deferToVi i ed ev = do
  st <- get
  case handleVi (stVi st) ev ed of
    ViUnhandled -> pure ()
    ViPending keys -> modify (\s -> s {stVi = keys})
    ViDone keys edNext -> do
      modify (\s -> s {stVi = keys})
      if bufferLines edNext /= bufferLines ed
        then do
          rebuildWith i edNext
          when (edMode edNext == Normal) (applyToDisk i)
        else setEd i edNext

-- A paste key, whichever keymap asked: consult the system clipboard,
-- then hand the key to the editor with the slot already refreshed. An
-- empty slot after that really is empty, and the message says which of
-- the two reasons applies.
pasteKey ::
  (Int -> EdState -> V.Event -> EventM Name St ()) ->
  Int ->
  V.Event ->
  EventM Name St ()
pasteKey deliver i ev = do
  pullClipboard
  st <- get
  let
    ed = (fvEd (stFiles st !! i)) {edKill = stKill st}
  if T.null (edKill ed)
    then
      setMsg
        ( if clipboardReadable (stClip st)
            then "nothing to paste — y yanks (the pending filter is Space p)"
            else "nothing to paste — no clipboard helper here (wl-copy, xclip, xsel, pbcopy)"
        )
    else deliver i ed ev

deferToModal :: Int -> EdState -> V.Event -> EventM Name St ()
deferToModal i ed ev =
  case handleNormal ev ed of
    Just edNext ->
      if bufferLines edNext /= bufferLines ed
        then do
          rebuildWith i edNext
          when (edMode edNext == Normal) (applyToDisk i)
        else setEd i edNext
    Nothing -> pure ()
