{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Cite
  ( openCitePicker
  , handleCiteKey
  , pasteIntoPopup
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Graphics.Vty qualified as V
import Remax.Git qualified as Git
import Remax.Modal qualified as Modal
import Remax.UI.Core.Types qualified as Types
import Remax.Vi qualified as Vi

openCitePicker :: Brick.EventM Types.Name Types.St ()
openCitePicker = do
  st <- State.get
  let
    pull = case Git.subjectOf (Types.stMode st) of
      Git.Pull pr _ -> Just pr
      _ -> Nothing
  commits <- MonadIO.liftIO (Git.citableCommits pull (Types.stRoot st))
  case commits of
    [] -> Types.setMsg "no commits to cite"
    _ -> State.modify (\s -> s {Types.stCiting = Just Types.CitePicker {Types.cpCommits = commits, Types.cpAt = 0}})

handleCiteKey :: V.Event -> Types.CitePicker -> Brick.EventM Types.Name Types.St ()
handleCiteKey ev picker = do
  chord <- Types.readChord ev
  case chord of
    Types.CancelKey -> closeCitePicker
    Types.PrefixStarted -> pure ()
    Types.PrefixedKey _ -> Types.setMsg "C-c C-k cancels"
    Types.PlainKey _ -> citeKey ev picker

citeKey :: V.Event -> Types.CitePicker -> Brick.EventM Types.Name Types.St ()
citeKey ev picker =
  case ev of
    V.EvKey V.KEsc [] -> closeCitePicker
    V.EvKey (V.KChar 'g') [V.MCtrl] -> closeCitePicker
    V.EvKey V.KEnter [] -> citeAt picker
    _ -> case Vi.rowStep ev of
      Just d ->
        let
          at = max 0 (min (length (Types.cpCommits picker) - 1) (Types.cpAt picker + d))
        in
          State.modify (\s -> s {Types.stCiting = Just picker {Types.cpAt = at}})
      Nothing -> Types.setMsg "j/k pick \183 \9166 inserts the sha \183 Esc"

closeCitePicker :: Brick.EventM Types.Name Types.St ()
closeCitePicker = State.modify (\s -> s {Types.stCiting = Nothing})

citeAt :: Types.CitePicker -> Brick.EventM Types.Name Types.St ()
citeAt picker = do
  closeCitePicker
  Monad.forM_ (Types.safeIdx (Types.cpCommits picker) (Types.cpAt picker)) (pasteIntoPopup . Git.ccSha)

pasteIntoPopup :: T.Text -> Brick.EventM Types.Name Types.St ()
pasteIntoPopup text = do
  st <- State.get
  Monad.forM_ (Types.stNotePopup st) $ \np ->
    case Modal.handleInsert (V.EvPaste (TE.encodeUtf8 text)) ((Types.npEd np) {Modal.edMode = Modal.Insert}) of
      Modal.InsertHandled ed -> setEd ed
      Modal.InsertExit ed -> setEd ed
      Modal.InsertUnhandled -> pure ()

setEd :: Modal.EdState -> Brick.EventM Types.Name Types.St ()
setEd ed =
  State.modify (\s -> s {Types.stNotePopup = fmap (\np -> np {Types.npEd = ed}) (Types.stNotePopup s)})
