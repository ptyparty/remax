{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Status
  ( statusBar
  ) where

import Brick
  ( AttrName
  , Padding (Max)
  , Widget
  , hBox
  , padLeft
  , txt
  , withAttr
  , withDefAttr
  , (<+>)
  )
import Data.List
  ( find
  )
import Data.Text (Text)
import qualified Data.Text as T
import Remax.Config
import Remax.Modal
import Remax.UI.Core.Attrs
import Remax.UI.Core.Ink
import Remax.UI.Core.Types
import Remax.UI.Core.View
import Remax.UI.Core.Widgets
import Remax.Vi
  ( viLabel
  )

statusBar :: St -> Widget Name
statusBar st =
  let
    middle = case stPrompt st of
      Just (SearchPrompt input) ->
        txt (searchSigil st <> input <> "\9612")
      Just (CmdPrompt input) -> txt (":" <> input <> "\9612")
      Nothing -> withAttr msgAttr (txt (padT (stMsg st)))
  in
    withDefAttr statusAttr $
      modeChip st
        <+> pollChip st
        <+> declSeg st
        <+> meterSeg st
        <+> txt "  "
        <+> middle
        <+> padLeft Max (withAttr hintAttr (txt (statusHint st)))

chip :: AttrName -> Text -> Widget Name
chip dotAttr label =
  hBox
    [ txt " "
    , withAttr dotAttr (txt "\9679")
    , withAttr statusAttr (txt (" " <> label <> " "))
    ]

-- what the buffer is waiting for, most specific first: the space
-- menu, an emacs chord, a half-typed vi command, insert, then plain
-- normal — with vi's own resting label saying which keymap you are in
modeChip :: St -> Widget Name
modeChip st
  | stSpace st = chip modeDotNorAttr "spc\8230"
  | stIdleGo st = chip modeDotNorAttr "g\8230"
  | otherwise = case stEmacsPrefix st of
      Just pfx -> chip modeDotNorAttr ("C-" <> T.singleton pfx <> "\8230")
      Nothing -> case cfgKeys (stConfig st) of
        KeysEmacs -> chip modeDotNorAttr "emx"
        KeysVi -> viChip st
        KeysHelix -> helixChip st

viChip :: St -> Widget Name
viChip st =
  case currentFileMay st of
    Nothing -> chip modeDotNorAttr "vi"
    Just fv -> case edMode (fvEd fv) of
      Insert -> chip modeDotInsAttr "ins"
      Normal -> case viLabel (stVi st) of
        Just p -> chip modeDotNorAttr (p <> "\8230")
        Nothing
          | edExtend (fvEd fv) -> chip modeDotNorAttr "vis"
          | otherwise -> chip modeDotNorAttr "vi"

helixChip :: St -> Widget Name
helixChip st =
  case currentFileMay st of
    Nothing -> chip modeDotNorAttr "nor"
    Just fv -> case edMode (fvEd fv) of
      Insert -> chip modeDotInsAttr "ins"
      Normal -> case pendingLabel (fvEd fv) of
        Just p -> chip modeDotNorAttr (p <> "\8230")
        Nothing -> chip modeDotNorAttr "nor"

-- the startup warning scrolls away with the next message; this keeps
-- "the watcher is a second behind" on screen for the whole session
pollChip :: St -> Widget Name
pollChip st =
  if stWatchMode st == PollingWatch
    then withAttr watchPollAttr (txt " \9888 poll ")
    else txt ""

declSeg :: St -> Widget Name
declSeg st =
  case currentFileMay st of
    Just fv ->
      -- nearest decl at or above the cursor, found by walking up
      -- from it rather than rebuilding the buffer every frame
      case find isDeclLine (bufferUpward (fvEd fv)) of
        Just d ->
          withAttr trayDeclAttr (txt (" " <> T.take 32 (declName d) <> " \183"))
        Nothing -> txt ""
    Nothing -> txt ""

-- in vi the verbs sit under the leader, so the hint has to say so
-- Named for the keymap in use, all three of them. The verbs sit
-- under a leader in vi and under C-c in emacs, and emacs has no
-- space leader at all — so it has no space menu and no ␣? either,
-- and reaches the sheet the way it reaches every other command.
verbHint :: KeyMode -> Text
verbHint keys =
  case keys of
    KeysVi -> "\9251\9166 accept · \9251! flag · \9251q question · \9251? keys "
    KeysEmacs ->
      "C-c \9166 accept · C-c ! flag · C-c ? question · M-x help "
    KeysHelix -> "\9166 accept · ! flag · ? question · \9251 menu · \9251? keys "

statusHint :: St -> Text
statusHint st =
  let
    keys = cfgKeys (stConfig st)
  in
    case currentFileMay st of
      Nothing ->
        promptKeys keys <> " command · \9251? keys · " <> quitKeys keys <> " quit "
      Just fv ->
        (if fvStale fv then "hunks refresh on Esc · " else "") <> verbHint keys

meterSeg :: St -> Widget Name
meterSeg st =
  let
    total = sum (map (length . fvRuns) (stFiles st))
    pending = sum (map fvPending (stFiles st))
  in
    if total == 0
      then txt ""
      else txt "  " <+> meterW (total - pending) total
