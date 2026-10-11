{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Status
  ( statusBar
  ) where

import Brick ((<+>))
import Brick qualified as Brick
import Data.List qualified as List
import Data.Text qualified as T
import Remax.Config qualified as Config
import Remax.Modal qualified as Modal
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Ink qualified as Ink
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.View qualified as View
import Remax.UI.Core.Widgets qualified as Widgets
import Remax.Vi qualified as Vi

statusBar :: Types.St -> Brick.Widget Types.Name
statusBar st =
  let
    middle = case Types.stPrompt st of
      Just (Types.SearchPrompt input) ->
        Brick.txt (Types.searchSigil st <> input <> "\9612")
      Just (Types.CmdPrompt input) -> Brick.txt (":" <> input <> "\9612")
      Nothing -> Brick.withAttr Attrs.msgAttr (Brick.txt (Widgets.padT (Types.stMsg st)))
  in
    Brick.withDefAttr Attrs.statusAttr $
      modeChip st
        <+> pollChip st
        <+> declSeg st
        <+> meterSeg st
        <+> Brick.txt "  "
        <+> middle
        <+> Brick.padLeft Brick.Max (Brick.withAttr Attrs.hintAttr (Brick.txt (statusHint st)))

chip :: Brick.AttrName -> T.Text -> Brick.Widget Types.Name
chip dotAttr label =
  Brick.hBox
    [ Brick.txt " "
    , Brick.withAttr dotAttr (Brick.txt "\9679")
    , Brick.withAttr Attrs.statusAttr (Brick.txt (" " <> label <> " "))
    ]

-- what the buffer is waiting for, most specific first: the space
-- menu, an emacs chord, a half-typed vi command, insert, then plain
-- normal — with vi's own resting label saying which keymap you are in
modeChip :: Types.St -> Brick.Widget Types.Name
modeChip st
  | Types.stSpace st = chip Attrs.modeDotNorAttr "spc\8230"
  | Types.stIdleGo st = chip Attrs.modeDotNorAttr "g\8230"
  | otherwise = case Types.stEmacsPrefix st of
      Just pfx -> chip Attrs.modeDotNorAttr ("C-" <> T.singleton pfx <> "\8230")
      Nothing -> case Config.cfgKeys (Types.stConfig st) of
        Config.KeysEmacs -> chip Attrs.modeDotNorAttr "emx"
        Config.KeysVi -> viChip st
        Config.KeysHelix -> helixChip st

viChip :: Types.St -> Brick.Widget Types.Name
viChip st =
  case Types.currentFileMay st of
    Nothing -> chip Attrs.modeDotNorAttr "vi"
    Just fv -> case Modal.edMode (Types.fvEd fv) of
      Modal.Insert -> chip Attrs.modeDotInsAttr "ins"
      Modal.Normal -> case Vi.viLabel (Types.stVi st) of
        Just p -> chip Attrs.modeDotNorAttr (p <> "\8230")
        Nothing
          | Modal.edExtend (Types.fvEd fv) -> chip Attrs.modeDotNorAttr "vis"
          | otherwise -> chip Attrs.modeDotNorAttr "vi"

helixChip :: Types.St -> Brick.Widget Types.Name
helixChip st =
  case Types.currentFileMay st of
    Nothing -> chip Attrs.modeDotNorAttr "nor"
    Just fv -> case Modal.edMode (Types.fvEd fv) of
      Modal.Insert -> chip Attrs.modeDotInsAttr "ins"
      Modal.Normal -> case Modal.pendingLabel (Types.fvEd fv) of
        Just p -> chip Attrs.modeDotNorAttr (p <> "\8230")
        Nothing -> chip Attrs.modeDotNorAttr "nor"

-- the startup warning scrolls away with the next message; this keeps
-- "the watcher is a second behind" on screen for the whole session
pollChip :: Types.St -> Brick.Widget Types.Name
pollChip st =
  if Types.stWatchMode st == Types.PollingWatch
    then Brick.withAttr Attrs.watchPollAttr (Brick.txt " \9888 poll ")
    else Brick.txt ""

declSeg :: Types.St -> Brick.Widget Types.Name
declSeg st =
  case Types.currentFileMay st of
    Just fv ->
      -- nearest decl at or above the cursor, found by walking up
      -- from it rather than rebuilding the buffer every frame
      case List.find View.isDeclLine (Modal.bufferUpward (Types.fvEd fv)) of
        Just d ->
          Brick.withAttr Attrs.trayDeclAttr (Brick.txt (" " <> T.take 32 (View.declName d) <> " \183"))
        Nothing -> Brick.txt ""
    Nothing -> Brick.txt ""

-- in vi the verbs sit under the leader, so the hint has to say so
-- Named for the keymap in use, all three of them. The verbs sit
-- under a leader in vi and under C-c in emacs, and emacs has no
-- space leader at all — so it has no space menu and no ␣? either,
-- and reaches the sheet the way it reaches every other command.
verbHint :: Config.KeyMode -> T.Text
verbHint keys =
  case keys of
    Config.KeysVi -> "\9251\9166 accept · \9251! flag · \9251q question · \9251? keys "
    Config.KeysEmacs ->
      "C-c \9166 accept · C-c ! flag · C-c ? question · M-x help "
    Config.KeysHelix -> "\9166 accept · ! flag · ? question · \9251 menu · \9251? keys "

statusHint :: Types.St -> T.Text
statusHint st =
  let
    keys = Config.cfgKeys (Types.stConfig st)
  in
    case Types.currentFileMay st of
      Nothing ->
        Types.promptKeys keys <> " command · \9251? keys · " <> Types.quitKeys keys <> " quit "
      Just fv ->
        (if Types.fvStale fv then "hunks refresh on Esc · " else "") <> verbHint keys

meterSeg :: Types.St -> Brick.Widget Types.Name
meterSeg st =
  let
    total = sum (map (length . Types.fvRuns) (Types.stFiles st))
    pending = sum (map Types.fvPending (Types.stFiles st))
  in
    if total == 0
      then Brick.txt ""
      else Brick.txt "  " <+> Ink.meterW (total - pending) total
