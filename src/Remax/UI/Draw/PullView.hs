{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.PullView
  ( pullViewBody
  , rowWidget
  ) where

import Brick qualified as Brick
import Brick.Types qualified as BrickTypes
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Graphics.Vty qualified as V
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Markdown qualified as Markdown
import Remax.UI.Core.PullView qualified as PullView
import Remax.UI.Core.Types qualified as Types

pullViewBody :: Types.St -> BrickTypes.Widget Types.Name
pullViewBody st =
  Brick.viewport Types.PullVP Brick.Vertical $
    BrickTypes.Widget BrickTypes.Greedy BrickTypes.Fixed $ do
      ctx <- BrickTypes.getContext
      let
        width = max 40 (BrickTypes.availWidth ctx - 6)
        rows = PullView.timeline st width
        selected = PullView.selectedStop st
        banded r = selected /= Nothing && PullView.rowStop r == selected
        drawn = map (\r -> rowWidget (banded r) r) rows ++ [Brick.txt " "]
        bandedRows = List.findIndices banded rows
        shown = case bandedRows of
          (first : more) ->
            let
              lead = length (List.dropWhileEnd (Maybe.isNothing . PullView.rowStop) (take first rows))
              (before, rest) = splitAt lead drawn
              (leadIn, fromBand) = splitAt (first - lead) rest
              (focus, after) = splitAt (foldr max first more - first + 1 + rowsBelowSelection) fromBand
              shownBand = if lead == 0 then focus else [Brick.visible (Brick.vBox focus)]
            in
              before ++ [Brick.visible (Brick.vBox (leadIn ++ shownBand))] ++ after
          [] -> drawn
      BrickTypes.render (Brick.vBox shown)

rowsBelowSelection :: Int
rowsBelowSelection = 2

rowWidget :: Bool -> PullView.Row -> BrickTypes.Widget Types.Name
rowWidget banded row =
  let
    gutter = case PullView.rowGutter row of
      PullView.Dot -> Brick.withAttr Attrs.replyInlineAttr (Brick.txt " \9679 ")
      PullView.Pending -> Brick.withAttr Attrs.questionMarkAttr (Brick.txt " \9676 ")
      PullView.Rail -> Brick.withAttr Attrs.hintAttr (Brick.txt " \9474 ")
      PullView.Branch -> Brick.withAttr Attrs.hintAttr (Brick.txt " \9500\9472")
      PullView.Blank -> Brick.txt "   "
    seg s =
      let
        plain = Brick.withAttr (Markdown.segAttr s) (Brick.txt (Markdown.segText s))
      in
        case Markdown.segUrl s of
          Just url -> Brick.modifyDefAttr (`V.withURL` url) plain
          Nothing -> plain
    body = case PullView.rowSegs row of
      [] -> Brick.txt " "
      segs -> Brick.hBox (map seg segs)
    line = Brick.padRight Brick.Max (Brick.hBox [gutter, body])
    tinted = case PullView.rowFill row >>= Markdown.rowFill of
      Just fill -> Brick.withDefAttr fill line
      Nothing -> line
  in
    if banded then Brick.withDefAttr Attrs.selAttr tinted else tinted
