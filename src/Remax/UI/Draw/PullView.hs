{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.PullView
  ( pullViewBody
  , rowWidget
  ) where

import Brick qualified as Brick
import Brick.Types qualified as BrickTypes
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
        selected = fmap (\b -> (Types.bandConv b, Types.bandPost b)) (Types.stBand st)
        bandedAt i r = selected /= Nothing && PullView.rowPost r == selected && i >= (0 :: Int)
        drawn = zipWith (\i r -> rowWidget (bandedAt i r) r) [0 ..] rows ++ [Brick.txt " "]
        bandedRows = map fst (filter (uncurry bandedAt) (zip [0 ..] rows))
        shown = case bandedRows of
          (first : more) ->
            let
              (before, rest) = splitAt first drawn
              (focus, after) = splitAt (foldr max first more - first + 1 + rowsBelowSelection) rest
            in
              before ++ [Brick.visible (Brick.vBox focus)] ++ after
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
