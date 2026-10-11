{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Tray
  ( tray
  ) where

import Brick qualified as Brick
import Brick.Types qualified as BrickTypes
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Remax.DiffModel qualified as DiffModel
import Remax.Store qualified as Store
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Ink qualified as Ink
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Verdict qualified as Verdict
import Remax.UI.Core.View qualified as View
import Remax.UI.Core.Widgets qualified as Widgets
import Remax.UI.Model.Tray qualified as TrayModel

-- The tray is drawn as images for the same reason the body is: it holds
-- a row per hunk in the whole change set, and every one of them was a
-- box of boxes redrawn on each keystroke. See PenSet.
tray :: Types.St -> BrickTypes.Widget Types.Name
tray st =
  Brick.hLimit TrayModel.trayWidth $
    Brick.padRight (Brick.Pad 1) $
      Brick.viewport Types.TrayVP Brick.Vertical $
        BrickTypes.Widget BrickTypes.Greedy BrickTypes.Fixed $ do
          ctx <- BrickTypes.getContext
          let
            pens = Ink.PenSet (BrickTypes.ctxAttrName ctx) (BrickTypes.ctxAttrMap ctx)
            width = BrickTypes.availWidth ctx
            rows = TrayModel.trayRows st
            total = length rows
            (lo, hi) = trayBand st total
            -- The rows outside the band are still *there* — the tray
            -- holds a row per hunk in the change set, and the viewport
            -- has to keep measuring the whole thing or scrolling would
            -- stop short of the end. They are one background fill
            -- apiece rather than an image per row, which costs nothing
            -- to build and is never rasterized: the viewport crops it
            -- away. Drawing all of them cost thirteen milliseconds a
            -- keystroke on a change set of thirteen thousand hunks,
            -- which is most of a frame spent on rows nobody can see.
            blank n = if n <= 0 then V.emptyImage else V.backgroundFill width n
            band = map (trayEntryImage st pens width) (take (hi - lo) (drop lo rows))
          BrickTypes.render . Brick.raw . V.vertCat $
            [blank lo] ++ band ++ [blank (total - hi)]

-- The rows worth drawing: a band around what the viewport showed last
-- frame and the hunk under review.
trayBand :: Types.St -> Int -> (Int, Int)
trayBand st total =
  let
    (seenTop, seenHeight) = Types.stTraySeen st
    -- Before the first frame there is no viewport to have seen,
    -- so start at the top and draw enough to fill any pane.
    paneRows = if seenHeight <= 0 then 200 else seenHeight
    -- Where the tray is pointed, or is about to be. Before the
    -- first frame there is no reading to go on, and the tray
    -- follows the hunk under review — which on a large change
    -- set is nowhere near the top, so assuming the top drew a
    -- band nobody could see. A jump lands there in the same
    -- event that scrolls, one frame ahead of any reading, so
    -- the current row is worth covering either way.
    focus = TrayModel.trayCurrentRow st
    anchor =
      if seenHeight <= 0
        then max 0 (focus - paneRows `div` 2)
        else seenTop
    -- A margin either side of what is on screen, because this
    -- reckoning is a frame behind: the scroll that moved the
    -- tray happened after the last time brick told us where it
    -- was. Scrolling moves it eight rows at a time and a jump
    -- re-frames it outright, so a screen of slack is plenty.
    margin = max 60 paneRows
  in
    ( max 0 (min anchor focus - margin)
    , min total (max (anchor + paneRows) focus + margin)
    )

trayEntryImage :: Types.St -> Ink.PenSet -> Int -> TrayModel.TrayRow -> V.Image
trayEntryImage st pens width entry =
  case entry of
    TrayModel.TrayRowFile fi _hidden fv -> trayFileImage st pens width fi fv
    TrayModel.TrayRowHunk fi hi r rawDecl -> trayHunkImage st pens width fi hi r rawDecl
    TrayModel.TrayRowRemark _fi _ri rm -> trayRemarkImage st pens width rm

markAttrFor :: Char -> Brick.AttrName
markAttrFor mark =
  case mark of
    '!' -> Attrs.flagMarkAttr
    '?' -> Attrs.questionMarkAttr
    '\10003' -> Attrs.doneMarkAttr
    _ -> Attrs.pendingMarkAttr

trayFileImage :: Types.St -> Ink.PenSet -> Int -> Int -> Types.FileView -> V.Image
trayFileImage st pens width fi fv =
  let
    total = length (Types.fvRuns fv)
    done = length (filter (not . Verdict.runPending (Types.stVerdicts st)) (Types.fvRuns fv))
    meterCols = Ink.meterTextWidth done total
    name = Widgets.shortPath (TrayModel.trayWidth - 3 - meterCols) (T.pack (Types.fvPath fv))
    nameAttr = if fi == Types.stCurF st then Attrs.trayFileCurAttr else Attrs.trayFileAttr
    nameImg = Ink.inkT width (Ink.penFor pens nameAttr) (Widgets.padT name)
    endImg = Ink.inkT (width - V.imageWidth nameImg) (Ink.penPlain pens) " "
    -- the meter is pushed to the right of what the name leaves,
    -- with the gap in front of it rather than behind
    gap = width - V.imageWidth nameImg - V.imageWidth endImg
    meterImg = Ink.meterInk pens gap done total
  in
    Ink.fitRow (Ink.penPlain pens) width $
      V.horizCat
        [ nameImg
        , V.charFill (Ink.penPlain pens) ' ' (max 0 (gap - V.imageWidth meterImg)) (1 :: Int)
        , meterImg
        , endImg
        ]

trayHunkImage :: Types.St -> Ink.PenSet -> Int -> Int -> Int -> DiffModel.Run -> T.Text -> V.Image
trayHunkImage st pens width fi hi r rawDecl =
  let
    -- asked once for the frame, not once per row: clampH measures the
    -- run list, and a row per hunk asking how many hunks there are is
    -- the change set squared
    curHunk = Types.clampH (Types.fvRuns (Types.currentFile st)) (Types.stCurH st)
    decl =
      if T.null rawDecl
        then ""
        else " " <> T.take 12 (View.declName rawDecl)
    current = fi == Types.stCurF st && hi == curHunk
    -- the current row reads every attribute against the selection
    -- as its default, so the highlight runs the width of the tray
    rowPens = if current then Ink.penUnder Attrs.floatSelAttr pens else pens
    mark = Verdict.runMark (Types.stVerdicts st) r
    noted = any (`Map.member` Types.stThreads st) (DiffModel.runKeys r)
  in
    Ink.fitRow (Ink.penPlain rowPens) width $
      Ink.spanInk
        width
        [ \left ->
            if current
              then Ink.inkT left (Ink.penFor rowPens Attrs.floatSelBarAttr) "\9614 "
              else Ink.inkT left (Ink.penPlain rowPens) "  "
        , \left -> Ink.inkT left (Ink.penFor rowPens (markAttrFor mark)) (T.singleton mark)
        , \left -> Ink.inkT left (Ink.penPlain rowPens) (" :" <> Types.tshow (DiffModel.runAnchor r + 1))
        , \left -> Ink.inkT left (Ink.penFor rowPens Attrs.trayDeclAttr) decl
        , \left -> if noted then Ink.inkT left (Ink.penFor rowPens Attrs.noteInlineAttr) " \8801" else V.emptyImage
        ]

-- A remark sits between the hunks rather than among them: it is
-- about a line the change never touched, so it carries the line
-- itself where a hunk carries its declaration.
trayRemarkImage :: Types.St -> Ink.PenSet -> Int -> Types.Remark -> V.Image
trayRemarkImage st pens width rm =
  let
    mark = case Map.lookup (Types.rmKey rm) (Types.stVerdicts st) of
      Just Store.Flagged -> '!'
      Just Store.Questioned -> '?'
      Just Store.Reviewed -> '\10003'
      Nothing -> '\183'
    noted = Map.member (Types.rmKey rm) (Types.stThreads st)
  in
    Ink.fitRow (Ink.penPlain pens) width $
      Ink.spanInk
        width
        [ \left -> Ink.inkT left (Ink.penPlain pens) "  "
        , \left -> Ink.inkT left (Ink.penFor pens (markAttrFor mark)) (T.singleton mark)
        , \left -> Ink.inkT left (Ink.penPlain pens) (" :" <> Types.tshow (Types.rmRow rm + 1))
        , \left -> Ink.inkT left (Ink.penFor pens Attrs.trayDeclAttr) (" " <> T.take 12 (T.strip (Types.rmText rm)))
        , \left -> if noted then Ink.inkT left (Ink.penFor pens Attrs.noteInlineAttr) " \8801" else V.emptyImage
        ]
