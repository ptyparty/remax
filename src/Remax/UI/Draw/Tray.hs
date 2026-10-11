{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Tray
  ( tray
  ) where

import Brick
  ( AttrName
  , Padding (Pad)
  , ViewportType (Vertical)
  , Widget
  , hLimit
  , padRight
  , raw
  , viewport
  )
import Brick.Types
  ( Size (Fixed, Greedy)
  , Widget (Widget)
  , availWidth
  , ctxAttrMap
  , ctxAttrName
  , getContext
  , render
  )
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Remax.DiffModel
import Remax.Store
import Remax.UI.Attrs
import Remax.UI.Ink
import Remax.UI.Tray
import Remax.UI.Types
import Remax.UI.Verdict
import Remax.UI.View
import Remax.UI.Widgets

-- The tray is drawn as images for the same reason the body is: it holds
-- a row per hunk in the whole change set, and every one of them was a
-- box of boxes redrawn on each keystroke. See PenSet.
tray :: St -> Widget Name
tray st =
  hLimit trayWidth $
    padRight (Pad 1) $
      viewport TrayVP Vertical $
        Widget Greedy Fixed $ do
          ctx <- getContext
          let
            pens = PenSet (ctxAttrName ctx) (ctxAttrMap ctx)
            width = availWidth ctx
            rows = trayRows st
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
          render . raw . V.vertCat $
            [blank lo] ++ band ++ [blank (total - hi)]

-- The rows worth drawing: a band around what the viewport showed last
-- frame and the hunk under review.
trayBand :: St -> Int -> (Int, Int)
trayBand st total =
  let
    (seenTop, seenHeight) = stTraySeen st
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
    focus = trayCurrentRow st
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

trayEntryImage :: St -> PenSet -> Int -> TrayRow -> V.Image
trayEntryImage st pens width entry =
  case entry of
    TrayRowFile fi _hidden fv -> trayFileImage st pens width fi fv
    TrayRowHunk fi hi r rawDecl -> trayHunkImage st pens width fi hi r rawDecl
    TrayRowRemark _fi _ri rm -> trayRemarkImage st pens width rm

markAttrFor :: Char -> AttrName
markAttrFor mark =
  case mark of
    '!' -> flagMarkAttr
    '?' -> questionMarkAttr
    '\10003' -> doneMarkAttr
    _ -> pendingMarkAttr

trayFileImage :: St -> PenSet -> Int -> Int -> FileView -> V.Image
trayFileImage st pens width fi fv =
  let
    total = length (fvRuns fv)
    done = length (filter (not . runPending (stVerdicts st)) (fvRuns fv))
    meterCols = meterTextWidth done total
    name = shortPath (trayWidth - 3 - meterCols) (T.pack (fvPath fv))
    nameAttr = if fi == stCurF st then trayFileCurAttr else trayFileAttr
    nameImg = inkT width (penFor pens nameAttr) (padT name)
    endImg = inkT (width - V.imageWidth nameImg) (penPlain pens) " "
    -- the meter is pushed to the right of what the name leaves,
    -- with the gap in front of it rather than behind
    gap = width - V.imageWidth nameImg - V.imageWidth endImg
    meterImg = meterInk pens gap done total
  in
    fitRow (penPlain pens) width $
      V.horizCat
        [ nameImg
        , V.charFill (penPlain pens) ' ' (max 0 (gap - V.imageWidth meterImg)) (1 :: Int)
        , meterImg
        , endImg
        ]

trayHunkImage :: St -> PenSet -> Int -> Int -> Int -> Run -> Text -> V.Image
trayHunkImage st pens width fi hi r rawDecl =
  let
    -- asked once for the frame, not once per row: clampH measures the
    -- run list, and a row per hunk asking how many hunks there are is
    -- the change set squared
    curHunk = clampH (fvRuns (currentFile st)) (stCurH st)
    decl =
      if T.null rawDecl
        then ""
        else " " <> T.take 12 (declName rawDecl)
    current = fi == stCurF st && hi == curHunk
    -- the current row reads every attribute against the selection
    -- as its default, so the highlight runs the width of the tray
    rowPens = if current then penUnder floatSelAttr pens else pens
    mark = runMark (stVerdicts st) r
    noted = any (`Map.member` stThreads st) (runKeys r)
  in
    fitRow (penPlain rowPens) width $
      spanInk
        width
        [ \left ->
            if current
              then inkT left (penFor rowPens floatSelBarAttr) "\9614 "
              else inkT left (penPlain rowPens) "  "
        , \left -> inkT left (penFor rowPens (markAttrFor mark)) (T.singleton mark)
        , \left -> inkT left (penPlain rowPens) (" :" <> tshow (runAnchor r + 1))
        , \left -> inkT left (penFor rowPens trayDeclAttr) decl
        , \left -> if noted then inkT left (penFor rowPens noteInlineAttr) " \8801" else V.emptyImage
        ]

-- A remark sits between the hunks rather than among them: it is
-- about a line the change never touched, so it carries the line
-- itself where a hunk carries its declaration.
trayRemarkImage :: St -> PenSet -> Int -> Remark -> V.Image
trayRemarkImage st pens width rm =
  let
    mark = case Map.lookup (rmKey rm) (stVerdicts st) of
      Just Flagged -> '!'
      Just Questioned -> '?'
      Just Reviewed -> '\10003'
      Nothing -> '\183'
    noted = Map.member (rmKey rm) (stThreads st)
  in
    fitRow (penPlain pens) width $
      spanInk
        width
        [ \left -> inkT left (penPlain pens) "  "
        , \left -> inkT left (penFor pens (markAttrFor mark)) (T.singleton mark)
        , \left -> inkT left (penPlain pens) (" :" <> tshow (rmRow rm + 1))
        , \left -> inkT left (penFor pens trayDeclAttr) (" " <> T.take 12 (T.strip (rmText rm)))
        , \left -> if noted then inkT left (penFor pens noteInlineAttr) " \8801" else V.emptyImage
        ]
