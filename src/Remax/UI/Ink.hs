{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Ink
  ( PenSet (..)
  , penFor
  , penPlain
  , penUnder
  , inkT
  , fitRow
  , spanInk
  , segsInk
  , emphInk
  , selInk
  , splitAtCol
  , markerAttrFor
  , meterW
  , meterInk
  , meterTextWidth
  ) where

import Brick
  ( AttrMap
  , AttrName
  , Widget
  , attrMapLookup
  , hBox
  , setDefaultAttr
  , txt
  , withAttr
  )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Remax.Highlight
import Remax.UI.Attrs
import Remax.UI.Types
import Remax.UI.Widgets

-- The body is drawn as vty images rather than composed out of brick
-- widgets. Brick's boxes carry border-map bookkeeping through every
-- composition — merging, translating and cropping the border segments a
-- box might have to join — and a six-hundred-row window whose every row
-- was a dozen nested boxes paid for that thousands of times a frame, for
-- borders the body does not have. It was three quarters of the time
-- spent between a keystroke and the screen.
--
-- What a widget did at render time is done here instead, and the pens
-- are how: an attribute name resolves against exactly the map a withAttr
-- would have consulted, so the colors are the ones the widgets produced.
data PenSet = PenSet
  { penBaseName :: AttrName -- the ambient name, what plain text renders in
  , penAttrs :: AttrMap
  }

-- The attribute a withAttr of this name would have resolved to.
penFor :: PenSet -> AttrName -> V.Attr
penFor pens name = attrMapLookup name (penAttrs pens)

-- The attribute unnamed text renders in: the map's default, which is
-- what withDefAttr replaces.
penPlain :: PenSet -> V.Attr
penPlain pens = penFor pens (penBaseName pens)

-- withDefAttr: the named attribute becomes the default everything under
-- it inherits from, so a token that sets only a foreground picks up the
-- selection or the word-change wash behind it.
penUnder :: AttrName -> PenSet -> PenSet
penUnder name pens =
  pens {penAttrs = setDefaultAttr (penFor pens name) (penAttrs pens)}

-- Text clipped to a column budget, the way a nested box clipped it
-- against the width its siblings had left. Empty text draws nothing,
-- which is what brick's txt does with it.
inkT :: Int -> V.Attr -> Text -> V.Image
inkT width attr t =
  if width <= 0 || T.null t
    then V.emptyImage
    else
      let
        -- a column is never narrower than a character, so taking that
        -- many characters cannot come up short of the budget
        img = V.text' attr (T.take width t)
      in
        if V.imageWidth img > width
          then V.cropRight width img
          else img

-- A row filled out to the pane's width, or cropped back to it: what a
-- padRight Max did, and what the box around it did afterwards.
fitRow :: V.Attr -> Int -> V.Image -> V.Image
fitRow attr width img =
  let
    short = width - V.imageWidth img
  in
    if short > 0
      then V.horizJoin img (V.charFill attr ' ' short (1 :: Int))
      else
        if short < 0
          then V.cropRight width img
          else img

-- Pieces laid left to right against one shared budget, each offered only
-- what the pieces before it did not use.
spanInk :: Int -> [Int -> V.Image] -> V.Image
spanInk width pieces =
  let
    go _ [] = []
    go left (piece : rest) =
      if left <= 0
        then []
        else
          let
            img = piece left
          in
            img : go (left - V.imageWidth img) rest
  in
    V.horizCat (go width pieces)

-- One run of segments, each drawn in the attribute its name resolves to.
segsInk :: PenSet -> Int -> [Seg] -> V.Image
segsInk pens width segs =
  spanInk width (map (\(a, t) left -> inkT left (penFor pens a) t) segs)

-- Runs of segments that read their attributes against different pens,
-- sharing one budget across the lot.
runsInk :: Int -> [(PenSet, [Seg])] -> V.Image
runsInk width parts =
  spanInk width (map (\(pens, segs) left -> segsInk pens left segs) parts)

-- Segments with the given columns wearing the word-change wash. The wash
-- sets only a background, so a token keeps whatever colour the grammar
-- gave it and gains the mark on top.
emphInk :: PenSet -> Int -> [(Int, Int)] -> [Seg] -> V.Image
emphInk pens width spans segs =
  let
    washed = penUnder wordChangeAttr pens
    go _ [] rest = [(pens, rest)]
    go col ((s, e) : more) rest =
      let
        (before, fromStart) = splitAtCol (s - col) rest
        (mid, after) = splitAtCol (e + 1 - s) fromStart
      in
        (pens, before) : (washed, mid) : go (e + 1) more after
  in
    runsInk width (go 0 spans segs)

-- Token runs with the char selection's columns highlighted.
selInk :: PenSet -> Int -> [Seg] -> Int -> Maybe Int -> V.Image
selInk pens width segs s mEnd =
  let
    (before, rest) = splitAtCol s segs
    (mid, after) = case mEnd of
      Nothing -> (rest, [])
      Just e -> splitAtCol (e + 1 - s) rest
    selPens = penUnder selAttr pens
    midSegs = case nonEmptySegs mid of
      -- keep the selection visible across empty stretches
      [] -> [(penBaseName pens, " ")]
      ms -> ms
  in
    runsInk width [(pens, before), (selPens, midSegs), (pens, after)]

splitAtCol :: Int -> [Seg] -> ([Seg], [Seg])
splitAtCol =
  let
    go k xs =
      case xs of
        [] -> ([], [])
        ((a, t) : rest)
          | k <= 0 -> ([], xs)
          | T.length t <= k ->
              let
                (taken, left) = go (k - T.length t) rest
              in
                ((a, t) : taken, left)
          | otherwise -> ([(a, T.take k t)], (a, T.drop k t) : rest)
  in
    go

markerAttrFor :: Char -> AttrName
markerAttrFor mark =
  case mark of
    '+' -> addedMarkAttr
    '-' -> removedMarkAttr
    _ -> ctxMarkAttr

-- Progress as quiet blocks: one cell per hunk up to ten, then scaled.
-- The filled run only completes when the work does.
-- How many cells the meter has, and how many of them are filled. The
-- filled run only completes when the work does.
meterCells :: Int -> Int -> (Int, Int)
meterCells done total =
  let
    cells = min 10 (max 1 total)
    filled =
      if done >= total
        then cells
        else min (cells - 1) (div (done * cells) (max 1 total))
  in
    (cells, filled)

meterW :: Int -> Int -> Widget Name
meterW done total =
  let
    (cells, filled) = meterCells done total
  in
    hBox
      [ withAttr meterOnAttr (txt (T.replicate filled "\9648"))
      , withAttr meterOffAttr (txt (T.replicate (cells - filled) "\9649"))
      , withAttr hintAttr (txt (" " <> tshow done <> "/" <> tshow total))
      ]

meterInk :: PenSet -> Int -> Int -> Int -> V.Image
meterInk pens width done total =
  let
    (cells, filled) = meterCells done total
  in
    spanInk
      width
      [ \left -> inkT left (penFor pens meterOnAttr) (T.replicate filled "\9648")
      , \left -> inkT left (penFor pens meterOffAttr) (T.replicate (cells - filled) "\9649")
      , \left ->
          inkT left (penFor pens hintAttr) (" " <> tshow done <> "/" <> tshow total)
      ]

meterTextWidth :: Int -> Int -> Int
meterTextWidth done total =
  min 10 (max 1 total)
    + 2
    + T.length (tshow done)
    + T.length (tshow total)

-- Events --------------------------------------------------------------------------
