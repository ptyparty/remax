{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Core.Widgets
  ( hairline
  , floatFrame
  , cornerFrame
  , infoPopupLayer
  , shortPath
  , nonEmptySegs
  , throbGlyph
  , busyGlyph
  , padT
  , clipT
  , columnRow
  , columnWidths
  ) where

import Brick qualified as Brick
import Brick.Types qualified as BrickTypes
import Brick.Widgets.Border qualified as Border
import Brick.Widgets.Border.Style qualified as BorderStyle
import Brick.Widgets.Center qualified as Center
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Remax.Highlight qualified as Highlight
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Types qualified as Types

-- A quiet full-width rule; the chrome's only horizontal structure now
-- that the reverse-video slabs are gone.
hairline :: BrickTypes.Widget Types.Name
hairline = Brick.vLimit 1 (Brick.withAttr Attrs.hairAttr (Brick.fill '\9472'))

-- Shared float chrome: rounded frame with the title in the border, an
-- elevated surface, inner breathing room, and a dim hint as the last
-- interior row.
-- The bordered box every popup is drawn in, without saying where it goes.
frameBox :: T.Text -> T.Text -> BrickTypes.Widget Types.Name -> BrickTypes.Widget Types.Name
frameBox title hint body =
  Brick.withDefAttr Attrs.floatAttr
    . Brick.withBorderStyle BorderStyle.unicodeRounded
    . Border.borderWithLabel (Brick.withAttr Attrs.floatTitleAttr (Brick.txt (" " <> title <> " ")))
    . Brick.padTop (Brick.Pad 1)
    . Brick.padLeft (Brick.Pad 2)
    . Brick.padRight (Brick.Pad 2)
    $ Brick.vBox
      [ body
      , Brick.padTop (Brick.Pad 1) (Brick.withAttr Attrs.hintAttr (Brick.txt hint))
      ]

floatFrame :: T.Text -> T.Text -> BrickTypes.Widget Types.Name -> BrickTypes.Widget Types.Name
floatFrame title hint = Center.centerLayer . frameBox title hint

-- Where the key reminders go: down in the corner, out of the way of the
-- line being read. They answer "what can I press now", which is worth
-- glancing at and never worth reading over the top of the code — and
-- they all go to the same corner, so the eye learns one place.
cornerFrame :: T.Text -> T.Text -> BrickTypes.Widget Types.Name -> BrickTypes.Widget Types.Name
cornerFrame title hint body =
  -- Placed by translation rather than by padding it into the corner.
  -- Max padding is not empty space: it paints its own background, so a
  -- box pushed down there takes the status line with it.
  BrickTypes.Widget BrickTypes.Fixed BrickTypes.Fixed $ do
    ctx <- BrickTypes.getContext
    box <- BrickTypes.render (frameBox title hint body)
    let
      img = Brick.image box
      -- one column in from the right, and one row up from the status line
      x = max 0 (BrickTypes.availWidth ctx - V.imageWidth img - 1)
      y = max 0 (BrickTypes.availHeight ctx - V.imageHeight img - 1)
    BrickTypes.render (Brick.translateBy (Brick.Location (x, y)) (Brick.raw img))

-- A read-only text popup (full commit messages and kin); any key closes.
infoPopupLayer :: T.Text -> T.Text -> BrickTypes.Widget Types.Name
infoPopupLayer title body =
  BrickTypes.Widget BrickTypes.Greedy BrickTypes.Greedy $ do
    ctx <- BrickTypes.getContext
    let
      boxWidth = min 100 (max 50 (BrickTypes.availWidth ctx - 20))
      bodyRows = max 4 (BrickTypes.availHeight ctx - 14)
    BrickTypes.render $
      floatFrame title "any key closes" $
        Brick.hLimit boxWidth (Brick.padRight Brick.Max (Brick.vLimit bodyRows (Brick.txtWrap (padT body))))

-- Long paths compress like s/R/Quote.hs: every directory shrinks to its
-- first character, the file name stays whole, and if even that is too
-- wide the front is elided so the end — the part that tells files apart —
-- always survives.
shortPath :: Int -> T.Text -> T.Text
shortPath w path =
  let
    parts = T.splitOn "/" path
    squeezed = case reverse parts of
      (file : dirs) ->
        T.intercalate "/" (reverse (file : map (T.take 1) dirs))
      [] -> path
  in
    if T.length path <= w
      then path
      else
        if T.length squeezed <= w
          then squeezed
          else "\8230" <> T.takeEnd (w - 1) squeezed

nonEmptySegs :: [Highlight.Seg] -> [Highlight.Seg]
nonEmptySegs = filter (not . T.null . snd)

-- Waiting on you: a slow breath, four frames to a cycle.
throbGlyph :: Int -> T.Text
throbGlyph n =
  case n `mod` 4 of
    0 -> "\183" -- ·
    1 -> "\8226" -- •
    2 -> "\9679" -- ●
    _ -> "\8226"

-- Busy on your behalf: two frames, so it reads as quicker than the
-- breath even though both are redrawn on the same beat.
busyGlyph :: Int -> T.Text
busyGlyph n =
  if even n
    then "\10022" -- ✦
    else "\10023" -- ✧

padT :: T.Text -> T.Text
padT t = if T.null t then " " else t

clipT :: Int -> T.Text -> T.Text
clipT w t = if T.length t <= w then t else T.take (max 1 (w - 1)) t <> "\8230"

-- Lay cells out in real columns: each leading column as wide as its
-- widest cell up to a cap, anything longer clipped, so one long branch
-- name or ref decoration can never shove its neighbours out of line.
-- The final cell runs free.
columnRow :: [Int] -> [T.Text] -> T.Text
columnRow widths cells =
  let
    clip w t = if T.length t <= w then t else T.take (max 1 (w - 1)) t <> "\8230"
    lead = zipWith (\w c -> T.justifyLeft (w + 2) ' ' (clip w c)) widths cells
    rest = drop (length widths) cells
  in
    T.concat lead <> T.intercalate " " rest

-- Column widths measured from the data, each capped so a single outlier
-- cannot stretch the whole table.
columnWidths :: [Int] -> [[T.Text]] -> [Int]
columnWidths caps rows =
  let
    at i r = Maybe.fromMaybe "" (Types.safeIdx r i)
    widest i = maximum (1 : map (T.length . at i) rows)
  in
    zipWith (\cap i -> min cap (widest i)) caps [0 ..]
