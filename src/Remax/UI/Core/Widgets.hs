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

import Brick
  ( Location (..)
  , Padding (Max, Pad)
  , Widget
  , fill
  , hLimit
  , image
  , padLeft
  , padRight
  , padTop
  , raw
  , translateBy
  , txt
  , txtWrap
  , vBox
  , vLimit
  , withAttr
  , withBorderStyle
  , withDefAttr
  )
import Brick.Types
  ( Size (Fixed, Greedy)
  , Widget (Widget)
  , availHeight
  , availWidth
  , getContext
  , render
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Data.Maybe
  ( fromMaybe
  )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Remax.Highlight
import Remax.UI.Core.Attrs
import Remax.UI.Core.Types

-- A quiet full-width rule; the chrome's only horizontal structure now
-- that the reverse-video slabs are gone.
hairline :: Widget Name
hairline = vLimit 1 (withAttr hairAttr (fill '\9472'))

-- Shared float chrome: rounded frame with the title in the border, an
-- elevated surface, inner breathing room, and a dim hint as the last
-- interior row.
-- The bordered box every popup is drawn in, without saying where it goes.
frameBox :: Text -> Text -> Widget Name -> Widget Name
frameBox title hint body =
  withDefAttr floatAttr
    . withBorderStyle BS.unicodeRounded
    . B.borderWithLabel (withAttr floatTitleAttr (txt (" " <> title <> " ")))
    . padTop (Pad 1)
    . padLeft (Pad 2)
    . padRight (Pad 2)
    $ vBox
      [ body
      , padTop (Pad 1) (withAttr hintAttr (txt hint))
      ]

floatFrame :: Text -> Text -> Widget Name -> Widget Name
floatFrame title hint = C.centerLayer . frameBox title hint

-- Where the key reminders go: down in the corner, out of the way of the
-- line being read. They answer "what can I press now", which is worth
-- glancing at and never worth reading over the top of the code — and
-- they all go to the same corner, so the eye learns one place.
cornerFrame :: Text -> Text -> Widget Name -> Widget Name
cornerFrame title hint body =
  -- Placed by translation rather than by padding it into the corner.
  -- Max padding is not empty space: it paints its own background, so a
  -- box pushed down there takes the status line with it.
  Widget Fixed Fixed $ do
    ctx <- getContext
    box <- render (frameBox title hint body)
    let
      img = image box
      -- one column in from the right, and one row up from the status line
      x = max 0 (availWidth ctx - V.imageWidth img - 1)
      y = max 0 (availHeight ctx - V.imageHeight img - 1)
    render (translateBy (Location (x, y)) (raw img))

-- A read-only text popup (full commit messages and kin); any key closes.
infoPopupLayer :: Text -> Text -> Widget Name
infoPopupLayer title body =
  Widget Greedy Greedy $ do
    ctx <- getContext
    let
      boxWidth = min 100 (max 50 (availWidth ctx - 20))
      bodyRows = max 4 (availHeight ctx - 14)
    render $
      floatFrame title "any key closes" $
        hLimit boxWidth (padRight Max (vLimit bodyRows (txtWrap (padT body))))

-- Long paths compress like s/R/Quote.hs: every directory shrinks to its
-- first character, the file name stays whole, and if even that is too
-- wide the front is elided so the end — the part that tells files apart —
-- always survives.
shortPath :: Int -> Text -> Text
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

nonEmptySegs :: [Seg] -> [Seg]
nonEmptySegs = filter (not . T.null . snd)

-- Waiting on you: a slow breath, four frames to a cycle.
throbGlyph :: Int -> Text
throbGlyph n =
  case n `mod` 4 of
    0 -> "\183" -- ·
    1 -> "\8226" -- •
    2 -> "\9679" -- ●
    _ -> "\8226"

-- Busy on your behalf: two frames, so it reads as quicker than the
-- breath even though both are redrawn on the same beat.
busyGlyph :: Int -> Text
busyGlyph n =
  if even n
    then "\10022" -- ✦
    else "\10023" -- ✧

padT :: Text -> Text
padT t = if T.null t then " " else t

clipT :: Int -> Text -> Text
clipT w t = if T.length t <= w then t else T.take (max 1 (w - 1)) t <> "\8230"

-- Lay cells out in real columns: each leading column as wide as its
-- widest cell up to a cap, anything longer clipped, so one long branch
-- name or ref decoration can never shove its neighbours out of line.
-- The final cell runs free.
columnRow :: [Int] -> [Text] -> Text
columnRow widths cells =
  let
    clip w t = if T.length t <= w then t else T.take (max 1 (w - 1)) t <> "\8230"
    lead = zipWith (\w c -> T.justifyLeft (w + 2) ' ' (clip w c)) widths cells
    rest = drop (length widths) cells
  in
    T.concat lead <> T.intercalate " " rest

-- Column widths measured from the data, each capped so a single outlier
-- cannot stretch the whole table.
columnWidths :: [Int] -> [[Text]] -> [Int]
columnWidths caps rows =
  let
    at i r = fromMaybe "" (safeIdx r i)
    widest i = maximum (1 : map (T.length . at i) rows)
  in
    zipWith (\cap i -> min cap (widest i)) caps [0 ..]
