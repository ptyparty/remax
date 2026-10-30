{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Tour
  ( tourOverviewLayer
  ) where

import Brick qualified as Brick
import Brick.Types qualified as BrickTypes
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.Config qualified as Config
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Widgets qualified as Widgets

tourOverviewLayer :: Types.St -> Types.TourOverview -> BrickTypes.Widget Types.Name
tourOverviewLayer st overview =
  BrickTypes.Widget BrickTypes.Greedy BrickTypes.Greedy $ do
    ctx <- BrickTypes.getContext
    let
      stops = Types.toStops overview
      chosen = Types.safeIdx stops (Types.toSel overview)
      boxWidth = max 60 (BrickTypes.availWidth ctx - 10)
      bodyRows = max 6 (BrickTypes.availHeight ctx - 10)
      listWidth = boxWidth * 2 `div` 5
      previewWidth = boxWidth - listWidth - 3
      title = "tour \183 " <> Types.tshow (length stops) <> " stops"
      body =
        Brick.hBox
          [ Brick.hLimit listWidth (Brick.vLimit bodyRows (listColumn overview listWidth bodyRows))
          , Brick.withAttr Attrs.hintAttr (Brick.hLimit 1 (Brick.vLimit bodyRows (Brick.fill '\9474')))
          , Brick.txt " "
          , Brick.hLimit previewWidth (Brick.vLimit bodyRows (previewColumn overview chosen))
          ]
    BrickTypes.render (Widgets.floatFrame title (hint (Config.cfgKeys (Types.stConfig st))) (Brick.hLimit boxWidth body))

listColumn :: Types.TourOverview -> Int -> Int -> BrickTypes.Widget Types.Name
listColumn overview width rows =
  let
    at = Types.toSel overview
    start = if at < rows then 0 else at - rows + 1
    shown = take rows (drop start (zip [0 ..] (Types.toStops overview)))
  in
    Brick.padRight Brick.Max (Brick.vBox (map (\(i, stop) -> stopRow overview width (i == at) stop) shown))

stopRow :: Types.TourOverview -> Int -> Bool -> Types.TourStop -> BrickTypes.Widget Types.Name
stopRow overview width isChosen stop =
  let
    marker = if Types.toHere overview == Just (Types.tsNumber stop) then "\9656 " else "  "
    line = marker <> Types.tshow (Types.tsNumber stop) <> ". " <> stopHeadline stop
    attr
      | isChosen = Attrs.floatSelAttr
      | null (Types.tsPlaces stop) = Attrs.hintAttr
      | otherwise = Attrs.msgAttr
  in
    Brick.withAttr attr (Brick.txt (Widgets.padT (Widgets.clipT width line)))

stopHeadline :: Types.TourStop -> T.Text
stopHeadline stop =
  case Types.tsTitle stop of
    Just t | not (T.null (T.strip t)) -> t
    _ -> Maybe.fromMaybe "" (Maybe.listToMaybe (T.lines (Types.tsNote stop)))

previewColumn :: Types.TourOverview -> Maybe Types.TourStop -> BrickTypes.Widget Types.Name
previewColumn overview chosen =
  case chosen of
    Nothing -> Brick.txt " "
    Just stop ->
      let
        heading = Brick.withAttr Attrs.guideAttr (Brick.txtWrap (Widgets.padT (stopHeading stop)))
        note = map (Brick.txtWrap . Widgets.padT) (T.lines (Types.tsNote stop))
        places = case Types.tsPlaces stop of
          [] -> [Brick.withAttr Attrs.hintAttr (Brick.txt "none of its hunks are in this view")]
          ps -> map (Brick.withAttr Attrs.hintAttr . Brick.txt . placeText) ps
        rows = [heading, Brick.txt " "] ++ note ++ [Brick.txt " "] ++ places
      in
        Brick.padBottom Brick.Max (Brick.vBox (drop (Types.toScroll overview) rows))

stopHeading :: Types.TourStop -> T.Text
stopHeading stop =
  "stop " <> Types.tshow (Types.tsNumber stop) <> maybe "" ("  \183  " <>) (Types.tsTitle stop)

placeText :: (T.Text, Int) -> T.Text
placeText (path, n) =
  path <> if n > 1 then "  \183  " <> Types.tshow n <> " hunks" else ""

hint :: Config.KeyMode -> T.Text
hint keys =
  case keys of
    Config.KeysEmacs -> "C-n/C-p moves \183 \9166 goes to the stop \183 C-d/C-u scrolls \183 C-c C-k closes"
    _ -> "j/k moves \183 \9166 goes to the stop \183 C-d/C-u scrolls \183 Esc closes"
