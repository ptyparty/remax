{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Tour
  ( openTourOverview
  , handleTourOverviewKey
  ) where

import Brick qualified as Brick
import Control.Applicative ((<|>))
import Control.Monad.State qualified as State
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Remax.Store qualified as Store
import Remax.UI.Action.Verdicts qualified as VerdictAction
import Remax.UI.Core.Tour qualified as Tour
import Remax.UI.Core.Types qualified as Types

openTourOverview :: Brick.EventM Types.Name Types.St ()
openTourOverview = do
  st <- State.get
  case Types.stTour st of
    [] -> Types.setMsg "no tour recorded for this view \8212 :tour ask requests one"
    steps -> do
      let
        groups = Tour.stepGroups st
        here = currentStop st
        stops = zipWith3 (stopOf st) [1 ..] steps groups
      State.modify $ \s ->
        s
          { Types.stTourOverview =
              Just
                Types.TourOverview
                  { Types.toStops = stops
                  , Types.toSel = maybe 0 (subtract 1) here
                  , Types.toHere = here
                  , Types.toScroll = 0
                  }
          }

stopOf :: Types.St -> Int -> Store.TourStep -> [(Int, Int)] -> Types.TourStop
stopOf st n step grp =
  let
    pathOf f = maybe "?" (T.pack . Types.fvPath) (Types.safeIdx (Types.stFiles st) f)
    places = map (\fs -> (pathOf (NEL.head fs), length fs)) (NEL.group (map fst grp))
  in
    Types.TourStop
      { Types.tsNumber = n
      , Types.tsTitle = Store.tourTitle step
      , Types.tsNote = Store.tourNote step
      , Types.tsPlaces = places
      }

currentStop :: Types.St -> Maybe Int
currentStop st =
  let
    cursor = do
      fv <- Types.currentFileMay st
      Just (Types.stCurF st, Types.clampH (Types.fvRuns fv) (Types.stCurH st))
    stopAt pair = do
      (i, _, _) <- Tour.stepPosition st pair
      Just (i + 1)
  in
    (cursor >>= stopAt) <|> (Types.stTourLast st >>= stopAt)

handleTourOverviewKey :: V.Event -> Types.TourOverview -> Brick.EventM Types.Name Types.St ()
handleTourOverviewKey ev overview = do
  chord <- Types.readChord ev
  case chord of
    Types.CancelKey -> closeOverview
    Types.PrefixStarted -> pure ()
    Types.PrefixedKey _ -> Types.setMsg "C-c C-k closes"
    Types.PlainKey _ -> listKey ev overview

setOverview :: Types.TourOverview -> Brick.EventM Types.Name Types.St ()
setOverview overview = State.modify (\s -> s {Types.stTourOverview = Just overview})

closeOverview :: Brick.EventM Types.Name Types.St ()
closeOverview = State.modify (\s -> s {Types.stTourOverview = Nothing})

listKey :: V.Event -> Types.TourOverview -> Brick.EventM Types.Name Types.St ()
listKey ev overview =
  let
    stops = Types.toStops overview
    at = Types.toSel overview
    moveTo i = setOverview overview {Types.toSel = max 0 (min (length stops - 1) i), Types.toScroll = 0}
    scrollBy d = setOverview overview {Types.toScroll = max 0 (Types.toScroll overview + d)}
  in
    case ev of
      V.EvKey (V.KChar 'j') [] -> moveTo (at + 1)
      V.EvKey V.KDown [] -> moveTo (at + 1)
      V.EvKey (V.KChar 'n') [V.MCtrl] -> moveTo (at + 1)
      V.EvKey (V.KChar 'k') [] -> moveTo (at - 1)
      V.EvKey V.KUp [] -> moveTo (at - 1)
      V.EvKey (V.KChar 'p') [V.MCtrl] -> moveTo (at - 1)
      V.EvKey (V.KChar 'g') [] -> moveTo 0
      V.EvKey (V.KChar 'G') [] -> moveTo (length stops - 1)
      V.EvKey (V.KChar 'd') [V.MCtrl] -> scrollBy 10
      V.EvKey (V.KChar 'u') [V.MCtrl] -> scrollBy (-10)
      V.EvKey V.KEnter [] -> goTo (Types.safeIdx stops at)
      V.EvKey (V.KChar 'q') [] -> closeOverview
      V.EvKey V.KEsc [] -> closeOverview
      _ -> pure ()

goTo :: Maybe Types.TourStop -> Brick.EventM Types.Name Types.St ()
goTo chosen =
  case chosen of
    Nothing -> pure ()
    Just stop
      | null (Types.tsPlaces stop) -> Types.setMsg ("stop " <> Types.tshow (Types.tsNumber stop) <> "'s hunks are not in this view")
      | otherwise -> do
          closeOverview
          VerdictAction.gotoTourStep (Types.tsNumber stop)
