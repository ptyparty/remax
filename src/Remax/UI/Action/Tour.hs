{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Tour
  ( openTourOverview
  , handleTourOverviewKey
  , playTour
  , stopPlay
  ) where

import Brick qualified as Brick
import Brick.BChan qualified as BChan
import Control.Applicative ((<|>))
import Control.Concurrent qualified as Concurrent
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import GHC.Clock qualified as GHCClock
import Graphics.Vty qualified as V
import Remax.DiffModel qualified as DiffModel
import Remax.Store qualified as Store
import Remax.UI.Action.Verdicts qualified as VerdictAction
import Remax.UI.Core.Tour qualified as Tour
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Verdict qualified as Verdict
import Remax.UI.Model.Nav qualified as Nav

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
                  , Types.toLeftOut = maybe "" (\lo -> " \183 " <> Tour.leftOutText lo) (Tour.tourLeftOut st)
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

playTour :: Types.PlayPace -> Brick.EventM Types.Name Types.St ()
playTour pace = do
  st <- State.get
  stamp <- MonadIO.liftIO GHCClock.getMonotonicTimeNSec
  let
    ordered = Tour.tourOrder st
    here = (Types.stCurF st, Types.clampH (Types.fvRuns (Types.currentFile st)) (Types.stCurH st))
    fromHere = maybe ordered (`drop` ordered) (List.elemIndex here ordered)
    ahead = filter (Verdict.pairUnvisited st) fromHere
  case (Tour.tourAbsence st, ahead) of
    (Just why, _) -> Types.setMsg why
    (Nothing, []) -> Types.setMsg "every hunk on the tour from here has a verdict \8212 nothing to play"
    (Nothing, first : rest) -> do
      let
        playing =
          Types.TourPlay
            { Types.tpId = fromIntegral stamp
            , Types.tpAhead = Maybe.mapMaybe (hunkKeyAt st) rest
            , Types.tpPlayed = 0
            , Types.tpTotal = length ahead
            , Types.tpPace = pace
            }
      landAndWait playing first

landAndWait :: Types.TourPlay -> (Int, Int) -> Brick.EventM Types.Name Types.St ()
landAndWait playing pair = do
  let
    played = playing {Types.tpPlayed = Types.tpPlayed playing + 1}
  State.modify (\s -> s {Types.stTourPlay = Just played})
  Nav.landOnTour (playingText played) pair
  st <- State.get
  let
    chan = Types.stChan st
    lineCount = maybe 1 (length . DiffModel.runKeys) (Verdict.runAt st pair)
    seconds = case Types.tpPace played of
      Types.PaceFixed n -> max 1 n
      Types.PaceBySize -> min 30 (3 + lineCount `div` 2)
    wake = do
      Concurrent.threadDelay (seconds * 1000000)
      BChan.writeBChan chan (Types.FromJob (playNext (Types.tpId played)))
  Monad.void (MonadIO.liftIO (Concurrent.forkIO wake))

playingText :: Types.TourPlay -> T.Text
playingText playing =
  "\9654 playing the tour \183 hunk "
    <> Types.tshow (Types.tpPlayed playing)
    <> " of "
    <> Types.tshow (Types.tpTotal playing)
    <> " \183 Esc or q stops"

playNext :: Int -> Brick.EventM Types.Name Types.St ()
playNext wanted = do
  st <- State.get
  case Types.stTourPlay st of
    Just playing
      | Types.tpId playing == wanted ->
          case nextUnvisited st (Types.tpAhead playing) of
            Nothing -> do
              State.modify (\s -> s {Types.stTourPlay = Nothing})
              Types.setMsg "\9632 played the tour to its end"
            Just (pair, rest) -> landAndWait playing {Types.tpAhead = rest} pair
    _ -> pure ()

hunkKeyAt :: Types.St -> (Int, Int) -> Maybe T.Text
hunkKeyAt st pair = Verdict.runAt st pair >>= Maybe.listToMaybe . DiffModel.runKeys

nextUnvisited :: Types.St -> [T.Text] -> Maybe ((Int, Int), [T.Text])
nextUnvisited st keys =
  let
    places = Map.fromList (concatMap (\(pair, r) -> map (\k -> (k, pair)) (DiffModel.runKeys r)) (Verdict.allRunsInOrder st))
    go remaining =
      case remaining of
        [] -> Nothing
        (k : rest) ->
          case Map.lookup k places of
            Just pair | Verdict.pairUnvisited st pair -> Just (pair, rest)
            _ -> go rest
  in
    go keys

stopPlay :: Brick.EventM Types.Name Types.St ()
stopPlay = do
  st <- State.get
  case Types.stTourPlay st of
    Nothing -> Types.setMsg "the tour is not playing"
    Just playing -> do
      State.modify (\s -> s {Types.stTourPlay = Nothing})
      Types.setMsg
        ( "\9632 stopped playing the tour after "
            <> Types.tshow (Types.tpPlayed playing)
            <> " of "
            <> Types.tshow (Types.tpTotal playing)
            <> " hunks \183 :tour play goes on from here"
        )

goTo :: Maybe Types.TourStop -> Brick.EventM Types.Name Types.St ()
goTo chosen =
  case chosen of
    Nothing -> pure ()
    Just stop
      | null (Types.tsPlaces stop) -> Types.setMsg ("stop " <> Types.tshow (Types.tsNumber stop) <> "'s hunks are not in this view")
      | otherwise -> do
          closeOverview
          VerdictAction.gotoTourStep (Types.tsNumber stop)
