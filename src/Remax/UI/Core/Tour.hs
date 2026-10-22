{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Core.Tour
  ( loadTourFor
  , installTour
  , recordView
  , tourOrder
  , tourAbsence
  , stepGroups
  , stepPosition
  ) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Database.SQLite.Simple qualified as SQLite
import Remax.DiffModel qualified as DiffModel
import Remax.Git qualified as Git
import Remax.Store qualified as Store
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Verdict qualified as Verdict

loadTourFor :: SQLite.Connection -> FilePath -> Git.ReviewMode -> IO [Store.TourStep]
loadTourFor conn root mode = Store.loadTour conn (T.pack root) (Git.subjectText mode)

recordView :: SQLite.Connection -> FilePath -> Git.ReviewMode -> IO ()
recordView conn root mode = Store.recordView conn (T.pack root) (Git.modeToText mode)

installTour :: [Store.TourStep] -> Types.St -> Types.St
installTour steps st =
  let
    keyed step = map (\k -> (k, Store.tourSeq step)) (Store.tourKeys step)
    placed = st {Types.stTour = steps, Types.stTourKeys = Map.fromList (concatMap keyed steps)}
    post step = Store.Post {Store.postAuthor = Store.tourAuthor step, Store.postBody = Store.tourNote step}
    leadKeys step grp =
      case grp of
        (first : _) | Just r <- Verdict.runAt placed first -> DiffModel.runKeys r
        _ -> Store.tourKeys step
    noted = concat (zipWith (\step grp -> map (\k -> (k, [post step])) (leadKeys step grp)) steps (stepGroups placed))
  in
    placed {Types.stTourNotes = Map.fromListWith (flip (++)) noted}

stepGroups :: Types.St -> [[(Int, Int)]]
stepGroups st =
  let
    runs = Verdict.allRunsInOrder st
    claim seen step =
      let
        keys = Set.fromList (Store.tourKeys step)
        mine = [p | (p, r) <- runs, Set.notMember p seen, any (`Set.member` keys) (DiffModel.runKeys r)]
      in
        (Set.union seen (Set.fromList mine), mine)
  in
    snd (List.mapAccumL claim Set.empty (Types.stTour st))

tourOrder :: Types.St -> [(Int, Int)]
tourOrder = concat . stepGroups

tourAbsence :: Types.St -> Maybe T.Text
tourAbsence st
  | null (Types.stTour st) = Just "no tour recorded for this view"
  | null (tourOrder st) = Just "the tour's hunks are no longer in this view"
  | otherwise = Nothing

stepPosition :: Types.St -> (Int, Int) -> Maybe (Int, Int, Int)
stepPosition st here =
  Maybe.listToMaybe
    [ (i, j, length grp)
    | (i, grp) <- zip [0 ..] (stepGroups st)
    , Just j <- [List.elemIndex here grp]
    ]
