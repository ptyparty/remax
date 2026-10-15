{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Model.Tray
  ( TrayRow (..)
  , trayRows
  , trayWidth
  , trayCurrentRow
  ) where

import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.DiffModel qualified as DiffModel
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Verdict qualified as Verdict

-- A tray row together with what it needs to draw itself.
--
-- The payload travels with the row because looking it up afterwards
-- means counting into the file's list of runs, once per row — and the
-- tray holds a row per hunk in the whole change set, so that is
-- quadratic in the size of the change set on every frame. A single file
-- with a few thousand hunks put seconds between a keystroke and the
-- screen; see allRunsInOrder for the other half of the same mistake.
data TrayRow
  = TrayRowFile Int Int Types.FileView -- file index, hidden hunks, the file
  | TrayRowHunk Int Int DiffModel.Run T.Text -- file, hunk, the run, its declaration
  | TrayRowRemark Int Int Types.Remark -- file, index among that file's remarks

-- Which row this is, for the places that only need to point at one.
trayEntryOf :: TrayRow -> Types.TrayEntry
trayEntryOf row = case row of
  TrayRowFile fi hidden _ -> Types.TrayFile fi hidden
  TrayRowHunk fi hi _ _ -> Types.TrayHunk fi hi
  TrayRowRemark fi ri _ -> Types.TrayRemark fi ri

trayEntries :: Types.St -> [Types.TrayEntry]
trayEntries = map trayEntryOf . trayRows

trayRows :: Types.St -> [TrayRow]
trayRows st =
  let
    curF = Types.stCurF st
    curH = Types.clampH (Types.fvRuns (Types.currentFile st)) (Types.stCurH st)
    hiddenHunk fi (hi, r, _) =
      let
        isCurrent = fi == curF && hi == curH
        skipped = not (Verdict.walkAllows st r)
      in
        not isCurrent && skipped
    fileGroup (fi, fv) =
      let
        -- the declarations are a list parallel to the runs, and a file
        -- whose decls have not been worked out yet simply has none
        numbered =
          zip3 [0 ..] (Types.fvRuns fv) (Types.fvRunDecls fv ++ repeat T.empty)
        (hiddenOnes, shown) = splitHiddenShown fi numbered
      in
        TrayRowFile fi (length hiddenOnes) fv
          : map (\(hi, r, decl) -> TrayRowHunk fi hi r decl) shown
          ++ zipWith (TrayRowRemark fi) [0 ..] (Types.fvRemarks fv)
    splitHiddenShown fi numbered =
      ( filter (hiddenHunk fi) numbered
      , filter (not . hiddenHunk fi) numbered
      )
  in
    concatMap fileGroup (zip [0 ..] (Types.stFiles st))

trayWidth :: Int
trayWidth = 32

-- Which tray row the hunk under review is on. Where the tray is pointed
-- when it follows, and — because that is where it will be pointed — part
-- of deciding which rows are worth drawing.
trayCurrentRow :: Types.St -> Int
trayCurrentRow st =
  case Types.currentFileMay st of
    Nothing -> 0
    Just fv ->
      let
        h = Types.clampH (Types.fvRuns fv) (Types.stCurH st)
        isCur entry = case entry of
          Types.TrayHunk f hh -> f == Types.stCurF st && hh == h
          _ -> False
      in
        Maybe.fromMaybe 0 (List.findIndex isCur (trayEntries st))
