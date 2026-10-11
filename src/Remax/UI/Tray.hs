{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Tray
  ( TrayRow (..)
  , trayRows
  , trayWidth
  , trayCurrentRow
  ) where

import Data.List
  ( findIndex
  )
import Data.Maybe
  ( fromMaybe
  )
import Data.Text (Text)
import qualified Data.Text as T
import Remax.DiffModel
import Remax.UI.Types
import Remax.UI.Verdict

-- A tray row together with what it needs to draw itself.
--
-- The payload travels with the row because looking it up afterwards
-- means counting into the file's list of runs, once per row — and the
-- tray holds a row per hunk in the whole change set, so that is
-- quadratic in the size of the change set on every frame. A single file
-- with a few thousand hunks put seconds between a keystroke and the
-- screen; see allRunsInOrder for the other half of the same mistake.
data TrayRow
  = TrayRowFile Int Int FileView -- file index, hidden hunks, the file
  | TrayRowHunk Int Int Run Text -- file, hunk, the run, its declaration
  | TrayRowRemark Int Int Remark -- file, index among that file's remarks

-- Which row this is, for the places that only need to point at one.
trayEntryOf :: TrayRow -> TrayEntry
trayEntryOf row = case row of
  TrayRowFile fi hidden _ -> TrayFile fi hidden
  TrayRowHunk fi hi _ _ -> TrayHunk fi hi
  TrayRowRemark fi ri _ -> TrayRemark fi ri

trayEntries :: St -> [TrayEntry]
trayEntries = map trayEntryOf . trayRows

trayRows :: St -> [TrayRow]
trayRows st =
  let
    curF = stCurF st
    curH = clampH (fvRuns (currentFile st)) (stCurH st)
    hiddenHunk fi (hi, r, _) =
      let
        mark = runMark (stVerdicts st) r
        isCurrent = fi == curF && hi == curH
        settled = stOnlyPending st && mark == '\10003'
        wrongKind = not (runFilterAllows (stRunFilter st) r)
      in
        not isCurrent && (settled || wrongKind)
    fileGroup (fi, fv) =
      let
        -- the declarations are a list parallel to the runs, and a file
        -- whose decls have not been worked out yet simply has none
        numbered =
          zip3 [0 ..] (fvRuns fv) (fvRunDecls fv ++ repeat T.empty)
        (hiddenOnes, shown) = splitHiddenShown fi numbered
      in
        TrayRowFile fi (length hiddenOnes) fv
          : map (\(hi, r, decl) -> TrayRowHunk fi hi r decl) shown
          ++ zipWith (TrayRowRemark fi) [0 ..] (fvRemarks fv)
    splitHiddenShown fi numbered =
      ( filter (hiddenHunk fi) numbered
      , filter (not . hiddenHunk fi) numbered
      )
  in
    concatMap fileGroup (zip [0 ..] (stFiles st))

trayWidth :: Int
trayWidth = 32

-- Which tray row the hunk under review is on. Where the tray is pointed
-- when it follows, and — because that is where it will be pointed — part
-- of deciding which rows are worth drawing.
trayCurrentRow :: St -> Int
trayCurrentRow st =
  case currentFileMay st of
    Nothing -> 0
    Just fv ->
      let
        h = clampH (fvRuns fv) (stCurH st)
        isCur entry = case entry of
          TrayHunk f hh -> f == stCurF st && hh == h
          _ -> False
      in
        fromMaybe 0 (findIndex isCur (trayEntries st))
