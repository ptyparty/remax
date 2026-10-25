module Remax.SoftWrap
  ( RowLayout (..)
  , RowEnd (..)
  , SoftRow (..)
  , softRows
  , cursorSoftRow
  , rowsFrom
  ) where

import Data.List qualified as List
import Data.Text qualified as T

data RowLayout
  = LineRows
  | WrappedRows Int

data RowEnd = LineContinues | LineEnds

data SoftRow = SoftRow
  { srLine :: Int
  , srStart :: Int
  , srText :: T.Text
  , srEnd :: RowEnd
  }

softRows :: Int -> [T.Text] -> [SoftRow]
softRows width ls = concat (zipWith (lineSoftRows width) [0 ..] ls)

lineSoftRows :: Int -> Int -> T.Text -> [SoftRow]
lineSoftRows width i =
  let
    go start t
      | T.compareLength t width /= GT = [SoftRow i start t LineEnds]
      | otherwise =
          let
            cut = case T.findIndex (== ' ') (T.reverse (T.take (width + 1) t)) of
              Just fromEnd | fromEnd < width -> width + 1 - fromEnd
              _ -> width
            (row, rest) = T.splitAt cut t
          in
            SoftRow i start row LineContinues : go (start + cut) rest
  in
    go 0

cursorSoftRow :: Int -> Int -> [SoftRow] -> (Int, SoftRow)
cursorSoftRow cr cc rows =
  let
    pick found (ix, r) =
      if srLine r == cr && srStart r <= cc then (ix, r) else found
  in
    List.foldl' pick (0, SoftRow cr 0 T.empty LineEnds) (zip [0 ..] (takeWhile (\r -> srLine r <= cr) rows))

rowsFrom :: Int -> Int -> [T.Text] -> (Int, Int) -> (Int, Int)
rowsFrom width n ls (cr, cc) =
  let
    rows = softRows width ls
    (ix, here) = cursorSoftRow cr cc rows
    target = max 0 (min (length rows - 1) (ix + n))
    lastOffset row =
      case srEnd row of
        LineContinues -> T.length (srText row) - 1
        LineEnds -> T.length (srText row)
  in
    case drop target rows of
      row : _
        | target /= ix ->
            (srLine row, srStart row + min (cc - srStart here) (lastOffset row))
      _ -> (cr, cc)
