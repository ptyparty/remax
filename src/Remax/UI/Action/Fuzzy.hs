{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Fuzzy
  ( Needle
  , needleFor
  , fuzzyScore
  , fuzzyScoreOn
  , subseqPositions
  , highlightSubseq
  , highlightInfix
  , highlightAt
  ) where

import Brick qualified as Brick
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as T
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Widgets qualified as Widgets
import System.FilePath qualified as FilePath

-- Case-insensitive subsequence match; lower scores first. Prefers
-- matches inside the file name, then tight spans, then short paths.
-- A query, lowercased and measured once, so that scoring a candidate
-- against it costs nothing but the walk.
data Needle = Needle
  { ndChars :: String
  , ndLen :: !Int
  , ndBlank :: !Bool
  }

needleFor :: T.Text -> Needle
needleFor q =
  let
    cs = map lowerFast (T.unpack q)
  in
    Needle cs (length cs) (T.null q)

-- Case folding for matching paths, which are very nearly always ASCII.
--
-- Data.Char.toLower is a Unicode table lookup, and the scorer calls it
-- once per character of every candidate — at a third of a million paths
-- that is tens of millions of table lookups for each character typed,
-- and it is most of what the finder spends its time on. Anything outside
-- ASCII still goes the long way round.
lowerFast :: Char -> Char
lowerFast c
  | c >= 'A' && c <= 'Z' = Char.chr (Char.ord c + 32)
  | c < '\128' = c
  | otherwise = Char.toLower c

fuzzyScore :: T.Text -> FilePath -> Maybe (Int, Int, Int)
fuzzyScore q = fuzzyScoreOn (needleFor q)

-- How well a path matches, or Nothing if it does not.
--
-- Walks the path where it lies, lowercasing each character as it is
-- compared. The obvious version builds a lowercased copy of the
-- candidate and pairs every character of it with an index — two lists
-- per candidate, per keystroke. Against a few thousand paths that is
-- invisible; against the three hundred thousand a large repository has
-- once ignored files are shown, it is well over a gigabyte of garbage
-- for every character typed, and the session goes to its knees.
fuzzyScoreOn :: Needle -> FilePath -> Maybe (Int, Int, Int)
fuzzyScoreOn nd path
  | ndBlank nd = Just (0, 0, length path)
  | otherwise =
      let
        -- One walk of the path, which yields everything the score wants:
        -- whether the query is a subsequence of it and where, how long
        -- it is, and where its file name begins.
        --
        -- Separately these are three more walks of the same string —
        -- matching, then finding the last separator, then counting — and
        -- against a few hundred thousand candidates a walk is not
        -- something to spend four times. The file name comes back as the
        -- tail of the path itself rather than a copy of it, which a list
        -- gives away for nothing.
        go needle hay !i !firstAt !lastAt tailAfterSep =
          case hay of
            [] -> (needle, i, firstAt, lastAt, tailAfterSep)
            (h : hs) ->
              let
                nextTail = if FilePath.isPathSeparator h then hs else tailAfterSep
              in
                case needle of
                  (c : cs)
                    | c == lowerFast h ->
                        go
                          cs
                          hs
                          (i + 1)
                          (if firstAt < 0 then i else firstAt)
                          i
                          nextTail
                  _ -> go needle hs (i + 1) firstAt lastAt nextTail
        (leftover, len, firstPos, lastPos, base) =
          go (ndChars nd) path 0 (-1) (-1) path
      in
        if not (null leftover)
          then Nothing
          else
            Just
              ( if subsequenceLower (ndChars nd) base then 0 else 1
              , (lastPos - firstPos) - (ndLen nd - 1)
              , len
              )

subsequenceLower :: String -> String -> Bool
subsequenceLower needle hay =
  case (needle, hay) of
    ([], _) -> True
    (_, []) -> False
    (c : cs, h : hs) ->
      if c == lowerFast h
        then subsequenceLower cs hs
        else subsequenceLower needle hs

-- Where each query character lands in the shown text (greedy,
-- case-insensitive); Nothing when the text no longer contains the
-- subsequence, as with an elided path.
subseqPositions :: T.Text -> T.Text -> Maybe [Int]
subseqPositions q t =
  let
    walk needle hay acc =
      case (needle, hay) of
        ([], _) -> Just (reverse acc)
        (_, []) -> Nothing
        (c : cs, (i, h) : hs) ->
          if Char.toLower c == Char.toLower h
            then walk cs hs (i : acc)
            else walk (c : cs) hs acc
  in
    walk (T.unpack q) (zip [0 ..] (T.unpack t)) []

highlightSubseq :: T.Text -> T.Text -> Brick.Widget Types.Name
highlightSubseq q t =
  case subseqPositions q t of
    Just ps
      | not (null ps) -> highlightAt (Set.fromList ps) t
    _ -> Brick.txt (Widgets.padT t)

highlightInfix :: T.Text -> T.Text -> Brick.Widget Types.Name
highlightInfix q t =
  let
    (pre, rest) = T.breakOn q t
  in
    if T.null q || T.null rest
      then Brick.txt (Widgets.padT t)
      else
        Brick.hBox
          [ Brick.txt pre
          , Brick.withAttr Attrs.matchCharAttr (Brick.txt (T.take (T.length q) rest))
          , Brick.txt (T.drop (T.length q) rest)
          ]

highlightAt :: Set.Set Int -> T.Text -> Brick.Widget Types.Name
highlightAt ps t =
  let
    marked = zipWith (\i c -> (Set.member i ps, c)) [0 ..] (T.unpack t)
    groups = List.groupBy (\a b -> fst a == fst b) marked
    seg g = case g of
      [] -> Brick.txt ""
      ((hit, _) : _) ->
        let
          segText = T.pack (map snd g)
        in
          if hit
            then Brick.withAttr Attrs.matchCharAttr (Brick.txt segText)
            else Brick.txt segText
  in
    Brick.hBox (map seg groups)

-- The editor escape hatch -----------------------------------------------------------
