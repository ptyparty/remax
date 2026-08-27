{-# LANGUAGE OverloadedStrings #-}

-- The pure decoration model: the working file's lines diffed against a
-- base, grouped into runs (hunks as they stand right now), with syntax
-- tokens and content keys attached so rendering never looks anything up.
module Remax.DiffModel
  ( RowR (..)
  , Run (..)
  , RunKind (..)
  , SplitConfig (..)
  , defaultSplitConfig
  , diffRows
  , assemble
  , forceRows
  , forceTokLines
  ) where

import Brick (AttrName)
import Data.Algorithm.Diff (Diff, PolyDiff (..), getGroupedDiff)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Remax.Key (lineKeys)

data RowR = RowR
  { rMark :: !Char
  , rNewRow :: !(Maybe Int) -- buffer row this display row shows, if any
  , rText :: !Text -- old text for '-' rows, buffer text otherwise
  , rToks :: [(AttrName, Text)] -- [] for old-side rows
  , rRunIx :: !(Maybe Int)
  , rKey :: !(Maybe Text) -- content key for changed rows
  }

-- One group of consecutive +/- display rows. A maximal group bigger than
-- splitThreshold is split at blank changed lines (function boundaries in
-- most languages), so a rewritten module reviews as several hunks rather
-- than one monolith. Content keys carry no run identity, so recorded
-- verdicts are unaffected by how the grouping falls.
data Run = Run
  { runAnchor :: !Int -- buffer row to park the cursor on
  , runKeys :: [Text] -- one key per changed line, in display order
  , runLines :: [Text] -- the marked lines themselves, aligned with runKeys
  , runKind :: !RunKind -- prose or code, for the code/comment filter
  }

-- Whether a hunk changes only comments. Reading a branch's prose and
-- reading its logic are different passes, so the filter needs to tell
-- them apart — and it has to do so for files nothing has opened yet,
-- which rules out the tree-sitter scopes (they exist only once a file
-- has been displayed, and never for the removed side of a diff).
data RunKind = RunCode | RunComment
  deriving (Eq, Show)

-- The line comment leaders trusted by both the splitter and the
-- code/comment filter. Deliberately short: a leader that fires on
-- ordinary code would quietly hide real changes.
lineCommentLeaders :: [Text]
lineCommentLeaders = ["--", "//", "#", ";"]

blockCommentOpeners :: [Text]
blockCommentOpeners = ["{-", "/*", "<!--"]

-- A hunk is prose when every non-blank line it changes is one: a line
-- comment, the opening of a block comment, or a line already inside one.
-- An all-blank hunk is code, since there is no prose in it to read.
runKindOf :: [(Text, Bool)] -> RunKind
runKindOf ls =
  let
    meaningful = filter (not . T.null . T.strip . fst) ls
    isComment (t, inside) =
      inside
        || any
          (`T.isPrefixOf` T.strip t)
          (lineCommentLeaders ++ blockCommentOpeners)
  in
    if not (null meaningful) && all isComment meaningful
      then RunComment
      else RunCode

-- One display row per diff row, with equal prefix/suffix trimmed off
-- before Myers so ordinary edits diff only the touched region.
diffRows :: [Text] -> [Text] -> [(Char, Maybe Int, Text)]
diffRows old new =
  let
    pre = length (takeWhile id (zipWith (==) old new))
    oldTail = drop pre old
    newTail = drop pre new
    suf =
      length (takeWhile id (zipWith (==) (reverse oldTail) (reverse newTail)))
    oldMid = take (length oldTail - suf) oldTail
    newMid = take (length newTail - suf) newTail
    contextRow n (k, l) = (' ', Just (n + k), l)
    preRows = map (contextRow 0) (zip [0 ..] (take pre new))
    sufStart = pre + length newMid
    sufRows =
      map (contextRow sufStart) (zip [0 ..] (drop (length newTail - suf) newTail))
    go :: Int -> [Diff [Text]] -> [(Char, Maybe Int, Text)]
    go _ [] = []
    go n (grp : rest) =
      case grp of
        Both bs _ ->
          map (contextRow n) (zip [0 ..] bs) ++ go (n + length bs) rest
        First os ->
          map (\o -> ('-', Nothing, o)) os ++ go n rest
        Second ns ->
          map (\(k, l) -> ('+', Just (n + k), l)) (zip [0 ..] ns)
            ++ go (n + length ns) rest
  in
    preRows ++ go pre (getGroupedDiff oldMid newMid) ++ sufRows

-- Attach run indexes, syntax tokens, and content keys to raw diff rows in
-- single tandem walks (the row streams are aligned by construction, so no
-- per-row indexing is ever needed).
assemble ::
  SplitConfig ->
  Text -> -- repository root, part of every key
  Text -> -- file path, part of every key
  [(Char, Maybe Int, Text)] ->
  [[(AttrName, Text)]] ->
  ([RowR], [Run])
assemble sc repo file rows toks =
  let
    ixs = runIndexes sc rows
    markedLine (m, _, t) =
      if m == ' ' then Nothing else Just (T.cons m t)
    keys = lineKeys repo file (mapMaybe markedLine rows)
  in
    (buildRows rows toks ixs keys, buildRuns rows ixs keys)

-- How aggressively oversized hunks split — user-tunable via the config
-- file: chunks at or under splitThreshold stay one hunk; inside one
-- function a piece over pieceMax keeps splitting (at blanks, then at
-- the least-indented line when there are none); pieces under pieceMin
-- merge forward, but only within their function, never across a strong
-- boundary.
data SplitConfig = SplitConfig
  { splitThreshold :: Int
  , pieceMin :: Int
  , pieceMax :: Int
  }

defaultSplitConfig :: SplitConfig
defaultSplitConfig =
  SplitConfig
    { splitThreshold = 20
    , pieceMin = 6
    , pieceMax = 32
    }

runIndexes :: SplitConfig -> [(Char, Maybe Int, Text)] -> [Maybe Int]
runIndexes sc rows0 =
  let
    isCtx (m, _, _) = m == ' '
    go _ [] = []
    go k rs =
      case rs of
        (r : _)
          | isCtx r ->
              let
                (ctx, rest) = span isCtx rs
              in
                map (const Nothing) ctx ++ go k rest
        _ ->
          let
            (chg, rest) = break isCtx rs
            pieces = splitChunk sc chg
            ixsFor pieceIx piece = map (const (Just (k + pieceIx))) piece
          in
            concat (zipWith ixsFor [0 ..] pieces) ++ go (k + length pieces) rest
  in
    go (0 :: Int) rows0

-- Split one contiguous changed chunk, when it is big enough to be worth
-- it, in three tiers. Strong boundaries — a blank changed line whose
-- next changed line starts at column 0, i.e. a function boundary —
-- always split, and nothing ever merges back across one. Within a
-- function, a segment over pieceMax splits at its interior blanks, with
-- runts under pieceMin merging forward; a blankless segment still over
-- pieceMax is cut at its least-indented line. Boundary blanks attach to
-- the piece before them; a chunk with added lines splits only at added
-- blanks (so a rewrite's removed block stays attached to its first
-- replacement).
-- Which rows of a chunk sit inside a block comment ({- -}, /* */,
-- <!-- -->): a blank line in there is prose, not a boundary. Line
-- comments need no tracking — a blank line ends them by definition.
-- Delimiters inside string literals over-count harmlessly (the split
-- just gets more conservative).
insideBlockComment :: [(Char, Maybe Int, Text)] -> [Bool]
insideBlockComment rows0 =
  let
    step (hs, c, xml) (_, _, t) =
      let
        inside = hs > 0 || c > 0 || xml > 0
        hsNext = max 0 (hs + T.count "{-" t - T.count "-}" t)
        cNext = max 0 (min 1 (c + T.count "/*" t - T.count "*/" t))
        xmlNext = max 0 (min 1 (xml + T.count "<!--" t - T.count "-->" t))
      in
        ((hsNext, cNext, xmlNext), inside)
    go _ [] = []
    go st (r : rs) =
      let
        (stNext, inside) = step st r
      in
        inside : go stNext rs
  in
    go (0 :: Int, 0 :: Int, 0 :: Int) rows0

splitChunk :: SplitConfig -> [(Char, Maybe Int, Text)] -> [[(Char, Maybe Int, Text)]]
splitChunk sc chg0 =
  let
    insides = insideBlockComment chg0
    chg = zip chg0 insides
    splitMark =
      if any (\((m, _, _), _) -> m == '+') chg then '+' else '-'
    isBlank ((m, _, t), inside) =
      m == splitMark && T.null (T.strip t) && not inside
    isChanged ((m, _, _), _) = m == splitMark
    atTopLevel ((_, _, t), inside) =
      not inside && not (T.null t) && not (isSpaceChar (T.head t))
    isSpaceChar c = c == ' ' || c == '\t'
    -- a blank run is strong when the code resuming after it is at
    -- column zero
    strongAt rest =
      case dropWhile isBlank rest of
        (next : _) -> isChanged next && atTopLevel next
        [] -> False
    cutAt boundary rs =
      case rs of
        [] -> []
        _ ->
          let
            (body, rest1) = break boundary rs
            (blanks, rest2) = span isBlank rest1
          in
            if null body
              then case cutAt boundary rest2 of
                (nextPiece : more) -> (blanks ++ nextPiece) : more
                [] -> [blanks]
              else (body ++ blanks) : cutAt boundary rest2
    strongBoundary rs = case rs of
      (r : rest) -> isBlank r && strongAt (r : rest)
      [] -> False
    -- tier 1: function boundaries (checked on the row stream so the
    -- blank can look ahead to what follows it)
    strongSegments rs =
      case rs of
        [] -> []
        _ ->
          let
            (body, rest1) = breakOnStream strongBoundary rs
            (blanks, rest2) = span isBlank rest1
          in
            if null body
              then case strongSegments rest2 of
                (nextSeg : more) -> (blanks ++ nextSeg) : more
                [] -> [blanks]
              else (body ++ blanks) : strongSegments rest2
    breakOnStream p rs =
      case rs of
        [] -> ([], [])
        (r : rest) ->
          if p (r : rest)
            then ([], r : rest)
            else
              let
                (body, tailRows) = breakOnStream p rest
              in
                (r : body, tailRows)
    -- tier 2: inside one function — blanks, then forward runt merging
    weakSplit seg =
      if length seg <= pieceMax sc
        then [seg]
        else concatMap indentCut (mergeRunts (cutAt isBlank seg))
    mergeRunts ps =
      case ps of
        [] -> []
        [only] -> [only]
        (a : b : rest) ->
          if length a < pieceMin sc
            then mergeRunts ((a ++ b) : rest)
            else case mergeRunts (b : rest) of
              (merged : more)
                | length merged < pieceMin sc && null more -> [a ++ merged]
              merged -> a : merged
    -- tier 3: a blankless slab still over pieceMax cuts at its least-
    -- indented changed line, both sides kept above pieceMin
    indentCut piece =
      if length piece <= pieceMax sc
        then [piece]
        else
          let
            isLineComment t =
              any (`T.isPrefixOf` T.strip t) lineCommentLeaders
            -- never cut inside a block comment, and never between
            -- the lines of a comment run — only before its first
            -- line, so a comment stays glued to the code it heads
            indentOf (((m, _, t), inside), prevComment) =
              if m == ' '
                || T.null (T.strip t)
                || inside
                || (isLineComment t && prevComment)
                then maxBound
                else T.length (T.takeWhile isSpaceChar t)
            prevIsComment =
              False
                : map
                  ( \((_, _, t), inside) ->
                      inside || isLineComment t
                  )
                  piece
            window =
              take
                (min (pieceMax sc) (length piece - pieceMin sc) - pieceMin sc)
                ( drop
                    (pieceMin sc)
                    (zip [0 :: Int ..] (map indentOf (zip piece prevIsComment)))
                )
            best = case window of
              [] -> Nothing
              _ ->
                let
                  smallest = minimum (map snd window)
                in
                  if smallest == maxBound
                    then Nothing
                    else lookupFirst smallest window
            lookupFirst v pairs =
              case filter (\(_, ind) -> ind == v) pairs of
                ((i, _) : _) -> Just i
                [] -> Nothing
          in
            case best of
              Nothing -> [piece]
              Just i -> take i piece : indentCut (drop i piece)
  in
    if length chg0 <= splitThreshold sc
      then [chg0]
      else map (map fst) (concatMap weakSplit (strongSegments chg))

buildRows ::
  [(Char, Maybe Int, Text)] ->
  [[(AttrName, Text)]] ->
  [Maybe Int] ->
  [Text] ->
  [RowR]
buildRows =
  let
    go [] _ _ _ = []
    go ((m, mr, t) : rs) ts (ix : ixs) ks =
      let
        (tok, remainingToks) = case mr of
          Just _ -> case ts of
            (tl : rest) -> (tl, rest)
            [] -> ([], [])
          Nothing -> ([], ts)
        (key, remainingKeys) =
          if m == ' '
            then (Nothing, ks)
            else case ks of
              (k : rest) -> (Just k, rest)
              [] -> (Nothing, [])
      in
        RowR m mr t tok ix key : go rs remainingToks ixs remainingKeys
    go _ _ [] _ = []
  in
    go

buildRuns :: [(Char, Maybe Int, Text)] -> [Maybe Int] -> [Text] -> [Run]
buildRuns rows ixs keys0 =
  let
    bufferRowOf (_, mr, _) = mr
    lastRow = maximum (0 : mapMaybe bufferRowOf rows)
    -- nexts !! i = the buffer row of the first row after i that has
    -- one, so a delete-only run can anchor where the deletion now sits
    nexts =
      drop 1 (scanr (\(_, mr, _) acc -> fromMaybe acc mr) lastRow rows)
    hasNoRun ((_, mix, _), _) = mix == Nothing
    runIxOf ((_, mix, _), _) = mix
    addedRowAnchor ((row, _, _), _) = case row of
      ('+', mr, _) -> mr
      _ -> Nothing
    go [] _ = []
    go entries@(first : _) ks
      | hasNoRun first = go (dropWhile hasNoRun entries) ks
      | otherwise =
          -- group by equal index, not mere contiguity: an oversized
          -- chunk was split into several adjacent runs
          let
            headIx = runIxOf first
            (grp, rest) = span (\e -> runIxOf e == headIx) entries
            markedOf (((m, _, t), _, _), _) = T.cons m t
            classified (((_, _, t), _, _), inside) = (t, inside)
            n = length grp
            anchor = case mapMaybe addedRowAnchor grp of
              (r : _) -> r
              [] -> case grp of
                (((_, _, nxt), _) : _) -> nxt
                [] -> 0
          in
            Run
              { runAnchor = anchor
              , runKeys = take n ks
              , runLines = map markedOf grp
              , runKind = runKindOf (map classified grp)
              }
              : go rest (drop n ks)
  in
    go (zip (zip3 rows ixs nexts) (insideBlockComment rows)) keys0

forceTokLines :: [[(AttrName, Text)]] -> ()
forceTokLines =
  let
    forceSeg (a, s) rest = a `seq` s `seq` rest
    forceLine line rest = foldr forceSeg () line `seq` rest
  in
    foldr forceLine ()

forceRows :: [(Char, Maybe Int, Text)] -> ()
forceRows =
  let
    forceMaybe m = case m of
      Just x -> x `seq` ()
      Nothing -> ()
    forceRow (m, mr, t) rest = m `seq` forceMaybe mr `seq` t `seq` rest
  in
    foldr forceRow ()
