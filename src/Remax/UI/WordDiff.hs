{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.WordDiff
  ( intralineSpans
  , shiftSpans
  ) where

import Data.Algorithm.Diff (PolyDiff (..), getGroupedDiff)
import Data.Char
  ( isAlphaNum
  , isSpace
  )
import Data.List
  ( sortOn
  )
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( mapMaybe
  )
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Remax.DiffModel

-- Words, for the purpose of telling two versions of a line apart: runs of
-- identifier characters, runs of space, runs of operator characters, and
-- brackets and separators one at a time. Each carries the column it
-- starts at, so a difference can be pointed at on screen.
--
-- An operator is a word in its own right — >>= and <$> are read as units
-- and change as units — but the brackets around one are not part of it.
-- Lumping all punctuation together made ")+(" a single token, so
-- changing the operator between two parenthesised terms marked the
-- parentheses along with it.
wordTokens :: Text -> [(Int, Text)]
wordTokens t =
  let
    kind c
      | isAlphaNum c || c == '_' = 0 :: Int
      | isSpace c = 1
      | c `elem` ("()[]{},;" :: String) = 2
      | otherwise = 3
    -- kind 2 never joins anything, itself included: "))" is two brackets
    sameWord a b = kind a == kind b && kind a /= 2
    groups = T.groupBy sameWord t
    starts = scanl (\n g -> n + T.length g) 0 groups
  in
    zip starts groups

-- What two lines have in common, counted two ways, because the two
-- questions asked of it want different answers.
--
-- Whether to mark at all is about words: a line where one long
-- identifier was renamed has little of its text left in common and is
-- still plainly an edit, so counting characters there would call it a
-- rewrite and say nothing.
--
-- Which of several candidates to pair is about characters: two lines can
-- share the same number of words and not be equally alike. Choosing
-- between "keeps <$>" and "keeps createdAtField" is a tie by the word,
-- and not remotely a tie to a reader.
data WordMatch = WordMatch
  { wmKeptWords :: !Int
  , wmWords :: !Int
  , wmKeptChars :: !Int
  , wmChars :: !Int
  , wmSpans :: ([(Int, Int)], [(Int, Int)])
  }

-- Below half their words in common two lines are a rewrite rather than an
-- edit, and pointing at every word says no more than the + and - already
-- did. Half rather than a third: a line whose binding name and = survive
-- already has a third of itself in common.
worthMarking :: WordMatch -> Bool
worthMarking wm = wmWords wm > 0 && wmKeptWords wm * 2 >= wmWords wm

-- How alike two lines are, for choosing between candidates. Nothing
-- compares this across different pairs of lines except bestPairs.
matchShare :: WordMatch -> Int
matchShare wm = (wmKeptChars wm * 1000) `div` max 1 (wmChars wm)

-- How much two lines have in common, and where they differ: the words
-- shared, the length of the longer, and the columns to mark on each side.
--
-- Whole words rather than characters: a renamed identifier should light
-- up as a name, not as a scatter of letters inside one. Under
-- WhitespaceIgnored the blanks are dropped before the comparison rather
-- than merely discounted — having said spacing is not a difference,
-- pointing at it anyway would be the noise that setting exists to avoid.
wordMatch :: WhitespaceMode -> Text -> Text -> WordMatch
wordMatch mode oldLine newLine =
  let
    tokensOf line = case mode of
      WhitespaceMatters -> wordTokens line
      WhitespaceIgnored ->
        filter (not . T.all isSpace . snd) (wordTokens line)
    olds = tokensOf oldLine
    news = tokensOf newLine
    grouped = getGroupedDiff (map snd olds) (map snd news)
    -- measured on words, not on the blanks between them: two lines with
    -- nothing whatever in common still share their spaces, and counting
    -- those as common was enough to call a rewrite an edit
    solid = filter (not . T.all isSpace)
    shared = solid (concatMap (\g -> case g of Both bs _ -> bs; _ -> []) grouped)
    oldWords = solid (map snd olds)
    newWords = solid (map snd news)
    chars = sum . map T.length
    spanOf toks i n =
      case (drop i toks, drop (i + n - 1) toks) of
        ((start, _) : _, (end, endText) : _) ->
          [(start, end + T.length endText - 1)]
        _ -> []
    walk _ _ [] = ([], [])
    walk oi ni (g : gs) =
      case g of
        Both bs _ -> walk (oi + length bs) (ni + length bs) gs
        First os ->
          let
            (a, b) = walk (oi + length os) ni gs
          in
            (spanOf olds oi (length os) ++ a, b)
        Second ns ->
          let
            (a, b) = walk oi (ni + length ns) gs
          in
            (a, spanOf news ni (length ns) ++ b)
  in
    WordMatch
      { wmKeptWords = length shared
      , wmWords = max (length oldWords) (length newWords)
      , wmKeptChars = chars shared
      , wmChars = max (chars oldWords) (chars newWords)
      , wmSpans = walk 0 0 grouped
      }

-- Which columns to mark on each row of the window. Rows arrive as a block
-- of removals followed by a block of additions, and the nth removal is
-- read against the nth addition.
--
-- Blocks of unequal size are paired too, as far as the shorter one goes.
-- Requiring them to match one for one looked like the careful choice and
-- was not: editing a line and adding another beside it makes a block of
-- one against two, which is an ordinary thing to do and left the edited
-- line with no marks at all. Nothing is risked by pairing anyway, because
-- a pair that does not resemble each other is dropped by wordSpans on the
-- way — the guard against marking the wrong words already lives there.
-- Above this many candidate pairings a block goes unmarked.
--
-- Matching is quadratic in the size of the block, and a block that large
-- is a wholesale rewrite where the marks would say nothing anyway. It
-- also cannot be matched honestly: only the rows in the render window
-- are on hand, so a block running past both ends of it offers the tail
-- of the removals against the head of the additions, and pairing those
-- produces marks that are confident and wrong.
pairingBudget :: Int
pairingBudget = 2500

-- Which added line each removed line was edited into. Not the one in the
-- same position: removing a line and editing the one below it is an
-- everyday shape, and by position that reads the deleted line against
-- the edited one's replacement, finds no resemblance, and leaves the
-- edit unmarked.
--
-- So every pairing is scored and the best taken first, then the best of
-- what is left, and so on. A line with no partner it resembles keeps
-- none, which is the honest answer for one that was purely added or
-- purely deleted.
bestPairs ::
  WhitespaceMode ->
  [RowR] ->
  [RowR] ->
  [(Int, Int, ([(Int, Int)], [(Int, Int)]))]
bestPairs mode removed added =
  let
    scored =
      [ (matchShare wm, i, j, wmSpans wm)
      | (i, o) <- zip [0 :: Int ..] removed
      , (j, n) <- zip [0 :: Int ..] added
      , let
          wm = wordMatch mode (rText o) (rText n)
      , worthMarking wm
      ]
    -- most text in common first; ties go to the earlier rows, which keeps
    -- equal-sized blocks pairing straight down as they read
    ranked = sortOn (\(share, i, j, _) -> (negate share, i, j)) scored
    take' _ _ [] = []
    take' usedO usedN ((_, i, j, spans) : rest)
      | Set.member i usedO || Set.member j usedN = take' usedO usedN rest
      | otherwise =
          (i, j, spans) : take' (Set.insert i usedO) (Set.insert j usedN) rest
  in
    take' Set.empty Set.empty ranked

intralineSpans :: WhitespaceMode -> [RowR] -> [[(Int, Int)]]
intralineSpans mode =
  let
    go [] = []
    go rows@(r : _)
      | rMark r == '-' =
          let
            (removed, afterRemoved) = span ((== '-') . rMark) rows
            (added, rest) = span ((== '+') . rMark) afterRemoved
            chosen =
              if length removed * length added > pairingBudget
                then []
                else bestPairs mode removed added
            olds = Map.fromList [(i, fst sp) | (i, _, sp) <- chosen]
            news = Map.fromList [(j, snd sp) | (_, j, sp) <- chosen]
            spansFor m n = [Map.findWithDefault [] k m | k <- [0 .. n - 1]]
          in
            spansFor olds (length removed)
              ++ spansFor news (length added)
              ++ go rest
      | otherwise = [] : go (drop 1 rows)
  in
    go

-- Ranges moved into the coordinates of a horizontally scrolled line, and
-- those scrolled off the left dropped.
shiftSpans :: Int -> [(Int, Int)] -> [(Int, Int)]
shiftSpans off =
  mapMaybe
    ( \(s, e) ->
        if e < off then Nothing else Just (max 0 (s - off), e - off)
    )
