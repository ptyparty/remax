-- A minimal line-oriented text zipper, replacing Data.Text.Zipper: that
-- library builds its zipper by inserting the buffer character by
-- character, appending to its line list at every newline — quadratic in
-- line count, and multiple seconds of stall for a 20k-line file. This
-- one constructs in O(1) from the line list and keeps every edit local
-- to the current line.
--
-- The phantom parameter keeps the call sites' `TextZipper Text`
-- signatures compiling unchanged; the content is always Text.
module Remax.Zipper
  ( TextZipper
  , textZipper
  , getText
  , lineCount
  , linesUpward
  , currentLine
  , cursorPosition
  , moveCursor
  , moveLeft
  , moveRight
  , moveUp
  , moveDown
  , gotoBOL
  , gotoEOL
  , insertChar
  , insertMany
  , breakLine
  , deleteChar
  , deletePrevChar
  ) where

import Data.Text qualified as T

-- Invariant: the buffer always holds at least one line (tzLeft/tzRight
-- form the current one); tzAbove is reversed. tzAboveLen and tzBelowLen
-- are the lengths of their lists, carried rather than recomputed: the
-- cursor's row and the buffer's line count are both wanted on every
-- frame, and walking a million-line list twice a redraw is the kind of
-- cost that only shows up on the files least able to afford it.
data TextZipper a = TextZipper
  { tzAbove :: [T.Text]
  , tzAboveLen :: !Int
  , tzLeft :: T.Text
  , tzRight :: T.Text
  , tzBelow :: [T.Text]
  , tzBelowLen :: !Int
  }

-- The second argument mirrors Data.Text.Zipper's line limit; the review
-- buffer never limits lines, so it is accepted and ignored.
textZipper :: [T.Text] -> Maybe Int -> TextZipper T.Text
textZipper ls _limit =
  case ls of
    [] -> TextZipper [] 0 T.empty T.empty [] 0
    (first : rest) -> TextZipper [] 0 T.empty first rest (length rest)

getText :: TextZipper T.Text -> [T.Text]
getText tz =
  reverse (tzAbove tz) ++ (currentLine tz : tzBelow tz)

-- O(1), and always equal to length (getText tz).
lineCount :: TextZipper T.Text -> Int
lineCount tz = tzAboveLen tz + 1 + tzBelowLen tz

-- The cursor's line and everything above it, nearest first. tzAbove is
-- already held in that order, so a search upward from the cursor walks
-- only as far as its answer instead of rebuilding the whole buffer to
-- look at the near end of it.
linesUpward :: TextZipper T.Text -> [T.Text]
linesUpward tz = currentLine tz : tzAbove tz

currentLine :: TextZipper T.Text -> T.Text
currentLine tz = tzLeft tz <> tzRight tz

cursorPosition :: TextZipper T.Text -> (Int, Int)
cursorPosition tz = (tzAboveLen tz, T.length (tzLeft tz))

-- Clamps to the buffer, like the library it replaces.
moveCursor :: (Int, Int) -> TextZipper T.Text -> TextZipper T.Text
moveCursor (row, col) tz =
  let
    total = lineCount tz
    ls = getText tz
    r = max 0 (min row (total - 1))
    above = reverse (take r ls)
    (line, below) = case drop r ls of
      (l : rest) -> (l, rest)
      [] -> (T.empty, [])
    c = max 0 (min col (T.length line))
  in
    TextZipper above r (T.take c line) (T.drop c line) below (total - r - 1)

moveLeft :: TextZipper T.Text -> TextZipper T.Text
moveLeft tz =
  if T.null (tzLeft tz)
    then tz
    else
      tz
        { tzLeft = T.init (tzLeft tz)
        , tzRight = T.cons (T.last (tzLeft tz)) (tzRight tz)
        }

moveRight :: TextZipper T.Text -> TextZipper T.Text
moveRight tz =
  case T.uncons (tzRight tz) of
    Nothing -> tz
    Just (c, rest) -> tz {tzLeft = T.snoc (tzLeft tz) c, tzRight = rest}

moveUp :: TextZipper T.Text -> TextZipper T.Text
moveUp tz =
  case tzAbove tz of
    [] -> tz
    (prev : rest) ->
      let
        col = min (T.length (tzLeft tz)) (T.length prev)
      in
        TextZipper
          rest
          (tzAboveLen tz - 1)
          (T.take col prev)
          (T.drop col prev)
          (currentLine tz : tzBelow tz)
          (tzBelowLen tz + 1)

moveDown :: TextZipper T.Text -> TextZipper T.Text
moveDown tz =
  case tzBelow tz of
    [] -> tz
    (next : rest) ->
      let
        col = min (T.length (tzLeft tz)) (T.length next)
      in
        TextZipper
          (currentLine tz : tzAbove tz)
          (tzAboveLen tz + 1)
          (T.take col next)
          (T.drop col next)
          rest
          (tzBelowLen tz - 1)

gotoBOL :: TextZipper T.Text -> TextZipper T.Text
gotoBOL tz = tz {tzLeft = T.empty, tzRight = currentLine tz}

gotoEOL :: TextZipper T.Text -> TextZipper T.Text
gotoEOL tz = tz {tzLeft = currentLine tz, tzRight = T.empty}

insertChar :: Char -> TextZipper T.Text -> TextZipper T.Text
insertChar ch tz =
  if ch == '\n'
    then breakLine tz
    else tz {tzLeft = T.snoc (tzLeft tz) ch}

-- Pasted content may span lines; everything else is one line-local
-- append per segment.
insertMany :: T.Text -> TextZipper T.Text -> TextZipper T.Text
insertMany t tz =
  case T.split (== '\n') t of
    [] -> tz
    [single] -> tz {tzLeft = tzLeft tz <> single}
    (first : more) ->
      case reverse more of
        [] -> tz {tzLeft = tzLeft tz <> first}
        (final : revMiddle) ->
          tz
            { tzAbove = revMiddle ++ ((tzLeft tz <> first) : tzAbove tz)
            , tzAboveLen = tzAboveLen tz + 1 + length revMiddle
            , tzLeft = final
            }

breakLine :: TextZipper T.Text -> TextZipper T.Text
breakLine tz =
  tz
    { tzAbove = tzLeft tz : tzAbove tz
    , tzAboveLen = tzAboveLen tz + 1
    , tzLeft = T.empty
    }

-- Delete at the cursor; at end of line the next line joins up, the way
-- editors treat Del.
deleteChar :: TextZipper T.Text -> TextZipper T.Text
deleteChar tz =
  case T.uncons (tzRight tz) of
    Just (_, rest) -> tz {tzRight = rest}
    Nothing ->
      case tzBelow tz of
        [] -> tz
        (next : rest) ->
          tz {tzRight = next, tzBelow = rest, tzBelowLen = tzBelowLen tz - 1}

-- Backspace; at column zero the line joins the one above, cursor at the
-- junction.
deletePrevChar :: TextZipper T.Text -> TextZipper T.Text
deletePrevChar tz =
  if T.null (tzLeft tz)
    then case tzAbove tz of
      [] -> tz
      (prev : rest) ->
        tz {tzAbove = rest, tzAboveLen = tzAboveLen tz - 1, tzLeft = prev}
    else tz {tzLeft = T.init (tzLeft tz)}
