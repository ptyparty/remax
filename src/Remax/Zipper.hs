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

import Data.Text (Text)
import qualified Data.Text as T

-- Invariant: the buffer always holds at least one line (tzLeft/tzRight
-- form the current one); tzAbove is reversed.
data TextZipper a = TextZipper
  { tzAbove :: [Text]
  , tzLeft :: Text
  , tzRight :: Text
  , tzBelow :: [Text]
  }

-- The second argument mirrors Data.Text.Zipper's line limit; the review
-- buffer never limits lines, so it is accepted and ignored.
textZipper :: [Text] -> Maybe Int -> TextZipper Text
textZipper ls _limit =
  case ls of
    [] -> TextZipper [] T.empty T.empty []
    (first : rest) -> TextZipper [] T.empty first rest

getText :: TextZipper Text -> [Text]
getText tz =
  reverse (tzAbove tz) ++ (currentLine tz : tzBelow tz)

currentLine :: TextZipper Text -> Text
currentLine tz = tzLeft tz <> tzRight tz

cursorPosition :: TextZipper Text -> (Int, Int)
cursorPosition tz = (length (tzAbove tz), T.length (tzLeft tz))

-- Clamps to the buffer, like the library it replaces.
moveCursor :: (Int, Int) -> TextZipper Text -> TextZipper Text
moveCursor (row, col) tz =
  let ls = getText tz
      lastRow = length ls - 1
      r = max 0 (min row lastRow)
      above = reverse (take r ls)
      (line, below) = case drop r ls of
        (l : rest) -> (l, rest)
        [] -> (T.empty, [])
      c = max 0 (min col (T.length line))
  in TextZipper above (T.take c line) (T.drop c line) below

moveLeft :: TextZipper Text -> TextZipper Text
moveLeft tz =
  if T.null (tzLeft tz)
    then tz
    else
      tz
        { tzLeft = T.init (tzLeft tz)
        , tzRight = T.cons (T.last (tzLeft tz)) (tzRight tz)
        }

moveRight :: TextZipper Text -> TextZipper Text
moveRight tz =
  case T.uncons (tzRight tz) of
    Nothing -> tz
    Just (c, rest) -> tz { tzLeft = T.snoc (tzLeft tz) c, tzRight = rest }

moveUp :: TextZipper Text -> TextZipper Text
moveUp tz =
  case tzAbove tz of
    [] -> tz
    (prev : rest) ->
      let col = min (T.length (tzLeft tz)) (T.length prev)
      in TextZipper
           rest
           (T.take col prev)
           (T.drop col prev)
           (currentLine tz : tzBelow tz)

moveDown :: TextZipper Text -> TextZipper Text
moveDown tz =
  case tzBelow tz of
    [] -> tz
    (next : rest) ->
      let col = min (T.length (tzLeft tz)) (T.length next)
      in TextZipper
           (currentLine tz : tzAbove tz)
           (T.take col next)
           (T.drop col next)
           rest

gotoBOL :: TextZipper Text -> TextZipper Text
gotoBOL tz = tz { tzLeft = T.empty, tzRight = currentLine tz }

gotoEOL :: TextZipper Text -> TextZipper Text
gotoEOL tz = tz { tzLeft = currentLine tz, tzRight = T.empty }

insertChar :: Char -> TextZipper Text -> TextZipper Text
insertChar ch tz =
  if ch == '\n'
    then breakLine tz
    else tz { tzLeft = T.snoc (tzLeft tz) ch }

-- Pasted content may span lines; everything else is one line-local
-- append per segment.
insertMany :: Text -> TextZipper Text -> TextZipper Text
insertMany t tz =
  case T.split (== '\n') t of
    [] -> tz
    [single] -> tz { tzLeft = tzLeft tz <> single }
    (first : more) ->
      case reverse more of
        [] -> tz { tzLeft = tzLeft tz <> first }
        (final : revMiddle) ->
          tz
            { tzAbove = revMiddle ++ ((tzLeft tz <> first) : tzAbove tz)
            , tzLeft = final
            }

breakLine :: TextZipper Text -> TextZipper Text
breakLine tz =
  tz
    { tzAbove = tzLeft tz : tzAbove tz
    , tzLeft = T.empty
    }

-- Delete at the cursor; at end of line the next line joins up, the way
-- editors treat Del.
deleteChar :: TextZipper Text -> TextZipper Text
deleteChar tz =
  case T.uncons (tzRight tz) of
    Just (_, rest) -> tz { tzRight = rest }
    Nothing ->
      case tzBelow tz of
        [] -> tz
        (next : rest) -> tz { tzRight = next, tzBelow = rest }

-- Backspace; at column zero the line joins the one above, cursor at the
-- junction.
deletePrevChar :: TextZipper Text -> TextZipper Text
deletePrevChar tz =
  if T.null (tzLeft tz)
    then case tzAbove tz of
      [] -> tz
      (prev : rest) -> tz { tzAbove = rest, tzLeft = prev }
    else tz { tzLeft = T.init (tzLeft tz) }
