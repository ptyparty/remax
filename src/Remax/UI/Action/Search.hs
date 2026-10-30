{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Search
  ( searchWordUnderCursor
  , searchWordBackward
  , openSearchPrompt
  , searchRepeat
  , searchStep
  , wordUnderCursor
  , isIdentChar
  ) where

import Brick qualified as Brick
import Control.Applicative ((<|>))
import Control.Monad qualified as Monad
import Control.Monad.State qualified as State
import Data.Char qualified as Char
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.Modal qualified as Modal
import Remax.UI.Action.Picker qualified as PickerAction
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Model.Nav qualified as Nav
import Remax.Zipper qualified as Z

-- '*': prime the search with the selected text (single-line selections)
-- or the identifier under the cursor, then jump to its next occurrence.
searchWordUnderCursor :: Brick.EventM Types.Name Types.St ()
searchWordUnderCursor = searchWordDir 1

-- vi's '#': the same needle, walked the other way.
searchWordBackward :: Brick.EventM Types.Name Types.St ()
searchWordBackward = searchWordDir (-1)

searchWordDir :: Int -> Brick.EventM Types.Name Types.St ()
searchWordDir dir = do
  st <- State.get
  case Types.currentFileMay st >>= wordUnderCursor . Types.fvEd of
    Nothing -> State.modify (\s -> s {Types.stMsg = "no word under cursor"})
    Just needle -> do
      State.modify
        (\s -> s {Types.stLastSearch = needle, Types.stGrepNav = Nothing, Types.stSearchDir = dir})
      searchStep dir needle

wordUnderCursor :: Modal.EdState -> Maybe T.Text
wordUnderCursor ed =
  let
    ls = Modal.bufferLines ed
    rowText r = Maybe.fromMaybe "" (Types.safeIdx ls r)
    -- only a deliberate v-extend selection overrides the cursor word;
    -- the incidental selections that w/e-style motions leave behind drag
    -- neighboring punctuation along and are not what * means
    selectionNeedle = do
      ((lr, lc), (hr, hc)) <- Modal.charSelection ed
      if not (Modal.edExtend ed) || (lr, lc) == (hr, hc) || lr /= hr
        then Nothing
        else
          let
            l = rowText lr
          in
            Just (T.take (hc + 1 - lc) (T.drop lc l))
    wordNeedle =
      let
        (row, col) = Modal.cursorRC ed
        l = rowText row
        -- on punctuation, scan forward to the next word on the line
        -- (vim's * does the same) rather than giving up
        effCol =
          if col < T.length l && isIdentChar (T.index l col)
            then Just col
            else fmap (col +) (T.findIndex isIdentChar (T.drop col l))
      in
        case effCol of
          Nothing -> Nothing
          Just c ->
            let
              start =
                T.length (T.dropWhileEnd isIdentChar (T.take c l))
              len =
                T.length (T.takeWhile isIdentChar (T.drop start l))
            in
              Just (T.take len (T.drop start l))
  in
    selectionNeedle <|> wordNeedle

isIdentChar :: Char -> Bool
isIdentChar ch = Char.isAlphaNum ch || ch == '_' || ch == '\''

-- `/` searches forward and vi's `?` backward; the prompt wears whichever
-- sigil opened it, and n/N repeat along that grain rather than always
-- downward. In the keymaps with no `?`, the direction is always forward
-- and this is exactly the old behavior.
openSearchPrompt :: Int -> Brick.EventM Types.Name Types.St ()
openSearchPrompt dir =
  State.modify (\s -> s {Types.stPrompt = Just (Types.SearchPrompt ""), Types.stSearchDir = dir})

-- n keeps the search's own direction, N reverses it.
searchRepeat :: Int -> Brick.EventM Types.Name Types.St ()
searchRepeat sense = do
  st <- State.get
  searchNext (sense * Types.stSearchDir st)

-- n/N: when the last search came from the grep picker, walk its hit
-- list across files without re-searching; otherwise repeat the in-file
-- search as always.
searchNext :: Int -> Brick.EventM Types.Name Types.St ()
searchNext dir = do
  st <- State.get
  case Types.stGrepNav st of
    Just (q, hits, ix)
      | not (null hits) -> do
          let
            n = length hits
            ix2 = (((ix + dir) `mod` n) + n) `mod` n
          case Types.safeIdx hits ix2 of
            Just (path, line) -> do
              State.modify (\s -> s {Types.stGrepNav = Just (q, hits, ix2)})
              PickerAction.openGrepHit path line q
              Types.setMsg ("grep hit " <> Types.tshow (ix2 + 1) <> "/" <> Types.tshow n)
            Nothing -> pure ()
    _ ->
      Monad.unless
        (T.null (Types.stLastSearch st))
        (searchStep dir (Types.stLastSearch st))

-- Literal search over the current file's buffer, helix-shaped: the match
-- becomes the selection, n/N walk matches, wrapping at either end.
searchStep :: Int -> T.Text -> Brick.EventM Types.Name Types.St ()
searchStep dir needle = do
  st <- State.get
  case Types.currentFileMay st of
    Nothing -> Types.setMsg "no file on screen"
    Just fv -> do
      let
        i = Types.stCurF st
        ed = Types.fvEd fv
        pos = Modal.cursorRC ed
        matchesInLine (r, l) =
          map (\(pre, _) -> (r, T.length pre)) (T.breakOnAll needle l)
        matches = concatMap matchesInLine (zip [0 ..] (Modal.bufferLines ed))
      case matches of
        [] -> State.modify (\s -> s {Types.stMsg = "no match: " <> needle})
        _ -> do
          let
            after = filter (> pos) matches
            before = filter (< pos) matches
            (mr, mc) =
              if dir > 0
                then case after ++ matches of
                  (m : _) -> m
                  [] -> pos
                else case reverse before ++ reverse matches of
                  (m : _) -> m
                  [] -> pos
            moved =
              ed
                { Modal.edZipper = Z.moveCursor (mr, mc + T.length needle - 1) (Modal.edZipper ed)
                , Modal.edSelAnchor = Just (mr, mc)
                , Modal.edSelKind = Modal.CharSel
                }
          Nav.setEd i moved
          State.modify (\s -> Types.hidePullView s {Types.stMsg = " "})
