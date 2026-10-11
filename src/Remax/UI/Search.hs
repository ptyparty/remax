{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Search
  ( searchWordUnderCursor
  , searchWordBackward
  , openSearchPrompt
  , searchRepeat
  , searchStep
  ) where

import Brick
  ( EventM
  )
import Control.Applicative ((<|>))
import Control.Monad
  ( unless
  )
import Control.Monad.State
  ( get
  , modify
  )
import Data.Char
  ( isAlphaNum
  )
import Data.Maybe
  ( fromMaybe
  )
import Data.Text (Text)
import qualified Data.Text as T
import Remax.Modal
import Remax.UI.Nav
import Remax.UI.Picker
import Remax.UI.Types
import qualified Remax.Zipper as Z

-- '*': prime the search with the selected text (single-line selections)
-- or the identifier under the cursor, then jump to its next occurrence.
searchWordUnderCursor :: EventM Name St ()
searchWordUnderCursor = searchWordDir 1

-- vi's '#': the same needle, walked the other way.
searchWordBackward :: EventM Name St ()
searchWordBackward = searchWordDir (-1)

searchWordDir :: Int -> EventM Name St ()
searchWordDir dir = do
  st <- get
  let
    ed = fvEd (currentFile st)
    ls = bufferLines ed
    rowText r = fromMaybe "" (safeIdx ls r)
    -- only a deliberate v-extend selection overrides the cursor word;
    -- the incidental selections that w/e-style motions leave behind drag
    -- neighboring punctuation along and are not what * means
    selectionNeedle = do
      ((lr, lc), (hr, hc)) <- charSelection ed
      if not (edExtend ed) || (lr, lc) == (hr, hc) || lr /= hr
        then Nothing
        else
          let
            l = rowText lr
          in
            Just (T.take (hc + 1 - lc) (T.drop lc l))
    isIdent ch = isAlphaNum ch || ch == '_' || ch == '\''
    wordNeedle =
      let
        (row, col) = cursorRC ed
        l = rowText row
        -- on punctuation, scan forward to the next word on the line
        -- (vim's * does the same) rather than giving up
        effCol =
          if col < T.length l && isIdent (T.index l col)
            then Just col
            else fmap (col +) (T.findIndex isIdent (T.drop col l))
      in
        case effCol of
          Nothing -> Nothing
          Just c ->
            let
              start =
                T.length (T.dropWhileEnd isIdent (T.take c l))
              len =
                T.length (T.takeWhile isIdent (T.drop start l))
            in
              Just (T.take len (T.drop start l))
  case selectionNeedle <|> wordNeedle of
    Nothing -> modify (\s -> s {stMsg = "no word under cursor"})
    Just needle -> do
      modify
        (\s -> s {stLastSearch = needle, stGrepNav = Nothing, stSearchDir = dir})
      searchStep dir needle

-- `/` searches forward and vi's `?` backward; the prompt wears whichever
-- sigil opened it, and n/N repeat along that grain rather than always
-- downward. In the keymaps with no `?`, the direction is always forward
-- and this is exactly the old behavior.
openSearchPrompt :: Int -> EventM Name St ()
openSearchPrompt dir =
  modify (\s -> s {stPrompt = Just (SearchPrompt ""), stSearchDir = dir})

-- n keeps the search's own direction, N reverses it.
searchRepeat :: Int -> EventM Name St ()
searchRepeat sense = do
  st <- get
  searchNext (sense * stSearchDir st)

-- n/N: when the last search came from the grep picker, walk its hit
-- list across files without re-searching; otherwise repeat the in-file
-- search as always.
searchNext :: Int -> EventM Name St ()
searchNext dir = do
  st <- get
  case stGrepNav st of
    Just (q, hits, ix)
      | not (null hits) -> do
          let
            n = length hits
            ix2 = (((ix + dir) `mod` n) + n) `mod` n
          case safeIdx hits ix2 of
            Just (path, line) -> do
              modify (\s -> s {stGrepNav = Just (q, hits, ix2)})
              openGrepHit path line q
              setMsg ("grep hit " <> tshow (ix2 + 1) <> "/" <> tshow n)
            Nothing -> pure ()
    _ ->
      unless
        (T.null (stLastSearch st))
        (searchStep dir (stLastSearch st))

-- Literal search over the current file's buffer, helix-shaped: the match
-- becomes the selection, n/N walk matches, wrapping at either end.
searchStep :: Int -> Text -> EventM Name St ()
searchStep dir needle = do
  st <- get
  let
    i = stCurF st
    fv = stFiles st !! i
    ed = fvEd fv
    pos = cursorRC ed
    matchesInLine (r, l) =
      map (\(pre, _) -> (r, T.length pre)) (T.breakOnAll needle l)
    matches = concatMap matchesInLine (zip [0 ..] (bufferLines ed))
  case matches of
    [] -> modify (\s -> s {stMsg = "no match: " <> needle})
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
            { edZipper = Z.moveCursor (mr, mc + T.length needle - 1) (edZipper ed)
            , edSelAnchor = Just (mr, mc)
            , edSelKind = CharSel
            }
      setEd i moved
      modify (\s -> s {stMsg = " "})
