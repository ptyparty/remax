{-# LANGUAGE OverloadedStrings #-}

-- Emacs-flavored editing: modeless and chord-driven. The mark-based
-- region maps onto the same selection the review verbs consume, so
-- C-space then movement highlights exactly what C-c Enter would accept.
-- Region boundaries follow emacs (mark to point, point exclusive)
-- rather than helix's inclusive cells.
module Remax.Emacs
  ( handleEmacs
  ) where

import Data.Char (isAlphaNum, isControl)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as V

import Remax.Modal
import qualified Remax.Zipper as Z

handleEmacs :: V.Event -> EdState -> Maybe EdState
handleEmacs ev ed =
  case ev of
    -- movement (an active mark extends the region, like transient-mark)
    V.EvKey (V.KChar 'f') [V.MCtrl] -> Just (onZipper Z.moveRight ed)
    V.EvKey V.KRight [] -> Just (onZipper Z.moveRight ed)
    V.EvKey (V.KChar 'b') [V.MCtrl] -> Just (onZipper Z.moveLeft ed)
    V.EvKey V.KLeft [] -> Just (onZipper Z.moveLeft ed)
    V.EvKey (V.KChar 'n') [V.MCtrl] -> Just (onZipper Z.moveDown ed)
    V.EvKey V.KDown [] -> Just (onZipper Z.moveDown ed)
    V.EvKey (V.KChar 'p') [V.MCtrl] -> Just (onZipper Z.moveUp ed)
    V.EvKey V.KUp [] -> Just (onZipper Z.moveUp ed)
    V.EvKey (V.KChar 'a') [V.MCtrl] -> Just (onZipper Z.gotoBOL ed)
    V.EvKey V.KHome [] -> Just (onZipper Z.gotoBOL ed)
    V.EvKey (V.KChar 'e') [V.MCtrl] -> Just (onZipper Z.gotoEOL ed)
    V.EvKey V.KEnd [] -> Just (onZipper Z.gotoEOL ed)
    V.EvKey (V.KChar 'f') [V.MMeta] -> Just (moveTo (forwardWord ed) ed)
    V.EvKey (V.KChar 'b') [V.MMeta] -> Just (moveTo (backwardWord ed) ed)
    V.EvKey (V.KChar '<') [V.MMeta] -> Just (moveTo (0, 0) ed)
    V.EvKey (V.KChar '>') [V.MMeta] ->
      let
        ls = bufferLines ed
        lastRow = max 0 (length ls - 1)
        lastCol = case reverse ls of
          (l : _) -> T.length l
          [] -> 0
      in
        Just (moveTo (lastRow, lastCol) ed)
    -- mark and region
    V.EvKey (V.KChar ' ') [V.MCtrl] -> Just (setMark ed)
    V.EvKey (V.KChar '@') [V.MCtrl] -> Just (setMark ed)
    V.EvKey (V.KChar 'g') [V.MCtrl] -> Just (clearMark ed)
    V.EvKey V.KEsc [] -> Just (clearMark ed)
    V.EvKey (V.KChar 'w') [V.MCtrl] -> killRegion True ed
    V.EvKey (V.KChar 'w') [V.MMeta] -> killRegion False ed
    -- killing and yanking
    V.EvKey (V.KChar 'k') [V.MCtrl] -> Just (killToEOL ed)
    V.EvKey (V.KChar 'y') [V.MCtrl] -> Just (yank ed)
    -- deleting
    V.EvKey (V.KChar 'd') [V.MCtrl] -> Just (destructive Z.deleteChar ed)
    V.EvKey V.KDel [] -> Just (destructive Z.deleteChar ed)
    V.EvKey V.KBS [] -> Just (destructive Z.deletePrevChar ed)
    -- undo (terminals send C-/ as C-_)
    V.EvKey (V.KChar '_') [V.MCtrl] -> Just (undoOnce ed)
    V.EvKey (V.KChar '/') [V.MCtrl] -> Just (undoOnce ed)
    -- inserting
    V.EvKey V.KEnter [] -> Just (destructive Z.breakLine ed)
    V.EvKey (V.KChar '\t') [] -> Just (selfInsertText "  " ed)
    V.EvPaste bytes ->
      Just (destructive (Z.insertMany (TE.decodeUtf8 bytes)) ed)
    V.EvKey (V.KChar c) []
      | not (isControl c) -> Just (selfInsert c ed)
    _ -> Nothing

-- Plain zipper movement; the mark (selection anchor) stays put.
onZipper :: (Z.TextZipper Text -> Z.TextZipper Text) -> EdState -> EdState
onZipper f ed = breakInsertRun ed {edZipper = f (edZipper ed)}

moveTo :: (Int, Int) -> EdState -> EdState
moveTo pos = onZipper (Z.moveCursor pos)

setMark :: EdState -> EdState
setMark ed =
  breakInsertRun
    ed {edSelAnchor = Just (cursorRC ed), edSelKind = CharSel}

clearMark :: EdState -> EdState
clearMark ed = breakInsertRun ed {edSelAnchor = Nothing}

-- Self-inserts share one undo snapshot per typing run; any other
-- command ends the run. edInsertEntry doubles as the run flag — the
-- helix insert machinery it belongs to is idle in emacs mode.
breakInsertRun :: EdState -> EdState
breakInsertRun ed = ed {edInsertEntry = Nothing}

selfInsert :: Char -> EdState -> EdState
selfInsert c ed =
  let
    armed = case edInsertEntry ed of
      Just _ -> ed
      Nothing -> (pushUndo ed) {edInsertEntry = Just EntryI}
  in
    armed {edZipper = Z.insertChar c (edZipper armed)}

selfInsertText :: Text -> EdState -> EdState
selfInsertText t ed =
  let
    armed = case edInsertEntry ed of
      Just _ -> ed
      Nothing -> (pushUndo ed) {edInsertEntry = Just EntryI}
  in
    armed {edZipper = Z.insertMany t (edZipper armed)}

destructive :: (Z.TextZipper Text -> Z.TextZipper Text) -> EdState -> EdState
destructive f ed =
  let
    backed = pushUndo (breakInsertRun ed)
  in
    backed {edZipper = f (edZipper backed)}

pushUndo :: EdState -> EdState
pushUndo ed =
  ed
    { edUndo = take 100 (edZipper ed : edUndo ed)
    , edTouched = True
    }

undoOnce :: EdState -> EdState
undoOnce ed =
  case edUndo ed of
    [] -> ed
    (z : rest) ->
      breakInsertRun
        ed {edZipper = z, edUndo = rest, edSelAnchor = Nothing}

-- C-k: kill to end of line, or the newline itself when already there.
killToEOL :: EdState -> EdState
killToEOL ed =
  let
    (row, col) = cursorRC ed
    ls = bufferLines ed
    line = case drop row ls of
      (l : _) -> l
      [] -> T.empty
    backed = pushUndo (breakInsertRun ed)
  in
    if col < T.length line
      then
        backed
          { edZipper = Z.moveCursor (row, col) (Z.textZipper (replaceAt row (T.take col line) ls) Nothing)
          , edKill = T.drop col line
          }
      else
        backed
          { edZipper = Z.deleteChar (edZipper backed)
          , edKill = "\n"
          }

yank :: EdState -> EdState
yank ed =
  if T.null (edKill ed)
    then ed
    else destructive (Z.insertMany (edKill ed)) ed

-- The emacs region: mark to point, point-exclusive. Nothing when no
-- mark is set (the caller leaves the event unhandled → no-op).
killRegion :: Bool -> EdState -> Maybe EdState
killRegion alsoDelete ed = do
  anchor <- edSelAnchor ed
  let
    point = cursorRC ed
    (lo, hi) = if anchor <= point then (anchor, point) else (point, anchor)
    ls = bufferLines ed
    killed = extractRegion ls lo hi
  if alsoDelete
    then
      let
        backed = pushUndo (breakInsertRun ed)
        remaining = deleteRegion ls lo hi
      in
        Just
          backed
            { edZipper = Z.moveCursor lo (Z.textZipper remaining Nothing)
            , edKill = killed
            , edSelAnchor = Nothing
            }
    else
      Just
        (breakInsertRun ed {edKill = killed, edSelAnchor = Nothing})

extractRegion :: [Text] -> (Int, Int) -> (Int, Int) -> Text
extractRegion ls (lr, lc) (hr, hc) =
  let
    lineAt i = case drop i ls of
      (l : _) -> l
      [] -> T.empty
  in
    if lr == hr
      then T.take (hc - lc) (T.drop lc (lineAt lr))
      else
        T.intercalate
          "\n"
          ( T.drop lc (lineAt lr)
              : map lineAt [lr + 1 .. hr - 1]
              ++ [T.take hc (lineAt hr)]
          )

deleteRegion :: [Text] -> (Int, Int) -> (Int, Int) -> [Text]
deleteRegion ls (lr, lc) (hr, hc) =
  let
    lineAt i = case drop i ls of
      (l : _) -> l
      [] -> T.empty
    merged = T.take lc (lineAt lr) <> T.drop hc (lineAt hr)
  in
    take lr ls ++ (merged : drop (hr + 1) ls)

replaceAt :: Int -> Text -> [Text] -> [Text]
replaceAt i new ls = take i ls ++ (new : drop (i + 1) ls)

-- forward-word / backward-word, crossing line boundaries.
wordChar :: Char -> Bool
wordChar c = isAlphaNum c || c == '_'

forwardWord :: EdState -> (Int, Int)
forwardWord ed =
  let
    ls = bufferLines ed
    step (r, c) =
      let
        line = case drop r ls of
          (l : _) -> l
          [] -> T.empty
      in
        if c < T.length line
          then Just (T.index line c, (r, c + 1))
          else
            if r + 1 < length ls
              then Just ('\n', (r + 1, 0))
              else Nothing
    skip p pos = case step pos of
      Just (ch, next)
        | p ch -> skip p next
      _ -> pos
  in
    skip wordChar (skip (not . wordChar) (cursorRC ed))

backwardWord :: EdState -> (Int, Int)
backwardWord ed =
  let
    ls = bufferLines ed
    back (r, c) =
      if c > 0
        then Just ((r, c - 1), T.index (lineAt r) (c - 1))
        else
          if r > 0
            then Just ((r - 1, T.length (lineAt (r - 1))), '\n')
            else Nothing
    lineAt i = case drop i ls of
      (l : _) -> l
      [] -> T.empty
    skip p pos = case back pos of
      Just (prev, ch)
        | p ch -> skip p prev
      _ -> pos
  in
    skip wordChar (skip (not . wordChar) (cursorRC ed))
