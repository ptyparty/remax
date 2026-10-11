{-# LANGUAGE OverloadedStrings #-}

-- Emacs-flavored editing: modeless and chord-driven. The mark-based
-- region maps onto the same selection the review verbs consume, so
-- C-space then movement highlights exactly what C-c Enter would accept.
-- Region boundaries follow emacs (mark to point, point exclusive)
-- rather than helix's inclusive cells.
module Remax.Emacs
  ( handleEmacs
  ) where

import Data.Char qualified as Char
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Graphics.Vty qualified as V

import Remax.Modal qualified as Modal
import Remax.Zipper qualified as Z

handleEmacs :: V.Event -> Modal.EdState -> Maybe Modal.EdState
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
        ls = Modal.bufferLines ed
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
      | not (Char.isControl c) -> Just (selfInsert c ed)
    _ -> Nothing

-- Plain zipper movement; the mark (selection anchor) stays put.
onZipper :: (Z.TextZipper T.Text -> Z.TextZipper T.Text) -> Modal.EdState -> Modal.EdState
onZipper f ed = breakInsertRun ed {Modal.edZipper = f (Modal.edZipper ed)}

moveTo :: (Int, Int) -> Modal.EdState -> Modal.EdState
moveTo pos = onZipper (Z.moveCursor pos)

setMark :: Modal.EdState -> Modal.EdState
setMark ed =
  breakInsertRun
    ed {Modal.edSelAnchor = Just (Modal.cursorRC ed), Modal.edSelKind = Modal.CharSel}

clearMark :: Modal.EdState -> Modal.EdState
clearMark ed = breakInsertRun ed {Modal.edSelAnchor = Nothing}

-- Self-inserts share one undo snapshot per typing run; any other
-- command ends the run. edInsertEntry doubles as the run flag — the
-- helix insert machinery it belongs to is idle in emacs mode.
breakInsertRun :: Modal.EdState -> Modal.EdState
breakInsertRun ed = ed {Modal.edInsertEntry = Nothing}

selfInsert :: Char -> Modal.EdState -> Modal.EdState
selfInsert c ed =
  let
    armed = case Modal.edInsertEntry ed of
      Just _ -> ed
      Nothing -> (pushUndo ed) {Modal.edInsertEntry = Just Modal.EntryI}
  in
    armed {Modal.edZipper = Z.insertChar c (Modal.edZipper armed)}

selfInsertText :: T.Text -> Modal.EdState -> Modal.EdState
selfInsertText t ed =
  let
    armed = case Modal.edInsertEntry ed of
      Just _ -> ed
      Nothing -> (pushUndo ed) {Modal.edInsertEntry = Just Modal.EntryI}
  in
    armed {Modal.edZipper = Z.insertMany t (Modal.edZipper armed)}

destructive :: (Z.TextZipper T.Text -> Z.TextZipper T.Text) -> Modal.EdState -> Modal.EdState
destructive f ed =
  let
    backed = pushUndo (breakInsertRun ed)
  in
    backed {Modal.edZipper = f (Modal.edZipper backed)}

pushUndo :: Modal.EdState -> Modal.EdState
pushUndo ed =
  ed
    { Modal.edUndo = take 100 (Modal.edZipper ed : Modal.edUndo ed)
    , Modal.edRedo = []
    , Modal.edTouched = True
    }

undoOnce :: Modal.EdState -> Modal.EdState
undoOnce ed =
  case Modal.edUndo ed of
    [] -> ed
    (z : rest) ->
      breakInsertRun
        ed
          { Modal.edZipper = z
          , Modal.edUndo = rest
          , Modal.edRedo = take 100 (Modal.edZipper ed : Modal.edRedo ed)
          , Modal.edSelAnchor = Nothing
          }

-- C-k: kill to end of line, or the newline itself when already there.
killToEOL :: Modal.EdState -> Modal.EdState
killToEOL ed =
  let
    (row, col) = Modal.cursorRC ed
    ls = Modal.bufferLines ed
    line = case drop row ls of
      (l : _) -> l
      [] -> T.empty
    backed = pushUndo (breakInsertRun ed)
  in
    if col < T.length line
      then
        backed
          { Modal.edZipper = Z.moveCursor (row, col) (Z.textZipper (replaceAt row (T.take col line) ls) Nothing)
          , Modal.edKill = T.drop col line
          }
      else
        backed
          { Modal.edZipper = Z.deleteChar (Modal.edZipper backed)
          , Modal.edKill = "\n"
          }

yank :: Modal.EdState -> Modal.EdState
yank ed =
  if T.null (Modal.edKill ed)
    then ed
    else destructive (Z.insertMany (Modal.edKill ed)) ed

-- The emacs region: mark to point, point-exclusive. Nothing when no
-- mark is set (the caller leaves the event unhandled → no-op).
killRegion :: Bool -> Modal.EdState -> Maybe Modal.EdState
killRegion alsoDelete ed = do
  anchor <- Modal.edSelAnchor ed
  let
    point = Modal.cursorRC ed
    (lo, hi) = if anchor <= point then (anchor, point) else (point, anchor)
    ls = Modal.bufferLines ed
    killed = extractRegion ls lo hi
  if alsoDelete
    then
      let
        backed = pushUndo (breakInsertRun ed)
        remaining = deleteRegion ls lo hi
      in
        Just
          backed
            { Modal.edZipper = Z.moveCursor lo (Z.textZipper remaining Nothing)
            , Modal.edKill = killed
            , Modal.edSelAnchor = Nothing
            }
    else
      -- M-w is the copy; C-w and C-k cut, and a cut is a removal rather
      -- than a request to reach the system clipboard
      Just
        ( breakInsertRun
            ed {Modal.edKill = killed, Modal.edSelAnchor = Nothing, Modal.edCopied = True}
        )

extractRegion :: [T.Text] -> (Int, Int) -> (Int, Int) -> T.Text
extractRegion ls (lr, lc) (hr, hc) =
  let
    row = Modal.lineAt ls
  in
    if lr == hr
      then T.take (hc - lc) (T.drop lc (row lr))
      else
        T.intercalate
          "\n"
          ( T.drop lc (row lr)
              : map row [lr + 1 .. hr - 1]
              ++ [T.take hc (row hr)]
          )

deleteRegion :: [T.Text] -> (Int, Int) -> (Int, Int) -> [T.Text]
deleteRegion ls (lr, lc) (hr, hc) =
  let
    merged = T.take lc (Modal.lineAt ls lr) <> T.drop hc (Modal.lineAt ls hr)
  in
    take lr ls ++ (merged : drop (hr + 1) ls)

replaceAt :: Int -> T.Text -> [T.Text] -> [T.Text]
replaceAt i new ls = take i ls ++ (new : drop (i + 1) ls)

-- forward-word / backward-word, crossing line boundaries.
wordChar :: Char -> Bool
wordChar c = Char.isAlphaNum c || c == '_'

forwardWord :: Modal.EdState -> (Int, Int)
forwardWord ed =
  let
    ls = Modal.bufferLines ed
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
    skip wordChar (skip (not . wordChar) (Modal.cursorRC ed))

backwardWord :: Modal.EdState -> (Int, Int)
backwardWord ed =
  let
    ls = Modal.bufferLines ed
    back (r, c) =
      if c > 0
        then Just ((r, c - 1), T.index (Modal.lineAt ls r) (c - 1))
        else
          if r > 0
            then Just ((r - 1, T.length (Modal.lineAt ls (r - 1))), '\n')
            else Nothing
    skip p pos = case back pos of
      Just (prev, ch)
        | p ch -> skip p prev
      _ -> pos
  in
    skip wordChar (skip (not . wordChar) (Modal.cursorRC ed))
