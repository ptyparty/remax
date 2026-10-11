{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Model.Nav
  ( withEd
  , matchRun
  , currentRunOf
  , setEd
  , publishCopy
  , pullClipboard
  , park
  , parkM
  , scrollHunkIntoView
  , scrollTrayToCurrent
  , switchRun
  , selectedChanges
  , selectedRuns
  , runChanges
  , hunkUnderCursor
  , parkAnchor
  , jumpToAnchor
  , gotoLine
  , setCurrentHunkHere
  , selectCurrentHunk
  ) where

import Brick
  ( EventM
  , Viewport (..)
  , ViewportScroll (..)
  , getVtyHandle
  , lookupViewport
  , viewportScroll
  )
import Control.Monad
  ( when
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
  ( get
  , modify
  )
import Data.List
  ( find
  , findIndex
  , maximumBy
  )
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( fromMaybe
  , listToMaybe
  , mapMaybe
  )
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Remax.Clipboard
import Remax.Config
import Remax.DiffModel
import Remax.Modal
import Remax.Store
import Remax.UI.Core.Types
import Remax.UI.Core.Verdict
import Remax.UI.Core.View
import Remax.UI.Model.Tray
import qualified Remax.Zipper as Z

withEd :: EdState -> FileView -> FileView
withEd ed fv = fv {fvEd = ed}

-- Which run in the rebuilt list is "the same hunk" as before: the one
-- sharing the most content keys, ties and misses settled by anchor
-- proximity. Content keys make this survive hunks appearing, vanishing,
-- and reshaping above the current one.
matchRun :: Maybe Run -> [Run] -> Int
matchRun mOld runs =
  case (mOld, runs) of
    (_, []) -> 0
    (Nothing, _) -> 0
    (Just old, _) ->
      let
        keySet = Set.fromList (runKeys old)
        score (ix, r) =
          ( length (filter (`Set.member` keySet) (runKeys r))
          , negate (abs (runAnchor r - runAnchor old))
          , ix
          )
        scored = map score (zip [0 :: Int ..] runs)
        (bestScore, _, bestIx) =
          maximumBy (comparing (\(sc, prox, _) -> (sc, prox))) scored
        (_, _, nearIx) =
          maximumBy (comparing (\(_, prox, _) -> prox)) scored
      in
        if bestScore > 0 then bestIx else nearIx

currentRunOf :: St -> Int -> Maybe Run
currentRunOf st i = do
  fv <- safeIdx (stFiles st) i
  let
    rs = fvRuns fv
  safeIdx rs (clampH rs (stCurH st))

setEd :: Int -> EdState -> EventM Name St ()
setEd i ed = do
  modify (\s -> updateFile i (withEd (ed {edCopied = False})) s {stKill = edKill ed})
  publishCopy ed

-- Every keymap's copy lands in the same slot, so this is the one place
-- the system clipboard has to be told: whatever was just deliberately
-- copied goes out. Deletes never set the flag, so pulling a line out of
-- a file cannot clobber what you copied from somewhere else.
publishCopy :: EdState -> EventM Name St ()
publishCopy ed = do
  st <- get
  let
    copied = edKill ed
  when (edCopied ed && not (T.null copied) && copied /= stClipSent st) $ do
    vty <- getVtyHandle
    ok <-
      liftIO
        (writeClipboard (stClip st) (V.outputByteBuffer (V.outputIface vty)) copied)
    when ok (modify (\s -> s {stClipSent = copied}))

-- The other direction, run just before a paste: if the clipboard holds
-- something remax did not put there, that is what you meant to paste.
-- Text arriving from outside carries its own line-wise sense — a copy of
-- whole lines ends in a newline, which is exactly what makes p put them
-- on their own lines.
pullClipboard :: EventM Name St ()
pullClipboard = do
  st <- get
  arrived <- liftIO (readClipboard (stClip st))
  case arrived of
    Just text
      | not (T.null text)
      , text /= stClipSent st ->
          modify (\s -> s {stKill = text, stClipSent = text})
    _ -> pure ()

park :: Int -> Int -> St -> St
park f h st =
  case safeIdx (stFiles st) f of
    Nothing -> st
    Just fv
      | f /= stCurF st ->
          case currentFileMay st of
            Just prev ->
              parkGo f h (st {stAltFile = Just (fvPath prev, stCurH st)}) fv
            Nothing -> parkGo f h st fv
    Just fv -> parkGo f h st fv

parkGo :: Int -> Int -> St -> FileView -> St
parkGo f h st fv =
  let
    rs = fvRuns fv
    clamped = clampH rs h
    parked = st {stCurF = f, stCurH = clamped, stMsg = " "}
    -- land on the first line still needing attention, not merely
    -- the top of the hunk
    target r =
      fromMaybe (runAnchor r) (firstUnacceptedRow st fv clamped)
    moveTo r v =
      v
        { fvEd =
            (fvEd v)
              { edZipper = Z.moveCursor (target r, 0) (edZipper (fvEd v))
              , edSelAnchor = Nothing
              }
        }
  in
    case safeIdx rs clamped of
      Nothing -> parked
      Just r -> updateFile f (moveTo r) parked

-- The buffer row of the first line in a run whose verdict is not
-- Reviewed (removed lines borrow the row they rest before).
firstUnacceptedRow :: St -> FileView -> Int -> Maybe Int
firstUnacceptedRow st fv h =
  let
    rows = fvRows fv
    points = restingPoints fv
    unaccepted (row, point) = do
      k <- rKey row
      if rRunIx row == Just h && Map.lookup k (stVerdicts st) /= Just Reviewed
        then Just (fromMaybe point (rNewRow row))
        else Nothing
  in
    case mapMaybe unaccepted (zip rows points) of
      (r : _) -> Just r
      [] -> Nothing

-- park, then ask the viewport to frame the whole hunk.
parkM :: Int -> Int -> EventM Name St ()
parkM f h = do
  modify (park f h)
  scrollHunkIntoView

-- Frame the current hunk: centered when it fits, top-aligned with a
-- small margin when it is taller than the screen, so as much of it as
-- possible is on screen rather than just its anchor line at the edge.
scrollHunkIntoView :: EventM Name St ()
scrollHunkIntoView = do
  st <- get
  case currentFileMay st of
    Nothing -> pure ()
    Just fv -> scrollHunkIntoViewOf fv

scrollHunkIntoViewOf :: FileView -> EventM Name St ()
scrollHunkIntoViewOf fv = do
  st <- get
  mvp <- lookupViewport BodyVP
  screenRows <- terminalRows
  let
    rs = fvRuns fv
    h = clampH rs (stCurH st)
    inCurrentRun (_, row) = rRunIx row == Just h
    runRows = map fst (filter inCurrentRun (zip [0 :: Int ..] (fvRows fv)))
  case (mvp, runRows) of
    (Just vp, (start : _)) -> do
      let
        len = length runRows
        vpH = snd (_vpSize vp)
        -- the body draws from its window's first row, so the viewport's
        -- coordinates are relative to that same origin. A jump can land
        -- anywhere, so the window is moved with it and written back here
        -- rather than left for the drift check to discover.
        origin = snd (bodyWindow screenRows st fv)
        noteRoom = noteTextWidth (fst (_vpSize vp)) fv
        startIn = max 0 (start - origin)
        -- best-effort correction for virtual note rows rendered above
        -- the hunk (exact when the render window covers them)
        renderStart =
          startIn
            + sum
              ( map
                  length
                  ( take
                      startIn
                      ( drop
                          origin
                          (shownNoteLines noteRoom st (fvRows fv))
                      )
                  )
              )
        top =
          if len + 4 >= vpH
            then max 0 (renderStart - 2)
            else max 0 (renderStart - ((vpH - len) `div` 2))
      modify (updateFile (stCurF st) (\f -> f {fvOrigin = origin}))
      setTop (viewportScroll BodyVP) top
    _ -> pure ()
  -- follow in the tray too — an explicit scroll on hunk jumps only, so
  -- manual tray scrolling is never fought over
  scrollTrayToCurrent

-- Center the tray on the current hunk's entry. Also needed whenever the
-- entry list reshapes under the viewport (the pending filter toggling),
-- or the scroll position strands past the shrunken content.
scrollTrayToCurrent :: EventM Name St ()
scrollTrayToCurrent = do
  st <- get
  mtvp <- lookupViewport TrayVP
  case mtvp of
    Just tvp ->
      setTop
        (viewportScroll TrayVP)
        (max 0 (trayCurrentRow st - (snd (_vpSize tvp) `div` 2)))
    Nothing -> pure ()

-- Walk hunks in (file, hunk) order, wrapping; with pending-only mode on,
-- hunks that already have verdicts are skipped entirely, and the
-- code/comment filter skips whichever kind you are not reading.
switchRun :: Int -> EventM Name St ()
switchRun d = do
  st <- get
  let
    eligible (_, r) =
      (not (stOnlyPending st) || runPending (stVerdicts st) r)
        && runFilterAllows (stRunFilter st) r
    pairs = map fst (filter eligible (allRunsInOrder st))
    here =
      ( stCurF st
      , clampH (fvRuns (currentFile st)) (stCurH st)
      )
    -- name whichever filter emptied the list, so an empty walk is never
    -- mistaken for a finished review
    emptyMsg = case stRunFilter st of
      FilterCode -> "no code hunks left"
      FilterComments -> "no comment hunks left"
      FilterAll -> "no pending hunks left"
  case pairs of
    [] -> modify (\s -> s {stMsg = emptyMsg})
    _ -> do
      let
        after = filter (> here) pairs
        before = filter (< here) pairs
        target =
          if d > 0
            then case after ++ pairs of
              (p : _) -> p
              [] -> here
            else case reverse before ++ reverse pairs of
              (p : _) -> p
              [] -> here
      parkAnchor (fst target) (snd target)

-- Changed lines within a buffer-row span, as (key, marked line, 1-based
-- position) triples. A removed line has no buffer row of its own; it
-- counts when the row it now sits before falls inside the span, and
-- borrows that row's position.
selectedChanges :: Bool -> FileView -> Int -> Int -> [(Text, Text, Int)]
selectedChanges withRemarks fv lo hi =
  let
    rows = fvRows fv
    points = restingPoints fv
    inSpan row point = case rNewRow row of
      Just nr -> nr >= lo && nr <= hi
      Nothing -> point >= lo && point <= hi
    pick (row, point) = do
      k <-
        if rMark row == ' '
          then if withRemarks then rRemarkKey row else Nothing
          else rKey row
      if inSpan row point
        then Just (k, T.cons (rMark row) (rText row), fromMaybe point (rNewRow row) + 1)
        else Nothing
  in
    mapMaybe pick (zip rows points)

-- Which hunks a line span actually touches, by the same reckoning
-- selectedChanges uses — a removed line counts against the buffer row it
-- now rests before, so a deletion is selectable at all.
selectedRuns :: FileView -> Int -> Int -> Set.Set Int
selectedRuns fv lo hi =
  let
    rows = fvRows fv
    points = restingPoints fv
    inSpan row point = case rNewRow row of
      Just nr -> nr >= lo && nr <= hi
      Nothing -> point >= lo && point <= hi
    pick (row, point) = if inSpan row point then rRunIx row else Nothing
  in
    Set.fromList (mapMaybe pick (zip rows points))

-- Every changed line of one run, same shape as selectedChanges.
runChanges :: FileView -> Int -> [(Text, Text, Int)]
runChanges fv h =
  let
    rows = fvRows fv
    points = restingPoints fv
    pick (row, point) = do
      k <- rKey row
      if rRunIx row == Just h
        then Just (k, T.cons (rMark row) (rText row), fromMaybe point (rNewRow row) + 1)
        else Nothing
  in
    mapMaybe pick (zip rows points)

hunkUnderCursor :: St -> Maybe Int
hunkUnderCursor st = do
  fv <- currentFileMay st
  let
    (cur, _) = cursorRC (fvEd fv)
    resting = rowRestPoints fv
    directHit (row, _) =
      if rNewRow row == Just cur then rRunIx row else Nothing
    restingHit (row, point) =
      if rNewRow row == Nothing && point == cur then rRunIx row else Nothing
    -- Several delete-only hunks can rest at one line — an all-removed
    -- file rests every one of them at line 0 — and then the cursor
    -- cannot say which is meant. The hunk under review is the honest
    -- answer: it is the one ] and [ moved to and the one the bar marks.
    restingOn = mapMaybe restingHit resting
    onReview = clampH (fvRuns fv) (stCurH st)
  case mapMaybe directHit resting of
    (h : _) -> Just h
    [] ->
      case find (== onReview) restingOn of
        Just h -> Just h
        Nothing -> listToMaybe restingOn

-- Review-flow navigation parks AND drops the review anchor there, so '
-- can always come back; investigation moves (pickers, grep, ga, open)
-- park without touching it.
parkAnchor :: Int -> Int -> EventM Name St ()
parkAnchor f h = do
  parkM f h
  st <- get
  case currentFileMay st of
    Just fv ->
      modify $ \s ->
        s {stAnchor = Just (fvPath fv, clampH (fvRuns fv) (stCurH st))}
    Nothing -> pure ()

-- ': jump back to the hunk under review, wherever the investigation
-- wandered — across files included. With no anchor yet, it reframes
-- the current hunk as before.
jumpToAnchor :: EventM Name St ()
jumpToAnchor = do
  st <- get
  case stAnchor st of
    Just (path, h) ->
      case findIndex ((== path) . fvPath) (stFiles st) of
        Just f -> parkM f h
        Nothing -> setMsg ("anchored file left the session: " <> T.pack path)
    Nothing -> parkM (stCurF st) (stCurH st)

-- :N — park the cursor on line N of the current file, 1-based.
-- Z.moveCursor clamps, so :0 lands on the first line and a number past
-- the end lands on the last. This is an investigation move: the '
-- anchor and the hunk under review both stay where they are.
gotoLine :: Int -> EventM Name St ()
gotoLine n = do
  st <- get
  case currentFileMay st of
    Nothing -> setMsg "no file on screen"
    Just fv ->
      let
        ed = fvEd fv
        move = Z.moveCursor (n - 1, 0)
      in
        setEd (stCurF st) $
          if cfgKeys (stConfig st) == KeysEmacs
            -- emacs: point moves, the mark stays, so the region grows
            then ed {edZipper = move (edZipper ed), edInsertEntry = Nothing}
            else plainMove move ed

-- Space h: make the hunk the cursor is standing in the current one —
-- the current hunk being what a selection-less X, ! or ? acts on, and
-- what ' comes home to. The deliberate counterpart of ': that one goes
-- back to where you were reviewing, this one says review here instead.
setCurrentHunkHere :: EventM Name St ()
setCurrentHunkHere = do
  st <- get
  case hunkUnderCursor st of
    Nothing -> setMsg "cursor isn't in a hunk"
    Just h -> do
      modify (\s -> s {stCurH = h, stMsg = " "})
      moved <- get
      case currentFileMay moved of
        Just fv -> modify (\s -> s {stAnchor = Just (fvPath fv, h)})
        Nothing -> pure ()

selectCurrentHunk :: EventM Name St ()
selectCurrentHunk = do
  st <- get
  let
    fv = currentFile st
    inCurrentRun row =
      rRunIx row == Just (clampH (fvRuns fv) (stCurH st))
    hunkRows = mapMaybe rNewRow (filter inCurrentRun (fvRows fv))
    -- a delete-only hunk has no buffer rows of its own; its span is the
    -- anchor line the deletion sits at
    pickSpan =
      case (hunkRows, currentRunOf st (stCurF st)) of
        ((first : rest), _) ->
          case reverse (first : rest) of
            (lastRow : _) -> Just (first, lastRow)
            [] -> Nothing
        ([], Just r) -> Just (runAnchor r, runAnchor r)
        _ -> Nothing
  if hunkUnderCursor st /= Just (clampH (fvRuns fv) (stCurH st))
    then setMsg "cursor isn't in the current hunk — Space h makes this one current"
    else case pickSpan of
      Nothing -> pure ()
      Just (first, lastRow) -> do
        let
          ed = fvEd fv
          selected =
            ed
              { edZipper = Z.moveCursor (lastRow, 0) (edZipper ed)
              , edSelAnchor = Just (first, 0)
              , edSelKind = LineSel
              }
        setEd (stCurF st) selected
