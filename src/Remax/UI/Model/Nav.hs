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
  , RunStep (..)
  , planRun
  , takeRun
  , selectedChanges
  , selectedRuns
  , runChanges
  , hunkUnderCursor
  , parkAnchor
  , jumpToAnchor
  , parkOnLine
  , gotoLine
  , setCurrentHunkHere
  , selectCurrentHunk
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Ord qualified as Ord
import Data.Set qualified as Set
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Remax.Clipboard qualified as Clipboard
import Remax.Config qualified as Config
import Remax.DiffModel qualified as DiffModel
import Remax.Modal qualified as Modal
import Remax.Store qualified as Store
import Remax.UI.Core.Anchor qualified as Anchor
import Remax.UI.Core.Threads qualified as Threads
import Remax.UI.Core.Tour qualified as Tour
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Verdict qualified as Verdict
import Remax.UI.Core.View qualified as View
import Remax.UI.Model.Tray qualified as TrayModel
import Remax.Zipper qualified as Z

withEd :: Modal.EdState -> Types.FileView -> Types.FileView
withEd ed fv = fv {Types.fvEd = ed}

-- Which run in the rebuilt list is "the same hunk" as before: the one
-- sharing the most content keys, ties and misses settled by anchor
-- proximity. Content keys make this survive hunks appearing, vanishing,
-- and reshaping above the current one.
matchRun :: Maybe DiffModel.Run -> [DiffModel.Run] -> Int
matchRun mOld runs =
  case (mOld, runs) of
    (_, []) -> 0
    (Nothing, _) -> 0
    (Just old, _) ->
      let
        keySet = Set.fromList (DiffModel.runKeys old)
        score (ix, r) =
          ( length (filter (`Set.member` keySet) (DiffModel.runKeys r))
          , negate (abs (DiffModel.runAnchor r - DiffModel.runAnchor old))
          , ix
          )
        scored = map score (zip [0 :: Int ..] runs)
        (bestScore, _, bestIx) =
          List.maximumBy (Ord.comparing (\(sc, prox, _) -> (sc, prox))) scored
        (_, _, nearIx) =
          List.maximumBy (Ord.comparing (\(_, prox, _) -> prox)) scored
      in
        if bestScore > 0 then bestIx else nearIx

currentRunOf :: Types.St -> Int -> Maybe DiffModel.Run
currentRunOf st i = do
  fv <- Types.safeIdx (Types.stFiles st) i
  let
    rs = Types.fvRuns fv
  Types.safeIdx rs (Types.clampH rs (Types.stCurH st))

setEd :: Int -> Modal.EdState -> Brick.EventM Types.Name Types.St ()
setEd i ed = do
  State.modify (\s -> Verdict.updateFile i (withEd (ed {Modal.edCopied = False})) s {Types.stKill = Modal.edKill ed})
  publishCopy ed

-- Every keymap's copy lands in the same slot, so this is the one place
-- the system clipboard has to be told: whatever was just deliberately
-- copied goes out. Deletes never set the flag, so pulling a line out of
-- a file cannot clobber what you copied from somewhere else.
publishCopy :: Modal.EdState -> Brick.EventM Types.Name Types.St ()
publishCopy ed = do
  st <- State.get
  let
    copied = Modal.edKill ed
  Monad.when (Modal.edCopied ed && not (T.null copied) && copied /= Types.stClipSent st) $ do
    vty <- Brick.getVtyHandle
    ok <-
      MonadIO.liftIO
        (Clipboard.writeClipboard (Types.stClip st) (V.outputByteBuffer (V.outputIface vty)) copied)
    Monad.when ok (State.modify (\s -> s {Types.stClipSent = copied}))

-- The other direction, run just before a paste: if the clipboard holds
-- something remax did not put there, that is what you meant to paste.
-- Text arriving from outside carries its own line-wise sense — a copy of
-- whole lines ends in a newline, which is exactly what makes p put them
-- on their own lines.
pullClipboard :: Brick.EventM Types.Name Types.St ()
pullClipboard = do
  st <- State.get
  arrived <- MonadIO.liftIO (Clipboard.readClipboard (Types.stClip st))
  case arrived of
    Just text
      | not (T.null text)
      , text /= Types.stClipSent st ->
          State.modify (\s -> s {Types.stKill = text, Types.stClipSent = text})
    _ -> pure ()

park :: Int -> Int -> Types.St -> Types.St
park f h st =
  case Types.safeIdx (Types.stFiles st) f of
    Nothing -> st
    Just fv
      | f /= Types.stCurF st ->
          case Types.currentFileMay st of
            Just prev ->
              parkGo f h (st {Types.stAltFile = Just (Types.fvPath prev, Types.stCurH st)}) fv
            Nothing -> parkGo f h st fv
    Just fv -> parkGo f h st fv

parkGo :: Int -> Int -> Types.St -> Types.FileView -> Types.St
parkGo f h st fv =
  let
    rs = Types.fvRuns fv
    clamped = Types.clampH rs h
    parked = st {Types.stCurF = f, Types.stCurH = clamped, Types.stMsg = " "}
    -- land on the first line still needing attention, not merely
    -- the top of the hunk
    target r =
      Maybe.fromMaybe (DiffModel.runAnchor r) (firstUnacceptedRow st fv clamped)
    moveTo r v =
      v
        { Types.fvEd =
            (Types.fvEd v)
              { Modal.edZipper = Z.moveCursor (target r, 0) (Modal.edZipper (Types.fvEd v))
              , Modal.edSelAnchor = Nothing
              }
        }
  in
    case Types.safeIdx rs clamped of
      Nothing -> parked
      Just r -> Verdict.updateFile f (moveTo r) parked

-- The buffer row of the first line in a run whose verdict is not
-- Reviewed (removed lines borrow the row they rest before).
firstUnacceptedRow :: Types.St -> Types.FileView -> Int -> Maybe Int
firstUnacceptedRow st fv h =
  let
    rows = Types.fvRows fv
    points = View.restingPoints fv
    unaccepted (row, point) = do
      k <- DiffModel.rKey row
      if DiffModel.rRunIx row == Just h && Map.lookup k (Types.stVerdicts st) /= Just Store.Reviewed
        then Just (Maybe.fromMaybe point (DiffModel.rNewRow row))
        else Nothing
  in
    case Maybe.mapMaybe unaccepted (zip rows points) of
      (r : _) -> Just r
      [] -> Nothing

-- park, then ask the viewport to frame the whole hunk.
parkM :: Int -> Int -> Brick.EventM Types.Name Types.St ()
parkM f h = do
  State.modify (park f h)
  scrollHunkIntoView

-- Frame the current hunk: centered when it fits, top-aligned with a
-- small margin when it is taller than the screen, so as much of it as
-- possible is on screen rather than just its anchor line at the edge.
scrollHunkIntoView :: Brick.EventM Types.Name Types.St ()
scrollHunkIntoView = do
  st <- State.get
  case Types.currentFileMay st of
    Nothing -> pure ()
    Just fv -> scrollHunkIntoViewOf fv

scrollHunkIntoViewOf :: Types.FileView -> Brick.EventM Types.Name Types.St ()
scrollHunkIntoViewOf fv = do
  st <- State.get
  mvp <- Brick.lookupViewport Types.BodyVP
  screenRows <- View.terminalRows
  let
    rs = Types.fvRuns fv
    h = Types.clampH rs (Types.stCurH st)
    inCurrentRun (_, row) = DiffModel.rRunIx row == Just h
    runRows = map fst (filter inCurrentRun (zip [0 :: Int ..] (Types.fvRows fv)))
  case (mvp, runRows) of
    (Just vp, (start : _)) -> do
      let
        len = length runRows
        vpH = snd (Brick._vpSize vp)
        -- the body draws from its window's first row, so the viewport's
        -- coordinates are relative to that same origin. A jump can land
        -- anywhere, so the window is moved with it and written back here
        -- rather than left for the drift check to discover.
        origin = snd (View.bodyWindow screenRows st fv)
        noteRoom = View.noteTextWidth (fst (Brick._vpSize vp)) fv
        startIn = max 0 (start - origin)
        -- best-effort correction for virtual note rows rendered above
        -- the hunk (exact when the render window covers them)
        renderStart =
          startIn
            + sum (map (Threads.rowLineCount st noteRoom . Anchor.conversationsAt st fv) [origin .. origin + startIn - 1])
            + sum (map length (take (startIn + 1) (drop origin (View.shownGuideLines noteRoom st (Types.fvRows fv)))))
        top =
          if len + 4 >= vpH
            then max 0 (renderStart - 2)
            else max 0 (renderStart - ((vpH - len) `div` 2))
      State.modify (Verdict.updateFile (Types.stCurF st) (\f -> f {Types.fvOrigin = origin}))
      Brick.setTop (Brick.viewportScroll Types.BodyVP) top
    _ -> pure ()
  -- follow in the tray too — an explicit scroll on hunk jumps only, so
  -- manual tray scrolling is never fought over
  scrollTrayToCurrent

-- Center the tray on the current hunk's entry. Also needed whenever the
-- entry list reshapes under the viewport (the pending filter toggling),
-- or the scroll position strands past the shrunken content.
scrollTrayToCurrent :: Brick.EventM Types.Name Types.St ()
scrollTrayToCurrent = do
  st <- State.get
  mtvp <- Brick.lookupViewport Types.TrayVP
  case mtvp of
    Just tvp ->
      Brick.setTop
        (Brick.viewportScroll Types.TrayVP)
        (max 0 (TrayModel.trayCurrentRow st - (snd (Brick._vpSize tvp) `div` 2)))
    Nothing -> pure ()

data RunStep
  = RunTo (Int, Int)
  | RunWraps (Int, Int)
  | RunEmpty T.Text

-- Walk hunks in (file, hunk) order, wrapping; with pending-only mode on,
-- hunks that already have verdicts are skipped entirely, and the
-- code/comment filter skips whichever kind you are not reading.
planRun :: Types.St -> Int -> RunStep
planRun st d =
  let
    eligible (_, r) = Verdict.walkAllows st r
    pairs = case Types.stWalk st of
      Types.WalkTour -> Tour.tourOrder st
      _ -> map fst (filter eligible (Verdict.allRunsInOrder st))
    here =
      ( Types.stCurF st
      , Types.clampH (Types.fvRuns (Types.currentFile st)) (Types.stCurH st)
      )
    -- name whichever filter emptied the list, so an empty walk is never
    -- mistaken for a finished review
    emptyMsg = case Types.stWalk st of
      Types.WalkUnresolved -> "nothing outstanding"
      Types.WalkTour -> "no tour steps here"
      _ -> "no pending hunks left"
  in
    case (NEL.nonEmpty pairs, Types.stWalk st) of
      (Nothing, _) -> RunEmpty emptyMsg
      (Just _, Types.WalkTour) -> RunTo (stepAlong pairs here d)
      (Just found, _) -> stepPast found here d

takeRun :: RunStep -> Brick.EventM Types.Name Types.St ()
takeRun step =
  case step of
    RunEmpty msg -> State.modify (\s -> s {Types.stMsg = msg})
    RunTo target -> landRun target
    RunWraps target -> landRun target

landRun :: (Int, Int) -> Brick.EventM Types.Name Types.St ()
landRun target = do
  st <- State.get
  parkAnchor (fst target) (snd target)
  Monad.when (Types.stWalk st == Types.WalkTour) $
    State.modify (\s -> s {Types.stTourLast = Just target})

stepPast :: NEL.NonEmpty (Int, Int) -> (Int, Int) -> Int -> RunStep
stepPast pairs here d =
  let
    after = NEL.filter (> here) pairs
    before = NEL.filter (< here) pairs
  in
    if d > 0
      then maybe (RunWraps (NEL.head pairs)) RunTo (Maybe.listToMaybe after)
      else maybe (RunWraps (NEL.last pairs)) RunTo (Maybe.listToMaybe (reverse before))

stepAlong :: [(Int, Int)] -> (Int, Int) -> Int -> (Int, Int)
stepAlong pairs here d =
  let
    n = length pairs
    at i = Maybe.fromMaybe here (Types.safeIdx pairs (i `mod` n))
  in
    case List.elemIndex here pairs of
      Just i -> at (i + d)
      Nothing -> if d > 0 then at 0 else at (n - 1)

-- Changed lines within a buffer-row span, as (key, marked line, 1-based
-- position) triples. A removed line has no buffer row of its own; it
-- counts when the row it now sits before falls inside the span, and
-- borrows that row's position.
selectedChanges :: Bool -> Types.FileView -> Int -> Int -> [(T.Text, T.Text, Int)]
selectedChanges withRemarks fv lo hi =
  let
    rows = Types.fvRows fv
    points = View.restingPoints fv
    inSpan row point = case DiffModel.rNewRow row of
      Just nr -> nr >= lo && nr <= hi
      Nothing -> point >= lo && point <= hi
    pick (row, point) = do
      k <-
        if DiffModel.rMark row == ' '
          then if withRemarks then DiffModel.rRemarkKey row else Nothing
          else DiffModel.rKey row
      if inSpan row point
        then Just (k, T.cons (DiffModel.rMark row) (DiffModel.rText row), Maybe.fromMaybe point (DiffModel.rNewRow row) + 1)
        else Nothing
  in
    Maybe.mapMaybe pick (zip rows points)

-- Which hunks a line span actually touches, by the same reckoning
-- selectedChanges uses — a removed line counts against the buffer row it
-- now rests before, so a deletion is selectable at all.
selectedRuns :: Types.FileView -> Int -> Int -> Set.Set Int
selectedRuns fv lo hi =
  let
    rows = Types.fvRows fv
    points = View.restingPoints fv
    inSpan row point = case DiffModel.rNewRow row of
      Just nr -> nr >= lo && nr <= hi
      Nothing -> point >= lo && point <= hi
    pick (row, point) = if inSpan row point then DiffModel.rRunIx row else Nothing
  in
    Set.fromList (Maybe.mapMaybe pick (zip rows points))

-- Every changed line of one run, same shape as selectedChanges.
runChanges :: Types.FileView -> Int -> [(T.Text, T.Text, Int)]
runChanges fv h =
  let
    rows = Types.fvRows fv
    points = View.restingPoints fv
    pick (row, point) = do
      k <- DiffModel.rKey row
      if DiffModel.rRunIx row == Just h
        then Just (k, T.cons (DiffModel.rMark row) (DiffModel.rText row), Maybe.fromMaybe point (DiffModel.rNewRow row) + 1)
        else Nothing
  in
    Maybe.mapMaybe pick (zip rows points)

hunkUnderCursor :: Types.St -> Maybe Int
hunkUnderCursor st = do
  fv <- Types.currentFileMay st
  let
    (cur, _) = Modal.cursorRC (Types.fvEd fv)
    resting = View.rowRestPoints fv
    directHit (row, _) =
      if DiffModel.rNewRow row == Just cur then DiffModel.rRunIx row else Nothing
    restingHit (row, point) =
      if DiffModel.rNewRow row == Nothing && point == cur then DiffModel.rRunIx row else Nothing
    -- Several delete-only hunks can rest at one line — an all-removed
    -- file rests every one of them at line 0 — and then the cursor
    -- cannot say which is meant. The hunk under review is the honest
    -- answer: it is the one ] and [ moved to and the one the bar marks.
    restingOn = Maybe.mapMaybe restingHit resting
    onReview = Types.clampH (Types.fvRuns fv) (Types.stCurH st)
  case Maybe.mapMaybe directHit resting of
    (h : _) -> Just h
    [] ->
      case List.find (== onReview) restingOn of
        Just h -> Just h
        Nothing -> Maybe.listToMaybe restingOn

-- Review-flow navigation parks AND drops the review anchor there, so '
-- can always come back; investigation moves (pickers, grep, ga, open)
-- park without touching it.
parkAnchor :: Int -> Int -> Brick.EventM Types.Name Types.St ()
parkAnchor f h = do
  parkM f h
  st <- State.get
  case Types.currentFileMay st of
    Just fv ->
      State.modify $ \s -> s {Types.stAnchor = Just (Types.anchorAt fv (Types.stCurH st))}
    Nothing -> pure ()

-- ': jump back to the hunk under review, wherever the investigation
-- wandered — across files included. With no anchor yet, it reframes
-- the current hunk as before.
jumpToAnchor :: Brick.EventM Types.Name Types.St ()
jumpToAnchor = do
  st <- State.get
  case Types.stAnchor st of
    Just an ->
      case anchorTarget st an of
        Just (f, h) -> do
          parkM f h
          Types.setMsg "back at the hunk under review"
        Nothing -> Types.setMsg ("anchored file left the session: " <> T.pack (Types.anPath an))
    Nothing -> parkM (Types.stCurF st) (Types.stCurH st)

anchorTarget :: Types.St -> Types.Anchor -> Maybe (Int, Int)
anchorTarget st an = do
  (f, fv) <- List.find ((== Types.anPath an) . Types.fvPath . snd) (zip [0 ..] (Types.stFiles st))
  pure (f, Types.anchorHunkIn an fv)

parkOnLine :: Types.Anchor -> Types.LineMark -> Brick.EventM Types.Name Types.St ()
parkOnLine an mark = do
  st <- State.get
  case anchorTarget st an of
    Nothing -> Types.setMsg ("no longer in the change set: " <> T.pack (Types.anPath an))
    Just (f, h) -> do
      parkM f h
      parked <- State.get
      Monad.forM_ (Types.safeIdx (Types.stFiles parked) f) $ \fv ->
        Monad.forM_ (markedRow fv mark) (setEd f . placeAt (Types.fvEd fv) (Types.lmSpan mark))

markedRow :: Types.FileView -> Types.LineMark -> Maybe Int
markedRow fv mark =
  fmap View.rowAt (List.find ((== Just (Types.lmKey mark)) . DiffModel.rKey . fst) (View.rowRestPoints fv))

placeAt :: Modal.EdState -> Maybe (Int, Int) -> Int -> Modal.EdState
placeAt ed found row =
  case found of
    Just (col, len) ->
      ed
        { Modal.edZipper = Z.moveCursor (row, col + max 1 len - 1) (Modal.edZipper ed)
        , Modal.edSelAnchor = Just (row, col)
        , Modal.edSelKind = Modal.CharSel
        }
    Nothing ->
      ed
        { Modal.edZipper = Z.moveCursor (row, 0) (Modal.edZipper ed)
        , Modal.edSelAnchor = Nothing
        }

-- :N — park the cursor on line N of the current file, 1-based.
-- Z.moveCursor clamps, so :0 lands on the first line and a number past
-- the end lands on the last. This is an investigation move: the '
-- anchor and the hunk under review both stay where they are.
gotoLine :: Int -> Brick.EventM Types.Name Types.St ()
gotoLine n = do
  st <- State.get
  case Types.currentFileMay st of
    Nothing -> Types.setMsg "no file on screen"
    Just fv ->
      let
        ed = Types.fvEd fv
        move = Z.moveCursor (n - 1, 0)
      in
        setEd (Types.stCurF st) $
          if Config.cfgKeys (Types.stConfig st) == Config.KeysEmacs
            -- emacs: point moves, the mark stays, so the region grows
            then ed {Modal.edZipper = move (Modal.edZipper ed), Modal.edInsertEntry = Nothing}
            else Modal.plainMove move ed

-- Space h: make the hunk the cursor is standing in the current one —
-- the current hunk being what a selection-less X, ! or ? acts on, and
-- what ' comes home to. The deliberate counterpart of ': that one goes
-- back to where you were reviewing, this one says review here instead.
setCurrentHunkHere :: Brick.EventM Types.Name Types.St ()
setCurrentHunkHere = do
  st <- State.get
  case hunkUnderCursor st of
    Nothing -> Types.setMsg "cursor isn't in a hunk"
    Just h -> do
      State.modify (\s -> s {Types.stCurH = h, Types.stMsg = " "})
      moved <- State.get
      case Types.currentFileMay moved of
        Just fv -> State.modify (\s -> s {Types.stAnchor = Just (Types.anchorAt fv h)})
        Nothing -> pure ()

selectCurrentHunk :: Brick.EventM Types.Name Types.St ()
selectCurrentHunk = do
  st <- State.get
  let
    fv = Types.currentFile st
    inCurrentRun row =
      DiffModel.rRunIx row == Just (Types.clampH (Types.fvRuns fv) (Types.stCurH st))
    hunkRows = Maybe.mapMaybe DiffModel.rNewRow (filter inCurrentRun (Types.fvRows fv))
    -- a delete-only hunk has no buffer rows of its own; its span is the
    -- anchor line the deletion sits at
    pickSpan =
      case (hunkRows, currentRunOf st (Types.stCurF st)) of
        ((first : rest), _) ->
          case reverse (first : rest) of
            (lastRow : _) -> Just (first, lastRow)
            [] -> Nothing
        ([], Just r) -> Just (DiffModel.runAnchor r, DiffModel.runAnchor r)
        _ -> Nothing
  if hunkUnderCursor st /= Just (Types.clampH (Types.fvRuns fv) (Types.stCurH st))
    then Types.setMsg "cursor isn't in the current hunk — Space h makes this one current"
    else case pickSpan of
      Nothing -> pure ()
      Just (first, lastRow) -> do
        let
          ed = Types.fvEd fv
          selected =
            ed
              { Modal.edZipper = Z.moveCursor (lastRow, 0) (Modal.edZipper ed)
              , Modal.edSelAnchor = Just (first, 0)
              , Modal.edSelKind = Modal.LineSel
              }
        setEd (Types.stCurF st) selected
