{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Core.View
  ( newSideLines
  , lastRestingRow
  , restingPoints
  , declAt
  , isDeclLine
  , declsForAnchors
  , declName
  , windowRowsFor
  , terminalRows
  , terminalColumns
  , windowSpanFor
  , bodyWindow
  , gutterWidth
  , noteTextWidth
  , shownGuideLines
  , hScrollFor
  , currentLineOf
  , activeSelectionRows
  , rowRestPoints
  , rowAt
  , verdictLineSpan
  , collapseSelection
  , cursorDisplayRow
  ) where

import Brick qualified as Brick
import Control.Monad.IO.Class qualified as MonadIO
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Remax.DiffModel qualified as DiffModel
import Remax.Modal qualified as Modal
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Verdict qualified as Verdict

-- The new side of the diff, and the buffer every row calculation is
-- measured against. A deleted file has no working copy at all, but its
-- editor buffer still holds the single empty line a text zipper cannot
-- do without. Counting that line would diff it as an added blank and
-- would push every removed row's resting point one line past the end of
-- the buffer, where no cursor can ever reach it — so a deletion that
-- nothing has typed into answers with no lines whatsoever.
newSideLines :: Types.FileView -> [T.Text]
newSideLines fv =
  let
    ls = Modal.bufferLines (Types.fvEd fv)
  in
    case (Types.fvDeleted fv, ls) of
      (True, [""]) -> []
      _ -> ls

-- length (newSideLines fv), without rebuilding the buffer to count it.
-- The one case that has to look at content is a single-line buffer,
-- where looking is free.
newSideLineCount :: Types.FileView -> Int
newSideLineCount fv =
  let
    n = Modal.bufferLineCount (Types.fvEd fv)
  in
    if Types.fvDeleted fv && n == 1 && Modal.bufferLines (Types.fvEd fv) == [""]
      then 0
      else n

-- Where a removed line rests when nothing follows it.
--
-- Removed lines own no buffer row; each answers to the row it now sits
-- before, which is how a deletion is selectable at all. The ones at the
-- very end of a file have no row after them, and their point used to be
-- one past the last line — a place no cursor can reach. So a hunk of
-- nothing but removals at the end of a file could not be got at: the
-- cursor was never in it, X would not select it, Enter would not accept
-- it, and each refusal named another that refused in turn.
--
-- They rest on the last line instead, which is the line a reader puts
-- the cursor on to mean them, and is what a removal in the middle of a
-- file has always done with the line below it.
lastRestingRow :: Types.FileView -> Int
lastRestingRow fv = max 0 (newSideLineCount fv - 1)

-- The buffer row each display row answers to, in display order: its own
-- row, or for a removed line the row it rests before.
restingPoints :: Types.FileView -> [Int]
restingPoints fv =
  drop
    1
    ( scanr
        (\row acc -> Maybe.fromMaybe acc (DiffModel.rNewRow row))
        (lastRestingRow fv)
        (Types.fvRows fv)
    )

-- The top-level declaration a buffer row falls under: the nearest line
-- at or above it whose first character sits in column zero and isn't a
-- comment or closing bracket — the boundary heuristic hunk splitting
-- already trusts. A tree-sitter walk could be exact; this is right
-- nearly always and costs one upward scan at rebuild time.
declAt :: [T.Text] -> Int -> Maybe T.Text
declAt ls row = List.find isDeclLine (reverse (take (row + 1) ls))

isDeclLine :: T.Text -> Bool
isDeclLine l =
  case T.uncons l of
    Just (c, _) ->
      not (Char.isSpace c)
        && not
          ( any
              (`T.isPrefixOf` l)
              ["--", "//", "#", ";", "{-", "/*", "*", "<!--", "}", ")", "]"]
          )
    Nothing -> False

-- declAt for every run's anchor at once, in a single walk of the buffer
-- rather than one upward scan per run. A diff that blows its budget
-- falls back to marking every line in the file, which splits into tens
-- of thousands of runs; a scan apiece is quadratic in the file, and the
-- difference between a load that takes seconds and one that never
-- finishes at all. Anchors arrive in file order — buildRuns walks the
-- rows forward and every anchor is a buffer row at or after the last —
-- which is what lets one pass answer them all.
declsForAnchors :: [T.Text] -> [Int] -> [T.Text]
declsForAnchors ls anchors =
  let
    go _ _ [] _ = []
    go ix seen pending@(a : more) rest =
      if ix > a
        then seen : go ix seen more rest
        else case rest of
          [] -> map (const seen) pending
          (l : further) ->
            go (ix + 1) (if isDeclLine l then l else seen) pending further
  in
    go (0 :: Int) T.empty anchors ls

-- Just the name from a declaration line: its first word, or two when
-- the first is a defining keyword (data Foo, def foo, class Bar ...).
declName :: T.Text -> T.Text
declName decl =
  case T.words decl of
    (w1 : w2 : _)
      | w1
          `elem` [ "data"
                 , "type"
                 , "newtype"
                 , "class"
                 , "instance"
                 , "module"
                 , "def"
                 , "fn"
                 , "func"
                 , "function"
                 , "impl"
                 , "pub"
                 , "let"
                 , "const"
                 , "var"
                 , "export"
                 ] ->
          w1 <> " " <> w2
    (w1 : _) -> w1
    [] -> ""

-- How far the render window reaches past the cursor, measured in screens
-- rather than in a flat number of rows.
--
-- The window has to outrun the viewport in both directions or the pane
-- shows blank where the body ran out, and the slack below is exactly one
-- screen — see bodyWindow for why that is the bound. A constant big
-- enough for the tallest terminal is a constant far too big for an
-- ordinary one, and every row of it is drawn on every keystroke: six
-- hundred rows for a pane that shows forty was most of what a keypress
-- cost. The floor keeps a very short pane from re-anchoring constantly.
windowRowsFor :: Int -> Int
windowRowsFor screenRows = max 100 (2 * screenRows)

-- The terminal's height. Read afresh rather than remembered: the draw
-- and the handlers that move the window have to size it the same way or
-- the body slides under the cursor, and a resize must not be able to
-- reach one of them before the other.
terminalRows :: Brick.EventM Types.Name Types.St Int
terminalRows = do
  vty <- Brick.getVtyHandle
  (_, rows) <- MonadIO.liftIO (V.displayBounds (V.outputIface vty))
  pure rows

terminalColumns :: Brick.EventM Types.Name Types.St Int
terminalColumns = do
  vty <- Brick.getVtyHandle
  (columns, _) <- MonadIO.liftIO (V.displayBounds (V.outputIface vty))
  pure columns

-- How many display rows the body draws at once, and how close the cursor
-- may come to an edge before the window is moved.
--
-- The slack is a whole screen because that is what it takes. The
-- viewport shows one screen containing the cursor, and it may sit
-- anywhere in that screen, so the rows drawn have to reach a screen
-- either side of the cursor for the pane to be full whichever way the
-- reader arrived at the line.
windowSpanFor :: Int -> Int
windowSpanFor screenRows = 2 * windowRowsFor screenRows

windowSlackFor :: Int -> Int
windowSlackFor screenRows = windowRowsFor screenRows `div` 2

-- The display row the body is framed on, and the first row of the render
-- window.
--
-- That first row is also the body's coordinate origin. brick renders a
-- vertical viewport's contents with a fixed available height of 100000
-- rows (Brick.Widgets.Core.unrestricted), so a body that padded its way
-- down to an absolute display row fell off the end of that budget: the
-- padding consumed the whole allowance and the real rows were cropped
-- away, leaving a blank pane on any file whose current hunk sat past
-- line 100000. Everything the body renders is therefore in window
-- coordinates, and the places that scroll the viewport subtract the same
-- origin so they agree with what was drawn.
--
-- The origin has to hold still while the cursor moves, which is why it
-- is carried on the FileView rather than derived from the cursor. An
-- origin that slid with the cursor kept it a fixed distance into the
-- window, so the visibility request never changed, brick never scrolled
-- again, and the file slid upward under a cursor nailed to one screen
-- line. So a stored origin is kept as long as the cursor stays clear of
-- the window's edges, and only when it does not is the window moved to
-- sit around the cursor again.
bodyWindow :: Int -> Types.St -> Types.FileView -> (Int, Int)
bodyWindow screenRows st fv =
  let
    windowRows = windowRowsFor screenRows
    windowSpan = windowSpanFor screenRows
    windowSlack = windowSlackFor screenRows
    rows = Types.fvRows fv
    curH = Types.clampH (Types.fvRuns fv) (Types.stCurH st)
    curRow = fst (Modal.cursorRC (Types.fvEd fv))
    -- Normally the cursor's row, but an all-removed file owns no buffer
    -- rows at all, so nothing carries the cursor and the view would be
    -- nailed to the top: there the hunk under review is what the reader
    -- is looking at, and ] and [ are how they move.
    cursorOut =
      case List.findIndex (\r -> DiffModel.rNewRow r == Just curRow) rows of
        Just out -> out
        Nothing -> Maybe.fromMaybe 0 (List.findIndex (\r -> DiffModel.rRunIx r == Just curH) rows)
    held = Types.fvOrigin fv
    inside =
      cursorOut >= held + windowSlack
        && cursorOut <= held + windowSpan - windowSlack
    -- clear of both edges, or already showing the top of the file
    settled = inside || (held == 0 && cursorOut <= windowSpan - windowSlack)
  in
    (cursorOut, if settled then held else max 0 (cursorOut - windowRows))

-- Where each row's reviewer note (and any agent reply to it) should
-- render: annotations apply to every line of their target, so
-- consecutive rows carrying the same pair show it once, under the last
-- of them — a footnote to the block, not an echo on every line. One
-- entry per row, [] where nothing renders. Each rendered line knows its
-- post's author and whether it opens that post, so the conversation
-- reads in order with a sense of who said what.
-- The line-number gutter's width, and with it the indent every note row
-- carries. The render and the scroll correction both have to reckon a
-- note's height the same way or their idea of what is above the cursor
-- drifts apart, so both ask here.
gutterWidth :: Types.FileView -> Int
gutterWidth fv = length (show (max 1 (Modal.bufferLineCount (Types.fvEd fv))))

noteTextWidth :: Int -> Types.FileView -> Int
noteTextWidth paneWidth fv = max 12 (paneWidth - (gutterWidth fv + 5))

shownGuideLines :: Int -> Types.St -> [DiffModel.RowR] -> [[Types.NoteLine]]
shownGuideLines width st rows =
  let
    guideOf r =
      Maybe.listToMaybe (Maybe.mapMaybe (`Map.lookup` Types.stTourMarks st) (Verdict.rowFeedbackKeys r))
    guides = map guideOf rows
    prevs = Nothing : guides
    room = max 12 (width - 2)
    fold ln = case Modal.greedyWrap room (T.words ln) of
      [] -> [""]
      wss -> map T.unwords wss
    markLines mk =
      let
        header =
          "stop "
            <> Types.tshow (Types.tmStop mk)
            <> "/"
            <> Types.tshow (Types.tmTotal mk)
            <> maybe "" ("  \183  " <>) (Types.tmTitle mk)
        body = maybe [] (concatMap fold . T.lines) (Types.tmNote mk)
      in
        Types.NoteLine "tour" True header : map (Types.NoteLine "tour" False) body
    pick (mprev, mg) = case mg of
      Just mk
        | mprev /= Just mk -> markLines mk
      _ -> []
  in
    map pick (zip prevs guides)

-- How far the view is shifted right: zero until the cursor closes in on
-- the pane's right edge, then just enough to keep a small margin ahead
-- of it. Depends only on the cursor column and the pane width, so every
-- line shifts in step.
hScrollFor :: Int -> Int -> Int -> Int
hScrollFor paneWidth numW curCol =
  let
    contentWidth = max 1 (paneWidth - (numW + 5))
    margin = 8
  in
    max 0 (curCol - (contentWidth - margin))

currentLineOf :: Modal.EdState -> T.Text
currentLineOf ed =
  Maybe.fromMaybe "" (Types.safeIdx (Modal.bufferLines ed) (fst (Modal.cursorRC ed)))

-- The buffer rows a selection covers, if one is active. A collapsed
-- char selection (just the cursor cell) does not count as "selected
-- lines" for verdicts.
activeSelectionRows :: Modal.EdState -> Maybe (Int, Int)
activeSelectionRows ed =
  case Modal.selectionRows ed of
    Just rowSpan -> Just rowSpan
    Nothing ->
      case Modal.charSelection ed of
        Just (loPos, hiPos)
          | loPos /= hiPos -> Just (fst loPos, fst hiPos)
        _ -> Nothing

-- The current hunk as a line-wise selection — the review-unit motion.
-- Every verb then composes with it: X r reviews it, X d deletes it,
-- X > indents it.
-- Which hunk the cursor is standing in: the run of the cursor's display
-- row, or — for a delete-only hunk, which owns no buffer rows — the run
-- whose deletion rests at the cursor's line.
-- Where each row sits in the buffer: its own line, or — for a removed
-- line, which has none — the line it rests before.
--
-- A hunk that only removes occupies no buffer line whatsoever, so
-- anything that reasons about rows by buffer position has to consult
-- this or it will simply not see such a hunk.
rowRestPoints :: Types.FileView -> [(DiffModel.RowR, Int)]
rowRestPoints fv = zip (Types.fvRows fv) (restingPoints fv)

-- The buffer line a row answers to, whether it owns one or rests before
-- one.
rowAt :: (DiffModel.RowR, Int) -> Int
rowAt (row, point) = Maybe.fromMaybe point (DiffModel.rNewRow row)

-- The line span a line-scoped verdict acts on: the selection when one is
-- active, the cursor's line otherwise.
verdictLineSpan :: Modal.EdState -> (Int, Int)
verdictLineSpan ed =
  case activeSelectionRows ed of
    Just rowSpan -> rowSpan
    Nothing ->
      let
        row = fst (Modal.cursorRC ed)
      in
        (row, row)

collapseSelection :: Modal.EdState -> Modal.EdState
collapseSelection ed = ed {Modal.edSelAnchor = Nothing, Modal.edExtend = False}

cursorDisplayRow :: [DiffModel.RowR] -> Int -> Int
cursorDisplayRow rows row =
  Maybe.fromMaybe 0 (List.findIndex (\r -> DiffModel.rNewRow r == Just row) rows)
