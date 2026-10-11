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
  , windowSpanFor
  , bodyWindow
  , gutterWidth
  , noteTextWidth
  , shownNoteLines
  , hScrollFor
  , currentLineOf
  , activeSelectionRows
  , rowRestPoints
  , rowAt
  , verdictLineSpan
  , collapseSelection
  , cursorDisplayRow
  ) where

import Brick
  ( EventM
  , getVtyHandle
  )
import Control.Monad.IO.Class (liftIO)
import Data.Char
  ( isSpace
  )
import Data.List
  ( find
  , findIndex
  )
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( fromMaybe
  , listToMaybe
  , mapMaybe
  )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Remax.DiffModel
import Remax.Modal
import Remax.Store
import Remax.UI.Core.Types
import Remax.UI.Core.Verdict

-- The new side of the diff, and the buffer every row calculation is
-- measured against. A deleted file has no working copy at all, but its
-- editor buffer still holds the single empty line a text zipper cannot
-- do without. Counting that line would diff it as an added blank and
-- would push every removed row's resting point one line past the end of
-- the buffer, where no cursor can ever reach it — so a deletion that
-- nothing has typed into answers with no lines whatsoever.
newSideLines :: FileView -> [Text]
newSideLines fv =
  let
    ls = bufferLines (fvEd fv)
  in
    case (fvDeleted fv, ls) of
      (True, [""]) -> []
      _ -> ls

-- length (newSideLines fv), without rebuilding the buffer to count it.
-- The one case that has to look at content is a single-line buffer,
-- where looking is free.
newSideLineCount :: FileView -> Int
newSideLineCount fv =
  let
    n = bufferLineCount (fvEd fv)
  in
    if fvDeleted fv && n == 1 && bufferLines (fvEd fv) == [""]
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
lastRestingRow :: FileView -> Int
lastRestingRow fv = max 0 (newSideLineCount fv - 1)

-- The buffer row each display row answers to, in display order: its own
-- row, or for a removed line the row it rests before.
restingPoints :: FileView -> [Int]
restingPoints fv =
  drop
    1
    ( scanr
        (\row acc -> fromMaybe acc (rNewRow row))
        (lastRestingRow fv)
        (fvRows fv)
    )

-- The top-level declaration a buffer row falls under: the nearest line
-- at or above it whose first character sits in column zero and isn't a
-- comment or closing bracket — the boundary heuristic hunk splitting
-- already trusts. A tree-sitter walk could be exact; this is right
-- nearly always and costs one upward scan at rebuild time.
declAt :: [Text] -> Int -> Maybe Text
declAt ls row = find isDeclLine (reverse (take (row + 1) ls))

isDeclLine :: Text -> Bool
isDeclLine l =
  case T.uncons l of
    Just (c, _) ->
      not (isSpace c)
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
declsForAnchors :: [Text] -> [Int] -> [Text]
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
declName :: Text -> Text
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
terminalRows :: EventM Name St Int
terminalRows = do
  vty <- getVtyHandle
  (_, rows) <- liftIO (V.displayBounds (V.outputIface vty))
  pure rows

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
bodyWindow :: Int -> St -> FileView -> (Int, Int)
bodyWindow screenRows st fv =
  let
    windowRows = windowRowsFor screenRows
    windowSpan = windowSpanFor screenRows
    windowSlack = windowSlackFor screenRows
    rows = fvRows fv
    curH = clampH (fvRuns fv) (stCurH st)
    curRow = fst (cursorRC (fvEd fv))
    -- Normally the cursor's row, but an all-removed file owns no buffer
    -- rows at all, so nothing carries the cursor and the view would be
    -- nailed to the top: there the hunk under review is what the reader
    -- is looking at, and ] and [ are how they move.
    cursorOut =
      case findIndex (\r -> rNewRow r == Just curRow) rows of
        Just out -> out
        Nothing -> fromMaybe 0 (findIndex (\r -> rRunIx r == Just curH) rows)
    held = fvOrigin fv
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
gutterWidth :: FileView -> Int
gutterWidth fv = length (show (max 1 (bufferLineCount (fvEd fv))))

noteTextWidth :: Int -> FileView -> Int
noteTextWidth paneWidth fv = max 12 (paneWidth - (gutterWidth fv + 4))

shownNoteLines :: Int -> St -> [RowR] -> [[NoteLine]]
shownNoteLines width st rows =
  let
    annOf r =
      listToMaybe (mapMaybe (`Map.lookup` stThreads st) (rowFeedbackKeys r))
    anns = map annOf rows
    nexts = drop 1 anns ++ [Nothing]
    -- A note is prose and the pane is narrow, so it is folded to fit
    -- rather than run off the edge where nobody can read it. The
    -- author's own line breaks are kept and each of their lines folded
    -- in turn, so a deliberate paragraph survives while a long one still
    -- lands on screen.
    --
    -- The opening row carries the glyph, and an agent's name besides;
    -- folding to what that leaves means no row overflows, and the
    -- continuations simply have more room than they use.
    postLines post =
      let
        author = postAuthor post
        lead = if author == "reviewer" then 4 else 2 + T.length author + 3
        room = max 12 (width - lead)
        fold ln = case greedyWrap room (T.words ln) of
          [] -> [""]
          wss -> map T.unwords wss
        flowed = concatMap fold (T.lines (postBody post))
      in
        case flowed of
          [] -> []
          (first : rest) ->
            NoteLine author True first : map (NoteLine author False) rest
    pick (ma, mnext) = case ma of
      Just posts
        | mnext /= Just posts -> concatMap postLines posts
      _ -> []
  in
    map pick (zip anns nexts)

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

currentLineOf :: EdState -> Text
currentLineOf ed =
  fromMaybe "" (safeIdx (bufferLines ed) (fst (cursorRC ed)))

-- The buffer rows a selection covers, if one is active. A collapsed
-- char selection (just the cursor cell) does not count as "selected
-- lines" for verdicts.
activeSelectionRows :: EdState -> Maybe (Int, Int)
activeSelectionRows ed =
  case selectionRows ed of
    Just rowSpan -> Just rowSpan
    Nothing ->
      case charSelection ed of
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
rowRestPoints :: FileView -> [(RowR, Int)]
rowRestPoints fv = zip (fvRows fv) (restingPoints fv)

-- The buffer line a row answers to, whether it owns one or rests before
-- one.
rowAt :: (RowR, Int) -> Int
rowAt (row, point) = fromMaybe point (rNewRow row)

-- The line span a line-scoped verdict acts on: the selection when one is
-- active, the cursor's line otherwise.
verdictLineSpan :: EdState -> (Int, Int)
verdictLineSpan ed =
  case activeSelectionRows ed of
    Just rowSpan -> rowSpan
    Nothing ->
      let
        row = fst (cursorRC ed)
      in
        (row, row)

collapseSelection :: EdState -> EdState
collapseSelection ed = ed {edSelAnchor = Nothing, edExtend = False}

cursorDisplayRow :: [RowR] -> Int -> Int
cursorDisplayRow rows row =
  fromMaybe 0 (findIndex (\r -> rNewRow r == Just row) rows)
