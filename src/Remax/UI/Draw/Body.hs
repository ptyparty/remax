{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Body
  ( idleBody
  , renderFile
  ) where

import Brick
  ( AttrName
  , Location (..)
  , Padding (Max)
  , Widget
  , hBox
  , padBottom
  , padRight
  , raw
  , showCursor
  , txt
  , vBox
  , visibleRegion
  , withAttr
  )
import Brick.Types
  ( Size (Fixed, Greedy)
  , Widget (Widget)
  , ctxAttrMap
  , ctxAttrName
  , getContext
  , render
  )
import Data.List (zipWith4)
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( fromMaybe
  , listToMaybe
  , mapMaybe
  )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Remax.Config
import Remax.DiffModel
import Remax.Modal
import Remax.Store
import Remax.UI.Core.Attrs
import Remax.UI.Core.Ink
import Remax.UI.Core.Types
import Remax.UI.Core.Verdict
import Remax.UI.Core.View
import Remax.UI.Core.Widgets
import Remax.UI.Core.WordDiff

idleBody :: St -> Widget Name
idleBody st =
  let
    keys = cfgKeys (stConfig st)
    hint =
      if length (stRoots st) > 1
        then "  watching for changes · gr switches repository · :base"
        else "  watching for changes · :base REF or :mode switches the view"
  in
    -- On its own line, and last, because this is the screen a reader
    -- reaches with nothing to do on it — and the one they reach by
    -- opening remax somewhere there was nothing to review, wanting out
    -- again. It used to sit at the end of the line above, behind three
    -- other things.

    padBottom Max . padRight Max $
      vBox
        [ txt " "
        , withAttr doneMarkAttr (txt "  ✓ all clear — nothing to review")
        , txt " "
        , withAttr hintAttr (txt hint)
        , -- the key lit and the word dim, the way the space menu reads,
          -- rather than in the colour that everywhere else means reviewed
          hBox
            [ withAttr noteInlineAttr (txt ("  " <> quitKeys keys))
            , withAttr hintAttr (txt " quits")
            ]
        ]

data RowItem = RowItem
  { riRow :: RowR
  , riPoint :: Int
  , riNotes :: [NoteLine]
  , riEmph :: [(Int, Int)]
  }

data WindowView = WindowView
  { wvLo :: Int
  , wvHi :: Int
  , wvCursorOut :: Int
  , wvItems :: [RowItem]
  , wvNoteRows :: [[NoteLine]]
  }

data RowPaint = RowPaint
  { rpPens :: PenSet
  , rpRowPens :: PenSet
  , rpBarName :: AttrName
  , rpBarGlyph :: Char
  , rpNumW :: Int
  , rpHOff :: Int
  , rpIsCursor :: Bool
  , rpVerdict :: Maybe Verdict
  }

renderFile :: Int -> Int -> St -> FileView -> Widget Name
renderFile paneWidth screenRows st fv =
  let
    view = windowView paneWidth screenRows st fv
    numW = gutterWidth fv
    curCol = snd (cursorRC (fvEd fv))
    -- how far the whole window is shifted right; every line shifts in
    -- step, so it is worked out once rather than per row
    hOff = hScrollFor paneWidth numW curCol
    -- keep a margin of context visible around the cursor, so the view
    -- scrolls before the cursor reaches the top or bottom edge; virtual
    -- note rows above the cursor shift where it lands in the rendering
    cursorIn = wvCursorOut view - wvLo view
    notesAbove = sum (map length (take cursorIn (wvNoteRows view)))
    cursorRender = cursorIn + notesAbove
    scrollOff = cfgScrolloff (stConfig st)
    marginTop = max 0 (cursorRender - scrollOff)
    marginHeight = cursorRender + scrollOff - marginTop + 1
  in
    Widget Greedy Fixed $ do
      ctx <- getContext
      let
        pens = PenSet (ctxAttrName ctx) (ctxAttrMap ctx)
        blocks = map (rowImages paneWidth st fv pens numW hOff) (wvItems view)
        bodyImg = raw (composeBody st fv screenRows (wvHi view) pens blocks)
        placed = case cursorScreenRow blocks of
          Just y ->
            showCursor TheCursor (Location (numW + 5 + curCol - hOff, y)) bodyImg
          Nothing -> bodyImg
      render (visibleRegion (Location (0, marginTop)) (1, marginHeight) placed)

-- Which display row the view keeps in sight, and where the window
-- that carries it starts. lo is the body's origin: nothing above it
-- is drawn, so the pane never asks brick for a row it cannot reach.
windowView :: Int -> Int -> St -> FileView -> WindowView
windowView paneWidth screenRows st fv =
  let
    rows = fvRows fv
    (cursorOut, lo) = bodyWindow screenRows st fv
    hi = min (fvRowCount fv) (lo + windowSpanFor screenRows)
    -- one walk to the window, shared by everything that reads it
    fromLo = drop lo rows
    windowed = take (hi - lo) fromLo
    beyond = drop (hi - lo) fromLo
    -- the buffer row each removed line now sits before, so a line-wise
    -- selection can honestly include the deletions it would verdict.
    -- The scan only has to cover the window, seeded with the answer for
    -- the row just past it: the first real row that follows, or the end
    -- of the buffer when nothing does.
    seedPoint =
      case mapMaybe rNewRow beyond of
        (p : _) -> p
        [] -> lastRestingRow fv
    points =
      drop 1 (scanr (\row acc -> fromMaybe acc (rNewRow row)) seedPoint windowed)
    noteRows =
      take (hi - lo) (drop lo (shownNoteLines (noteTextWidth paneWidth fv) st rows))
    -- worked out once for the whole file when the rows were built, so
    -- the marks are the same wherever the window happens to sit
    emphs = take (hi - lo) (drop lo (fvEmph fv))
  in
    WindowView
      { wvLo = lo
      , wvHi = hi
      , wvCursorOut = cursorOut
      , wvItems = zipWith4 RowItem windowed points noteRows emphs
      , wvNoteRows = noteRows
      }

-- One row's images: the line itself, then any note rows folded under
-- it, paired with whether the cursor sits on this line. Counting the
-- cursor's screen row off the images that precede it is how the
-- cursor stays with its line whatever the notes above it do.
rowImages :: Int -> St -> FileView -> PenSet -> Int -> Int -> RowItem -> ([V.Image], Bool)
rowImages paneWidth st fv pens numW hOff item =
  let
    r = riRow item
    ed = fvEd fv
    curH = clampH (fvRuns fv) (stCurH st)
    isCurRun = rRunIx r == Just curH
    isCursor = rNewRow r == Just (fst (cursorRC ed))
    inSel = rowSelected (selectionRows ed) r (riPoint item)
    paint =
      RowPaint
        { rpPens = pens
        , -- a selected line reads every attribute it draws against a map
          -- whose default is the selection, so tokens that set only a
          -- foreground pick the highlight up behind them
          rpRowPens = if inSel then penUnder selAttr pens else pens
        , -- the first gutter column doubles as the border between tray
          -- and body: drawn on every row, in the highlight color across
          -- the current hunk's rows; the second column carries the
          -- line's status, so neither ever hides the other
          rpBarName = if isCurRun then curRunAttr else borderAttr
        , rpBarGlyph = if isCurRun then '\9612' else '\9614' -- wide ▌, thin ▎
        , rpNumW = numW
        , rpHOff = hOff
        , rpIsCursor = isCursor
        , rpVerdict =
            listToMaybe (mapMaybe (`Map.lookup` stVerdicts st) (rowFeedbackKeys r))
        }
    -- reviewed lines keep their full syntax colors; the gutter dot
    -- and the tray checkmark already say "answered for", and dimming
    -- whole regions made the buffer feel murky
    lineImg =
      spanInk
        paneWidth
        [ barInk paint
        , statusInk paint r
        , numInk paint r
        , markInk paint r
        , contentInk paint (charSelection ed) (riEmph item) r
        ]
  in
    (lineImg : map (noteInk paneWidth paint) (riNotes item), isCursor)

rowSelected :: Maybe (Int, Int) -> RowR -> Int -> Bool
rowSelected lineSel r point =
  case lineSel of
    Nothing -> False
    Just (a, b) -> case rNewRow r of
      Just nr -> nr >= a && nr <= b
      -- a removed line counts when its resting point does,
      -- matching how verdicts scope it
      Nothing -> point >= a && point <= b

-- the column span of the char selection on one display row: start
-- col, and Nothing for "through end of line"
rowSpan :: Maybe ((Int, Int), (Int, Int)) -> RowR -> Maybe (Int, Maybe Int)
rowSpan charSel r = do
  ((lr, lc), (hr, hc)) <- charSel
  nr <- rNewRow r
  if nr < lr || nr > hr
    then Nothing
    else
      Just
        ( if nr == lr then lc else 0
        , if nr == hr then Just hc else Nothing
        )

barInk :: RowPaint -> Int -> V.Image
barInk paint left =
  inkT left (penFor (rpRowPens paint) (rpBarName paint)) (T.singleton (rpBarGlyph paint))

-- the cursor paints this cell yellow and renders the verdict
-- glyph in black over it — shape carries the meaning where the
-- colors would fight the background (• pending, ✓ reviewed,
-- ! flagged, ? questioned)
statusInk :: RowPaint -> RowR -> Int -> V.Image
statusInk paint r left =
  let
    rowPens = rpRowPens paint
    statusPick = case rpVerdict paint of
      Just Flagged -> Just ('!', flagMarkAttr)
      Just Questioned -> Just ('?', questionMarkAttr)
      Just Reviewed -> Just ('\10003', doneMarkAttr)
      Nothing ->
        case rKey r of
          Just _ -> Just ('\8226', pendingMarkAttr)
          Nothing -> Nothing
  in
    case (rpIsCursor paint, statusPick) of
      (True, Just (c, a)) ->
        inkT left (penFor (penUnder cursorMarkAttr rowPens) a) (T.singleton c)
      (True, Nothing) -> inkT left (penFor rowPens cursorMarkAttr) " "
      (False, Just (c, a)) -> inkT left (penFor rowPens a) (T.singleton c)
      (False, Nothing) -> inkT left (penPlain rowPens) " "

numInk :: RowPaint -> RowR -> Int -> V.Image
numInk paint r left =
  let
    rowPens = rpRowPens paint
    numW = rpNumW paint
    numAttr
      | rpIsCursor paint = lineNrCurAttr
      | rpVerdict paint == Just Reviewed = lineNrDoneAttr
      | otherwise = lineNrAttr
  in
    case rNewRow r of
      Just nr ->
        inkT left (penFor rowPens numAttr) (T.justifyRight numW ' ' (tshow (nr + 1)) <> " ")
      Nothing ->
        inkT left (penFor rowPens lineNrAttr) (T.replicate (numW + 1) " ")

markInk :: RowPaint -> RowR -> Int -> V.Image
markInk paint r left =
  inkT left (penFor (rpRowPens paint) (markerAttrFor (rMark r))) (T.pack [rMark r, ' '])

-- content shifted hOff columns left, for reading long lines:
-- the view follows the cursor horizontally while the gutter
-- stays pinned
contentInk :: RowPaint -> Maybe ((Int, Int), (Int, Int)) -> [(Int, Int)] -> RowR -> Int -> V.Image
contentInk paint charSel emph r left =
  let
    hOff = rpHOff paint
    rowPens = rpRowPens paint
    marks = shiftSpans hOff emph
    sliced = snd (splitAtCol hOff (effRuns r))
  in
    if rMark r == '-'
      then removedInk rowPens marks (padT (T.drop hOff (rText r))) left
      else case rowSpan charSel r of
        -- a live selection outranks the marks: it is what the
        -- reader is pointing at this instant
        Just (s, mEnd)
          | fromMaybe maxBound mEnd >= hOff ->
              selInk rowPens left sliced (max 0 (s - hOff)) (fmap (subtract hOff) mEnd)
        _
          | not (null marks) -> emphInk rowPens left marks sliced
        _ ->
          case nonEmptySegs sliced of
            [] -> inkT left (penPlain rowPens) " "
            filled -> segsInk rowPens left filled

removedInk :: PenSet -> [(Int, Int)] -> Text -> Int -> V.Image
removedInk rowPens marks shown left =
  if null marks
    then inkT left (penFor rowPens removedTextAttr) shown
    else emphInk rowPens left marks [(removedTextAttr, shown)]

-- the reviewer's note renders inline as virtual rows under
-- the line it annotates — visible where the feedback lives
noteInk :: Int -> RowPaint -> NoteLine -> V.Image
noteInk paneWidth paint nl =
  let
    pens = rpPens paint
    fromReviewer = nlAuthor nl == "reviewer"
    glyph
      | not (nlOpens nl) = "  "
      | fromReviewer = "\9492 \8801 "
      | otherwise = "\8618 "
    label =
      if nlOpens nl && not fromReviewer
        then nlAuthor nl <> " · "
        else ""
    noteAttr = if fromReviewer then noteInlineAttr else replyInlineAttr
    body = T.replicate (rpNumW paint + 4) " " <> glyph <> label <> padT (nlText nl)
  in
    spanInk
      paneWidth
      [ \left -> inkT left (penFor pens (rpBarName paint)) (T.singleton (rpBarGlyph paint))
      , \left -> inkT left (penFor pens noteAttr) body
      ]

-- the cursor's screen row, or nothing at all when the window
-- carries no row for the cursor's line — an all-removed file
-- owns no buffer rows, and nothing there can wear a cursor
cursorScreenRow :: [([V.Image], Bool)] -> Maybe Int
cursorScreenRow blocks =
  let
    findY _ [] = Nothing
    findY y ((imgs, isCursor) : rest) =
      if isCursor
        then Just y
        else findY (y + length imgs) rest
  in
    findY (0 :: Int) blocks

composeBody :: St -> FileView -> Int -> Int -> PenSet -> [([V.Image], Bool)] -> V.Image
composeBody st fv screenRows hi pens blocks =
  let
    -- always leave border tail below the window so the divider
    -- spans the pane even when the content is shorter than the
    -- screen. Rows past the window are never addressed, so the
    -- tail only has to outlast the pane, not the file.
    tailRows =
      max
        (min (fvRowCount fv - hi) (windowRowsFor screenRows))
        (max 60 screenRows)
    bodyRows =
      concatMap fst blocks
        ++ [V.charFill (stBorder st) '\9614' (1 :: Int) tailRows]
    -- every row filled out to the width of the widest, so no
    -- attribute runs on past the text it belongs to
    fullWidth = maximum (0 : map V.imageWidth bodyRows)
    squared img =
      let
        short = fullWidth - V.imageWidth img
      in
        if short <= 0
          then img
          else V.horizJoin img (V.charFill (penPlain pens) ' ' short (V.imageHeight img))
  in
    V.vertCat (map squared bodyRows)
