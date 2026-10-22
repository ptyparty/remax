{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Body
  ( idleBody
  , renderFile
  ) where

import Brick qualified as Brick
import Brick.Types qualified as BrickTypes
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Remax.Config qualified as Config
import Remax.DiffModel qualified as DiffModel
import Remax.Modal qualified as Modal
import Remax.Store qualified as Store
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Ink qualified as Ink
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Verdict qualified as Verdict
import Remax.UI.Core.View qualified as View
import Remax.UI.Core.Widgets qualified as Widgets
import Remax.UI.Core.WordDiff qualified as WordDiff

idleBody :: Types.St -> BrickTypes.Widget Types.Name
idleBody st =
  let
    keys = Config.cfgKeys (Types.stConfig st)
    hint =
      if length (Types.stRoots st) > 1
        then "  watching for changes · gr switches repository · :base"
        else "  watching for changes · :base REF or :mode switches the view"
  in
    -- On its own line, and last, because this is the screen a reader
    -- reaches with nothing to do on it — and the one they reach by
    -- opening remax somewhere there was nothing to review, wanting out
    -- again. It used to sit at the end of the line above, behind three
    -- other things.

    Brick.padBottom Brick.Max . Brick.padRight Brick.Max $
      Brick.vBox
        [ Brick.txt " "
        , Brick.withAttr Attrs.doneMarkAttr (Brick.txt "  ✓ all clear — nothing to review")
        , Brick.txt " "
        , Brick.withAttr Attrs.hintAttr (Brick.txt hint)
        , -- the key lit and the word dim, the way the space menu reads,
          -- rather than in the colour that everywhere else means reviewed
          Brick.hBox
            [ Brick.withAttr Attrs.noteInlineAttr (Brick.txt ("  " <> Types.quitKeys keys))
            , Brick.withAttr Attrs.hintAttr (Brick.txt " quits")
            ]
        ]

data RowItem = RowItem
  { riRow :: DiffModel.RowR
  , riPoint :: Int
  , riNotes :: [Types.NoteLine]
  , riGuide :: [Types.NoteLine]
  , riEmph :: [(Int, Int)]
  }

data WindowView = WindowView
  { wvLo :: Int
  , wvHi :: Int
  , wvCursorOut :: Int
  , wvItems :: [RowItem]
  , wvNoteRows :: [[Types.NoteLine]]
  , wvGuideRows :: [[Types.NoteLine]]
  }

data RowPaint = RowPaint
  { rpPens :: Ink.PenSet
  , rpRowPens :: Ink.PenSet
  , rpBarName :: Brick.AttrName
  , rpBarGlyph :: Char
  , rpNumW :: Int
  , rpHOff :: Int
  , rpIsCursor :: Bool
  , rpVerdict :: Maybe Store.Verdict
  }

renderFile :: Int -> Int -> Types.St -> Types.FileView -> BrickTypes.Widget Types.Name
renderFile paneWidth screenRows st fv =
  let
    view = windowView paneWidth screenRows st fv
    numW = View.gutterWidth fv
    curCol = snd (Modal.cursorRC (Types.fvEd fv))
    -- how far the whole window is shifted right; every line shifts in
    -- step, so it is worked out once rather than per row
    hOff = View.hScrollFor paneWidth numW curCol
    -- keep a margin of context visible around the cursor, so the view
    -- scrolls before the cursor reaches the top or bottom edge; virtual
    -- note rows above the cursor shift where it lands in the rendering
    cursorIn = wvCursorOut view - wvLo view
    notesAbove =
      sum (map length (take cursorIn (wvNoteRows view)))
        + sum (map length (take (cursorIn + 1) (wvGuideRows view)))
    cursorRender = cursorIn + notesAbove
    scrollOff = Config.cfgScrolloff (Types.stConfig st)
    marginTop = max 0 (cursorRender - scrollOff)
    marginHeight = cursorRender + scrollOff - marginTop + 1
  in
    BrickTypes.Widget BrickTypes.Greedy BrickTypes.Fixed $ do
      ctx <- BrickTypes.getContext
      let
        pens = Ink.PenSet (BrickTypes.ctxAttrName ctx) (BrickTypes.ctxAttrMap ctx)
        blocks = map (rowImages paneWidth st fv pens numW hOff) (wvItems view)
        bodyImg = Brick.raw (composeBody st fv screenRows (wvHi view) pens blocks)
        placed = case cursorScreenRow blocks of
          Just y ->
            Brick.showCursor Types.TheCursor (Brick.Location (numW + 5 + curCol - hOff, y)) bodyImg
          Nothing -> bodyImg
      BrickTypes.render (Brick.visibleRegion (Brick.Location (0, marginTop)) (1, marginHeight) placed)

-- Which display row the view keeps in sight, and where the window
-- that carries it starts. lo is the body's origin: nothing above it
-- is drawn, so the pane never asks brick for a row it cannot reach.
windowView :: Int -> Int -> Types.St -> Types.FileView -> WindowView
windowView paneWidth screenRows st fv =
  let
    rows = Types.fvRows fv
    (cursorOut, lo) = View.bodyWindow screenRows st fv
    hi = min (Types.fvRowCount fv) (lo + View.windowSpanFor screenRows)
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
      case Maybe.mapMaybe DiffModel.rNewRow beyond of
        (p : _) -> p
        [] -> View.lastRestingRow fv
    points =
      drop 1 (scanr (\row acc -> Maybe.fromMaybe acc (DiffModel.rNewRow row)) seedPoint windowed)
    noteRows =
      take (hi - lo) (drop lo (View.shownNoteLines (View.noteTextWidth paneWidth fv) st rows))
    guideRows =
      take (hi - lo) (drop lo (View.shownGuideLines (View.noteTextWidth paneWidth fv) st rows))
    -- worked out once for the whole file when the rows were built, so
    -- the marks are the same wherever the window happens to sit
    emphs = take (hi - lo) (drop lo (Types.fvEmph fv))
  in
    WindowView
      { wvLo = lo
      , wvHi = hi
      , wvCursorOut = cursorOut
      , wvItems = List.zipWith5 RowItem windowed points noteRows guideRows emphs
      , wvNoteRows = noteRows
      , wvGuideRows = guideRows
      }

-- One row's images: the line itself, then any note rows folded under
-- it, paired with whether the cursor sits on this line. Counting the
-- cursor's screen row off the images that precede it is how the
-- cursor stays with its line whatever the notes above it do.
rowImages :: Int -> Types.St -> Types.FileView -> Ink.PenSet -> Int -> Int -> RowItem -> ([V.Image], [V.Image], Bool)
rowImages paneWidth st fv pens numW hOff item =
  let
    r = riRow item
    ed = Types.fvEd fv
    curH = Types.clampH (Types.fvRuns fv) (Types.stCurH st)
    isCurRun = DiffModel.rRunIx r == Just curH
    isCursor = DiffModel.rNewRow r == Just (fst (Modal.cursorRC ed))
    inSel = rowSelected (Modal.selectionRows ed) r (riPoint item)
    verdict = Maybe.listToMaybe (Maybe.mapMaybe (`Map.lookup` Types.stVerdicts st) (Verdict.rowFeedbackKeys r))
    paint =
      RowPaint
        { rpPens = pens
        , -- a selected line reads every attribute it draws against a map
          -- whose default is the selection, so tokens that set only a
          -- foreground pick the highlight up behind them
          rpRowPens =
            if inSel
              then Ink.penUnder Attrs.selAttr pens
              else maybe pens (`Ink.penUnder` pens) (verdictWash r verdict)
        , -- the first gutter column doubles as the border between tray
          -- and body: drawn on every row, in the highlight color across
          -- the current hunk's rows; the second column carries the
          -- line's status, so neither ever hides the other
          rpBarName = if isCurRun then Attrs.curRunAttr else Attrs.borderAttr
        , rpBarGlyph = if isCurRun then '\9612' else '\9614' -- wide ▌, thin ▎
        , rpNumW = numW
        , rpHOff = hOff
        , rpIsCursor = isCursor
        , rpVerdict = verdict
        }
    -- reviewed lines keep their full syntax colors; the gutter dot
    -- and the tray checkmark already say "answered for", and dimming
    -- whole regions made the buffer feel murky
    drawn =
      Ink.spanInk
        paneWidth
        [ barInk paint
        , statusInk paint r
        , numInk paint r
        , markInk paint r
        , contentInk paint (Modal.charSelection ed) (riEmph item) r
        ]
    lineImg =
      if not inSel && Maybe.isJust (verdictWash r verdict)
        then Ink.fitRow (Ink.penPlain (rpRowPens paint)) paneWidth drawn
        else drawn
  in
    (map (guideInk paneWidth paint) (riGuide item), lineImg : map (noteInk paneWidth paint) (riNotes item), isCursor)

verdictWash :: DiffModel.RowR -> Maybe Store.Verdict -> Maybe Brick.AttrName
verdictWash r verdict =
  if DiffModel.rMark r == ' '
    then Nothing
    else case verdict of
      Nothing -> Just Attrs.pendingWashAttr
      Just Store.Flagged -> Just Attrs.flaggedWashAttr
      Just Store.Questioned -> Just Attrs.questionedWashAttr
      Just Store.Reviewed -> Nothing

rowSelected :: Maybe (Int, Int) -> DiffModel.RowR -> Int -> Bool
rowSelected lineSel r point =
  case lineSel of
    Nothing -> False
    Just (a, b) -> case DiffModel.rNewRow r of
      Just nr -> nr >= a && nr <= b
      -- a removed line counts when its resting point does,
      -- matching how verdicts scope it
      Nothing -> point >= a && point <= b

-- the column span of the char selection on one display row: start
-- col, and Nothing for "through end of line"
rowSpan :: Maybe ((Int, Int), (Int, Int)) -> DiffModel.RowR -> Maybe (Int, Maybe Int)
rowSpan charSel r = do
  ((lr, lc), (hr, hc)) <- charSel
  nr <- DiffModel.rNewRow r
  if nr < lr || nr > hr
    then Nothing
    else
      Just
        ( if nr == lr then lc else 0
        , if nr == hr then Just hc else Nothing
        )

barInk :: RowPaint -> Int -> V.Image
barInk paint left =
  Ink.inkT left (Ink.penFor (rpRowPens paint) (rpBarName paint)) (T.singleton (rpBarGlyph paint))

-- the cursor paints this cell yellow and renders the verdict
-- glyph in black over it — shape carries the meaning where the
-- colors would fight the background (• pending, ✓ reviewed,
-- ! flagged, ? questioned)
statusInk :: RowPaint -> DiffModel.RowR -> Int -> V.Image
statusInk paint r left =
  let
    rowPens = rpRowPens paint
    statusPick = case rpVerdict paint of
      Just Store.Flagged -> Just ('!', Attrs.flagMarkAttr)
      Just Store.Questioned -> Just ('?', Attrs.questionMarkAttr)
      Just Store.Reviewed -> Just ('\10003', Attrs.doneMarkAttr)
      Nothing ->
        case DiffModel.rKey r of
          Just _ -> Just ('\8226', Attrs.pendingMarkAttr)
          Nothing -> Nothing
  in
    case (rpIsCursor paint, statusPick) of
      (True, Just (c, a)) ->
        Ink.inkT left (Ink.penFor (Ink.penUnder Attrs.cursorMarkAttr rowPens) a) (T.singleton c)
      (True, Nothing) -> Ink.inkT left (Ink.penFor rowPens Attrs.cursorMarkAttr) " "
      (False, Just (c, a)) -> Ink.inkT left (Ink.penFor rowPens a) (T.singleton c)
      (False, Nothing) -> Ink.inkT left (Ink.penPlain rowPens) " "

numInk :: RowPaint -> DiffModel.RowR -> Int -> V.Image
numInk paint r left =
  let
    rowPens = rpRowPens paint
    numW = rpNumW paint
    numAttr
      | rpIsCursor paint = Attrs.lineNrCurAttr
      | rpVerdict paint == Just Store.Reviewed = Attrs.lineNrDoneAttr
      | otherwise = Attrs.lineNrAttr
  in
    case DiffModel.rNewRow r of
      Just nr ->
        Ink.inkT left (Ink.penFor rowPens numAttr) (T.justifyRight numW ' ' (Types.tshow (nr + 1)) <> " ")
      Nothing ->
        Ink.inkT left (Ink.penFor rowPens Attrs.lineNrAttr) (T.replicate (numW + 1) " ")

markInk :: RowPaint -> DiffModel.RowR -> Int -> V.Image
markInk paint r left =
  Ink.inkT left (Ink.penFor (rpRowPens paint) (Ink.markerAttrFor (DiffModel.rMark r))) (T.pack [DiffModel.rMark r, ' '])

-- content shifted hOff columns left, for reading long lines:
-- the view follows the cursor horizontally while the gutter
-- stays pinned
contentInk :: RowPaint -> Maybe ((Int, Int), (Int, Int)) -> [(Int, Int)] -> DiffModel.RowR -> Int -> V.Image
contentInk paint charSel emph r left =
  let
    hOff = rpHOff paint
    rowPens = rpRowPens paint
    marks = WordDiff.shiftSpans hOff emph
    sliced = snd (Ink.splitAtCol hOff (Verdict.effRuns r))
  in
    if DiffModel.rMark r == '-'
      then removedInk rowPens marks (Widgets.padT (T.drop hOff (DiffModel.rText r))) left
      else case rowSpan charSel r of
        -- a live selection outranks the marks: it is what the
        -- reader is pointing at this instant
        Just (s, mEnd)
          | Maybe.fromMaybe maxBound mEnd >= hOff ->
              Ink.selInk rowPens left sliced (max 0 (s - hOff)) (fmap (subtract hOff) mEnd)
        _
          | not (null marks) -> Ink.emphInk rowPens left marks sliced
        _ ->
          case Widgets.nonEmptySegs sliced of
            [] -> Ink.inkT left (Ink.penPlain rowPens) " "
            filled -> Ink.segsInk rowPens left filled

removedInk :: Ink.PenSet -> [(Int, Int)] -> T.Text -> Int -> V.Image
removedInk rowPens marks shown left =
  if null marks
    then Ink.inkT left (Ink.penFor rowPens Attrs.removedTextAttr) shown
    else Ink.emphInk rowPens left marks [(Attrs.removedTextAttr, shown)]

-- the reviewer's note renders inline as virtual rows under
-- the line it annotates — visible where the feedback lives
guideInk :: Int -> RowPaint -> Types.NoteLine -> V.Image
guideInk paneWidth paint nl =
  let
    attr = Ink.penFor (rpPens paint) Attrs.guideAttr
    lead = if Types.nlOpens nl then "\9656 " else "  "
    body = T.replicate (rpNumW paint + 4) " " <> lead <> Types.nlText nl
  in
    Ink.spanInk
      paneWidth
      [ \left -> Ink.inkT left (Ink.penFor (rpPens paint) (rpBarName paint)) (T.singleton (rpBarGlyph paint))
      , \left -> Ink.fitRow attr left (Ink.inkT left attr body)
      ]

noteInk :: Int -> RowPaint -> Types.NoteLine -> V.Image
noteInk paneWidth paint nl =
  let
    pens = rpPens paint
    fromReviewer = Types.nlAuthor nl == "reviewer"
    glyph
      | not (Types.nlOpens nl) = "  "
      | fromReviewer = "\9492 \8801 "
      | otherwise = "\8618 "
    label =
      if Types.nlOpens nl && not fromReviewer
        then Types.nlAuthor nl <> " · "
        else ""
    noteAttr = if fromReviewer then Attrs.noteInlineAttr else Attrs.replyInlineAttr
    body = T.replicate (rpNumW paint + 4) " " <> glyph <> label <> Widgets.padT (Types.nlText nl)
  in
    Ink.spanInk
      paneWidth
      [ \left -> Ink.inkT left (Ink.penFor pens (rpBarName paint)) (T.singleton (rpBarGlyph paint))
      , \left -> Ink.inkT left (Ink.penFor pens noteAttr) body
      ]

-- the cursor's screen row, or nothing at all when the window
-- carries no row for the cursor's line — an all-removed file
-- owns no buffer rows, and nothing there can wear a cursor
cursorScreenRow :: [([V.Image], [V.Image], Bool)] -> Maybe Int
cursorScreenRow blocks =
  let
    findY _ [] = Nothing
    findY y ((above, imgs, isCursor) : rest) =
      if isCursor
        then Just (y + length above)
        else findY (y + length above + length imgs) rest
  in
    findY (0 :: Int) blocks

composeBody :: Types.St -> Types.FileView -> Int -> Int -> Ink.PenSet -> [([V.Image], [V.Image], Bool)] -> V.Image
composeBody st fv screenRows hi pens blocks =
  let
    -- always leave border tail below the window so the divider
    -- spans the pane even when the content is shorter than the
    -- screen. Rows past the window are never addressed, so the
    -- tail only has to outlast the pane, not the file.
    tailRows =
      max
        (min (Types.fvRowCount fv - hi) (View.windowRowsFor screenRows))
        (max 60 screenRows)
    bodyRows =
      concatMap (\(above, imgs, _) -> above ++ imgs) blocks
        ++ [V.charFill (Types.stBorder st) '\9614' (1 :: Int) tailRows]
    -- every row filled out to the width of the widest, so no
    -- attribute runs on past the text it belongs to
    fullWidth = maximum (0 : map V.imageWidth bodyRows)
    squared img =
      let
        short = fullWidth - V.imageWidth img
      in
        if short <= 0
          then img
          else V.horizJoin img (V.charFill (Ink.penPlain pens) ' ' short (V.imageHeight img))
  in
    V.vertCat (map squared bodyRows)
