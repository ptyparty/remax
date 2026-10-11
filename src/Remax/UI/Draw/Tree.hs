{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Tree
  ( fileMenuLayer
  , treeLayer
  ) where

import Brick
  ( AttrName
  , Padding (Max)
  , Widget
  , hBox
  , hLimit
  , padLeft
  , padRight
  , str
  , txt
  , vBox
  , withAttr
  , withDefAttr
  )
import Brick.Types
  ( Size (Greedy)
  , Widget (Widget)
  , availHeight
  , availWidth
  , getContext
  , render
  )
import Data.List
  ( find
  )
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Remax.Git
  ( FileScope (..)
  , PathChange (..)
  , PathStat (..)
  , pathChangeLetter
  )
import Remax.Tree
import Remax.UI.Attrs
import Remax.UI.Files
import Remax.UI.Fuzzy
import Remax.UI.Ink
import Remax.UI.Tree
import Remax.UI.Types
import Remax.UI.Verdict
import Remax.UI.Widgets
import System.FilePath
  ( takeFileName
  )

-- Ctrl-x's menu, drawn from the same table that dispatches it.
fileMenuLayer :: TreeView -> Maybe (Widget Name)
fileMenuLayer view =
  case tvOp view of
    Just OpMenu ->
      let
        item (key, label, _) =
          hBox
            [ withAttr noteInlineAttr (txt (" " <> key))
            , withAttr statusAttr (txt ("  " <> label))
            ]
      in
        Just
          ( cornerFrame "file" "any other key cancels" . hLimit 24 . padRight Max $
              vBox (map item fileMenuEntries)
          )
    _ -> Nothing

-- Fixed-width columns, so one long name cannot shove the numbers out of
-- line. hLimit does what columnRow does for the pickers, but keeps the
-- cells as widgets so they can carry their own colors.
treeCell :: Int -> Widget Name -> Widget Name
treeCell w = hLimit w . padRight Max

treeCellR :: Int -> Widget Name -> Widget Name
treeCellR w = hLimit w . padLeft Max

treeNameW, treeStatW, treeNumW, treeMeterW :: Int
treeNameW = 34
treeStatW = 2
treeNumW = 6
treeMeterW = 13

statusAttrFor :: PathChange -> AttrName
statusAttrFor change =
  case change of
    PathAdded -> addedMarkAttr
    PathModified -> pendingMarkAttr
    PathDeleted -> removedMarkAttr
    PathRenamed _ -> questionMarkAttr
    PathCopied _ -> questionMarkAttr
    PathUntracked -> hintAttr
    PathOtherChange -> hintAttr

verdictMarkAttr :: Char -> AttrName
verdictMarkAttr mark =
  case mark of
    '!' -> flagMarkAttr
    '?' -> questionMarkAttr
    '\10003' -> doneMarkAttr
    _ -> pendingMarkAttr

-- Which characters of a row's own label the filter matched.
--
-- Positions come from the row's witness — the full path the whole query
-- actually matches — and are then shifted onto the slice of it this row
-- displays. A directory has to be measured against a descendant's path
-- rather than its own: "remaxgit" matches src/Remax/Git.hs, but no part
-- of that query matches src/Remax alone, so asking the directory by
-- itself finds nothing and lights nothing. Every row on the way down to
-- a match now shows its share of it.
rowMatchPositions :: Text -> TreeRow -> Set.Set Int
rowMatchPositions query row
  | T.null query = Set.empty
  | otherwise =
      let
        witness = T.pack (trWitness row)
        label = T.dropWhileEnd (== '/') (trLabel row)
        -- the label is the tail of trPath, and trPath is a prefix of the
        -- witness, so this span is the same in both
        offset = length (trPath row) - T.length label
        inLabel p = p >= offset && p < offset + T.length label
      in
        case subseqPositions query witness of
          Just ps -> Set.fromList (map (subtract offset) (filter inLabel ps))
          Nothing -> Set.empty

treeRowWidget :: St -> Text -> Int -> Bool -> TreeRow -> Widget Name
treeRowWidget st query rowWidth isSel row =
  let
    lead =
      if isSel
        then withAttr floatSelBarAttr (txt "\9614")
        else txt " "
    body =
      case trKind row of
        RowDir roll open -> treeDirRow query rowWidth row roll open
        RowFile mStat hasHunks open -> treeFileRow st query rowWidth row mStat hasHunks open
        RowHunk ix -> treeHunkRow st row ix
    w = padRight Max (hBox [lead, txt " ", body])
  in
    if isSel then withDefAttr floatSelAttr w else w

-- the prefix (indent and fold marker) is never part of the path, so
-- it sits outside the highlighted span
--
-- A row with nothing to say about git has no columns to line up
-- with, so its name takes the whole row rather than being clipped at
-- the width the decorated rows need.
nameCellIn :: Text -> TreeRow -> Int -> AttrName -> Text -> Text -> Widget Name
nameCellIn query row width attr prefix label =
  let
    room = max 1 (width - T.length prefix)
    shown = clipT room label
  in
    treeCell
      width
      ( hBox
          [ withAttr attr (txt prefix)
          , withAttr attr (highlightAt (rowMatchPositions query row) shown)
          ]
      )

rowIndent :: TreeRow -> Text
rowIndent row = T.replicate (trDepth row * 2) " "

treeDirRow :: Text -> Int -> TreeRow -> Rollup -> Bool -> Widget Name
treeDirRow query rowWidth row roll open =
  let
    marker = if open then "\9662 " else "\9656 "
    changed = ruChanged roll > 0
    cell = if changed then nameCellIn query row treeNameW else nameCellIn query row (max treeNameW rowWidth)
    tally =
      [ treeCell treeStatW (txt " ")
      , treeCellR treeNumW (withAttr addedMarkAttr (txt ("+" <> tshow (ruAdded roll))))
      , treeCellR treeNumW (withAttr removedMarkAttr (txt ("\8722" <> tshow (ruRemoved roll))))
      , txt "  "
      , treeCell
          treeMeterW
          ( if open
              then txt " "
              else withAttr hintAttr (txt (tshow (ruChanged roll) <> " chg"))
          )
      ]
  in
    hBox (cell hintAttr (rowIndent row <> marker) (trLabel row <> "/") : (if changed then tally else []))

treeFileRow :: St -> Text -> Int -> TreeRow -> Maybe PathStat -> Bool -> Bool -> Widget Name
treeFileRow st query rowWidth row mStat hasHunks open =
  let
    marker
      | not hasHunks = "  "
      | open = "\9662 "
      | otherwise = "\9656 "
    curPath = maybe "" fvPath (currentFileMay st)
    nameAttr = if trPath row == curPath then trayFileCurAttr else trayFileAttr
    letter = case fmap psChange mStat of
      Just change -> withAttr (statusAttrFor change) (txt (pathChangeLetter change))
      Nothing -> txt " "
    -- untouched and not in the session: nothing to the right of
    -- the name, so the name gets the room
    bare = mStat == Nothing && not hasHunks
  in
    if bare
      then nameCellIn query row (max treeNameW rowWidth) nameAttr (rowIndent row <> marker) (trLabel row)
      else
        hBox
          ( [ nameCellIn query row treeNameW nameAttr (rowIndent row <> marker) (trLabel row)
            , treeCell treeStatW letter
            ]
              ++ countCells mStat
              ++ [txt "  ", meterCell st (trPath row)]
              ++ fromCell mStat
          )

countCells :: Maybe PathStat -> [Widget Name]
countCells mStat =
  case fmap psCounts mStat of
    Just (Just (a, r)) ->
      [ treeCellR treeNumW (if a > 0 then withAttr addedMarkAttr (txt ("+" <> tshow a)) else txt " ")
      , treeCellR treeNumW (if r > 0 then withAttr removedMarkAttr (txt ("\8722" <> tshow r)) else txt " ")
      ]
    Just Nothing ->
      case fmap psChange mStat of
        Just PathUntracked -> [treeCell (treeNumW * 2) (txt " ")]
        _ ->
          [ treeCellR treeNumW (withAttr trayDeclAttr (txt "bin"))
          , treeCell treeNumW (txt " ")
          ]
    Nothing -> [treeCell (treeNumW * 2) (txt " ")]

meterCell :: St -> FilePath -> Widget Name
meterCell st path =
  case find ((== path) . fvPath) (stFiles st) of
    Just fv
      | not (null (fvRuns fv)) ->
          let
            total = length (fvRuns fv)
            done = length (filter (not . runPending (stVerdicts st)) (fvRuns fv))
          in
            treeCell treeMeterW (meterW done total)
    _ -> treeCell treeMeterW (txt " ")

fromCell :: Maybe PathStat -> [Widget Name]
fromCell mStat =
  let
    renamedFrom from = withAttr hintAttr (txt (" \8592 " <> T.pack (takeFileName from)))
  in
    case fmap psChange mStat of
      Just (PathRenamed from) -> [renamedFrom from]
      Just (PathCopied from) -> [renamedFrom from]
      _ -> []

treeHunkRow :: St -> TreeRow -> Int -> Widget Name
treeHunkRow st row ix =
  case treeHunkInfo st (trPath row) ix of
    Nothing -> txt " "
    Just (mark, ln, decl, hasNote) ->
      hBox
        [ txt (rowIndent row <> "  ")
        , withAttr (verdictMarkAttr mark) (str [mark])
        , txt (" :" <> tshow ln)
        , withAttr trayDeclAttr (txt decl)
        , if hasNote then withAttr noteInlineAttr (txt " \8801") else txt ""
        ]

treeLayer :: St -> TreeView -> Widget Name
treeLayer st view =
  Widget Greedy Greedy $ do
    ctx <- getContext
    let
      boxWidth = min 96 (max 60 (availWidth ctx - 16))
      -- as tall as the tree needs and no taller: a folded tree should not
      -- open a full-height box of nothing
      roomFor = max 6 (availHeight ctx - 14)
      visible = treeVisible st view
      visibleCount = max 6 (min roomFor (length visible))
      -- what a row has to play with once the frame and the selection
      -- bar have taken theirs
      rowWidth = max treeNameW (boxWidth - 6)
      body = treeBodyRows st view visible visibleCount rowWidth
      filler = replicate (visibleCount - length body) (txt " ")
      title = if tvMode view == TreeFlat then "files" else "tree"
    render $
      floatFrame title (treeHint st view) $
        hLimit boxWidth (vBox (treeInputRow st view visible : txt " " : (body ++ filler)))

treeBodyRows :: St -> TreeView -> [TreeRow] -> Int -> Int -> [Widget Name]
treeBodyRows st view visible visibleCount rowWidth =
  let
    query = tvInput view
    sel = max 0 (min (length visible - 1) (tvSel view))
    start =
      max 0 (min (sel - div visibleCount 2) (max 0 (length visible - visibleCount)))
    -- when the enclosing directory (or file, for a hunk) has scrolled
    -- off, pin it: otherwise a row loses all sense of whose it is
    shown = take visibleCount (drop start visible)
    sticky = do
      top <- safeIdx shown 0
      parent <- trParent top
      find ((== parent) . trPath) (reverse (take start visible))
    row (i, r) = treeRowWidget st query rowWidth (i == sel) r
  in
    case sticky of
      Just parent ->
        withDefAttr floatSelAttr (treeRowWidget st query rowWidth False parent)
          : map row (drop 1 (zip [start ..] shown))
      Nothing -> map row (zip [start ..] shown)

treeInputRow :: St -> TreeView -> [TreeRow] -> Widget Name
treeInputRow st view visible =
  let
    -- the scope is worth saying outright: an empty-looking search is
    -- a different thing when the ignored files are in the haystack
    scoped = case stFileScope st of
      AllFiles -> " (all)"
      VisibleFiles -> ""
    counter =
      ( if T.null (tvInput view)
          then tshow (length (tvAll view)) <> " files"
          else -- files that matched, not rows: the directories kept to
          -- hold them are structure, not results
            tshow (treeFileCount visible) <> "/" <> tshow (length (tvAll view))
      )
        <> scoped
  in
    case tvOp view of
      Just (OpNaming action typed) ->
        hBox
          [ withAttr hintAttr (txt (" " <> namingLabel action <> " "))
          , withAttr statusAttr (txt (typed <> "\9612"))
          ]
      Just (OpConfirming path) ->
        withAttr statusAttr (txt (" delete " <> T.pack path <> " ?"))
      _ ->
        hBox
          [ withAttr statusAttr (txt (" " <> tvInput view <> "\9612"))
          , padLeft Max (withAttr hintAttr (txt (counter <> " ")))
          ]

treeHint :: St -> TreeView -> Text
treeHint st view =
  let
    flat = tvMode view == TreeFlat
    folding = if flat then "" else "\8594\8592 fold \183 "
    other = if flat then "tree" else "flat"
    showing = case stFileScope st of
      AllFiles -> "^a hide ignored \183 "
      VisibleFiles -> "^a all \183 "
  in
    case tvOp view of
      Just (OpNaming _ _) -> "\9166 does it \183 esc cancels"
      Just (OpConfirming _) -> "y deletes it \183 any other key cancels"
      _ ->
        "\8593\8595 move \183 "
          <> folding
          <> "\9166 open \183 \8633 "
          <> other
          <> " \183 "
          <> showing
          <> "^x file \183 esc closes"
