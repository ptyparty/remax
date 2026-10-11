{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Tree
  ( fileMenuLayer
  , treeLayer
  ) where

import Brick qualified as Brick
import Brick.Types qualified as BrickTypes
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as T
import Remax.Git qualified as Git
import Remax.Tree qualified as Tree
import Remax.UI.Action.Files qualified as FileAction
import Remax.UI.Action.Fuzzy qualified as Fuzzy
import Remax.UI.Action.Tree qualified as TreeAction
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Ink qualified as Ink
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Verdict qualified as Verdict
import Remax.UI.Core.Widgets qualified as Widgets
import System.FilePath qualified as FilePath

-- Ctrl-x's menu, drawn from the same table that dispatches it.
fileMenuLayer :: Types.TreeView -> Maybe (BrickTypes.Widget Types.Name)
fileMenuLayer view =
  case Types.tvOp view of
    Just Types.OpMenu ->
      let
        item (key, label, _) =
          Brick.hBox
            [ Brick.withAttr Attrs.noteInlineAttr (Brick.txt (" " <> key))
            , Brick.withAttr Attrs.statusAttr (Brick.txt ("  " <> label))
            ]
      in
        Just
          ( Widgets.cornerFrame "file" "any other key cancels" . Brick.hLimit 24 . Brick.padRight Brick.Max $
              Brick.vBox (map item FileAction.fileMenuEntries)
          )
    _ -> Nothing

-- Fixed-width columns, so one long name cannot shove the numbers out of
-- line. hLimit does what columnRow does for the pickers, but keeps the
-- cells as widgets so they can carry their own colors.
treeCell :: Int -> BrickTypes.Widget Types.Name -> BrickTypes.Widget Types.Name
treeCell w = Brick.hLimit w . Brick.padRight Brick.Max

treeCellR :: Int -> BrickTypes.Widget Types.Name -> BrickTypes.Widget Types.Name
treeCellR w = Brick.hLimit w . Brick.padLeft Brick.Max

treeNameW, treeStatW, treeNumW, treeMeterW :: Int
treeNameW = 34
treeStatW = 2
treeNumW = 6
treeMeterW = 13

statusAttrFor :: Git.PathChange -> Brick.AttrName
statusAttrFor change =
  case change of
    Git.PathAdded -> Attrs.addedMarkAttr
    Git.PathModified -> Attrs.pendingMarkAttr
    Git.PathDeleted -> Attrs.removedMarkAttr
    Git.PathRenamed _ -> Attrs.questionMarkAttr
    Git.PathCopied _ -> Attrs.questionMarkAttr
    Git.PathUntracked -> Attrs.hintAttr
    Git.PathOtherChange -> Attrs.hintAttr

verdictMarkAttr :: Char -> Brick.AttrName
verdictMarkAttr mark =
  case mark of
    '!' -> Attrs.flagMarkAttr
    '?' -> Attrs.questionMarkAttr
    '\10003' -> Attrs.doneMarkAttr
    _ -> Attrs.pendingMarkAttr

-- Which characters of a row's own label the filter matched.
--
-- Positions come from the row's witness — the full path the whole query
-- actually matches — and are then shifted onto the slice of it this row
-- displays. A directory has to be measured against a descendant's path
-- rather than its own: "remaxgit" matches src/Remax/Git.hs, but no part
-- of that query matches src/Remax alone, so asking the directory by
-- itself finds nothing and lights nothing. Every row on the way down to
-- a match now shows its share of it.
rowMatchPositions :: T.Text -> Tree.TreeRow -> Set.Set Int
rowMatchPositions query row
  | T.null query = Set.empty
  | otherwise =
      let
        witness = T.pack (Tree.trWitness row)
        label = T.dropWhileEnd (== '/') (Tree.trLabel row)
        -- the label is the tail of trPath, and trPath is a prefix of the
        -- witness, so this span is the same in both
        offset = length (Tree.trPath row) - T.length label
        inLabel p = p >= offset && p < offset + T.length label
      in
        case Fuzzy.subseqPositions query witness of
          Just ps -> Set.fromList (map (subtract offset) (filter inLabel ps))
          Nothing -> Set.empty

treeRowWidget :: Types.St -> T.Text -> Int -> Bool -> Tree.TreeRow -> BrickTypes.Widget Types.Name
treeRowWidget st query rowWidth isSel row =
  let
    lead =
      if isSel
        then Brick.withAttr Attrs.floatSelBarAttr (Brick.txt "\9614")
        else Brick.txt " "
    body =
      case Tree.trKind row of
        Tree.RowDir roll open -> treeDirRow query rowWidth row roll open
        Tree.RowFile mStat hasHunks open -> treeFileRow st query rowWidth row mStat hasHunks open
        Tree.RowHunk ix -> treeHunkRow st row ix
    w = Brick.padRight Brick.Max (Brick.hBox [lead, Brick.txt " ", body])
  in
    if isSel then Brick.withDefAttr Attrs.floatSelAttr w else w

-- the prefix (indent and fold marker) is never part of the path, so
-- it sits outside the highlighted span
--
-- A row with nothing to say about git has no columns to line up
-- with, so its name takes the whole row rather than being clipped at
-- the width the decorated rows need.
nameCellIn :: T.Text -> Tree.TreeRow -> Int -> Brick.AttrName -> T.Text -> T.Text -> BrickTypes.Widget Types.Name
nameCellIn query row width attr prefix label =
  let
    room = max 1 (width - T.length prefix)
    shown = Widgets.clipT room label
  in
    treeCell
      width
      ( Brick.hBox
          [ Brick.withAttr attr (Brick.txt prefix)
          , Brick.withAttr attr (Fuzzy.highlightAt (rowMatchPositions query row) shown)
          ]
      )

rowIndent :: Tree.TreeRow -> T.Text
rowIndent row = T.replicate (Tree.trDepth row * 2) " "

treeDirRow :: T.Text -> Int -> Tree.TreeRow -> Tree.Rollup -> Bool -> BrickTypes.Widget Types.Name
treeDirRow query rowWidth row roll open =
  let
    marker = if open then "\9662 " else "\9656 "
    changed = Tree.ruChanged roll > 0
    cell = if changed then nameCellIn query row treeNameW else nameCellIn query row (max treeNameW rowWidth)
    tally =
      [ treeCell treeStatW (Brick.txt " ")
      , treeCellR treeNumW (Brick.withAttr Attrs.addedMarkAttr (Brick.txt ("+" <> Types.tshow (Tree.ruAdded roll))))
      , treeCellR treeNumW (Brick.withAttr Attrs.removedMarkAttr (Brick.txt ("\8722" <> Types.tshow (Tree.ruRemoved roll))))
      , Brick.txt "  "
      , treeCell
          treeMeterW
          ( if open
              then Brick.txt " "
              else Brick.withAttr Attrs.hintAttr (Brick.txt (Types.tshow (Tree.ruChanged roll) <> " chg"))
          )
      ]
  in
    Brick.hBox (cell Attrs.hintAttr (rowIndent row <> marker) (Tree.trLabel row <> "/") : (if changed then tally else []))

treeFileRow :: Types.St -> T.Text -> Int -> Tree.TreeRow -> Maybe Git.PathStat -> Bool -> Bool -> BrickTypes.Widget Types.Name
treeFileRow st query rowWidth row mStat hasHunks open =
  let
    marker
      | not hasHunks = "  "
      | open = "\9662 "
      | otherwise = "\9656 "
    curPath = maybe "" Types.fvPath (Types.currentFileMay st)
    nameAttr = if Tree.trPath row == curPath then Attrs.trayFileCurAttr else Attrs.trayFileAttr
    letter = case fmap Git.psChange mStat of
      Just change -> Brick.withAttr (statusAttrFor change) (Brick.txt (Git.pathChangeLetter change))
      Nothing -> Brick.txt " "
    -- untouched and not in the session: nothing to the right of
    -- the name, so the name gets the room
    bare = mStat == Nothing && not hasHunks
  in
    if bare
      then nameCellIn query row (max treeNameW rowWidth) nameAttr (rowIndent row <> marker) (Tree.trLabel row)
      else
        Brick.hBox
          ( [ nameCellIn query row treeNameW nameAttr (rowIndent row <> marker) (Tree.trLabel row)
            , treeCell treeStatW letter
            ]
              ++ countCells mStat
              ++ [Brick.txt "  ", meterCell st (Tree.trPath row)]
              ++ fromCell mStat
          )

countCells :: Maybe Git.PathStat -> [BrickTypes.Widget Types.Name]
countCells mStat =
  case fmap Git.psCounts mStat of
    Just (Just (a, r)) ->
      [ treeCellR treeNumW (if a > 0 then Brick.withAttr Attrs.addedMarkAttr (Brick.txt ("+" <> Types.tshow a)) else Brick.txt " ")
      , treeCellR treeNumW (if r > 0 then Brick.withAttr Attrs.removedMarkAttr (Brick.txt ("\8722" <> Types.tshow r)) else Brick.txt " ")
      ]
    Just Nothing ->
      case fmap Git.psChange mStat of
        Just Git.PathUntracked -> [treeCell (treeNumW * 2) (Brick.txt " ")]
        _ ->
          [ treeCellR treeNumW (Brick.withAttr Attrs.trayDeclAttr (Brick.txt "bin"))
          , treeCell treeNumW (Brick.txt " ")
          ]
    Nothing -> [treeCell (treeNumW * 2) (Brick.txt " ")]

meterCell :: Types.St -> FilePath -> BrickTypes.Widget Types.Name
meterCell st path =
  case List.find ((== path) . Types.fvPath) (Types.stFiles st) of
    Just fv
      | not (null (Types.fvRuns fv)) ->
          let
            total = length (Types.fvRuns fv)
            done = length (filter (not . Verdict.runPending (Types.stVerdicts st)) (Types.fvRuns fv))
          in
            treeCell treeMeterW (Ink.meterW done total)
    _ -> treeCell treeMeterW (Brick.txt " ")

fromCell :: Maybe Git.PathStat -> [BrickTypes.Widget Types.Name]
fromCell mStat =
  let
    renamedFrom from = Brick.withAttr Attrs.hintAttr (Brick.txt (" \8592 " <> T.pack (FilePath.takeFileName from)))
  in
    case fmap Git.psChange mStat of
      Just (Git.PathRenamed from) -> [renamedFrom from]
      Just (Git.PathCopied from) -> [renamedFrom from]
      _ -> []

treeHunkRow :: Types.St -> Tree.TreeRow -> Int -> BrickTypes.Widget Types.Name
treeHunkRow st row ix =
  case TreeAction.treeHunkInfo st (Tree.trPath row) ix of
    Nothing -> Brick.txt " "
    Just (mark, ln, decl, hasNote) ->
      Brick.hBox
        [ Brick.txt (rowIndent row <> "  ")
        , Brick.withAttr (verdictMarkAttr mark) (Brick.str [mark])
        , Brick.txt (" :" <> Types.tshow ln)
        , Brick.withAttr Attrs.trayDeclAttr (Brick.txt decl)
        , if hasNote then Brick.withAttr Attrs.noteInlineAttr (Brick.txt " \8801") else Brick.txt ""
        ]

treeLayer :: Types.St -> Types.TreeView -> BrickTypes.Widget Types.Name
treeLayer st view =
  BrickTypes.Widget BrickTypes.Greedy BrickTypes.Greedy $ do
    ctx <- BrickTypes.getContext
    let
      boxWidth = min 96 (max 60 (BrickTypes.availWidth ctx - 16))
      -- as tall as the tree needs and no taller: a folded tree should not
      -- open a full-height box of nothing
      roomFor = max 6 (BrickTypes.availHeight ctx - 14)
      visible = TreeAction.treeVisible st view
      visibleCount = max 6 (min roomFor (length visible))
      -- what a row has to play with once the frame and the selection
      -- bar have taken theirs
      rowWidth = max treeNameW (boxWidth - 6)
      body = treeBodyRows st view visible visibleCount rowWidth
      filler = replicate (visibleCount - length body) (Brick.txt " ")
      title = if Types.tvMode view == Types.TreeFlat then "files" else "tree"
    BrickTypes.render $
      Widgets.floatFrame title (treeHint st view) $
        Brick.hLimit boxWidth (Brick.vBox (treeInputRow st view visible : Brick.txt " " : (body ++ filler)))

treeBodyRows :: Types.St -> Types.TreeView -> [Tree.TreeRow] -> Int -> Int -> [BrickTypes.Widget Types.Name]
treeBodyRows st view visible visibleCount rowWidth =
  let
    query = Types.tvInput view
    sel = max 0 (min (length visible - 1) (Types.tvSel view))
    start =
      max 0 (min (sel - div visibleCount 2) (max 0 (length visible - visibleCount)))
    -- when the enclosing directory (or file, for a hunk) has scrolled
    -- off, pin it: otherwise a row loses all sense of whose it is
    shown = take visibleCount (drop start visible)
    sticky = do
      top <- Types.safeIdx shown 0
      parent <- Tree.trParent top
      List.find ((== parent) . Tree.trPath) (reverse (take start visible))
    row (i, r) = treeRowWidget st query rowWidth (i == sel) r
  in
    case sticky of
      Just parent ->
        Brick.withDefAttr Attrs.floatSelAttr (treeRowWidget st query rowWidth False parent)
          : map row (drop 1 (zip [start ..] shown))
      Nothing -> map row (zip [start ..] shown)

treeInputRow :: Types.St -> Types.TreeView -> [Tree.TreeRow] -> BrickTypes.Widget Types.Name
treeInputRow st view visible =
  let
    -- the scope is worth saying outright: an empty-looking search is
    -- a different thing when the ignored files are in the haystack
    scoped = case Types.stFileScope st of
      Git.AllFiles -> " (all)"
      Git.VisibleFiles -> ""
    counter =
      ( if T.null (Types.tvInput view)
          then Types.tshow (length (Types.tvAll view)) <> " files"
          else -- files that matched, not rows: the directories kept to
          -- hold them are structure, not results
            Types.tshow (TreeAction.treeFileCount visible) <> "/" <> Types.tshow (length (Types.tvAll view))
      )
        <> scoped
  in
    case Types.tvOp view of
      Just (Types.OpNaming action typed) ->
        Brick.hBox
          [ Brick.withAttr Attrs.hintAttr (Brick.txt (" " <> FileAction.namingLabel action <> " "))
          , Brick.withAttr Attrs.statusAttr (Brick.txt (typed <> "\9612"))
          ]
      Just (Types.OpConfirming path) ->
        Brick.withAttr Attrs.statusAttr (Brick.txt (" delete " <> T.pack path <> " ?"))
      _ ->
        Brick.hBox
          [ Brick.withAttr Attrs.statusAttr (Brick.txt (" " <> Types.tvInput view <> "\9612"))
          , Brick.padLeft Brick.Max (Brick.withAttr Attrs.hintAttr (Brick.txt (counter <> " ")))
          ]

treeHint :: Types.St -> Types.TreeView -> T.Text
treeHint st view =
  let
    flat = Types.tvMode view == Types.TreeFlat
    folding = if flat then "" else "\8594\8592 fold \183 "
    other = if flat then "tree" else "flat"
    showing = case Types.stFileScope st of
      Git.AllFiles -> "^a hide ignored \183 "
      Git.VisibleFiles -> "^a all \183 "
  in
    case Types.tvOp view of
      Just (Types.OpNaming _ _) -> "\9166 does it \183 esc cancels"
      Just (Types.OpConfirming _) -> "y deletes it \183 any other key cancels"
      _ ->
        "\8593\8595 move \183 "
          <> folding
          <> "\9166 open \183 \8633 "
          <> other
          <> " \183 "
          <> showing
          <> "^x file \183 esc closes"
