{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.TreeKeys
  ( handleTreeKey
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Remax.Git qualified as Git
import Remax.Tree qualified as Tree
import Remax.UI.Action.Files qualified as FileAction
import Remax.UI.Action.Open qualified as Open
import Remax.UI.Action.Tree qualified as TreeAction
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Model.Nav qualified as Nav

-- The dialog is modal while an operation is under way: the menu, then a
-- path to type or a deletion to confirm. Browsing keys only apply when
-- none of that is open.
handleTreeKey :: V.Event -> Types.TreeView -> Brick.EventM Types.Name Types.St ()
handleTreeKey ev view =
  case Types.tvOp view of
    Just Types.OpMenu -> fileMenuKey ev view
    Just (Types.OpNaming action typed) -> namingKey ev view action typed
    Just (Types.OpConfirming path) -> confirmKey ev view path
    Nothing -> browseTreeKey ev view

-- Any key not in the table cancels, which is what the frame promises.
fileMenuKey :: V.Event -> Types.TreeView -> Brick.EventM Types.Name Types.St ()
fileMenuKey ev view =
  case ev of
    V.EvKey (V.KChar c) [] ->
      case List.find (\(key, _, _) -> key == T.singleton c) FileAction.fileMenuEntries of
        Just (_, _, act) -> act view
        Nothing -> TreeAction.setTree view {Types.tvOp = Nothing}
    _ -> TreeAction.setTree view {Types.tvOp = Nothing}

namingKey :: V.Event -> Types.TreeView -> Types.FileAction -> T.Text -> Brick.EventM Types.Name Types.St ()
namingKey ev view action typed = do
  st <- State.get
  let
    retype next = TreeAction.setTree view {Types.tvOp = Just (Types.OpNaming action next)}
  case ev of
    V.EvKey V.KEsc [] -> TreeAction.setTree view {Types.tvOp = Nothing}
    V.EvKey V.KBS [] -> retype (T.dropEnd 1 typed)
    V.EvKey (V.KChar c) [] -> retype (T.snoc typed c)
    V.EvKey V.KEnter [] ->
      case TreeAction.resolveInRepo (Types.stRoot st) typed of
        Left err -> do
          TreeAction.setTree view {Types.tvOp = Nothing}
          Types.setMsg err
        Right rel -> withWritableTree view $ \grant -> do
          outcome <- MonadIO.liftIO (FileAction.runFileAction grant (Types.stRoot st) action rel)
          -- a rename takes the old path out of the session; the other
          -- three leave what is already open alone
          let
            gone = case action of
              Types.RenamePath src -> Just src
              _ -> Nothing
          FileAction.finishFileOp view gone outcome
    _ -> pure ()

confirmKey :: V.Event -> Types.TreeView -> FilePath -> Brick.EventM Types.Name Types.St ()
confirmKey ev view path = do
  st <- State.get
  case ev of
    V.EvKey (V.KChar 'y') [] -> withWritableTree view $ \grant -> do
      outcome <- MonadIO.liftIO (FileAction.runDelete grant (Types.stRoot st) path)
      FileAction.finishFileOp view (Just path) outcome
    _ -> TreeAction.setTree view {Types.tvOp = Nothing}

withWritableTree :: Types.TreeView -> (Git.Writable -> Brick.EventM Types.Name Types.St ()) -> Brick.EventM Types.Name Types.St ()
withWritableTree view act = do
  st <- State.get
  case Git.writable (Types.effectiveMode st) of
    Right grant -> act grant
    Left tip -> do
      TreeAction.setTree view {Types.tvOp = Nothing}
      Types.setMsg (Types.readOnlyMsg tip)

browseTreeKey :: V.Event -> Types.TreeView -> Brick.EventM Types.Name Types.St ()
browseTreeKey ev view = do
  st <- State.get
  let
    visible = TreeAction.treeVisible st view
    bound = max 0 (length visible - 1)
    move d = TreeAction.setTree view {Types.tvSel = max 0 (min bound (Types.tvSel view + d))}
    selected = TreeAction.treeSelected st view
  case ev of
    -- Escape leaves, whatever has been typed. It used to clear the
    -- filter first and only close on a second press, which made leaving
    -- a two-step gesture that also threw the filter away — so ␣' brought
    -- the dialog back empty, having watched the query be deleted on the
    -- way out. The picker has always closed on the first Escape, and so
    -- does helix; a filter is cleared by deleting it.
    V.EvKey V.KEsc [] -> closeTree
    V.EvKey V.KUp [] -> move (-1)
    V.EvKey (V.KChar 'p') [V.MCtrl] -> move (-1)
    V.EvKey V.KDown [] -> move 1
    V.EvKey (V.KChar 'n') [V.MCtrl] -> move 1
    V.EvKey V.KRight [] -> Monad.forM_ selected (treeUnfoldRow view)
    V.EvKey V.KLeft [] -> Monad.forM_ selected (treeFoldRow st view)
    V.EvKey V.KEnter [] -> Monad.forM_ selected (treeActivate st view)
    -- Tab moves between flat and hierarchical without losing the filter,
    -- and lands on the same file it was pointing at. (Tab is safe to
    -- spend here: a path containing one is a problem no keybinding can
    -- fix.)
    V.EvKey (V.KChar '\t') [] -> FileAction.switchTreeMode view
    -- the ignored files, and back. A modified key because every
    -- printable one belongs to the filter.
    V.EvKey (V.KChar 'a') [V.MCtrl] -> TreeAction.toggleTreeScope view
    V.EvKey (V.KChar 'x') [V.MCtrl] -> TreeAction.setTree view {Types.tvOp = Just Types.OpMenu}
    V.EvKey V.KBS [] -> treeRetype st view (T.dropEnd 1 (Types.tvInput view))
    V.EvKey (V.KChar c) [] -> treeRetype st view (T.snoc (Types.tvInput view) c)
    _ -> pure ()

closeTree :: Brick.EventM Types.Name Types.St ()
closeTree = State.modify (\s -> s {Types.stTree = Nothing})

-- land on the first file that matched, not on a directory kept only
-- to hold it: the directories are structure, the file is the answer
treeRetype :: Types.St -> Types.TreeView -> T.Text -> Brick.EventM Types.Name Types.St ()
treeRetype st view q =
  let
    next = view {Types.tvInput = q, Types.tvSel = 0}
    -- flat rows lead with their file, so the answer is nought and
    -- asking costs a whole pass over the candidates — which the
    -- render is about to make anyway, and which on a large
    -- repository is seconds rather than milliseconds
    firstFile = case Types.tvMode view of
      Types.TreeFlat -> Just 0
      Types.TreeHierarchy -> List.findIndex Tree.rowIsFile (TreeAction.treeVisible st next)
  in
    TreeAction.setTree next {Types.tvSel = Maybe.fromMaybe 0 firstFile}

treeUnfoldRow :: Types.TreeView -> Tree.TreeRow -> Brick.EventM Types.Name Types.St ()
treeUnfoldRow view row =
  Monad.when (Tree.rowExpandable row) $
    TreeAction.setTree view {Types.tvExpanded = Set.insert (Tree.trPath row) (Types.tvExpanded view)}

treeFoldRow :: Types.St -> Types.TreeView -> Tree.TreeRow -> Brick.EventM Types.Name Types.St ()
treeFoldRow st view row
  | Tree.rowExpandable row && Set.member (Tree.trPath row) (Types.tvExpanded view) =
      TreeAction.setTree view {Types.tvExpanded = Set.delete (Tree.trPath row) (Types.tvExpanded view)}
  | otherwise = case Tree.trParent row of
      Nothing -> pure ()
      Just parent ->
        let
          folded = view {Types.tvExpanded = Set.delete parent (Types.tvExpanded view)}
          at = List.findIndex ((== parent) . Tree.trPath) (TreeAction.treeVisible st folded)
        in
          TreeAction.setTree folded {Types.tvSel = Maybe.fromMaybe (Types.tvSel view) at}

treeActivate :: Types.St -> Types.TreeView -> Tree.TreeRow -> Brick.EventM Types.Name Types.St ()
treeActivate st view row =
  case Tree.trKind row of
    Tree.RowDir _ open
      | open -> TreeAction.setTree view {Types.tvExpanded = Set.delete (Tree.trPath row) (Types.tvExpanded view)}
      | otherwise -> TreeAction.setTree view {Types.tvExpanded = Set.insert (Tree.trPath row) (Types.tvExpanded view)}
    Tree.RowHunk ix -> do
      closeTree
      case List.findIndex ((== Tree.trPath row) . Types.fvPath) (Types.stFiles st) of
        Just f -> Nav.visitAnchor f ix
        Nothing -> Types.setMsg "that file has left the session"
    -- a deletion opens like anything else now: the change set
    -- carries it as a file whose every line is removed
    Tree.RowFile _ hasHunks open
      | hasHunks && not open ->
          TreeAction.setTree view {Types.tvExpanded = Set.insert (Tree.trPath row) (Types.tvExpanded view)}
      | otherwise -> do
          closeTree
          Open.openFilePath (T.pack (Tree.trPath row))
