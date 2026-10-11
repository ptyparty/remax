{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.TreeKeys
  ( handleTreeKey
  ) where

import Brick
  ( EventM
  )
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
  ( get
  , modify
  )
import Data.List
  ( find
  , findIndex
  )
import Data.Maybe
  ( fromMaybe
  )
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Remax.Tree
import Remax.UI.Action.Files
import Remax.UI.Action.Open
import Remax.UI.Action.Tree
import Remax.UI.Core.Types
import Remax.UI.Model.Nav

-- The dialog is modal while an operation is under way: the menu, then a
-- path to type or a deletion to confirm. Browsing keys only apply when
-- none of that is open.
handleTreeKey :: V.Event -> TreeView -> EventM Name St ()
handleTreeKey ev view =
  case tvOp view of
    Just OpMenu -> fileMenuKey ev view
    Just (OpNaming action typed) -> namingKey ev view action typed
    Just (OpConfirming path) -> confirmKey ev view path
    Nothing -> browseTreeKey ev view

-- Any key not in the table cancels, which is what the frame promises.
fileMenuKey :: V.Event -> TreeView -> EventM Name St ()
fileMenuKey ev view =
  case ev of
    V.EvKey (V.KChar c) [] ->
      case find (\(key, _, _) -> key == T.singleton c) fileMenuEntries of
        Just (_, _, act) -> act view
        Nothing -> setTree view {tvOp = Nothing}
    _ -> setTree view {tvOp = Nothing}

namingKey :: V.Event -> TreeView -> FileAction -> Text -> EventM Name St ()
namingKey ev view action typed = do
  st <- get
  let
    retype next = setTree view {tvOp = Just (OpNaming action next)}
  case ev of
    V.EvKey V.KEsc [] -> setTree view {tvOp = Nothing}
    V.EvKey V.KBS [] -> retype (T.dropEnd 1 typed)
    V.EvKey (V.KChar c) [] -> retype (T.snoc typed c)
    V.EvKey V.KEnter [] ->
      case resolveInRepo (stRoot st) typed of
        Left err -> do
          setTree view {tvOp = Nothing}
          setMsg err
        Right rel -> do
          outcome <- liftIO (runFileAction (stRoot st) action rel)
          -- a rename takes the old path out of the session; the other
          -- three leave what is already open alone
          let
            gone = case action of
              RenamePath src -> Just src
              _ -> Nothing
          finishFileOp view gone outcome
    _ -> pure ()

confirmKey :: V.Event -> TreeView -> FilePath -> EventM Name St ()
confirmKey ev view path = do
  st <- get
  case ev of
    V.EvKey (V.KChar 'y') [] -> do
      outcome <- liftIO (runDelete (stRoot st) path)
      finishFileOp view (Just path) outcome
    _ -> setTree view {tvOp = Nothing}

browseTreeKey :: V.Event -> TreeView -> EventM Name St ()
browseTreeKey ev view = do
  st <- get
  let
    visible = treeVisible st view
    bound = max 0 (length visible - 1)
    move d = setTree view {tvSel = max 0 (min bound (tvSel view + d))}
    selected = treeSelected st view
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
    V.EvKey V.KRight [] -> forM_ selected (treeUnfoldRow view)
    V.EvKey V.KLeft [] -> forM_ selected (treeFoldRow st view)
    V.EvKey V.KEnter [] -> forM_ selected (treeActivate st view)
    -- Tab moves between flat and hierarchical without losing the filter,
    -- and lands on the same file it was pointing at. (Tab is safe to
    -- spend here: a path containing one is a problem no keybinding can
    -- fix.)
    V.EvKey (V.KChar '\t') [] -> switchTreeMode view
    -- the ignored files, and back. A modified key because every
    -- printable one belongs to the filter.
    V.EvKey (V.KChar 'a') [V.MCtrl] -> toggleTreeScope view
    V.EvKey (V.KChar 'x') [V.MCtrl] -> setTree view {tvOp = Just OpMenu}
    V.EvKey V.KBS [] -> treeRetype st view (T.dropEnd 1 (tvInput view))
    V.EvKey (V.KChar c) [] -> treeRetype st view (T.snoc (tvInput view) c)
    _ -> pure ()

closeTree :: EventM Name St ()
closeTree = modify (\s -> s {stTree = Nothing})

-- land on the first file that matched, not on a directory kept only
-- to hold it: the directories are structure, the file is the answer
treeRetype :: St -> TreeView -> Text -> EventM Name St ()
treeRetype st view q =
  let
    next = view {tvInput = q, tvSel = 0}
    -- flat rows lead with their file, so the answer is nought and
    -- asking costs a whole pass over the candidates — which the
    -- render is about to make anyway, and which on a large
    -- repository is seconds rather than milliseconds
    firstFile = case tvMode view of
      TreeFlat -> Just 0
      TreeHierarchy -> findIndex rowIsFile (treeVisible st next)
  in
    setTree next {tvSel = fromMaybe 0 firstFile}

treeUnfoldRow :: TreeView -> TreeRow -> EventM Name St ()
treeUnfoldRow view row =
  when (rowExpandable row) $
    setTree view {tvExpanded = Set.insert (trPath row) (tvExpanded view)}

treeFoldRow :: St -> TreeView -> TreeRow -> EventM Name St ()
treeFoldRow st view row
  | rowExpandable row && Set.member (trPath row) (tvExpanded view) =
      setTree view {tvExpanded = Set.delete (trPath row) (tvExpanded view)}
  | otherwise = case trParent row of
      Nothing -> pure ()
      Just parent ->
        let
          folded = view {tvExpanded = Set.delete parent (tvExpanded view)}
          at = findIndex ((== parent) . trPath) (treeVisible st folded)
        in
          setTree folded {tvSel = fromMaybe (tvSel view) at}

treeActivate :: St -> TreeView -> TreeRow -> EventM Name St ()
treeActivate st view row =
  case trKind row of
    RowDir _ open
      | open -> setTree view {tvExpanded = Set.delete (trPath row) (tvExpanded view)}
      | otherwise -> setTree view {tvExpanded = Set.insert (trPath row) (tvExpanded view)}
    RowHunk ix -> do
      closeTree
      case findIndex ((== trPath row) . fvPath) (stFiles st) of
        Just f -> parkAnchor f ix
        Nothing -> setMsg "that file has left the session"
    -- a deletion opens like anything else now: the change set
    -- carries it as a file whose every line is removed
    RowFile _ hasHunks open
      | hasHunks && not open ->
          setTree view {tvExpanded = Set.insert (trPath row) (tvExpanded view)}
      | otherwise -> do
          closeTree
          openFilePath (T.pack (trPath row))
