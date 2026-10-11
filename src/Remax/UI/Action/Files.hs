{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Files
  ( fileMenuEntries
  , namingLabel
  , runFileAction
  , runDelete
  , finishFileOp
  , switchTreeMode
  ) where

import Brick qualified as Brick
import Control.Exception qualified as Exception
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Remax.Git qualified as Git
import Remax.Tree qualified as Tree
import Remax.UI.Action.Tree qualified as TreeAction
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Model.Load qualified as Load
import System.Directory qualified as Directory
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

-- Ctrl-x's menu. One table, rendered and dispatched from the same place,
-- so a key cannot be bound here without appearing in the list — the
-- lesson the space menu already learned.
fileMenuEntries :: [(T.Text, T.Text, Types.TreeView -> Brick.EventM Types.Name Types.St ())]
fileMenuEntries =
  [ ("n", "new file", startNaming Types.NewFile)
  , ("d", "new directory", startNaming Types.NewDir)
  , ("r", "rename / move", startActing Types.RenamePath)
  , ("c", "duplicate", startActing Types.DuplicatePath)
  , ("x", "delete", startDelete)
  ]

-- The two creating actions start from a directory and a trailing slash,
-- so the path is already where the cursor was and only the name is left
-- to type.
startNaming :: (FilePath -> Types.FileAction) -> Types.TreeView -> Brick.EventM Types.Name Types.St ()
startNaming build view = do
  st <- State.get
  let
    dir = TreeAction.treeTargetDir st view
    seeded = if null dir then "" else T.pack dir <> "/"
  TreeAction.setTree view {Types.tvOp = Just (Types.OpNaming (build dir) seeded)}

-- The two acting-on-a-path actions start from the path itself, so
-- renaming is editing what is already written.
startActing :: (FilePath -> Types.FileAction) -> Types.TreeView -> Brick.EventM Types.Name Types.St ()
startActing build view = do
  st <- State.get
  case TreeAction.treeSelected st view of
    Nothing -> TreeAction.setTree view {Types.tvOp = Nothing} >> Types.setMsg "nothing selected"
    Just row ->
      TreeAction.setTree
        view {Types.tvOp = Just (Types.OpNaming (build (Tree.trPath row)) (T.pack (Tree.trPath row)))}

startDelete :: Types.TreeView -> Brick.EventM Types.Name Types.St ()
startDelete view = do
  st <- State.get
  case TreeAction.treeSelected st view of
    Nothing -> TreeAction.setTree view {Types.tvOp = Nothing} >> Types.setMsg "nothing selected"
    Just row -> TreeAction.setTree view {Types.tvOp = Just (Types.OpConfirming (Tree.trPath row))}

namingLabel :: Types.FileAction -> T.Text
namingLabel action =
  case action of
    Types.NewFile _ -> "new file"
    Types.NewDir _ -> "new directory"
    Types.RenamePath _ -> "rename to"
    Types.DuplicatePath _ -> "duplicate to"

-- Filesystem work fails in the ordinary ways, and the dialog would
-- rather say why on its status line than take the session down.
tryFileIO :: IO T.Text -> IO (Either T.Text T.Text)
tryFileIO act = do
  got <- Exception.try act :: IO (Either Exception.IOException T.Text)
  pure $ case got of
    Left e -> Left (T.pack (show e))
    Right msg -> Right msg

-- A tracked path moves and disappears through git, so a rename stays a
-- rename in the diff instead of arriving as an unrelated delete and add.
-- Everything else — new files, anything untracked, anything ignored —
-- is plain filesystem work.
runFileAction :: FilePath -> Types.FileAction -> FilePath -> IO (Either T.Text T.Text)
runFileAction root action rel = do
  let
    full = root </> rel
    named = T.pack rel
  taken <- (||) <$> Directory.doesFileExist full <*> Directory.doesDirectoryExist full
  case action of
    Types.NewFile _
      | taken -> pure (Left ("already there: " <> named))
      | otherwise -> tryFileIO $ do
          Directory.createDirectoryIfMissing True (FilePath.takeDirectory full)
          TIO.writeFile full ""
          pure ("new file " <> named)
    Types.NewDir _
      | taken -> pure (Left ("already there: " <> named))
      | otherwise -> tryFileIO $ do
          Directory.createDirectoryIfMissing True full
          pure ("new directory " <> named)
    Types.RenamePath src
      | src == rel -> pure (Left "that is where it already is")
      | taken -> pure (Left ("already there: " <> named))
      | otherwise -> do
          tracked <- Git.isTracked root src
          let
            done = "renamed to " <> named
          if tracked
            then do
              Directory.createDirectoryIfMissing True (FilePath.takeDirectory full)
              fmap (fmap (const done)) (Git.runGit root ["mv", "--", src, rel])
            else tryFileIO $ do
              Directory.createDirectoryIfMissing True (FilePath.takeDirectory full)
              Directory.renamePath (root </> src) full
              pure done
    Types.DuplicatePath src
      | src == rel -> pure (Left "give the copy a different name")
      | taken -> pure (Left ("already there: " <> named))
      | otherwise -> do
          srcIsDir <- Directory.doesDirectoryExist (root </> src)
          if srcIsDir
            then pure (Left "only files can be duplicated")
            else tryFileIO $ do
              Directory.createDirectoryIfMissing True (FilePath.takeDirectory full)
              Directory.copyFile (root </> src) full
              pure ("copied to " <> named)

runDelete :: FilePath -> FilePath -> IO (Either T.Text T.Text)
runDelete root rel = do
  let
    full = root </> rel
    done = "deleted " <> T.pack rel
  tracked <- Git.isTracked root rel
  isDir <- Directory.doesDirectoryExist full
  if tracked
    then fmap (fmap (const done)) (Git.runGit root ["rm", "-r", "-f", "--", rel])
    else tryFileIO $ do
      if isDir then Directory.removeDirectoryRecursive full else Directory.removeFile full
      pure done

-- A path that has just been renamed away or deleted is not worth holding
-- open: its buffer describes a file that is no longer there. A directory
-- takes everything under it.
dropFromSession :: FilePath -> Brick.EventM Types.Name Types.St ()
dropFromSession path = do
  let
    under p = p == path || (path ++ "/") `List.isPrefixOf` p
  State.modify $ \s ->
    let
      kept = filter (not . under . Types.fvPath) (Types.stFiles s)
    in
      s
        { Types.stFiles = kept
        , Types.stOpened = Set.filter (not . under) (Types.stOpened s)
        , Types.stCurF = max 0 (min (Types.stCurF s) (length kept - 1))
        , Types.stCurH = 0
        }

-- Run it, then let everything catch up: the dialog rebuilds from disk,
-- a path that went away stops being held open, and git is asked again so
-- the change set reflects what just happened.
finishFileOp :: Types.TreeView -> Maybe FilePath -> Either T.Text T.Text -> Brick.EventM Types.Name Types.St ()
finishFileOp view gone outcome = do
  TreeAction.setTree view {Types.tvOp = Nothing}
  case outcome of
    Left err -> Types.setMsg err
    Right msg -> do
      mapM_ dropFromSession gone
      Types.setMsg msg
      Load.forceGitRefresh
      grown <- State.get
      case Types.stTree grown of
        Just current -> TreeAction.refreshTreeView current
        Nothing -> pure ()

-- Flip the view, keeping the filter and the row. Coming back to the tree
-- opens the ancestors of wherever flat mode left you, so the file you
-- were on is on screen rather than folded away.
switchTreeMode :: Types.TreeView -> Brick.EventM Types.Name Types.St ()
switchTreeMode view = do
  st <- State.get
  let
    was = TreeAction.treeSelected st view
    onPath = fmap Tree.trPath was
    flipped = case Types.tvMode view of
      Types.TreeFlat -> Types.TreeHierarchy
      Types.TreeHierarchy -> Types.TreeFlat
    revealed = case (flipped, onPath) of
      (Types.TreeHierarchy, Just path) ->
        Set.union (Types.tvExpanded view) (Set.fromList (TreeAction.ancestorPaths path))
      _ -> Types.tvExpanded view
    next = view {Types.tvMode = flipped, Types.tvExpanded = revealed}
    landed = case onPath of
      Just path -> List.findIndex ((== path) . Tree.trPath) (TreeAction.treeVisible st next)
      Nothing -> Nothing
  TreeAction.setTree next {Types.tvSel = Maybe.fromMaybe 0 landed}
