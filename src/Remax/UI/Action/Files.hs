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

import Brick
  ( EventM
  )
import Control.Exception
  ( IOException
  , try
  )
import Control.Monad.State
  ( get
  , modify
  )
import Data.List
  ( findIndex
  , isPrefixOf
  )
import Data.Maybe
  ( fromMaybe
  )
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Remax.Git
  ( isTracked
  , runGit
  )
import Remax.Tree
import Remax.UI.Action.Tree
import Remax.UI.Core.Types
import Remax.UI.Model.Load
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , removeDirectoryRecursive
  , removeFile
  , renamePath
  )
import System.FilePath
  ( takeDirectory
  , (</>)
  )

-- Ctrl-x's menu. One table, rendered and dispatched from the same place,
-- so a key cannot be bound here without appearing in the list — the
-- lesson the space menu already learned.
fileMenuEntries :: [(Text, Text, TreeView -> EventM Name St ())]
fileMenuEntries =
  [ ("n", "new file", startNaming NewFile)
  , ("d", "new directory", startNaming NewDir)
  , ("r", "rename / move", startActing RenamePath)
  , ("c", "duplicate", startActing DuplicatePath)
  , ("x", "delete", startDelete)
  ]

-- The two creating actions start from a directory and a trailing slash,
-- so the path is already where the cursor was and only the name is left
-- to type.
startNaming :: (FilePath -> FileAction) -> TreeView -> EventM Name St ()
startNaming build view = do
  st <- get
  let
    dir = treeTargetDir st view
    seeded = if null dir then "" else T.pack dir <> "/"
  setTree view {tvOp = Just (OpNaming (build dir) seeded)}

-- The two acting-on-a-path actions start from the path itself, so
-- renaming is editing what is already written.
startActing :: (FilePath -> FileAction) -> TreeView -> EventM Name St ()
startActing build view = do
  st <- get
  case treeSelected st view of
    Nothing -> setTree view {tvOp = Nothing} >> setMsg "nothing selected"
    Just row ->
      setTree
        view {tvOp = Just (OpNaming (build (trPath row)) (T.pack (trPath row)))}

startDelete :: TreeView -> EventM Name St ()
startDelete view = do
  st <- get
  case treeSelected st view of
    Nothing -> setTree view {tvOp = Nothing} >> setMsg "nothing selected"
    Just row -> setTree view {tvOp = Just (OpConfirming (trPath row))}

namingLabel :: FileAction -> Text
namingLabel action =
  case action of
    NewFile _ -> "new file"
    NewDir _ -> "new directory"
    RenamePath _ -> "rename to"
    DuplicatePath _ -> "duplicate to"

-- Filesystem work fails in the ordinary ways, and the dialog would
-- rather say why on its status line than take the session down.
tryFileIO :: IO Text -> IO (Either Text Text)
tryFileIO act = do
  got <- try act :: IO (Either IOException Text)
  pure $ case got of
    Left e -> Left (T.pack (show e))
    Right msg -> Right msg

-- A tracked path moves and disappears through git, so a rename stays a
-- rename in the diff instead of arriving as an unrelated delete and add.
-- Everything else — new files, anything untracked, anything ignored —
-- is plain filesystem work.
runFileAction :: FilePath -> FileAction -> FilePath -> IO (Either Text Text)
runFileAction root action rel = do
  let
    full = root </> rel
    named = T.pack rel
  taken <- (||) <$> doesFileExist full <*> doesDirectoryExist full
  case action of
    NewFile _
      | taken -> pure (Left ("already there: " <> named))
      | otherwise -> tryFileIO $ do
          createDirectoryIfMissing True (takeDirectory full)
          TIO.writeFile full ""
          pure ("new file " <> named)
    NewDir _
      | taken -> pure (Left ("already there: " <> named))
      | otherwise -> tryFileIO $ do
          createDirectoryIfMissing True full
          pure ("new directory " <> named)
    RenamePath src
      | src == rel -> pure (Left "that is where it already is")
      | taken -> pure (Left ("already there: " <> named))
      | otherwise -> do
          tracked <- isTracked root src
          let
            done = "renamed to " <> named
          if tracked
            then do
              createDirectoryIfMissing True (takeDirectory full)
              fmap (fmap (const done)) (runGit root ["mv", "--", src, rel])
            else tryFileIO $ do
              createDirectoryIfMissing True (takeDirectory full)
              renamePath (root </> src) full
              pure done
    DuplicatePath src
      | src == rel -> pure (Left "give the copy a different name")
      | taken -> pure (Left ("already there: " <> named))
      | otherwise -> do
          srcIsDir <- doesDirectoryExist (root </> src)
          if srcIsDir
            then pure (Left "only files can be duplicated")
            else tryFileIO $ do
              createDirectoryIfMissing True (takeDirectory full)
              copyFile (root </> src) full
              pure ("copied to " <> named)

runDelete :: FilePath -> FilePath -> IO (Either Text Text)
runDelete root rel = do
  let
    full = root </> rel
    done = "deleted " <> T.pack rel
  tracked <- isTracked root rel
  isDir <- doesDirectoryExist full
  if tracked
    then fmap (fmap (const done)) (runGit root ["rm", "-r", "-f", "--", rel])
    else tryFileIO $ do
      if isDir then removeDirectoryRecursive full else removeFile full
      pure done

-- A path that has just been renamed away or deleted is not worth holding
-- open: its buffer describes a file that is no longer there. A directory
-- takes everything under it.
dropFromSession :: FilePath -> EventM Name St ()
dropFromSession path = do
  let
    under p = p == path || (path ++ "/") `isPrefixOf` p
  modify $ \s ->
    let
      kept = filter (not . under . fvPath) (stFiles s)
    in
      s
        { stFiles = kept
        , stOpened = Set.filter (not . under) (stOpened s)
        , stCurF = max 0 (min (stCurF s) (length kept - 1))
        , stCurH = 0
        }

-- Run it, then let everything catch up: the dialog rebuilds from disk,
-- a path that went away stops being held open, and git is asked again so
-- the change set reflects what just happened.
finishFileOp :: TreeView -> Maybe FilePath -> Either Text Text -> EventM Name St ()
finishFileOp view gone outcome = do
  setTree view {tvOp = Nothing}
  case outcome of
    Left err -> setMsg err
    Right msg -> do
      mapM_ dropFromSession gone
      setMsg msg
      forceGitRefresh
      grown <- get
      case stTree grown of
        Just current -> refreshTreeView current
        Nothing -> pure ()

-- Flip the view, keeping the filter and the row. Coming back to the tree
-- opens the ancestors of wherever flat mode left you, so the file you
-- were on is on screen rather than folded away.
switchTreeMode :: TreeView -> EventM Name St ()
switchTreeMode view = do
  st <- get
  let
    was = treeSelected st view
    onPath = fmap trPath was
    flipped = case tvMode view of
      TreeFlat -> TreeHierarchy
      TreeHierarchy -> TreeFlat
    revealed = case (flipped, onPath) of
      (TreeHierarchy, Just path) ->
        Set.union (tvExpanded view) (Set.fromList (ancestorPaths path))
      _ -> tvExpanded view
    next = view {tvMode = flipped, tvExpanded = revealed}
    landed = case onPath of
      Just path -> findIndex ((== path) . trPath) (treeVisible st next)
      Nothing -> Nothing
  setTree next {tvSel = fromMaybe 0 landed}
