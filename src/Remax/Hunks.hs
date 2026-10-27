{-# LANGUAGE OverloadedStrings #-}

module Remax.Hunks
  ( Hunk (..)
  , Numbered (..)
  , changeSetHunks
  ) where

import Control.Exception qualified as Exception
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Remax.Config qualified as Config
import Remax.DiffModel qualified as DiffModel
import Remax.Git qualified as Git
import Remax.Highlight qualified as Highlight
import System.Directory qualified as Directory
import System.FilePath ((</>))

data Hunk = Hunk
  { hunkId :: T.Text
  , hunkFile :: FilePath
  , hunkLine :: Int
  , hunkLines :: [T.Text]
  , hunkKeys :: [T.Text]
  , hunkNumbers :: [Numbered]
  }
  deriving (Eq, Show)

changeSetHunks :: Config.Config -> Git.ReviewMode -> FilePath -> IO [Hunk]
changeSetHunks cfg mode root = do
  ignores <- Config.loadIgnores root
  names <-
    fmap
      (filter (not . Config.ignoredPath ignores))
      (Git.reviewFiles mode (Config.cfgUntracked cfg) root)
  fmap concat (mapM (fileHunks cfg mode root) names)

fileHunks :: Config.Config -> Git.ReviewMode -> FilePath -> FilePath -> IO [Hunk]
fileHunks cfg mode root path = do
  sides <- fileSides mode root path
  pure $ case sides of
    Nothing -> []
    Just (base, new) ->
      let
        ws = if Config.cfgIgnoreWs cfg then DiffModel.WhitespaceIgnored else DiffModel.WhitespaceMatters
        rows = DiffModel.diffRows ws base new
        (_, runs) =
          DiffModel.assemble (Config.cfgSplit cfg) (T.pack root) (T.pack path) rows (Highlight.plainLines new)
        number numbers r =
          let
            (mine, rest) = List.splitAt (length (DiffModel.runKeys r)) numbers
          in
            (rest, toHunk path mine r)
      in
        Maybe.catMaybes (snd (List.mapAccumL number (changedNumbers rows) runs))

data Numbered = OnBase Int | OnHead Int
  deriving (Eq, Show)

changedNumbers :: [(Char, Maybe Int, T.Text)] -> [Numbered]
changedNumbers =
  let
    go base rows =
      case rows of
        [] -> []
        ((mark, headNo, _) : rest) -> case mark of
          '+' -> OnHead (maybe 1 (+ 1) headNo) : go base rest
          '-' -> OnBase base : go (base + 1) rest
          _ -> go (base + 1) rest
  in
    go 1

toHunk :: FilePath -> [Numbered] -> DiffModel.Run -> Maybe Hunk
toHunk path numbers r =
  case DiffModel.runKeys r of
    [] -> Nothing
    (first : _) ->
      Just
        Hunk
          { hunkId = first
          , hunkFile = path
          , hunkLine = DiffModel.runAnchor r + 1
          , hunkLines = DiffModel.runLines r
          , hunkKeys = DiffModel.runKeys r
          , hunkNumbers = numbers
          }

fileSides :: Git.ReviewMode -> FilePath -> FilePath -> IO (Maybe ([T.Text], [T.Text]))
fileSides mode root path =
  case Git.sourceOf mode of
    Git.FromRef tip -> do
      got <- Git.refContent root tip path
      case got of
        Just Git.RefBinary -> pure Nothing
        Just (Git.RefText new) -> do
          base <- Git.baseLines mode root path
          pure (Just (base, new))
        Nothing -> removedSides mode root path
    Git.FromWorkingTree -> do
      present <- Directory.doesFileExist (root </> path)
      if not present
        then removedSides mode root path
        else do
          readable <- Exception.try (Git.looksBinary root path) :: IO (Either Exception.IOException Bool)
          case readable of
            Right False -> do
              got <- Exception.try (TIO.readFile (root </> path)) :: IO (Either Exception.IOException T.Text)
              case got of
                Left _ -> pure Nothing
                Right txt -> do
                  base <- Git.baseLines mode root path
                  pure (Just (base, T.lines txt))
            _ -> pure Nothing

removedSides :: Git.ReviewMode -> FilePath -> FilePath -> IO (Maybe ([T.Text], [T.Text]))
removedSides mode root path = do
  base <- Git.baseLines mode root path
  pure $ case base of
    [] -> Nothing
    _ -> Just (base, [])
