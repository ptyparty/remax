{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Open
  ( gotoAltFile
  , openFilePath
  , closeCurrent
  , switchFile
  ) where

import Brick qualified as Brick
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Remax.Config qualified as Config
import Remax.Git qualified as Git
import Remax.TreeSitter qualified as TS
import Remax.UI.Action.Repo qualified as Repo
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Verdict qualified as Verdict
import Remax.UI.Model.Load qualified as Load
import Remax.UI.Model.Nav qualified as Nav
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

-- Space a: bounce to the alternate file — the one you were in before
-- the last file jump — restoring the hunk you'd been on. Pressing it
-- again bounces back, since each jump records the other side.
gotoAltFile :: Brick.EventM Types.Name Types.St ()
gotoAltFile = do
  st <- State.get
  case Types.stAltFile st of
    Nothing -> Types.setMsg "no alternate file yet"
    Just (path, h) ->
      case List.findIndex ((== path) . Types.fvPath) (Types.stFiles st) of
        Nothing -> Types.setMsg ("alternate file left the session: " <> T.pack path)
        Just f -> visitFile f (Just h)

-- Open any repository file in the session. A file already on screen is
-- jumped to; one outside the change set loads against its own content
-- (zero hunks — a plain editable view whose later session edits decorate
-- honestly). Opened files are pinned so git refreshes keep them around.
openFilePath :: T.Text -> Brick.EventM Types.Name Types.St ()
openFilePath rawPath = do
  st <- State.get
  let
    rootP = Types.stRoot st
    given = T.unpack rawPath
    joined = FilePath.normalise (if FilePath.isAbsolute given then given else rootP </> given)
    -- normalise tidies separators and dots but leaves .. where it stands,
    -- so a path written through a parent never matches the repository it
    -- actually lands in. Collapsed here rather than by asking the
    -- filesystem, so a path that does not exist yet still resolves.
    absolute =
      let
        step acc part =
          case part of
            "." -> acc
            ".." -> if length acc > 1 then init acc else acc
            _ -> acc ++ [part]
      in
        FilePath.joinPath (foldl step [] (FilePath.splitDirectories joined))
    -- Whether a path lies inside a repository, and where.
    --
    -- makeRelative alone is not the test. It never invents a ..,
    -- returning the path untouched when it cannot express one under the
    -- other — but normalise does not collapse a .. either, so a path
    -- written ../elsewhere still starts with the root textually and comes
    -- back looking like it belongs. Both have to be asked.
    under root =
      let
        rel = FilePath.makeRelative root absolute
      in
        if FilePath.isAbsolute rel || ".." `elem` FilePath.splitDirectories rel
          then Nothing
          else Just (FilePath.normalise rel)
    elsewhere =
      Maybe.listToMaybe
        [ (root, rel)
        | root <- Types.stRoots st
        , root /= rootP
        , Just rel <- [under root]
        ]
  case under rootP of
    Just rel -> openInRoot rel
    Nothing ->
      case elsewhere of
        -- it belongs to a checkout this session already knows: go there
        -- and open it, so it is keyed under the repository that owns it
        -- rather than under whichever one happened to be on screen
        Just (root, rel) -> do
          Repo.switchRepo root
          openInRoot rel
        Nothing ->
          Types.setMsg
            ( "outside "
                <> T.pack (FilePath.takeFileName rootP)
                <> ": "
                <> T.pack absolute
            )

-- Open a path already known to be inside the current repository.
openInRoot :: FilePath -> Brick.EventM Types.Name Types.St ()
openInRoot rel = do
  st <- State.get
  let
    rootP = Types.stRoot st
  case List.findIndex ((== rel) . Types.fvPath) (Types.stFiles st) of
    Just i -> do
      State.modify (\s -> s {Types.stOpened = Set.insert rel (Types.stOpened s)})
      visitFile i Nothing
    Nothing -> do
      got <- MonadIO.liftIO (loadOpened (Types.stConfig st) (Types.stHls st) (Types.effectiveMode st) rootP rel)
      case got of
        Nothing -> Types.setMsg ("cannot open " <> T.pack rel)
        Just fv -> do
          MonadIO.liftIO (Types.stWatch st (Types.fileDir rootP rel))
          State.modify $ \s ->
            s
              { Types.stFiles = Types.stFiles s ++ [Verdict.recountIn s fv]
              , Types.stOpened = Set.insert rel (Types.stOpened s)
              }
          grown <- State.get
          Nav.parkM (length (Types.stFiles grown) - 1) 0

-- Like loadOne, but the base is the file's own content right now.
loadOpened ::
  Config.Config ->
  TS.HighlighterTable ->
  Git.ReviewMode ->
  FilePath ->
  FilePath ->
  IO (Maybe Types.FileView)
loadOpened cfg hls mode root path = do
  got <- openedLines mode root path
  case got of
    Nothing -> pure Nothing
    Just newLs -> do
      (fv, _) <- Load.rebuildFV cfg (T.pack root) (Load.freshView hls path False newLs newLs)
      pure (Just fv)

openedLines :: Git.ReviewMode -> FilePath -> FilePath -> IO (Maybe [T.Text])
openedLines mode root path =
  case Git.subjectOf mode of
    Git.Ref tip -> do
      got <- Git.refContent root tip path
      pure $ case got of
        Just (Git.RefText ls) -> Just ls
        _ -> Nothing
    Git.WorkingTree -> do
      readable <- Exception.try (Git.looksBinary root path) :: IO (Either Exception.IOException Bool)
      case readable of
        Left _ -> pure Nothing
        Right True -> pure Nothing
        Right False -> do
          gotNew <- Exception.try (TIO.readFile (root </> path)) :: IO (Either Exception.IOException T.Text)
          pure (either (const Nothing) (Just . T.lines) gotNew)

closeCurrent :: Brick.EventM Types.Name Types.St ()
closeCurrent = do
  st <- State.get
  case Types.currentFileMay st of
    Nothing -> pure ()
    Just fv -> do
      let
        i = Types.stCurF st
      State.modify $ \s ->
        s
          { Types.stOpened = Set.delete (Types.fvPath fv) (Types.stOpened s)
          , Types.stFiles = take i (Types.stFiles s) ++ drop (i + 1) (Types.stFiles s)
          , Types.stCurF = max 0 (min i (length (Types.stFiles s) - 2))
          , Types.stCurH = 0
          }
      -- if the file belongs to the change set, the refresh reinstates it
      Load.forceGitRefresh

switchFile :: Int -> Brick.EventM Types.Name Types.St ()
switchFile d = do
  st <- State.get
  let
    n = length (Types.stFiles st)
  Monad.when (n > 1) $
    visitFile ((Types.stCurF st + d + n) `mod` n) Nothing

-- Change files without re-parking: the file's buffer keeps its own
-- cursor, so coming back finds it where it was left. Review-flow parks
-- still position deliberately; this is for browsing moves.
visitFile :: Int -> Maybe Int -> Brick.EventM Types.Name Types.St ()
visitFile f mh = do
  st <- State.get
  case Types.safeIdx (Types.stFiles st) f of
    Nothing -> pure ()
    Just fv -> do
      Monad.when (f /= Types.stCurF st) $
        State.modify $ \s ->
          s
            { Types.stAltFile =
                case Types.currentFileMay s of
                  Just prev -> Just (Types.fvPath prev, Types.stCurH s)
                  Nothing -> Types.stAltFile s
            }
      State.modify $ \s ->
        s
          { Types.stCurF = f
          , Types.stCurH = Types.clampH (Types.fvRuns fv) (Maybe.fromMaybe (Types.stCurH st) mh)
          , Types.stMsg = " "
          }

-- The fuzzy file finder -----------------------------------------------------------
