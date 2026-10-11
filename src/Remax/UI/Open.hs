{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Open
  ( gotoAltFile
  , openFilePath
  , closeCurrent
  , switchFile
  ) where

import Brick
  ( EventM
  )
import Control.Exception
  ( IOException
  , try
  )
import Control.Monad
  ( when
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
  ( get
  , modify
  )
import Data.List
  ( findIndex
  )
import Data.Maybe
  ( fromMaybe
  , listToMaybe
  )
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Remax.Config
import Remax.Git
  ( looksBinary
  )
import Remax.Modal
import qualified Remax.TreeSitter as TS
import Remax.UI.Load
import Remax.UI.Nav
import Remax.UI.Repo
import Remax.UI.Types
import Remax.UI.Verdict
import System.FilePath
  ( isAbsolute
  , joinPath
  , makeRelative
  , normalise
  , splitDirectories
  , takeFileName
  , (</>)
  )

-- Space a: bounce to the alternate file — the one you were in before
-- the last file jump — restoring the hunk you'd been on. Pressing it
-- again bounces back, since each jump records the other side.
gotoAltFile :: EventM Name St ()
gotoAltFile = do
  st <- get
  case stAltFile st of
    Nothing -> setMsg "no alternate file yet"
    Just (path, h) ->
      case findIndex ((== path) . fvPath) (stFiles st) of
        Nothing -> setMsg ("alternate file left the session: " <> T.pack path)
        Just f -> visitFile f (Just h)

-- Open any repository file in the session. A file already on screen is
-- jumped to; one outside the change set loads against its own content
-- (zero hunks — a plain editable view whose later session edits decorate
-- honestly). Opened files are pinned so git refreshes keep them around.
openFilePath :: Text -> EventM Name St ()
openFilePath rawPath = do
  st <- get
  let
    rootP = stRoot st
    given = T.unpack rawPath
    joined = normalise (if isAbsolute given then given else rootP </> given)
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
        joinPath (foldl step [] (splitDirectories joined))
    -- Whether a path lies inside a repository, and where.
    --
    -- makeRelative alone is not the test. It never invents a ..,
    -- returning the path untouched when it cannot express one under the
    -- other — but normalise does not collapse a .. either, so a path
    -- written ../elsewhere still starts with the root textually and comes
    -- back looking like it belongs. Both have to be asked.
    under root =
      let
        rel = makeRelative root absolute
      in
        if isAbsolute rel || ".." `elem` splitDirectories rel
          then Nothing
          else Just (normalise rel)
    elsewhere =
      listToMaybe
        [ (root, rel)
        | root <- stRoots st
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
          switchRepo root
          openInRoot rel
        Nothing ->
          setMsg
            ( "outside "
                <> T.pack (takeFileName rootP)
                <> ": "
                <> T.pack absolute
            )

-- Open a path already known to be inside the current repository.
openInRoot :: FilePath -> EventM Name St ()
openInRoot rel = do
  st <- get
  let
    rootP = stRoot st
  case findIndex ((== rel) . fvPath) (stFiles st) of
    Just i -> do
      modify (\s -> s {stOpened = Set.insert rel (stOpened s)})
      visitFile i Nothing
    Nothing -> do
      got <- liftIO (loadOpened (stConfig st) (stHls st) rootP rel)
      case got of
        Nothing -> setMsg ("cannot open " <> T.pack rel)
        Just fv -> do
          liftIO (stWatch st (fileDir rootP rel))
          modify $ \s ->
            s
              { stFiles = stFiles s ++ [recountIn s fv]
              , stOpened = Set.insert rel (stOpened s)
              }
          grown <- get
          parkM (length (stFiles grown) - 1) 0

-- Like loadOne, but the base is the file's own content right now.
loadOpened ::
  Config ->
  TS.HighlighterTable ->
  FilePath ->
  FilePath ->
  IO (Maybe FileView)
loadOpened cfg hls root path = do
  readable <- try (looksBinary root path) :: IO (Either IOException Bool)
  case readable of
    Left _ -> pure Nothing
    Right True -> pure Nothing
    Right False -> do
      gotNew <- try (TIO.readFile (root </> path)) :: IO (Either IOException Text)
      case gotNew of
        Left _ -> pure Nothing
        Right newTxt -> do
          let
            newLs = T.lines newTxt
            fv0 =
              FileView
                { fvPath = path
                , fvBase = newLs
                , fvEd = newEdState 0 newLs
                , fvRows = []
                , fvEmph = []
                , fvRowCount = 0
                , fvOrigin = 0
                , fvRuns = []
                , fvToks = []
                , fvStale = False
                , fvDeleted = False
                , fvHighlighted = False
                , -- the shebang is the fallback for a file with no
                  -- extension to go on, so the first line comes along
                  fvHl = TS.highlighterFor hls path (listToMaybe newLs)
                , fvRunDecls = []
                , fvRemarks = []
                , fvPending = 0
                , fvMtime = Nothing
                }
          (fv, _) <- rebuildFV cfg (T.pack root) fv0
          pure (Just fv)

closeCurrent :: EventM Name St ()
closeCurrent = do
  st <- get
  case currentFileMay st of
    Nothing -> pure ()
    Just fv -> do
      let
        i = stCurF st
      modify $ \s ->
        s
          { stOpened = Set.delete (fvPath fv) (stOpened s)
          , stFiles = take i (stFiles s) ++ drop (i + 1) (stFiles s)
          , stCurF = max 0 (min i (length (stFiles s) - 2))
          , stCurH = 0
          }
      -- if the file belongs to the change set, the refresh reinstates it
      forceGitRefresh

switchFile :: Int -> EventM Name St ()
switchFile d = do
  st <- get
  let
    n = length (stFiles st)
  when (n > 1) $
    visitFile ((stCurF st + d + n) `mod` n) Nothing

-- Change files without re-parking: the file's buffer keeps its own
-- cursor, so coming back finds it where it was left. Review-flow parks
-- still position deliberately; this is for browsing moves.
visitFile :: Int -> Maybe Int -> EventM Name St ()
visitFile f mh = do
  st <- get
  case safeIdx (stFiles st) f of
    Nothing -> pure ()
    Just fv -> do
      when (f /= stCurF st) $
        modify $ \s ->
          s
            { stAltFile =
                case currentFileMay s of
                  Just prev -> Just (fvPath prev, stCurH s)
                  Nothing -> stAltFile s
            }
      modify $ \s ->
        s
          { stCurF = f
          , stCurH = clampH (fvRuns fv) (fromMaybe (stCurH st) mh)
          , stMsg = " "
          }

-- The fuzzy file finder -----------------------------------------------------------
