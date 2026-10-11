{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Picker
  ( reopenLastSelector
  , openBufferPicker
  , openBasePicker
  , openRepoPicker
  , openBranchPicker
  , applyCheckout
  , openGrepPicker
  , openGrepHit
  , handlePickerKey
  ) where

import Brick
  ( EventM
  )
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
  ( get
  , modify
  )
import Data.List
  ( findIndex
  , sortOn
  )
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( fromMaybe
  , mapMaybe
  )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Graphics.Vty as V
import Remax.Git
  ( ReviewMode (..)
  , localBranches
  , recentCommits
  , runGit
  )
import Remax.Modal
import Remax.UI.Action.Fuzzy
import Remax.UI.Action.Open
import Remax.UI.Action.Repo
import Remax.UI.Action.Settings
import Remax.UI.Core.Jobs
import Remax.UI.Core.Types
import Remax.UI.Core.Widgets
import Remax.UI.Model.Load
import Remax.UI.Model.Nav
import qualified Remax.Zipper as Z
import System.FilePath
  ( takeFileName
  )
import System.Process
  ( readProcessWithExitCode
  )

-- Reopen it. Helix spells this ␣' and means the same thing by it: not
-- a fresh dialog of the same kind, but the one you had, with what you
-- had typed into it and where you had got to in the results.
reopenLastSelector :: EventM Name St ()
reopenLastSelector = do
  st <- get
  case stLastSelector st of
    Nothing -> setMsg "nothing selected yet this session"
    Just (LastTree tv) ->
      modify (\s -> s {stTree = Just tv, stPicker = Nothing})
    Just (LastPicker pk) ->
      modify (\s -> s {stPicker = Just pk, stTree = Nothing})

-- The picker narrowed to what the session already has open.
openBufferPicker :: EventM Name St ()
openBufferPicker = do
  st <- get
  modify $ \s ->
    s
      { stPicker =
          Just (refreshPicker (Picker PickFiles "" (map fvPath (stFiles st)) [] [] 0))
      }

-- A picker row carries three separate things, because they are not the
-- same string: the ref to act on, the columns to draw, and the text to
-- filter against. Filtering has to see the unclipped cells -- otherwise
-- the tail of a long branch name is both invisible and unsearchable --
-- and acting has to see the unclipped ref.
pickerRow :: Text -> Text -> Text -> FilePath
pickerRow ref display match =
  T.unpack (T.intercalate "\t" [ref, display, match])

splitPickerRow :: FilePath -> (Text, Text, Text)
splitPickerRow row =
  case T.splitOn "\t" (T.pack row) of
    (ref : display : match : _) -> (ref, display, match)
    [ref, display] -> (ref, display, display)
    _ -> (T.pack row, T.pack row, T.pack row)

-- Pick a base out of the history instead of spelling one: bare :base.
openBasePicker :: EventM Name St ()
openBasePicker = do
  st <- get
  commits <- liftIO (recentCommits 300 (stRoot st))
  let
    here ref = case stMode st of
      VsRef cur | cur == ref -> "  \8592 current"
      _ -> ""
    -- sha · how long ago · what points here · subject
    cellsFor (sha, ago, refs, subject) = [sha, ago, refs, subject <> here sha]
    cells =
      [ ["HEAD", "", "", "uncommitted changes" <> here "HEAD"]
      , ["branch", "", "", "the whole branch \8212 merge-base with the mainline"]
      ]
        ++ map cellsFor commits
    widths = columnWidths [12, 15, 28] cells
    refOf cs = fromMaybe "" (safeIdx cs 0)
    rows =
      map
        (\cs -> pickerRow (refOf cs) (columnRow widths cs) (T.unwords cs))
        cells
  modify $ \s ->
    s {stPicker = Just (refreshRowPicker (Picker PickBase "" rows [] [] 0))}

-- For the base and branch pickers, where the incoming order carries
-- meaning: rank on match quality alone and let the sort's stability keep
-- it within a tier. (refreshPicker's third component is path length,
-- which would scramble it.)
refreshRowPicker :: Picker -> Picker
refreshRowPicker pk =
  let
    q = pkInput pk
    quality t = fmap (\(a, b, _) -> (a, b)) (fuzzyScore q (T.unpack t))
    score row =
      let
        (ref, disp, match) = splitPickerRow row
      in
        fmap (\sc -> (sc, (ref, disp))) (quality match)
    ranked = map snd (sortOn fst (mapMaybe score (pkAll pk)))
  in
    pk
      { pkMatches = map snd ranked
      , pkTargets = map (\(ref, _) -> (T.unpack ref, 0)) ranked
      , pkSel = 0
      }

-- 'branch' means the merge-base, the same word :base already takes.
applyBaseChoice :: Text -> EventM Name St ()
applyBaseChoice ref
  | T.null ref = pure ()
  | ref == "branch" = setBaseBranch
  | otherwise = setBase (VsRef ref)

openRepoPicker :: EventM Name St ()
openRepoPicker = do
  rescanRepos
  st <- get
  case stRoots st of
    [] -> setMsg "no other repositories here"
    [_] -> setMsg "this is the only repository here"
    roots -> do
      let
        remembered root =
          Map.findWithDefault
            [T.pack (takeFileName root), "", "\8230"]
            root
            (stRepoInfo st)
        rows = repoPickerRows st (map (\root -> (root, remembered root)) roots)
        -- opening on the next one round makes Enter by itself "move on",
        -- while the list is still there to choose from deliberately
        startAt = case findIndex (== stRoot st) roots of
          Just i -> (i + 1) `mod` length roots
          Nothing -> 0
      -- refreshRowPicker ranks the rows and lands on the first, so the
      -- starting row is chosen after it has done that
      modify $ \s ->
        s
          { stPicker =
              Just
                ( (refreshRowPicker (Picker PickRepo "" rows [] [] 0))
                    { pkSel = startAt
                    }
                )
          }
      startJob "describing repositories" (forBounded 4 (describeRepo st) roots) $
        \outcome -> case outcome of
          Left err -> jobFailed "describing repositories" err
          Right described -> do
            modify $ \s ->
              s {stRepoInfo = Map.union (Map.fromList described) (stRepoInfo s)}
            latest <- get
            case stPicker latest of
              Just pk -> case pkMode pk of
                PickRepo ->
                  modify $ \s ->
                    s {stPicker = Just (replaceRepoRows (repoPickerRows latest described) pk)}
                _ -> pure ()
              Nothing -> pure ()

repoPickerRows :: St -> [(FilePath, [Text])] -> [FilePath]
repoPickerRows st described =
  let
    markCurrent (root, cells) =
      case (root == stRoot st, reverse cells) of
        (True, lastCell : before) -> (root, reverse ((lastCell <> "  \8592 current") : before))
        _ -> (root, cells)
    marked = map markCurrent described
    widths = columnWidths [24, 28, 16] (map snd marked)
    -- the row shows the basename; the ref carries the whole path, so
    -- a clipped name can never become the thing we switch to
    row (root, cells) =
      pickerRow
        (T.pack root)
        (columnRow widths cells)
        (T.pack root <> " " <> T.unwords cells)
  in
    map row marked

replaceRepoRows :: [FilePath] -> Picker -> Picker
replaceRepoRows rows pk =
  let
    chosen = fmap fst (safeIdx (pkTargets pk) (pkSel pk))
    refreshed = refreshRowPicker pk {pkAll = rows}
    kept = do
      ref <- chosen
      findIndex ((== ref) . fst) (pkTargets refreshed)
  in
    refreshed {pkSel = fromMaybe (pkSel pk) kept}

-- Check out a branch from the same overlay. A sixth explicit mutation
-- point, and like the others it only ever runs because you asked: the
-- picker is the confirmation, and git's own refusal is the guardrail
-- when the tree would be clobbered.
openBranchPicker :: EventM Name St ()
openBranchPicker = do
  st <- get
  case stRebase st of
    Just _ -> setMsg "mid-rebase — :continue or :abort! before switching branches"
    Nothing -> do
      branches <- liftIO (localBranches (stRoot st))
      let
        here b = if Just b == stBranch st then "  \8592 current" else ""
        -- branch · how long ago · the tip you would land on · subject
        cellsFor (b, ago, sha, subject) = [b, ago, sha, subject <> here b]
        cells = map cellsFor branches
        widths = columnWidths [28, 15, 12] cells
        refOf cs = fromMaybe "" (safeIdx cs 0)
        rows =
          map
            (\cs -> pickerRow (refOf cs) (columnRow widths cs) (T.unwords cs))
            cells
      if null rows
        then setMsg "no local branches"
        else modify $ \s ->
          s
            { stPicker =
                Just
                  (refreshRowPicker (Picker PickBranch "" rows [] [] 0))
            }

applyCheckout :: Text -> EventM Name St ()
applyCheckout branch
  | T.null branch = pure ()
  | otherwise = do
      st <- get
      if Just branch == stBranch st
        then setMsg ("already on " <> branch)
        else do
          res <- liftIO (runGit (stRoot st) ["checkout", T.unpack branch])
          case res of
            Left e -> setMsg e
            Right () -> do
              forceGitRefresh
              setMsg ("checked out " <> branch)

-- Project-wide search: git grep behind the same overlay, Enter jumping
-- straight to the hit.
openGrepPicker :: Text -> EventM Name St ()
openGrepPicker query = do
  st <- get
  (matches, targets) <- liftIO (grepRepo (stRoot st) query)
  modify $ \s ->
    s
      { stPicker =
          Just (Picker PickGrep query [] matches targets 0)
      }

-- Land on a grep hit: open the file, select the match on its line, and
-- prime n/N so the in-file search continues from there.
openGrepHit :: FilePath -> Int -> Text -> EventM Name St ()
openGrepHit path line query = do
  openFilePath (T.pack path)
  st <- get
  case currentFileMay st of
    Just fv
      | fvPath fv == path -> do
          let
            ed = fvEd fv
            ls = bufferLines ed
            row = max 0 (min (line - 1) (length ls - 1))
            lineText = case drop row ls of
              (l : _) -> l
              [] -> T.empty
            col = case T.breakOn query lineText of
              (pre, rest)
                | not (T.null rest) -> T.length pre
              _ -> 0
            width = max 1 (T.length query)
            placed =
              ed
                { edZipper = Z.moveCursor (row, col + width - 1) (edZipper ed)
                , edSelAnchor = Just (row, col)
                , edSelKind = CharSel
                }
          setEd (stCurF st) placed
          modify (\s -> s {stLastSearch = query})
    _ -> pure ()

-- Literal, case-sensitive, binary-skipping; capped so a short query
-- cannot flood the overlay.
grepRepo :: FilePath -> Text -> IO ([Text], [(FilePath, Int)])
grepRepo root query =
  if T.length query < 2
    then pure ([], [])
    else do
      (_, out, _) <-
        readProcessWithExitCode
          "git"
          [ "-C"
          , root
          , "grep"
          , "-nF"
          , "-I"
          , "--no-color"
          , "-e"
          , T.unpack query
          , "--"
          ]
          ""
      let
        hits = take 200 (T.lines (T.pack out))
        parse hit = do
          let
            (path, rest1) = T.breakOn ":" hit
          _ <- if T.null rest1 then Nothing else Just ()
          let
            (lineT, rest2) = T.breakOn ":" (T.drop 1 rest1)
          n <- case TR.decimal lineT of
            Right (v, leftover) | T.null leftover -> Just v
            _ -> Nothing
          let
            snippet = T.strip (T.drop 1 rest2)
            display =
              shortPath 36 path
                <> ":"
                <> lineT
                <> "  "
                <> T.take 160 snippet
          pure (display, (T.unpack path, n))
        parsed = mapMaybe parse hits
      pure (map fst parsed, map snd parsed)

refreshPicker :: Picker -> Picker
refreshPicker pk =
  let
    q = pkInput pk
    scored =
      mapMaybe (\path -> fmap (\sc -> (sc, path)) (fuzzyScore q path)) (pkAll pk)
    ranked = take 200 (map snd (sortOn fst scored))
  in
    pk
      { pkMatches = map (shortPath 120 . T.pack) ranked
      , pkTargets = map (\path -> (path, 0)) ranked
      , pkSel = 0
      }

handlePickerKey :: V.Event -> Picker -> EventM Name St ()
handlePickerKey ev pk =
  case ev of
    V.EvKey V.KEsc [] -> modify (\s -> s {stPicker = Nothing})
    V.EvKey V.KEnter [] -> do
      modify (\s -> s {stPicker = Nothing})
      forM_ (safeIdx (pkTargets pk) (pkSel pk)) (pickerChoose pk)
    V.EvKey V.KUp [] -> pickerMove pk (-1)
    V.EvKey (V.KChar 'p') [V.MCtrl] -> pickerMove pk (-1)
    V.EvKey V.KDown [] -> pickerMove pk 1
    V.EvKey (V.KChar 'n') [V.MCtrl] -> pickerMove pk 1
    V.EvKey V.KBS [] -> pickerRetype pk (T.dropEnd 1 (pkInput pk))
    V.EvKey (V.KChar c) [] -> pickerRetype pk (T.snoc (pkInput pk) c)
    _ -> pure ()

pickerMove :: Picker -> Int -> EventM Name St ()
pickerMove pk d =
  let
    bound = max 0 (length (pkMatches pk) - 1)
    next = max 0 (min bound (pkSel pk + d))
  in
    modify (\s -> s {stPicker = Just pk {pkSel = next}})

pickerRetype :: Picker -> Text -> EventM Name St ()
pickerRetype pk q =
  case pkMode pk of
    PickFiles -> modify (\s -> s {stPicker = Just (refreshPicker pk {pkInput = q})})
    PickGrep -> do
      st <- get
      (matches, targets) <- liftIO (grepRepo (stRoot st) q)
      modify $ \s ->
        s {stPicker = Just pk {pkInput = q, pkMatches = matches, pkTargets = targets, pkSel = 0}}
    _ -> modify (\s -> s {stPicker = Just (refreshRowPicker pk {pkInput = q})})

pickerChoose :: Picker -> (FilePath, Int) -> EventM Name St ()
pickerChoose pk (target, line) =
  case pkMode pk of
    PickBase -> applyBaseChoice (T.pack target)
    PickBranch -> applyCheckout (T.pack target)
    PickRepo -> switchRepo target
    _
      | line == 0 -> openFilePath (T.pack target)
      | otherwise -> do
          modify (\s -> s {stGrepNav = Just (pkInput pk, pkTargets pk, pkSel pk)})
          openGrepHit target line (pkInput pk)
