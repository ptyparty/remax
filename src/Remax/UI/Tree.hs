{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Tree
  ( openTree
  , openFileFinder
  , ancestorPaths
  , treeVisible
  , treeFileCount
  , treeSelected
  , setTree
  , treeHunkInfo
  , toggleTreeScope
  , refreshTreeView
  , treeTargetDir
  , resolveInRepo
  ) where

import Brick
  ( EventM
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
  ( get
  , modify
  )
import Data.List
  ( find
  , findIndex
  )
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( fromMaybe
  , isJust
  )
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Remax.Config
import Remax.DiffModel
import Remax.Git
  ( FileScope (..)
  , PathStat (..)
  , listFiles
  , pathStats
  )
import Remax.Tree
import Remax.UI.Fuzzy
import Remax.UI.Types
import Remax.UI.Verdict
import Remax.UI.View
import System.FilePath
  ( isAbsolute
  , makeRelative
  , normalise
  , splitDirectories
  , takeDirectory
  , (</>)
  )

openTree :: EventM Name St ()
openTree = openTreeIn TreeHierarchy

-- Ctrl-p and Space f land here: the same dialog, opened flat.
openFileFinder :: EventM Name St ()
openFileFinder = openTreeIn TreeFlat

-- What the dialog lists, under whatever scope the session is in. Two
-- exclusions hide a file: git's own, and .remaxignore's — and showing
-- everything means dropping both, since a reader asking for the ignored
-- files does not care which list did the ignoring.
treeSources :: St -> IO ([FilePath], Map.Map FilePath PathStat)
treeSources st = do
  let
    root = stRoot st
    scope = stFileScope st
  ignores <- loadIgnores root
  files <- listFiles scope root
  stats <- pathStats (effectiveMode st) root
  let
    hide path = case scope of
      AllFiles -> False
      VisibleFiles -> ignoredPath ignores path
  pure
    ( filter (not . hide) files
    , Map.filterWithKey (\p _ -> not (hide p)) stats
    )

-- The rows of a dialog whose file list has just been rebuilt: flat mode
-- wants every path, the tree wants them shaped, and both want the change
-- set folded in so a deleted file still has somewhere to sit.
treeContents ::
  [FilePath] ->
  Map.Map FilePath PathStat ->
  (TreeNode, [(FilePath, Maybe PathStat)], [FilePath])
treeContents paths statPaths =
  let
    changed = Map.keys statPaths
    flat =
      map
        (\p -> (p, Map.lookup p statPaths))
        (Set.toList (Set.fromList (paths ++ changed)))
  in
    (buildTree paths statPaths, flat, changed)

openTreeIn :: TreeMode -> EventM Name St ()
openTreeIn wanted = do
  st <- get
  (paths, statPaths) <- liftIO (treeSources st)
  let
    (node, flat, changed) = treeContents paths statPaths
    -- open the directories a change lives in, and the ones leading to
    -- the file under review, so the diff is on screen with no keystrokes
    here = maybe [] (\fv -> [fvPath fv]) (currentFileMay st)
    opened = Set.fromList (concatMap ancestorPaths (changed ++ here))
    view =
      TreeView
        { tvMode = wanted
        , tvRoot = node
        , tvAll = flat
        , tvExpanded = opened
        , tvSel = 0
        , tvInput = ""
        , tvOp = Nothing
        }
    onCurrent = case here of
      (p : _) -> selectTreePath st view p
      [] -> view
  modify (\s -> s {stTree = Just onCurrent})

ancestorPaths :: FilePath -> [FilePath]
ancestorPaths path =
  let
    segs = splitDirectories path
    prefixes n = T.unpack (T.intercalate "/" (map T.pack (take n segs)))
  in
    map prefixes [1 .. length segs - 1]

selectTreePath :: St -> TreeView -> FilePath -> TreeView
selectTreePath st view path =
  case findIndex ((== path) . trPath) (treeVisible st view) of
    Just i -> view {tvSel = i}
    Nothing -> view

-- How many hunks the session has for a path; zero means the file is not
-- expandable, which is also true of every file outside the change set.
treeHunkCount :: St -> FilePath -> Int
treeHunkCount st path =
  case find ((== path) . fvPath) (stFiles st) of
    Just fv -> length (fvRuns fv)
    Nothing -> 0

-- Typing narrows the tree where it stands: matching files survive,
-- directories survive if anything under them does, and the survivors
-- open themselves. The point of a tree is the structure, so a search
-- that threw it away and ranked a flat list would be answering a
-- different question — the file picker already answers that one.
treeVisible :: St -> TreeView -> [TreeRow]
treeVisible st view =
  let
    query = tvInput view
    treeFilter =
      if T.null query
        then ShowAll
        else ShowMatching (\path -> isJust (fuzzyScore query path))
  in
    case tvMode view of
      TreeHierarchy ->
        treeRows treeFilter (tvExpanded view) (treeHunkCount st) (tvRoot view)
      TreeFlat -> flatRows st view

-- Flat mode ranks rather than preserving order — that is what makes the
-- finder quick, and it is the one real difference between the two views.
-- Files keep their hunks: expanding one here works as it does in the
-- tree, since the rows are the same rows.
-- How many matches the flat finder ranks and shows. Well past a
-- paneful, and far short of what a large repository would otherwise make
-- it sort.
flatRowsKept :: Int
flatRowsKept = 500

-- A candidate and its score, ordered by the score and then the path.
-- The stat rides along without taking part in the comparison, which is
-- what keeps PathStat out of the Ord it would otherwise need.
data Ranked = Ranked (Int, Int, Int) FilePath (Maybe PathStat)

rankedKey :: Ranked -> ((Int, Int, Int), FilePath)
rankedKey (Ranked sc p _) = (sc, p)

rankedScore :: Ranked -> (Int, Int, Int)
rankedScore (Ranked sc _ _) = sc

instance Eq Ranked where
  a == b = rankedKey a == rankedKey b

instance Ord Ranked where
  compare = comparing rankedKey

flatRows :: St -> TreeView -> [TreeRow]
flatRows st view =
  let
    ranked = Set.toAscList (bestRanked (needleFor (tvInput view)) (tvAll view))
  in
    concatMap (\(Ranked _ p ms) -> flatRowsFor st view p ms) ranked

-- Only the best few hundred are ever looked at, so only the best few
-- hundred are kept. Ranking by sorting every candidate means the
-- whole list is decorated, merged and rebuilt for a pane that shows
-- thirty rows — on a repository with a third of a million paths in
-- it, that is hundreds of megabytes of garbage for each character
-- typed, several times over, and the session stops answering.
-- Rejected on the score alone once the set is full. Inserting first
-- and dropping the worst afterwards costs a handful of comparisons
-- per candidate, and comparing two candidates means comparing their
-- paths character by character — which is most of the work, for a
-- candidate that was never going to be shown.
bestRanked :: Needle -> [(FilePath, Maybe PathStat)] -> Set.Set Ranked
bestRanked nd =
  let
    step acc (p, ms) =
      case fuzzyScoreOn nd p of
        Nothing -> acc
        Just sc
          | Set.size acc < flatRowsKept -> Set.insert (Ranked sc p ms) acc
          | otherwise ->
              case Set.lookupMax acc of
                Just worst
                  | rankedScore worst <= sc -> acc
                _ -> Set.deleteMax (Set.insert (Ranked sc p ms) acc)
    -- folded here rather than with Data.List.foldl': it is in the
    -- Prelude from GHC 9.10 and not before, so importing it fails one of
    -- the two compilers this builds under and omitting it fails the other
    bestOf !acc rest =
      case rest of
        [] -> acc
        (x : xs) -> bestOf (step acc x) xs
  in
    bestOf Set.empty

flatRowsFor :: St -> TreeView -> FilePath -> Maybe PathStat -> [TreeRow]
flatRowsFor st view path mStat =
  let
    hunks = treeHunkCount st path
    isOpen = Set.member path (tvExpanded view)
    fileRow =
      TreeRow
        { trDepth = 0
        , trLabel = T.pack path
        , trPath = path
        , trWitness = path
        , trParent = Nothing
        , trKind = RowFile mStat (hunks > 0) isOpen
        }
    hunkRow i =
      TreeRow
        { trDepth = 1
        , trLabel = ""
        , trPath = path
        , trWitness = path
        , trParent = Just path
        , trKind = RowHunk i
        }
  in
    fileRow : (if isOpen && hunks > 0 then map hunkRow [0 .. hunks - 1] else [])

treeFileCount :: [TreeRow] -> Int
treeFileCount = length . filter rowIsFile

treeSelected :: St -> TreeView -> Maybe TreeRow
treeSelected st view = safeIdx (treeVisible st view) (tvSel view)

setTree :: TreeView -> EventM Name St ()
setTree view = modify (\s -> s {stTree = Just view})

-- What a hunk row shows, taken straight from the tray's own vocabulary:
-- the verdict mark, the line it starts on, the enclosing declaration, and
-- whether a conversation hangs off it.
treeHunkInfo :: St -> FilePath -> Int -> Maybe (Char, Int, Text, Bool)
treeHunkInfo st path ix = do
  fv <- find ((== path) . fvPath) (stFiles st)
  r <- safeIdx (fvRuns fv) ix
  let
    decl = case safeIdx (fvRunDecls fv) ix of
      Just d
        | not (T.null d) -> " " <> T.take 12 (declName d)
      _ -> ""
  pure
    ( runMark (stVerdicts st) r
    , runAnchor r + 1
    , decl
    , any (`Map.member` stThreads st) (runKeys r)
    )

-- Ctrl-a: take the exclusions off, and put them back. The dialog is a
-- browser as much as a finder, and the file you want is now and then one
-- that .gitignore or .remaxignore keeps out of the review — a generated
-- parser, something under a build directory. Nothing else changes: an
-- ignored file stays out of the change set, and this only governs what
-- the list offers.
--
-- The filter, the unfolded directories and the row under the cursor all
-- survive the flip, so it reads as the same list growing rather than a
-- new dialog. The wider listing is only asked for when it is wanted,
-- which matters in a repository whose ignored files outnumber the rest.
toggleTreeScope :: TreeView -> EventM Name St ()
toggleTreeScope view = do
  modify (\s -> s {stFileScope = flipScope (stFileScope s)})
  refreshTreeView view

-- Rebuild the dialog's list from disk, keeping the filter, the unfolded
-- directories and the row the cursor was on. Both the scope toggle and
-- the file operations end here: either way the list under the cursor has
-- changed and the dialog should read as the same one, moved on.
refreshTreeView :: TreeView -> EventM Name St ()
refreshTreeView view = do
  before <- get
  let
    was = fmap trPath (treeSelected before view)
  st <- get
  (paths, statPaths) <- liftIO (treeSources st)
  let
    (node, flat, _) = treeContents paths statPaths
    next = view {tvRoot = node, tvAll = flat}
    rows = treeVisible st next
    landed = do
      path <- was
      findIndex ((== path) . trPath) rows
    -- a row can go away under the cursor — hidden again, renamed,
    -- deleted — and then the cursor stays where it stood rather than
    -- past the end of a shorter list
    stayed = max 0 (min (tvSel view) (length rows - 1))
  setTree next {tvSel = fromMaybe stayed landed}

flipScope :: FileScope -> FileScope
flipScope scope =
  case scope of
    VisibleFiles -> AllFiles
    AllFiles -> VisibleFiles

-- Making and unmaking files ---------------------------------------------------

-- Where a new thing lands: a directory row is itself the destination, and
-- anything else offers the directory it sits in.
treeTargetDir :: St -> TreeView -> FilePath
treeTargetDir st view =
  let
    home row = case trKind row of
      RowDir _ _ -> trPath row
      _ -> takeDirectory (trPath row)
  in
    case fmap home (treeSelected st view) of
      Just "." -> ""
      Just dir -> dir
      Nothing -> ""

-- Keep an operation inside the repository. A typed path is normalised
-- and has to come out relative and free of ..; makeRelative hands back an
-- absolute path when the target is not under the root, which is exactly
-- the case worth refusing.
resolveInRepo :: FilePath -> Text -> Either Text FilePath
resolveInRepo root typed =
  let
    given = T.unpack (T.strip typed)
    rel =
      normalise
        (makeRelative root (if isAbsolute given then given else root </> given))
  in
    if T.null (T.strip typed)
      then Left "no path given"
      else
        if isAbsolute rel || ".." `elem` splitDirectories rel
          then Left "that path leaves the repository"
          else Right rel
