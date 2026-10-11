{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Tree
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

import Brick qualified as Brick
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Ord qualified as Ord
import Data.Set qualified as Set
import Data.Text qualified as T
import Remax.Config qualified as Config
import Remax.DiffModel qualified as DiffModel
import Remax.Git qualified as Git
import Remax.Tree qualified as Tree
import Remax.UI.Action.Fuzzy qualified as Fuzzy
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Verdict qualified as Verdict
import Remax.UI.Core.View qualified as View
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

openTree :: Brick.EventM Types.Name Types.St ()
openTree = openTreeIn Types.TreeHierarchy

-- Ctrl-p and Space f land here: the same dialog, opened flat.
openFileFinder :: Brick.EventM Types.Name Types.St ()
openFileFinder = openTreeIn Types.TreeFlat

-- What the dialog lists, under whatever scope the session is in. Two
-- exclusions hide a file: git's own, and .remaxignore's — and showing
-- everything means dropping both, since a reader asking for the ignored
-- files does not care which list did the ignoring.
treeSources :: Types.St -> IO ([FilePath], Map.Map FilePath Git.PathStat)
treeSources st = do
  let
    root = Types.stRoot st
    scope = Types.stFileScope st
  ignores <- Config.loadIgnores root
  files <- Git.listFiles scope root
  stats <- Git.pathStats (Types.effectiveMode st) root
  let
    hide path = case scope of
      Git.AllFiles -> False
      Git.VisibleFiles -> Config.ignoredPath ignores path
  pure
    ( filter (not . hide) files
    , Map.filterWithKey (\p _ -> not (hide p)) stats
    )

-- The rows of a dialog whose file list has just been rebuilt: flat mode
-- wants every path, the tree wants them shaped, and both want the change
-- set folded in so a deleted file still has somewhere to sit.
treeContents ::
  [FilePath] ->
  Map.Map FilePath Git.PathStat ->
  (Tree.TreeNode, [(FilePath, Maybe Git.PathStat)], [FilePath])
treeContents paths statPaths =
  let
    changed = Map.keys statPaths
    flat =
      map
        (\p -> (p, Map.lookup p statPaths))
        (Set.toList (Set.fromList (paths ++ changed)))
  in
    (Tree.buildTree paths statPaths, flat, changed)

openTreeIn :: Types.TreeMode -> Brick.EventM Types.Name Types.St ()
openTreeIn wanted = do
  st <- State.get
  (paths, statPaths) <- MonadIO.liftIO (treeSources st)
  let
    (node, flat, changed) = treeContents paths statPaths
    -- open the directories a change lives in, and the ones leading to
    -- the file under review, so the diff is on screen with no keystrokes
    here = maybe [] (\fv -> [Types.fvPath fv]) (Types.currentFileMay st)
    opened = Set.fromList (concatMap ancestorPaths (changed ++ here))
    view =
      Types.TreeView
        { Types.tvMode = wanted
        , Types.tvRoot = node
        , Types.tvAll = flat
        , Types.tvExpanded = opened
        , Types.tvSel = 0
        , Types.tvInput = ""
        , Types.tvOp = Nothing
        }
    onCurrent = case here of
      (p : _) -> selectTreePath st view p
      [] -> view
  State.modify (\s -> s {Types.stTree = Just onCurrent})

ancestorPaths :: FilePath -> [FilePath]
ancestorPaths path =
  let
    segs = FilePath.splitDirectories path
    prefixes n = T.unpack (T.intercalate "/" (map T.pack (take n segs)))
  in
    map prefixes [1 .. length segs - 1]

selectTreePath :: Types.St -> Types.TreeView -> FilePath -> Types.TreeView
selectTreePath st view path =
  case List.findIndex ((== path) . Tree.trPath) (treeVisible st view) of
    Just i -> view {Types.tvSel = i}
    Nothing -> view

-- How many hunks the session has for a path; zero means the file is not
-- expandable, which is also true of every file outside the change set.
treeHunkCount :: Types.St -> FilePath -> Int
treeHunkCount st path =
  case List.find ((== path) . Types.fvPath) (Types.stFiles st) of
    Just fv -> length (Types.fvRuns fv)
    Nothing -> 0

-- Typing narrows the tree where it stands: matching files survive,
-- directories survive if anything under them does, and the survivors
-- open themselves. The point of a tree is the structure, so a search
-- that threw it away and ranked a flat list would be answering a
-- different question — the file picker already answers that one.
treeVisible :: Types.St -> Types.TreeView -> [Tree.TreeRow]
treeVisible st view =
  let
    query = Types.tvInput view
    treeFilter =
      if T.null query
        then Tree.ShowAll
        else Tree.ShowMatching (\path -> Maybe.isJust (Fuzzy.fuzzyScore query path))
  in
    case Types.tvMode view of
      Types.TreeHierarchy ->
        Tree.treeRows treeFilter (Types.tvExpanded view) (treeHunkCount st) (Types.tvRoot view)
      Types.TreeFlat -> flatRows st view

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
data Ranked = Ranked (Int, Int, Int) FilePath (Maybe Git.PathStat)

rankedKey :: Ranked -> ((Int, Int, Int), FilePath)
rankedKey (Ranked sc p _) = (sc, p)

rankedScore :: Ranked -> (Int, Int, Int)
rankedScore (Ranked sc _ _) = sc

instance Eq Ranked where
  a == b = rankedKey a == rankedKey b

instance Ord Ranked where
  compare = Ord.comparing rankedKey

flatRows :: Types.St -> Types.TreeView -> [Tree.TreeRow]
flatRows st view =
  let
    ranked = Set.toAscList (bestRanked (Fuzzy.needleFor (Types.tvInput view)) (Types.tvAll view))
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
bestRanked :: Fuzzy.Needle -> [(FilePath, Maybe Git.PathStat)] -> Set.Set Ranked
bestRanked nd =
  let
    step acc (p, ms) =
      case Fuzzy.fuzzyScoreOn nd p of
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

flatRowsFor :: Types.St -> Types.TreeView -> FilePath -> Maybe Git.PathStat -> [Tree.TreeRow]
flatRowsFor st view path mStat =
  let
    hunks = treeHunkCount st path
    isOpen = Set.member path (Types.tvExpanded view)
    fileRow =
      Tree.TreeRow
        { Tree.trDepth = 0
        , Tree.trLabel = T.pack path
        , Tree.trPath = path
        , Tree.trWitness = path
        , Tree.trParent = Nothing
        , Tree.trKind = Tree.RowFile mStat (hunks > 0) isOpen
        }
    hunkRow i =
      Tree.TreeRow
        { Tree.trDepth = 1
        , Tree.trLabel = ""
        , Tree.trPath = path
        , Tree.trWitness = path
        , Tree.trParent = Just path
        , Tree.trKind = Tree.RowHunk i
        }
  in
    fileRow : (if isOpen && hunks > 0 then map hunkRow [0 .. hunks - 1] else [])

treeFileCount :: [Tree.TreeRow] -> Int
treeFileCount = length . filter Tree.rowIsFile

treeSelected :: Types.St -> Types.TreeView -> Maybe Tree.TreeRow
treeSelected st view = Types.safeIdx (treeVisible st view) (Types.tvSel view)

setTree :: Types.TreeView -> Brick.EventM Types.Name Types.St ()
setTree view = State.modify (\s -> s {Types.stTree = Just view})

-- What a hunk row shows, taken straight from the tray's own vocabulary:
-- the verdict mark, the line it starts on, the enclosing declaration, and
-- whether a conversation hangs off it.
treeHunkInfo :: Types.St -> FilePath -> Int -> Maybe (Char, Int, T.Text, Bool)
treeHunkInfo st path ix = do
  fv <- List.find ((== path) . Types.fvPath) (Types.stFiles st)
  r <- Types.safeIdx (Types.fvRuns fv) ix
  let
    decl = case Types.safeIdx (Types.fvRunDecls fv) ix of
      Just d
        | not (T.null d) -> " " <> T.take 12 (View.declName d)
      _ -> ""
  pure
    ( Verdict.runMark (Types.stVerdicts st) r
    , DiffModel.runAnchor r + 1
    , decl
    , any (`Map.member` Types.stThreads st) (DiffModel.runKeys r)
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
toggleTreeScope :: Types.TreeView -> Brick.EventM Types.Name Types.St ()
toggleTreeScope view = do
  State.modify (\s -> s {Types.stFileScope = flipScope (Types.stFileScope s)})
  refreshTreeView view

-- Rebuild the dialog's list from disk, keeping the filter, the unfolded
-- directories and the row the cursor was on. Both the scope toggle and
-- the file operations end here: either way the list under the cursor has
-- changed and the dialog should read as the same one, moved on.
refreshTreeView :: Types.TreeView -> Brick.EventM Types.Name Types.St ()
refreshTreeView view = do
  before <- State.get
  let
    was = fmap Tree.trPath (treeSelected before view)
  st <- State.get
  (paths, statPaths) <- MonadIO.liftIO (treeSources st)
  let
    (node, flat, _) = treeContents paths statPaths
    next = view {Types.tvRoot = node, Types.tvAll = flat}
    rows = treeVisible st next
    landed = do
      path <- was
      List.findIndex ((== path) . Tree.trPath) rows
    -- a row can go away under the cursor — hidden again, renamed,
    -- deleted — and then the cursor stays where it stood rather than
    -- past the end of a shorter list
    stayed = max 0 (min (Types.tvSel view) (length rows - 1))
  setTree next {Types.tvSel = Maybe.fromMaybe stayed landed}

flipScope :: Git.FileScope -> Git.FileScope
flipScope scope =
  case scope of
    Git.VisibleFiles -> Git.AllFiles
    Git.AllFiles -> Git.VisibleFiles

-- Making and unmaking files ---------------------------------------------------

-- Where a new thing lands: a directory row is itself the destination, and
-- anything else offers the directory it sits in.
treeTargetDir :: Types.St -> Types.TreeView -> FilePath
treeTargetDir st view =
  let
    home row = case Tree.trKind row of
      Tree.RowDir _ _ -> Tree.trPath row
      _ -> FilePath.takeDirectory (Tree.trPath row)
  in
    case fmap home (treeSelected st view) of
      Just "." -> ""
      Just dir -> dir
      Nothing -> ""

-- Keep an operation inside the repository. A typed path is normalised
-- and has to come out relative and free of ..; makeRelative hands back an
-- absolute path when the target is not under the root, which is exactly
-- the case worth refusing.
resolveInRepo :: FilePath -> T.Text -> Either T.Text FilePath
resolveInRepo root typed =
  let
    given = T.unpack (T.strip typed)
    rel =
      FilePath.normalise
        (FilePath.makeRelative root (if FilePath.isAbsolute given then given else root </> given))
  in
    if T.null (T.strip typed)
      then Left "no path given"
      else
        if FilePath.isAbsolute rel || ".." `elem` FilePath.splitDirectories rel
          then Left "that path leaves the repository"
          else Right rel
