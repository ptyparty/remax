{-# LANGUAGE OverloadedStrings #-}

-- The project tree, as a model with no opinion about where it is drawn.
--
-- Paths in, a flat list of rows out — the same shape trayEntries produces
-- for the tray's viewport, deliberately, because the tray is
-- file-to-hunk and this is directory-to-file-to-hunk with one level on
-- top. Rendering into the tray later should be a second consumer of
-- treeRows, not a second implementation.
module Remax.Tree
  ( TreeNode (..)
  , Rollup (..)
  , buildTree
  , TreeRow (..)
  , RowKind (..)
  , TreeFilter (..)
  , treeRows
  , rowIsDir
  , rowIsFile
  , rowExpandable
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (splitDirectories)

import Remax.Git (PathStat (..))

-- A directory has kids and no stat; a file has a stat and no kids. The
-- root is a directory with an empty path.
data TreeNode = TreeNode
  { tnName :: Text
  , tnPath :: FilePath
  , tnKids :: [TreeNode]
  , tnStat :: Maybe PathStat
  , tnRollup :: Rollup
  }

-- What a directory carries when it is folded, so its weight is visible
-- without expanding it.
data Rollup = Rollup
  { ruAdded :: Int
  , ruRemoved :: Int
  , ruChanged :: Int
  }
  deriving (Eq)

instance Semigroup Rollup where
  a <> b =
    Rollup
      (ruAdded a + ruAdded b)
      (ruRemoved a + ruRemoved b)
      (ruChanged a + ruChanged b)

instance Monoid Rollup where
  mempty = Rollup 0 0 0

-- Build from the union of the repository's files and the paths the diff
-- mentions: a deleted file is absent from disk but present in the diff,
-- and listing it is the only way it can be seen at all.
buildTree :: [FilePath] -> Map.Map FilePath PathStat -> TreeNode
buildTree paths stats =
  let
    every = Set.toList (Set.fromList (paths ++ Map.keys stats))
    insert node segs =
      case segs of
        [] -> node
        (seg : rest) ->
          let
            here = tnPath node
            kidPath = if null here then seg else here ++ "/" ++ seg
            isLeaf = null rest
            matches kid = tnName kid == T.pack seg && null (tnKids kid) == isLeaf
            fresh =
              TreeNode
                { tnName = T.pack seg
                , tnPath = kidPath
                , tnKids = []
                , tnStat = if isLeaf then Map.lookup kidPath stats else Nothing
                , tnRollup = mempty
                }
            (found, others) = (filter matches (tnKids node), filter (not . matches) (tnKids node))
            existing = case found of
              (k : _) -> k
              [] -> fresh
          in
            node {tnKids = others ++ [insert existing rest]}
    seeded = foldl (\n p -> insert n (splitDirectories p)) (TreeNode "" "" [] Nothing mempty) every
  in
    roll (sortTree seeded)

-- Directories first, then files, each alphabetically.
sortTree :: TreeNode -> TreeNode
sortTree node =
  let
    isDir kid = not (null (tnKids kid))
    dirs = filter isDir (tnKids node)
    files = filter (not . isDir) (tnKids node)
    byName = sortOnName
  in
    node {tnKids = map sortTree (byName dirs ++ byName files)}

sortOnName :: [TreeNode] -> [TreeNode]
sortOnName nodes =
  case nodes of
    [] -> []
    (pivot : rest) ->
      let
        before = filter (\n -> tnName n <= tnName pivot) rest
        after = filter (\n -> tnName n > tnName pivot) rest
      in
        sortOnName before ++ [pivot] ++ sortOnName after

roll :: TreeNode -> TreeNode
roll node =
  case tnKids node of
    [] ->
      node {tnRollup = statRollup (tnStat node)}
    kids ->
      let
        rolled = map roll kids
      in
        node {tnKids = rolled, tnRollup = foldMap tnRollup rolled}

statRollup :: Maybe PathStat -> Rollup
statRollup mStat =
  case mStat of
    Nothing -> mempty
    Just stat ->
      let
        (a, r) = case psCounts stat of
          Just counts -> counts
          Nothing -> (0, 0)
      in
        Rollup a r 1

-- One rendered line. A hunk row's path is its file's path; the index
-- says which hunk, so a row is addressable without a synthetic key.
data RowKind
  = RowDir Rollup Bool -- rollup, expanded
  | RowFile (Maybe PathStat) Bool Bool -- stat, has hunks, expanded
  | RowHunk Int -- hunk index within the file
  deriving (Eq)

data TreeRow = TreeRow
  { trDepth :: Int
  , trLabel :: Text
  , trPath :: FilePath
  , -- the full path whose match justifies this row: its own for a file,
    -- a matching descendant's for a directory. Only against this does
    -- the whole query line up, so it is what a highlight is computed
    -- from — see matchWitness.
    trWitness :: FilePath
  , trParent :: Maybe FilePath
  , trKind :: RowKind
  }

rowIsDir :: TreeRow -> Bool
rowIsDir row =
  case trKind row of
    RowDir _ _ -> True
    _ -> False

rowIsFile :: TreeRow -> Bool
rowIsFile row =
  case trKind row of
    RowFile _ _ _ -> True
    _ -> False

rowExpandable :: TreeRow -> Bool
rowExpandable row =
  case trKind row of
    RowDir _ _ -> True
    RowFile _ hasHunks _ -> hasHunks
    RowHunk _ -> False

-- Typing narrows the tree in place rather than flattening it: a file
-- survives if it matches, a directory survives if anything under it
-- does, and a surviving directory opens itself so the matches are
-- actually on screen. Structure is the thing being browsed, so a search
-- that discards it answers a different question than the one asked.
data TreeFilter
  = ShowAll
  | ShowMatching (FilePath -> Bool)

-- The path whose match justifies keeping a node: its own, for a file
-- that matched; the first matching file beneath it, for a directory.
--
-- A directory needs a descendant's path rather than its own because the
-- query is matched against a whole path. "remaxgit" matches
-- src/Remax/Git.hs, but nothing of it matches src/Remax on its own — so
-- asking the directory alone finds no match at all, and the directory's
-- share of one can only be recovered from a descendant that has it.
matchWitness :: TreeFilter -> TreeNode -> Maybe FilePath
matchWitness treeFilter node =
  case treeFilter of
    ShowAll -> Just (tnPath node)
    ShowMatching matches ->
      case tnKids node of
        [] -> if matches (tnPath node) then Just (tnPath node) else Nothing
        kids -> listToMaybe (mapMaybe (matchWitness treeFilter) kids)

filtering :: TreeFilter -> Bool
filtering treeFilter =
  case treeFilter of
    ShowAll -> False
    ShowMatching _ -> True

-- Flatten to visible rows. `expanded` holds directory paths and file
-- paths alike, so one set drives both levels. `hunkCount` reports how
-- many hunks a path has in the session, which is what makes a file
-- expandable.
treeRows ::
  TreeFilter ->
  Set.Set FilePath ->
  (FilePath -> Int) ->
  TreeNode ->
  [TreeRow]
treeRows treeFilter expanded hunkCount root =
  let
    walk depth parent node =
      concatMap
        (\kid -> maybe [] (emit depth parent kid) (matchWitness treeFilter kid))
        (tnKids node)
    emit depth parent node witness =
      let
        path = tnPath node
        -- a directory opens itself while filtering; a file does not, so
        -- hunks stay behind a deliberate keystroke
        isOpen =
          Set.member path expanded
            || (filtering treeFilter && not (null (tnKids node)))
      in
        case tnKids node of
          [] ->
            let
              n = hunkCount path
              hunkRow i =
                TreeRow
                  { trDepth = depth + 1
                  , trLabel = ""
                  , trPath = path
                  , trWitness = path
                  , trParent = Just path
                  , trKind = RowHunk i
                  }
            in
              TreeRow
                { trDepth = depth
                , trLabel = tnName node
                , trPath = path
                , trWitness = witness
                , trParent = parent
                , trKind = RowFile (tnStat node) (n > 0) isOpen
                }
                : (if isOpen && n > 0 then map hunkRow [0 .. n - 1] else [])
          _ ->
            let
              (label, target) = chain node
              openTarget =
                Set.member (tnPath target) expanded || filtering treeFilter
            in
              TreeRow
                { trDepth = depth
                , trLabel = label
                , trPath = tnPath target
                , trWitness = witness
                , trParent = parent
                , trKind = RowDir (tnRollup target) openTarget
                }
                : ( if openTarget
                      then walk (depth + 1) (Just (tnPath target)) target
                      else []
                  )
  in
    walk 0 Nothing root

-- A directory whose only child is a directory renders as one row:
-- "src/Remax/" rather than "src/" above "Remax/". Saves a level of
-- indent on every deep tree without hiding anything.
chain :: TreeNode -> (Text, TreeNode)
chain node =
  case tnKids node of
    [only]
      | not (null (tnKids only)) ->
          let
            (rest, target) = chain only
          in
            (tnName node <> "/" <> rest, target)
    _ -> (tnName node, node)
