{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Verdict
  ( allRunsInOrder
  , pairPending
  , pairUnvisited
  , filteredRunPairs
  , firstUnrespondedOrPending
  , runUnvisited
  , runPending
  , runMark
  , recountFile
  , recountIn
  , recountIfChanged
  , rowFeedbackKeys
  , effRuns
  , updateFile
  ) where

import Brick
  ( EventM
  , attrName
  )
import Control.Applicative ((<|>))
import Control.Exception
  ( evaluate
  )
import Control.Monad
  ( guard
  , when
  )
import Control.Monad.State
  ( get
  , modify
  )
import Data.List
  ( find
  )
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( catMaybes
  , mapMaybe
  )
import qualified Data.Set as Set
import Data.Text (Text)
import Remax.DiffModel
import Remax.Highlight
import Remax.Store
import Remax.UI.Jobs
import Remax.UI.Types
import Remax.UI.Widgets

-- Every (file, hunk) pair in display order, each carrying its run.
--
-- One walk down the files, rather than a pair and then an index back
-- into them to find out what it names. Reading a run by its pair costs
-- a walk to it, so asking once per hunk is quadratic in the size of the
-- change set — and the header and the status meter both ask once per
-- hunk on every frame. A single file with a few thousand hunks put
-- whole seconds between a keystroke and the screen, which is not merely
-- slow: there is no leaving a session that cannot process the three
-- keys of :q, and helix's keymap binds no other way out.
allRunsInOrder :: St -> [((Int, Int), Run)]
allRunsInOrder st =
  let
    fileRuns (fi, fv) = zipWith (\hi r -> ((fi, hi), r)) [0 ..] (fvRuns fv)
  in
    concatMap fileRuns (zip [0 ..] (stFiles st))

runAt :: St -> (Int, Int) -> Maybe Run
runAt st (f, h) = do
  fv <- safeIdx (stFiles st) f
  safeIdx (fvRuns fv) h

pairPending :: St -> (Int, Int) -> Bool
pairPending st pair =
  case runAt st pair of
    Just r -> runPending (stVerdicts st) r
    Nothing -> False

pairUnvisited :: St -> (Int, Int) -> Bool
pairUnvisited st pair =
  case runAt st pair of
    Just r -> runUnvisited (stVerdicts st) r
    Nothing -> False

-- Every hunk the filter admits, in display order.
filteredRunPairs :: St -> [(Int, Int)]
filteredRunPairs st =
  map fst (filter (runFilterAllows (stRunFilter st) . snd) (allRunsInOrder st))

-- Where a session begins: the first hunk nothing has been said about
-- yet; only when every hunk has some response does the first still-
-- pending (flagged/questioned) one win.
firstUnrespondedOrPending :: St -> Maybe (Int, Int)
firstUnrespondedOrPending st =
  fmap fst (find (runUnvisited (stVerdicts st) . snd) (filteredRunsInOrder st))
    <|> firstPending st

firstPending :: St -> Maybe (Int, Int)
firstPending st =
  fmap fst (find (runPending (stVerdicts st) . snd) (filteredRunsInOrder st))

filteredRunsInOrder :: St -> [((Int, Int), Run)]
filteredRunsInOrder st =
  filter (runFilterAllows (stRunFilter st) . snd) (allRunsInOrder st)

-- Unvisited: some line has no verdict at all — the hunk still needs a
-- first look, which is what auto-advance cares about.
runUnvisited :: Map.Map Text Verdict -> Run -> Bool
runUnvisited m r = any (`Map.notMember` m) (runKeys r)

-- Pending: anything not fully accepted as reviewed. Flagged and
-- questioned lines keep their hunk pending — they still need attention —
-- which is what the counts, the p filter, and navigation care about.
runPending :: Map.Map Text Verdict -> Run -> Bool
runPending m r =
  let
    accepted k = Map.lookup k m == Just Reviewed
  in
    not (all accepted (runKeys r))

runMark :: Map.Map Text Verdict -> Run -> Char
runMark m r =
  let
    verdicts = map (`Map.lookup` m) (runKeys r)
  in
    if runUnvisited m r
      then '\183' -- ·
      else
        if any (== Just Flagged) verdicts
          then '!'
          else
            if any (== Just Questioned) verdicts
              then '?'
              else '\10003' -- ✓

remarksFor :: Map.Map Text Verdict -> Map.Map Text [Post] -> FileView -> [Remark]
remarksFor verdicts threads fv =
  let
    pick row = do
      k <- rRemarkKey row
      -- a removed row keeps its remark too, but by then the hunk it
      -- belongs to is what carries it through the tray
      guard (rMark row == ' ')
      guard (Map.member k verdicts || Map.member k threads)
      n <- rNewRow row
      pure (Remark n k (rText row))
  in
    mapMaybe pick (fvRows fv)

recountFile :: Map.Map Text Verdict -> Map.Map Text [Post] -> FileView -> FileView
recountFile verdicts threads fv =
  fv
    { fvRemarks = remarksFor verdicts threads fv
    , fvPending = length (filter (runPending verdicts) (fvRuns fv))
    }

recountIn :: St -> FileView -> FileView
recountIn st = recountFile (stVerdicts st) (stThreads st)

fileKeys :: FileView -> Set.Set Text
fileKeys fv =
  Set.fromList (concatMap runKeys (fvRuns fv) ++ mapMaybe rRemarkKey (fvRows fv))

changedKeys :: Eq v => Map.Map Text v -> Map.Map Text v -> Set.Set Text
changedKeys before after =
  Map.keysSet
    ( Map.filter
        id
        ( Map.mergeWithKey
            (\_ x y -> Just (x /= y))
            (Map.map (const True))
            (Map.map (const True))
            before
            after
        )
    )

recountIfChanged :: EventM Name St ()
recountIfChanged = do
  st <- get
  let
    verdictsMoved = stVerdicts st /= stCountedVerdicts st
    threadsMoved = stThreads st /= stCountedThreads st
  when (verdictsMoved || threadsMoved) $ do
    let
      changed =
        Set.union
          (changedKeys (stCountedVerdicts st) (stVerdicts st))
          (changedKeys (stCountedThreads st) (stThreads st))
    modify (\s -> s {stCountedVerdicts = stVerdicts st, stCountedThreads = stThreads st})
    case currentFileMay st of
      Just fv
        | changed `Set.isSubsetOf` fileKeys fv ->
            modify (\s -> updateFile (stCurF s) id s)
      _ -> recountAll

recountInlineRows :: Int
recountInlineRows = 300000

recountAll :: EventM Name St ()
recountAll = do
  st <- get
  let
    verdicts = stVerdicts st
    threads = stThreads st
    counted =
      Map.fromList
        ( map
            (\fv -> (fvPath fv, (remarksFor verdicts threads fv, length (filter (runPending verdicts) (fvRuns fv)))))
            (stFiles st)
        )
    epoch = stEpoch st
    settle :: Either Text (Map.Map FilePath ([Remark], Int)) -> EventM Name St ()
    settle outcome = do
      latest <- get
      when (stEpoch latest == epoch) $
        case outcome of
          Left err -> jobFailed "recounting" err
          Right done ->
            modify $ \s ->
              s
                { stFiles =
                    map
                      ( \fv -> case Map.lookup (fvPath fv) done of
                          Just (rs, n) -> fv {fvRemarks = rs, fvPending = n}
                          Nothing -> fv
                      )
                      (stFiles s)
                , stCountedVerdicts = verdicts
                , stCountedThreads = threads
                }
    forced = Map.foldr (\(rs, n) acc -> sum (map rmRow rs) + n + acc) (0 :: Int) counted
  if sum (map fvRowCount (stFiles st)) <= recountInlineRows
    then modify (\s -> s {stFiles = map (recountIn s) (stFiles s)})
    else startJob "recounting" (evaluate forced >> pure counted) settle

-- Where a row's feedback lives. A changed row carries what was said
-- about the change; a line nobody has changed carries a remark, keyed
-- against the base; and a removed row can answer to either, because a
-- remark made while it was unchanged follows it here when somebody
-- changes it.
rowFeedbackKeys :: RowR -> [Text]
rowFeedbackKeys r = catMaybes [rKey r, rRemarkKey r]

effRuns :: RowR -> [Seg]
effRuns r =
  case nonEmptySegs (rToks r) of
    [] -> [(attrName "plain", rText r)]
    filled -> filled

-- Drawing the body ---------------------------------------------------------------

updateFile :: Int -> (FileView -> FileView) -> St -> St
updateFile i f st =
  let
    patch (j, fv) = if j == i then recountIn st (f fv) else fv
  in
    st {stFiles = map patch (zip [0 ..] (stFiles st))}
