{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Core.Verdict
  ( allRunsInOrder
  , pairPending
  , pairUnvisited
  , firstUnrespondedOrPending
  , runUnvisited
  , runPending
  , runUnresolved
  , walkAllows
  , runMark
  , recountFile
  , recountIn
  , recountIfChanged
  , rowFeedbackKeys
  , effRuns
  , updateFile
  ) where

import Brick qualified as Brick
import Control.Applicative ((<|>))
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Remax.DiffModel qualified as DiffModel
import Remax.Highlight qualified as Highlight
import Remax.Store qualified as Store
import Remax.UI.Core.Jobs qualified as Jobs
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Widgets qualified as Widgets

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
allRunsInOrder :: Types.St -> [((Int, Int), DiffModel.Run)]
allRunsInOrder st =
  let
    fileRuns (fi, fv) = zipWith (\hi r -> ((fi, hi), r)) [0 ..] (Types.fvRuns fv)
  in
    concatMap fileRuns (zip [0 ..] (Types.stFiles st))

runAt :: Types.St -> (Int, Int) -> Maybe DiffModel.Run
runAt st (f, h) = do
  fv <- Types.safeIdx (Types.stFiles st) f
  Types.safeIdx (Types.fvRuns fv) h

pairPending :: Types.St -> (Int, Int) -> Bool
pairPending st pair =
  case runAt st pair of
    Just r -> runPending (Types.stVerdicts st) r
    Nothing -> False

pairUnvisited :: Types.St -> (Int, Int) -> Bool
pairUnvisited st pair =
  case runAt st pair of
    Just r -> runUnvisited (Types.stVerdicts st) r
    Nothing -> False

-- Where a session begins: the first hunk nothing has been said about
-- yet; only when every hunk has some response does the first still-
-- pending (flagged/questioned) one win.
firstUnrespondedOrPending :: Types.St -> Maybe (Int, Int)
firstUnrespondedOrPending st =
  fmap fst (List.find (runUnvisited (Types.stVerdicts st) . snd) (allRunsInOrder st))
    <|> firstPending st

firstPending :: Types.St -> Maybe (Int, Int)
firstPending st =
  fmap fst (List.find (runPending (Types.stVerdicts st) . snd) (allRunsInOrder st))

-- Unvisited: some line has no verdict at all — the hunk still needs a
-- first look, which is what auto-advance cares about.
runUnvisited :: Map.Map T.Text Store.Verdict -> DiffModel.Run -> Bool
runUnvisited m r = any (`Map.notMember` m) (DiffModel.runKeys r)

-- Pending: anything not fully accepted as reviewed. Flagged and
-- questioned lines keep their hunk pending — they still need attention —
-- which is what the counts, the p filter, and navigation care about.
runPending :: Map.Map T.Text Store.Verdict -> DiffModel.Run -> Bool
runPending m r =
  let
    accepted k = Map.lookup k m == Just Store.Reviewed
  in
    not (all accepted (DiffModel.runKeys r))

runUnresolved :: Map.Map T.Text Store.Verdict -> Map.Map T.Text [Store.Post] -> DiffModel.Run -> Bool
runUnresolved verdicts threads r =
  let
    asked k =
      case Map.lookup k verdicts of
        Just Store.Flagged -> True
        Just Store.Questioned -> True
        _ -> Map.member k threads
  in
    any asked (DiffModel.runKeys r)

walkAllows :: Types.St -> DiffModel.Run -> Bool
walkAllows st r =
  case Types.stWalk st of
    Types.WalkAll -> True
    Types.WalkPending -> runPending (Types.stVerdicts st) r
    Types.WalkUnresolved -> runUnresolved (Types.stVerdicts st) (Types.stThreads st) r

runMark :: Map.Map T.Text Store.Verdict -> DiffModel.Run -> Char
runMark m r =
  let
    verdicts = map (`Map.lookup` m) (DiffModel.runKeys r)
  in
    if runUnvisited m r
      then '\183' -- ·
      else
        if any (== Just Store.Flagged) verdicts
          then '!'
          else
            if any (== Just Store.Questioned) verdicts
              then '?'
              else '\10003' -- ✓

remarksFor :: Map.Map T.Text Store.Verdict -> Map.Map T.Text [Store.Post] -> Types.FileView -> [Types.Remark]
remarksFor verdicts threads fv =
  let
    pick row = do
      k <- DiffModel.rRemarkKey row
      -- a removed row keeps its remark too, but by then the hunk it
      -- belongs to is what carries it through the tray
      Monad.guard (DiffModel.rMark row == ' ')
      Monad.guard (Map.member k verdicts || Map.member k threads)
      n <- DiffModel.rNewRow row
      pure (Types.Remark n k (DiffModel.rText row))
  in
    Maybe.mapMaybe pick (Types.fvRows fv)

recountFile :: Map.Map T.Text Store.Verdict -> Map.Map T.Text [Store.Post] -> Types.FileView -> Types.FileView
recountFile verdicts threads fv =
  fv
    { Types.fvRemarks = remarksFor verdicts threads fv
    , Types.fvPending = length (filter (runPending verdicts) (Types.fvRuns fv))
    }

recountIn :: Types.St -> Types.FileView -> Types.FileView
recountIn st = recountFile (Types.stVerdicts st) (Types.stThreads st)

fileKeys :: Types.FileView -> Set.Set T.Text
fileKeys fv =
  Set.fromList (concatMap DiffModel.runKeys (Types.fvRuns fv) ++ Maybe.mapMaybe DiffModel.rRemarkKey (Types.fvRows fv))

changedKeys :: Eq v => Map.Map T.Text v -> Map.Map T.Text v -> Set.Set T.Text
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

recountIfChanged :: Brick.EventM Types.Name Types.St ()
recountIfChanged = do
  st <- State.get
  let
    verdictsMoved = Types.stVerdicts st /= Types.stCountedVerdicts st
    threadsMoved = Types.stThreads st /= Types.stCountedThreads st
  Monad.when (verdictsMoved || threadsMoved) $ do
    let
      changed =
        Set.union
          (changedKeys (Types.stCountedVerdicts st) (Types.stVerdicts st))
          (changedKeys (Types.stCountedThreads st) (Types.stThreads st))
    State.modify (\s -> s {Types.stCountedVerdicts = Types.stVerdicts st, Types.stCountedThreads = Types.stThreads st})
    case Types.currentFileMay st of
      Just fv
        | changed `Set.isSubsetOf` fileKeys fv ->
            State.modify (\s -> updateFile (Types.stCurF s) id s)
      _ -> recountAll

recountInlineRows :: Int
recountInlineRows = 300000

recountAll :: Brick.EventM Types.Name Types.St ()
recountAll = do
  st <- State.get
  let
    verdicts = Types.stVerdicts st
    threads = Types.stThreads st
    counted =
      Map.fromList
        ( map
            (\fv -> (Types.fvPath fv, (remarksFor verdicts threads fv, length (filter (runPending verdicts) (Types.fvRuns fv)))))
            (Types.stFiles st)
        )
    epoch = Types.stEpoch st
    settle :: Either T.Text (Map.Map FilePath ([Types.Remark], Int)) -> Brick.EventM Types.Name Types.St ()
    settle outcome = do
      latest <- State.get
      Monad.when (Types.stEpoch latest == epoch) $
        case outcome of
          Left err -> Jobs.jobFailed "recounting" err
          Right done ->
            State.modify $ \s ->
              s
                { Types.stFiles =
                    map
                      ( \fv -> case Map.lookup (Types.fvPath fv) done of
                          Just (rs, n) -> fv {Types.fvRemarks = rs, Types.fvPending = n}
                          Nothing -> fv
                      )
                      (Types.stFiles s)
                , Types.stCountedVerdicts = verdicts
                , Types.stCountedThreads = threads
                }
    forced = Map.foldr (\(rs, n) acc -> sum (map Types.rmRow rs) + n + acc) (0 :: Int) counted
  if sum (map Types.fvRowCount (Types.stFiles st)) <= recountInlineRows
    then State.modify (\s -> s {Types.stFiles = map (recountIn s) (Types.stFiles s)})
    else Jobs.startJob "recounting" (Exception.evaluate forced >> pure counted) settle

-- Where a row's feedback lives. A changed row carries what was said
-- about the change; a line nobody has changed carries a remark, keyed
-- against the base; and a removed row can answer to either, because a
-- remark made while it was unchanged follows it here when somebody
-- changes it.
rowFeedbackKeys :: DiffModel.RowR -> [T.Text]
rowFeedbackKeys r = Maybe.catMaybes [DiffModel.rKey r, DiffModel.rRemarkKey r]

effRuns :: DiffModel.RowR -> [Highlight.Seg]
effRuns r =
  case Widgets.nonEmptySegs (DiffModel.rToks r) of
    [] -> [(Brick.attrName "plain", DiffModel.rText r)]
    filled -> filled

-- Drawing the body ---------------------------------------------------------------

updateFile :: Int -> (Types.FileView -> Types.FileView) -> Types.St -> Types.St
updateFile i f st =
  let
    patch (j, fv) = if j == i then recountIn st (f fv) else fv
  in
    st {Types.stFiles = map patch (zip [0 ..] (Types.stFiles st))}
