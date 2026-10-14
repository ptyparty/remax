{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Model.Load
  ( loadAll
  , rebuildFV
  , patchLine
  , ensureCurrentHighlighted
  , rebuildWith
  , freshView
  , reloadFromDisk
  , pollSubject
  , refreshGit
  , rebuildAllFiles
  , forceGitRefresh
  ) where

import Brick qualified as Brick
import Brick.BChan qualified as BChan
import Control.Applicative ((<|>))
import Control.Concurrent qualified as Concurrent
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock qualified as Clock
import GHC.Clock qualified as GHCClock
import Remax.Config qualified as Config
import Remax.DiffModel qualified as DiffModel
import Remax.Git qualified as Git
import Remax.Highlight qualified as Highlight
import Remax.Modal qualified as Modal
import Remax.TreeSitter qualified as TS
import Remax.UI.Core.Jobs qualified as Jobs
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Verdict qualified as Verdict
import Remax.UI.Core.View qualified as View
import Remax.UI.Core.WordDiff qualified as WordDiff
import Remax.UI.Model.Nav qualified as Nav
import Remax.UI.Model.Watch qualified as Watch
import Remax.Zipper qualified as Z
import System.Directory qualified as Directory
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import System.Timeout qualified as Timeout

loadAll :: Config.Config -> TS.HighlighterTable -> Git.ReviewMode -> FilePath -> IO [Types.FileView]
loadAll cfg hls mode root = do
  ignores <- Config.loadIgnores root
  names <-
    fmap
      (filter (not . Config.ignoredPath ignores))
      (Git.reviewFiles mode (Config.cfgUntracked cfg) root)
  Maybe.catMaybes <$> Jobs.forMaybeBounded (loadOne cfg hls mode root) names

loadOne ::
  Config.Config ->
  TS.HighlighterTable ->
  Git.ReviewMode ->
  FilePath ->
  FilePath ->
  IO (Maybe Types.FileView)
loadOne cfg hls mode root path =
  case Git.subjectOf mode of
    Git.WorkingTree -> do
      present <- Directory.doesFileExist (root </> path)
      if present
        then loadPresent cfg hls mode root path
        else loadRemoved cfg hls mode root path
    Git.Ref tip -> loadAtRef cfg hls mode root tip path

loadAtRef :: Config.Config -> TS.HighlighterTable -> Git.ReviewMode -> FilePath -> T.Text -> FilePath -> IO (Maybe Types.FileView)
loadAtRef cfg hls mode root tip path = do
  got <- Git.refContent root tip path
  case got of
    Nothing -> loadRemoved cfg hls mode root path
    Just Git.RefBinary -> pure Nothing
    Just (Git.RefText newLs) -> do
      base <- Git.baseLines mode root path
      fmap Just (buildView cfg hls root path False base newLs)

-- No working copy, but git still has the base: the change set
-- removed this file. It reviews as a file whose every line is a
-- removal, so accepting those lines is accepting the deletion.
-- Absent on both sides means there is nothing to say about it.
loadRemoved :: Config.Config -> TS.HighlighterTable -> Git.ReviewMode -> FilePath -> FilePath -> IO (Maybe Types.FileView)
loadRemoved cfg hls mode root path = do
  base <- Git.baseLines mode root path
  case base of
    [] -> pure Nothing
    _ -> fmap Just (buildView cfg hls root path True base [])

loadPresent :: Config.Config -> TS.HighlighterTable -> Git.ReviewMode -> FilePath -> FilePath -> IO (Maybe Types.FileView)
loadPresent cfg hls mode root path = do
  readable <- Exception.try (Git.looksBinary root path) :: IO (Either Exception.IOException Bool)
  case readable of
    Left _ -> pure Nothing
    Right True -> pure Nothing
    Right False -> do
      base <- Git.baseLines mode root path
      gotNew <- Exception.try (TIO.readFile (root </> path)) :: IO (Either Exception.IOException T.Text)
      case gotNew of
        Left _ -> pure Nothing
        Right newTxt -> do
          stamped <-
            Exception.try (Directory.getModificationTime (root </> path)) :: IO (Either Exception.IOException Clock.UTCTime)
          fv <- buildView cfg hls root path False base (T.lines newTxt)
          pure (Just fv {Types.fvMtime = either (const Nothing) Just stamped})

buildView :: Config.Config -> TS.HighlighterTable -> FilePath -> FilePath -> Bool -> [T.Text] -> [T.Text] -> IO Types.FileView
buildView cfg hls root path deleted base newLs = do
  (fv, _) <- rebuildFV cfg (T.pack root) (freshView hls path deleted base newLs)
  let
    anchor = case Types.fvRuns fv of
      (r : _) -> DiffModel.runAnchor r
      [] -> 0
  pure fv {Types.fvEd = Modal.newEdState anchor newLs}

freshView :: TS.HighlighterTable -> FilePath -> Bool -> [T.Text] -> [T.Text] -> Types.FileView
freshView hls path deleted base newLs =
  Types.FileView
    { Types.fvPath = path
    , Types.fvBase = base
    , Types.fvEd = Modal.newEdState 0 newLs
    , Types.fvRows = []
    , Types.fvEmph = []
    , Types.fvRowCount = 0
    , Types.fvOrigin = 0
    , Types.fvRuns = []
    , Types.fvToks = []
    , Types.fvStale = False
    , Types.fvDeleted = deleted
    , Types.fvHighlighted = False
    , -- the shebang is the fallback for a file with no
      -- extension to go on, so the first line comes along
      Types.fvHl = TS.highlighterFor hls path (Maybe.listToMaybe newLs)
    , Types.fvRunDecls = []
    , Types.fvRemarks = []
    , Types.fvPending = 0
    , Types.fvMtime = Nothing
    }

diffBudgetUs :: Int
diffBudgetUs = 400000

-- Real tokenization is paid only for files that have actually been shown;
-- everything else carries plain tokens until the cursor arrives. Shown
-- files use the vendored tree-sitter grammar when one claims the file,
-- and render plain when there is none or the parse blew its budget.
rebuildFV :: Config.Config -> T.Text -> Types.FileView -> IO (Types.FileView, Bool)
rebuildFV cfg repo fv = do
  let
    sc = Config.cfgSplit cfg
    ws = if Config.cfgIgnoreWs cfg then DiffModel.WhitespaceIgnored else DiffModel.WhitespaceMatters
  let
    newLs = View.newSideLines fv
  toks <-
    if Types.fvHighlighted fv
      then tokensFor fv newLs
      else pure (Highlight.plainLines newLs)
  (rows3, degraded) <- diffIO ws (Types.fvBase fv) newLs
  let
    (rows, runs) = DiffModel.assemble sc repo (T.pack (Types.fvPath fv)) rows3 toks
    decls = View.declsForAnchors newLs (map DiffModel.runAnchor runs)
  pure
    ( fv
        { Types.fvRows = rows
        , Types.fvRowCount = length rows
        , Types.fvRuns = runs
        , Types.fvToks = toks
        , Types.fvStale = False
        , Types.fvRunDecls = decls
        , Types.fvEmph = emphFor cfg rows
        }
    , degraded
    )

tokensFor :: Types.FileView -> [T.Text] -> IO [[Highlight.Seg]]
tokensFor fv newLs =
  case Types.fvHl fv of
    Nothing -> pure (Highlight.plainLines newLs)
    Just hl -> do
      got <- TS.tsHighlight hl newLs
      case got of
        Nothing -> pure (Highlight.plainLines newLs)
        Just scoped -> do
          let
            segs = map (map (\(scope, t) -> (Highlight.scopeSeg scope, t))) scoped
          _ <- Exception.evaluate (DiffModel.forceTokLines segs `seq` segs)
          pure segs

threadTokens :: [DiffModel.RowR] -> [[Highlight.Seg]] -> [DiffModel.RowR]
threadTokens rows ts =
  case rows of
    [] -> []
    (row : rest) -> case DiffModel.rNewRow row of
      Just _ -> case ts of
        (tl : more) -> row {DiffModel.rToks = tl} : threadTokens rest more
        [] -> row {DiffModel.rToks = []} : threadTokens rest []
      Nothing -> row {DiffModel.rToks = []} : threadTokens rest ts

-- Which columns the word diff marks on each of these rows, or nothing on
-- any of them when the reader has turned the marks off.
--
-- Lazy on purpose, and lazy all the way down: taking the window's slice
-- forces only how long each block is, so the pairing itself is paid for
-- by the blocks that actually have a row on screen — and paid once,
-- because the answer stays on the FileView until the rows are rebuilt.
emphFor :: Config.Config -> [DiffModel.RowR] -> [[(Int, Int)]]
emphFor cfg rows =
  if Config.cfgWordDiff cfg
    then
      WordDiff.intralineSpans
        (if Config.cfgIgnoreWs cfg then DiffModel.WhitespaceIgnored else DiffModel.WhitespaceMatters)
        rows
    else map (const []) rows

diffIO ::
  DiffModel.WhitespaceMode -> [T.Text] -> [T.Text] -> IO ([(Char, Maybe Int, T.Text)], Bool)
diffIO mode old new = do
  let
    parts = DiffModel.diffParts mode old new
  -- The clock belongs on the diff, not on the file: forcing every row
  -- under the same budget let a million-line file that differs by one
  -- line blow it. And the budget is wall-clock, so on a large heap a
  -- major GC pause can spend it on nobody's behalf — which is survivable
  -- only because giving up now costs about what the diff would have.
  got <- Timeout.timeout diffBudgetUs (Exception.evaluate (length (DiffModel.dpGrouped parts)))
  pure $ case got of
    Just _ -> (DiffModel.dpRows parts, False)
    Nothing -> (DiffModel.dpCoarse parts, True)

patchLine :: Config.Config -> Int -> T.Text -> Types.FileView -> Types.FileView
patchLine cfg row newText fv =
  let
    lineToks = Highlight.plainLine newText
    isPatchedContext r = DiffModel.rMark r == ' ' && DiffModel.rNewRow r == Just row
    touchedContext = any isPatchedContext (Types.fvRows fv)
    patchTok (i, tl) = if i == row then lineToks else tl
    patchRow r =
      if DiffModel.rNewRow r == Just row
        then r {DiffModel.rText = newText, DiffModel.rToks = lineToks}
        else r
    patched = map patchRow (Types.fvRows fv)
  in
    fv
      { Types.fvToks = map patchTok (zip [0 ..] (Types.fvToks fv))
      , Types.fvRows = patched
      , -- the line just typed into is one of these rows, so the marks
        -- that were worked out against its old text no longer describe it
        Types.fvEmph = emphFor cfg patched
      , Types.fvStale = Types.fvStale fv || touchedContext
      }

-- Verdict helpers -------------------------------------------------------------

-- Runs after every event (see the app wiring): if navigation just landed
-- on a file still carrying plain tokens, give it its real colors now.
ensureCurrentHighlighted :: Brick.EventM Types.Name Types.St ()
ensureCurrentHighlighted = do
  st <- State.get
  let
    i = Types.stCurF st
    withTokensOf toks current =
      current
        { Types.fvRows = threadTokens (Types.fvRows current) toks
        , Types.fvToks = toks
        , Types.fvHighlighted = True
        }
  case Types.safeIdx (Types.stFiles st) i of
    Nothing -> pure ()
    Just fv
      | Types.fvHighlighted fv -> pure ()
      | Types.fvRowCount fv <= highlightInlineRows -> do
          toks <- MonadIO.liftIO (tokensFor fv (View.newSideLines fv))
          State.modify (Verdict.updateFile i (withTokensOf toks))
      | Types.stHighlighting st == Just (Types.fvPath fv) -> pure ()
      | otherwise -> do
          State.modify (\s -> s {Types.stHighlighting = Just (Types.fvPath fv)})
          let
            before = View.newSideLines fv
            epoch = Types.stEpoch st
            label = "highlighting " <> T.pack (FilePath.takeFileName (Types.fvPath fv))
          Jobs.startJob label (Jobs.withSlot (Types.stWorkSlots st) (tokensFor fv before)) $ \outcome -> do
            latest <- State.get
            Monad.when (Types.stEpoch latest == epoch) $ do
              State.modify (\s -> s {Types.stHighlighting = Nothing})
              case (outcome, List.find ((== Types.fvPath fv) . Types.fvPath . snd) (zip [0 :: Int ..] (Types.stFiles latest))) of
                (Left err, _) -> Jobs.jobFailed label err
                (Right toks, Just (j, current))
                  | not (Types.fvHighlighted current) && View.newSideLines current == before ->
                      State.modify (Verdict.updateFile j (withTokensOf toks))
                _ -> pure ()

highlightInlineRows :: Int
highlightInlineRows = 4000

rebuildWith :: Int -> Modal.EdState -> Brick.EventM Types.Name Types.St ()
rebuildWith i ed = do
  State.modify (\s -> s {Types.stKill = Modal.edKill ed})
  Nav.publishCopy ed
  st <- State.get
  let
    fv = (Types.stFiles st !! i) {Types.fvEd = ed}
    oldCur =
      if i == Types.stCurF st then Nav.currentRunOf st i else Nothing
  (rebuilt, degraded) <- MonadIO.liftIO (rebuildFV (Types.stConfig st) (Types.repoText st) fv)
  State.modify (Verdict.updateFile i (const rebuilt))
  Monad.when (i == Types.stCurF st) $
    State.modify (\s -> s {Types.stCurH = Nav.matchRun oldCur (Types.fvRuns rebuilt)})
  Monad.when degraded $
    State.modify (\s -> s {Types.stMsg = "diff too large — file shown as one hunk"})

-- A file changed on disk. When the buffer holds no unapplied edits it is
-- reloaded in place (cursor kept); mid-insert the local edit wins until
-- Esc, and the change is only announced.
data Reloaded
  = ReloadVanished
  | ReloadUnchanged
  | ReloadReady Types.FileView Int

data ReloadPlan = ReloadPlan
  { rlPath :: FilePath
  , rlOldLines :: [T.Text]
  , rlCursor :: (Int, Int)
  , rlBase :: [T.Text]
  , rlEpoch :: Int
  }

reloadFromDisk :: FilePath -> Brick.EventM Types.Name Types.St ()
reloadFromDisk rel = do
  st <- State.get
  case Git.subjectOf (Types.effectiveMode st) of
    Git.Ref _ -> pure ()
    Git.WorkingTree -> reloadWorking rel st

reloadWorking :: FilePath -> Types.St -> Brick.EventM Types.Name Types.St ()
reloadWorking rel st =
  case List.find ((== rel) . Types.fvPath) (Types.stFiles st) of
    -- an unknown file changing may mean the change set just grew
    Nothing -> refreshGit
    Just fv
      | Map.member rel (Types.stReloading st) ->
          State.modify (\s -> s {Types.stReloading = Map.insert rel True (Types.stReloading s)})
      | otherwise -> do
          State.modify (\s -> s {Types.stReloading = Map.insert rel False (Types.stReloading s)})
          let
            plan =
              ReloadPlan
                { rlPath = rel
                , rlOldLines = Modal.bufferLines (Types.fvEd fv)
                , rlCursor = Modal.cursorRC (Types.fvEd fv)
                , rlBase = Types.fvBase fv
                , rlEpoch = Types.stEpoch st
                }
          Jobs.startJob
            ("reloading " <> T.pack rel)
            (Jobs.withSlot (Types.stWorkSlots st) (reloadWork st fv plan))
            (finishReload plan)

reloadWork :: Types.St -> Types.FileView -> ReloadPlan -> IO Reloaded
reloadWork st fv plan = do
  let
    path = Types.stRoot st </> rlPath plan
    oldLs = rlOldLines plan
    (row, col) = rlCursor plan
  got <- Exception.try (TIO.readFile path) :: IO (Either Exception.IOException T.Text)
  case got of
    Left _ -> pure ReloadVanished
    Right diskTxt
      | T.lines diskTxt == oldLs -> pure ReloadUnchanged
      | otherwise -> do
          let
            diskLs = T.lines diskTxt
          stamped <- Exception.try (Directory.getModificationTime path) :: IO (Either Exception.IOException Clock.UTCTime)
          newRow <- mapRowThroughEdit oldLs diskLs row
          let
            newCol = min col (T.length (Maybe.fromMaybe "" (Types.safeIdx diskLs newRow)))
            fresh = Modal.newEdState newRow diskLs
            placed = fresh {Modal.edZipper = Z.moveCursor (newRow, newCol) (Modal.edZipper fresh)}
          (rebuilt, _) <- rebuildFV (Types.stConfig st) (Types.repoText st) fv {Types.fvEd = placed}
          pure (ReloadReady rebuilt {Types.fvMtime = either (const Nothing) Just stamped} newRow)

finishReload :: ReloadPlan -> Either T.Text Reloaded -> Brick.EventM Types.Name Types.St ()
finishReload plan outcome = do
  queued <- State.get
  let
    rel = rlPath plan
  Monad.when (Types.stEpoch queued == rlEpoch plan) $ do
    let
      again = Map.findWithDefault False rel (Types.stReloading queued)
    State.modify (\s -> s {Types.stReloading = Map.delete rel (Types.stReloading s)})
    latest <- State.get
    case (outcome, List.find ((== rel) . Types.fvPath . snd) (zip [0 ..] (Types.stFiles latest))) of
      (Left err, _) -> Jobs.jobFailed "reloading" err
      (Right ReloadVanished, _) -> refreshGit
      (Right ReloadUnchanged, _) -> pure ()
      (Right (ReloadReady _ _), Nothing) -> pure ()
      (Right (ReloadReady rebuilt newRow), Just (i, current))
        | Modal.edMode (Types.fvEd current) == Modal.Insert ->
            Types.setMsg ("\9888 " <> T.pack rel <> " changed on disk; Esc applies your edit over it")
        | Modal.bufferLines (Types.fvEd current) /= rlOldLines plan || Types.fvBase current /= rlBase plan ->
            reloadFromDisk rel
        | otherwise -> installReload plan i current rebuilt newRow
    Monad.when again (reloadFromDisk rel)

installReload :: ReloadPlan -> Int -> Types.FileView -> Types.FileView -> Int -> Brick.EventM Types.Name Types.St ()
installReload plan i current rebuilt newRow = do
  latest <- State.get
  -- remember which screen row the cursor sat on, so the
  -- view doesn't jump when lines shift above it
  mvp <- Brick.lookupViewport Types.BodyVP
  screenRows <- View.terminalRows
  let
    (liveRow, liveCol) = Modal.cursorRC (Types.fvEd current)
    moved = (liveRow, liveCol) /= rlCursor plan
    landing =
      if moved
        then min liveRow (max 0 (Modal.bufferLineCount (Types.fvEd rebuilt) - 1))
        else newRow
    installed =
      if moved
        then rebuilt {Types.fvEd = placeCursor (landing, liveCol) (Types.fvEd rebuilt)}
        else rebuilt
    oldOut = View.cursorDisplayRow (Types.fvRows current) liveRow
    screenOff = case mvp of
      Just vp -> max 0 (oldOut - Brick._vpTop vp)
      Nothing -> 0
    oldCur = if i == Types.stCurF latest then Nav.currentRunOf latest i else Nothing
  State.modify (Verdict.updateFile i (const installed))
  Monad.when (i == Types.stCurF latest) $
    State.modify (\s -> s {Types.stCurH = Nav.matchRun oldCur (Types.fvRuns installed)})
  reloaded <- State.get
  let
    newOut = View.cursorDisplayRow (Types.fvRows installed) landing
    origin = snd (View.bodyWindow screenRows reloaded installed)
  State.modify (Verdict.updateFile i (\f -> f {Types.fvOrigin = origin}))
  Brick.setTop (Brick.viewportScroll Types.BodyVP) (max 0 (newOut - origin - screenOff))
  Types.setMsg ("\8635 reloaded " <> T.pack (rlPath plan))

pollSubject :: Brick.EventM Types.Name Types.St ()
pollSubject = do
  st <- State.get
  case Git.subjectOf (Types.effectiveMode st) of
    Git.Ref tip -> pollRef st tip
    Git.WorkingTree -> Monad.when (Types.stWatchMode st == Types.PollingWatch) (pollCurrentFile st)

refPollSeconds :: Double
refPollSeconds = 2

pollRef :: Types.St -> T.Text -> Brick.EventM Types.Name Types.St ()
pollRef st tip = do
  now <- MonadIO.liftIO GHCClock.getMonotonicTime
  Monad.when (now - Types.stLastRefPoll st >= refPollSeconds && not (Types.stRefreshing st)) $ do
    State.modify (\s -> s {Types.stLastRefPoll = now})
    MonadIO.liftIO . Monad.void . Concurrent.forkIO $ do
      at <- Git.commitAt (Types.stRoot st) tip
      Monad.when (at /= Types.stHead st) $
        Monad.void (BChan.writeBChanNonBlocking (Types.stChan st) Types.GitStateChanged)

pollCurrentFile :: Types.St -> Brick.EventM Types.Name Types.St ()
pollCurrentFile st =
  case Types.currentFileMay st of
    Nothing -> pure ()
    Just fv -> do
      got <-
        MonadIO.liftIO
          ( Exception.try (Directory.getModificationTime (Types.stRoot st </> Types.fvPath fv)) ::
              IO (Either Exception.IOException Clock.UTCTime)
          )
      case (got, Types.fvMtime fv) of
        (Left _, _) -> pure ()
        (Right mtime, Nothing) ->
          State.modify (Verdict.updateFile (Types.stCurF st) (\f -> f {Types.fvMtime = Just mtime}))
        (Right mtime, Just seen)
          | mtime /= seen && not (Map.member (Types.fvPath fv) (Types.stReloading st)) ->
              reloadFromDisk (Types.fvPath fv)
          | otherwise -> pure ()

-- Where the cursor's line went: the old buffer is diffed against the new
-- one and the row mapped through it, so the cursor follows its content
-- when lines shift, and lands on the nearest surviving line when its own
-- line was edited away. Best effort — a degraded (budget-blown) diff
-- falls back toward the top.
mapRowThroughEdit :: [T.Text] -> [T.Text] -> Int -> IO Int
mapRowThroughEdit oldLs newLs target = do
  -- tracking where a line went, not what counts as a change: spacing
  -- is part of the line's identity here whatever the setting says
  (rows, _) <- diffIO DiffModel.WhitespaceMatters oldLs newLs
  let
    clamp r = max 0 (min r (length newLs - 1))
    bufferRowOf (_, mnr, _) = mnr
    nextSurviving rest lastNew =
      case Maybe.mapMaybe bufferRowOf rest of
        (nr : _) -> nr
        [] -> Maybe.fromMaybe (max 0 (length newLs - 1)) lastNew
    go _ lastNew [] = Maybe.fromMaybe 0 lastNew
    go o lastNew ((m, mnr, _) : rest) =
      case m of
        '+' -> go o (mnr <|> lastNew) rest
        _ ->
          if o == target
            then case mnr of
              Just nr -> nr
              Nothing -> nextSurviving rest lastNew
            else go (o + 1) (mnr <|> lastNew) rest
  pure (clamp (go (0 :: Int) Nothing rows))

-- Something moved in .git: re-list the change set, refresh bases that
-- changed, add new files, drop vanished ones. Throttled, since rebases
-- and builds fire git events in bursts — but a burst's last event must
-- not be lost (it describes the settled state), so a throttled call
-- queues exactly one delayed re-check.
data Refreshed
  = Dropped
  | Unchanged FilePath
  | Replaced Types.FileView

data Refresh = Refresh
  { rfRebase :: Maybe Git.RebaseStatus
  , rfBranch :: Maybe T.Text
  , rfHead :: Maybe (T.Text, T.Text)
  , rfResults :: [Refreshed]
  }

refreshGit :: Brick.EventM Types.Name Types.St ()
refreshGit = do
  st <- State.get
  now <- MonadIO.liftIO GHCClock.getMonotonicTime
  if Types.stRefreshing st
    then State.modify (\s -> s {Types.stGitQueued = True})
    else
      if now - Types.stLastGit st <= 0.5
        then Monad.unless (Types.stGitQueued st) (queueRefresh st)
        else do
          State.modify (\s -> s {Types.stLastGit = now, Types.stGitQueued = False, Types.stRefreshing = True})
          Jobs.startJob "refreshing" (refreshWork st) (applyRefresh (Types.stEpoch st))

queueRefresh :: Types.St -> Brick.EventM Types.Name Types.St ()
queueRefresh st = do
  State.modify (\s -> s {Types.stGitQueued = True})
  Monad.void . MonadIO.liftIO . Concurrent.forkIO $ do
    Concurrent.threadDelay 700000
    Monad.void (BChan.writeBChanNonBlocking (Types.stChan st) Types.GitStateChanged)

refreshWork :: Types.St -> IO Refresh
refreshWork st = do
  let
    root = Types.stRoot st
    existing = Map.fromList (map (\fv -> (Types.fvPath fv, fv)) (Types.stFiles st))
  rebaseNow <- Git.rebaseStatus root
  let
    mode = Types.rebaseEffMode rebaseNow (Types.stMode st)
  (branchNow, headNow) <- Git.subjectPlace mode root
  let
    tipSettled = case Git.subjectOf mode of
      Git.Ref _ -> headNow == Types.stHead st && mode == Types.stMode st
      Git.WorkingTree -> False
    guarded name = do
      got <-
        if tipSettled && Map.member name existing
          then pure (Right (Unchanged name))
          else Exception.try (refreshOne st mode existing name)
      pure $ case got of
        Left e -> let _ = (e :: Exception.IOException) in Dropped
        Right r -> r
  ignores <- Config.loadIgnores root
  names <-
    fmap
      (filter (not . Config.ignoredPath ignores))
      (Git.reviewFiles mode (Config.cfgUntracked (Types.stConfig st)) root)
  refreshed <- Jobs.forBounded 8 guarded names
  extras <- Monad.forM (filter (`notElem` names) (Set.toList (Types.stOpened st))) (refreshOpened st mode existing)
  pure (Refresh rebaseNow branchNow headNow (refreshed ++ extras))

refreshOne :: Types.St -> Git.ReviewMode -> Map.Map FilePath Types.FileView -> FilePath -> IO Refreshed
refreshOne st mode existing name =
  case Map.lookup name existing of
    Nothing -> reloadOne st mode name
    Just fv ->
      case Git.subjectOf mode of
        Git.WorkingTree -> refreshWorking st mode name fv
        Git.Ref tip -> refreshAtRef st mode tip name fv

reloadOne :: Types.St -> Git.ReviewMode -> FilePath -> IO Refreshed
reloadOne st mode name =
  fmap
    (maybe Dropped (Replaced . Verdict.recountIn st))
    (loadOne (Types.stConfig st) (Types.stHls st) mode (Types.stRoot st) name)

refreshWorking :: Types.St -> Git.ReviewMode -> FilePath -> Types.FileView -> IO Refreshed
refreshWorking st mode name fv = do
  let
    root = Types.stRoot st
  -- a file that has come or gone since it was loaded is a
  -- different kind of view than the one in hand, so it is
  -- read afresh rather than patched
  present <- Directory.doesFileExist (root </> name)
  if present == Types.fvDeleted fv
    then reloadOne st mode name
    else do
      base <- Git.baseLines mode root name
      if base == Types.fvBase fv
        then pure (Unchanged name)
        else do
          (rebuilt, _) <- rebuildFV (Types.stConfig st) (Types.repoText st) fv {Types.fvBase = base}
          pure (Replaced (Verdict.recountIn st rebuilt))

refreshAtRef :: Types.St -> Git.ReviewMode -> T.Text -> FilePath -> Types.FileView -> IO Refreshed
refreshAtRef st mode tip name fv = do
  got <- Git.refContent (Types.stRoot st) tip name
  base <- Git.baseLines mode (Types.stRoot st) name
  let
    shown = if Types.fvDeleted fv then Nothing else Just (Modal.bufferLines (Types.fvEd fv))
    now = case got of
      Just (Git.RefText ls) -> Just ls
      _ -> Nothing
  if now == shown && base == Types.fvBase fv
    then pure (Unchanged name)
    else reloadOne st mode name

-- hand-opened files stay in the session even when the change set
-- doesn't claim them; once git stops seeing a difference (say the
-- file was committed), the base honestly rebases to the buffer so
-- stale hunks don't linger
refreshOpened :: Types.St -> Git.ReviewMode -> Map.Map FilePath Types.FileView -> FilePath -> IO Refreshed
refreshOpened st mode existing path =
  case Map.lookup path existing of
    Nothing -> pure Dropped
    Just fv -> do
      base <- Git.baseLines mode (Types.stRoot st) path
      let
        buf = Modal.bufferLines (Types.fvEd fv)
      if base == buf && Types.fvBase fv /= buf
        then do
          (rebuilt, _) <- rebuildFV (Types.stConfig st) (Types.repoText st) fv {Types.fvBase = buf}
          pure (Replaced (Verdict.recountIn st rebuilt))
        else pure (Unchanged path)

applyRefresh :: Int -> Either T.Text Refresh -> Brick.EventM Types.Name Types.St ()
applyRefresh epoch outcome = do
  before <- State.get
  Monad.when (Types.stEpoch before == epoch) $ do
    State.modify (\s -> s {Types.stRefreshing = False})
    case outcome of
      Left err -> Jobs.jobFailed "refreshing" err
      Right refresh -> installRefresh before refresh
    after <- State.get
    Monad.when (Types.stGitQueued after) $ do
      State.modify (\s -> s {Types.stGitQueued = False})
      refreshGit

installRefresh :: Types.St -> Refresh -> Brick.EventM Types.Name Types.St ()
installRefresh before refresh = do
  let
    gitMoved =
      rfHead refresh /= Types.stHead before
        || rfBranch refresh /= Types.stBranch before
        || rfRebase refresh /= Types.stRebase before
  State.modify $ \s ->
    s
      { Types.stRebase = rfRebase refresh
      , Types.stBranch = rfBranch refresh
      , Types.stHead = rfHead refresh
      }
  latest <- State.get
  let
    current = Map.fromList (map (\fv -> (Types.fvPath fv, fv)) (Types.stFiles latest))
    subject = Git.subjectOf (Types.effectiveMode latest)
    resolve r = case r of
      Dropped -> Nothing
      Unchanged path -> Map.lookup path current
      Replaced fv -> Just (Verdict.recountIn latest (mergeLive subject current fv))
    remaining = Maybe.mapMaybe resolve (rfResults refresh)
  Monad.forM_ remaining (MonadIO.liftIO . Types.stWatch latest . Types.fileDir (Types.stRoot latest) . Types.fvPath)
  Monad.when gitMoved (MonadIO.liftIO (Watch.watchTracked (Types.stWatch latest) (Types.stRoot latest)))
  applyChangeSet remaining

mergeLive :: Git.Subject -> Map.Map FilePath Types.FileView -> Types.FileView -> Types.FileView
mergeLive subject current fv =
  case Map.lookup (Types.fvPath fv) current of
    Nothing -> fv
    Just live
      | Modal.bufferLines (Types.fvEd live) == Modal.bufferLines (Types.fvEd fv) ->
          fv {Types.fvEd = Types.fvEd live, Types.fvOrigin = Types.fvOrigin live}
      | otherwise ->
          case subject of
            Git.WorkingTree -> live {Types.fvBase = Types.fvBase fv, Types.fvStale = True}
            Git.Ref _ -> fv {Types.fvEd = cursorCarried live fv, Types.fvOrigin = Types.fvOrigin live}

cursorCarried :: Types.FileView -> Types.FileView -> Modal.EdState
cursorCarried live fresh = placeCursor (Modal.cursorRC (Types.fvEd live)) (Types.fvEd fresh)

placeCursor :: (Int, Int) -> Modal.EdState -> Modal.EdState
placeCursor (row, col) ed =
  let
    landing = max 0 (min row (Modal.bufferLineCount ed - 1))
    width = maybe 0 T.length (Types.safeIdx (Modal.bufferLines ed) landing)
  in
    ed {Modal.edZipper = Z.moveCursor (landing, max 0 (min col width)) (Modal.edZipper ed)}

applyChangeSet :: [Types.FileView] -> Brick.EventM Types.Name Types.St ()
applyChangeSet remaining = do
  latest <- State.get
  let
    wasEmpty = null (Types.stFiles latest)
    firstLoad = not (Types.stLoadedOnce latest)
    pendingCount =
      length (filter (Verdict.runPending (Types.stVerdicts latest)) (concatMap Types.fvRuns remaining))
    totalCount = length (concatMap Types.fvRuns remaining)
  if null remaining
    -- the persistent session idles rather than exits: the watcher
    -- stays live and new work wakes the screen on its own
    then
      State.put
        latest
          { Types.stFiles = []
          , Types.stCurF = 0
          , Types.stCurH = 0
          , Types.stLoadedOnce = True
          , -- only on the way in: a later refresh that finds the
            -- session still idle must not talk over whatever the
            -- action that emptied it had to say
            Types.stMsg =
              if (wasEmpty && not firstLoad) || (firstLoad && Types.stWatchMode latest == Types.PollingWatch)
                then Types.stMsg latest
                else "change set is empty — watching for changes"
          }
    else do
      let
        curPath = case Types.currentFileMay latest of
          Just fv -> Types.fvPath fv
          Nothing -> ""
        oldCur = Nav.currentRunOf latest (Types.stCurF latest)
        newF = Maybe.fromMaybe 0 (List.findIndex ((== curPath) . Types.fvPath) remaining)
      State.put
        latest
          { Types.stFiles = remaining
          , Types.stCurF = newF
          , Types.stCurH = Nav.matchRun oldCur (Types.fvRuns (remaining !! newF))
          , Types.stLoadedOnce = True
          , Types.stMsg =
              if firstLoad && Types.stWatchMode latest == Types.NativeWatch
                then Types.tshow pendingCount <> " of " <> Types.tshow totalCount <> " hunks pending"
                else Types.stMsg latest
          }
      Monad.when wasEmpty $ do
        woken <- State.get
        case Verdict.firstUnrespondedOrPending woken of
          Just (f, h) -> Nav.parkAnchor f h
          Nothing -> Nav.parkAnchor 0 0

-- The : command line ----------------------------------------------------------

-- Split knobs reshape every file's hunks; rebuild them all (bounded
-- workers) and keep the current hunk by content.
rebuildAllFiles :: Brick.EventM Types.Name Types.St ()
rebuildAllFiles = do
  st <- State.get
  let
    cfg = Types.stConfig st
    oldCur = Nav.currentRunOf st (Types.stCurF st)
  rebuilt <-
    MonadIO.liftIO
      ( Jobs.forMaybeBounded
          (\fv -> fmap (Just . fst) (rebuildFV cfg (Types.repoText st) fv))
          (Types.stFiles st)
      )
  let
    files = map (Verdict.recountIn st) (Maybe.catMaybes rebuilt)
  Monad.unless (null files) $
    State.modify $ \s ->
      s
        { Types.stFiles = files
        , Types.stCurH =
            case Types.safeIdx files (Types.stCurF s) of
              Just fv -> Nav.matchRun oldCur (Types.fvRuns fv)
              Nothing -> 0
        }

-- Reflect git state right now, bypassing the watcher throttle — for
-- commands whose whole point is an immediate view change.
forceGitRefresh :: Brick.EventM Types.Name Types.St ()
forceGitRefresh = do
  State.modify (\s -> s {Types.stLastGit = 0, Types.stGitQueued = False})
  refreshGit
