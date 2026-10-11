{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Load
  ( loadAll
  , rebuildFV
  , patchLine
  , ensureCurrentHighlighted
  , rebuildWith
  , reloadFromDisk
  , pollCurrentFile
  , refreshGit
  , rebuildAllFiles
  , forceGitRefresh
  ) where

import Brick
  ( EventM
  , Viewport (..)
  , ViewportScroll (..)
  , lookupViewport
  , viewportScroll
  )
import Brick.BChan
  ( writeBChanNonBlocking
  )
import Control.Applicative ((<|>))
import Control.Concurrent
  ( forkIO
  , threadDelay
  )
import Control.Exception
  ( IOException
  , evaluate
  , try
  )
import Control.Monad
  ( forM
  , forM_
  , unless
  , void
  , when
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, modify, put)
import Data.List
  ( find
  , findIndex
  )
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( catMaybes
  , fromMaybe
  , listToMaybe
  , mapMaybe
  )
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime)
import GHC.Clock (getMonotonicTime)
import Remax.Config
import Remax.DiffModel
import Remax.Git
  ( RebaseStatus
  , ReviewMode (..)
  , baseLines
  , currentBranch
  , headCommit
  , looksBinary
  , rebaseStatus
  , reviewFiles
  )
import Remax.Highlight
import Remax.Modal
import qualified Remax.TreeSitter as TS
import Remax.UI.Jobs
import Remax.UI.Nav
import Remax.UI.Types
import Remax.UI.Verdict
import Remax.UI.View
import Remax.UI.Watch
import Remax.UI.WordDiff
import qualified Remax.Zipper as Z
import System.Directory
  ( doesFileExist
  , getModificationTime
  )
import System.FilePath
  ( takeFileName
  , (</>)
  )
import System.Timeout (timeout)

loadAll :: Config -> TS.HighlighterTable -> ReviewMode -> FilePath -> IO [FileView]
loadAll cfg hls mode root = do
  ignores <- loadIgnores root
  names <-
    fmap
      (filter (not . ignoredPath ignores))
      (reviewFiles mode (cfgUntracked cfg) root)
  catMaybes <$> forMaybeBounded (loadOne cfg hls mode root) names

loadOne ::
  Config ->
  TS.HighlighterTable ->
  ReviewMode ->
  FilePath ->
  FilePath ->
  IO (Maybe FileView)
loadOne cfg hls mode root path = do
  present <- doesFileExist (root </> path)
  if present
    then loadPresent cfg hls mode root path
    else loadRemoved cfg hls mode root path

-- No working copy, but git still has the base: the change set
-- removed this file. It reviews as a file whose every line is a
-- removal, so accepting those lines is accepting the deletion.
-- Absent on both sides means there is nothing to say about it.
loadRemoved :: Config -> TS.HighlighterTable -> ReviewMode -> FilePath -> FilePath -> IO (Maybe FileView)
loadRemoved cfg hls mode root path = do
  base <- baseLines mode root path
  case base of
    [] -> pure Nothing
    _ -> fmap Just (buildView cfg hls root path True base [])

loadPresent :: Config -> TS.HighlighterTable -> ReviewMode -> FilePath -> FilePath -> IO (Maybe FileView)
loadPresent cfg hls mode root path = do
  readable <- try (looksBinary root path) :: IO (Either IOException Bool)
  case readable of
    Left _ -> pure Nothing
    Right True -> pure Nothing
    Right False -> do
      base <- baseLines mode root path
      gotNew <- try (TIO.readFile (root </> path)) :: IO (Either IOException Text)
      case gotNew of
        Left _ -> pure Nothing
        Right newTxt -> do
          stamped <-
            try (getModificationTime (root </> path)) :: IO (Either IOException UTCTime)
          fv <- buildView cfg hls root path False base (T.lines newTxt)
          pure (Just fv {fvMtime = either (const Nothing) Just stamped})

buildView :: Config -> TS.HighlighterTable -> FilePath -> FilePath -> Bool -> [Text] -> [Text] -> IO FileView
buildView cfg hls root path deleted base newLs = do
  (fv, _) <- rebuildFV cfg (T.pack root) (freshView hls path deleted base newLs)
  let
    anchor = case fvRuns fv of
      (r : _) -> runAnchor r
      [] -> 0
  pure fv {fvEd = newEdState anchor newLs}

freshView :: TS.HighlighterTable -> FilePath -> Bool -> [Text] -> [Text] -> FileView
freshView hls path deleted base newLs =
  FileView
    { fvPath = path
    , fvBase = base
    , fvEd = newEdState 0 newLs
    , fvRows = []
    , fvEmph = []
    , fvRowCount = 0
    , fvOrigin = 0
    , fvRuns = []
    , fvToks = []
    , fvStale = False
    , fvDeleted = deleted
    , fvHighlighted = False
    , -- the shebang is the fallback for a file with no
      -- extension to go on, so the first line comes along
      fvHl = TS.highlighterFor hls path (listToMaybe newLs)
    , fvRunDecls = []
    , fvRemarks = []
    , fvPending = 0
    , fvMtime = Nothing
    }

diffBudgetUs :: Int
diffBudgetUs = 400000

-- Real tokenization is paid only for files that have actually been shown;
-- everything else carries plain tokens until the cursor arrives. Shown
-- files use the vendored tree-sitter grammar when one claims the file,
-- and render plain when there is none or the parse blew its budget.
rebuildFV :: Config -> Text -> FileView -> IO (FileView, Bool)
rebuildFV cfg repo fv = do
  let
    sc = cfgSplit cfg
    ws = if cfgIgnoreWs cfg then WhitespaceIgnored else WhitespaceMatters
  let
    newLs = newSideLines fv
  toks <-
    if fvHighlighted fv
      then tokensFor fv newLs
      else pure (plainLines newLs)
  (rows3, degraded) <- diffIO ws (fvBase fv) newLs
  let
    (rows, runs) = assemble sc repo (T.pack (fvPath fv)) rows3 toks
    decls = declsForAnchors newLs (map runAnchor runs)
  pure
    ( fv
        { fvRows = rows
        , fvRowCount = length rows
        , fvRuns = runs
        , fvToks = toks
        , fvStale = False
        , fvRunDecls = decls
        , fvEmph = emphFor cfg rows
        }
    , degraded
    )

tokensFor :: FileView -> [Text] -> IO [[Seg]]
tokensFor fv newLs =
  case fvHl fv of
    Nothing -> pure (plainLines newLs)
    Just hl -> do
      got <- TS.tsHighlight hl newLs
      case got of
        Nothing -> pure (plainLines newLs)
        Just scoped -> do
          let
            segs = map (map (\(scope, t) -> (scopeSeg scope, t))) scoped
          _ <- evaluate (forceTokLines segs `seq` segs)
          pure segs

threadTokens :: [RowR] -> [[Seg]] -> [RowR]
threadTokens rows ts =
  case rows of
    [] -> []
    (row : rest) -> case rNewRow row of
      Just _ -> case ts of
        (tl : more) -> row {rToks = tl} : threadTokens rest more
        [] -> row {rToks = []} : threadTokens rest []
      Nothing -> row {rToks = []} : threadTokens rest ts

-- Which columns the word diff marks on each of these rows, or nothing on
-- any of them when the reader has turned the marks off.
--
-- Lazy on purpose, and lazy all the way down: taking the window's slice
-- forces only how long each block is, so the pairing itself is paid for
-- by the blocks that actually have a row on screen — and paid once,
-- because the answer stays on the FileView until the rows are rebuilt.
emphFor :: Config -> [RowR] -> [[(Int, Int)]]
emphFor cfg rows =
  if cfgWordDiff cfg
    then
      intralineSpans
        (if cfgIgnoreWs cfg then WhitespaceIgnored else WhitespaceMatters)
        rows
    else map (const []) rows

diffIO ::
  WhitespaceMode -> [Text] -> [Text] -> IO ([(Char, Maybe Int, Text)], Bool)
diffIO mode old new = do
  let
    parts = diffParts mode old new
  -- The clock belongs on the diff, not on the file: forcing every row
  -- under the same budget let a million-line file that differs by one
  -- line blow it. And the budget is wall-clock, so on a large heap a
  -- major GC pause can spend it on nobody's behalf — which is survivable
  -- only because giving up now costs about what the diff would have.
  got <- timeout diffBudgetUs (evaluate (length (dpGrouped parts)))
  pure $ case got of
    Just _ -> (dpRows parts, False)
    Nothing -> (dpCoarse parts, True)

patchLine :: Config -> Int -> Text -> FileView -> FileView
patchLine cfg row newText fv =
  let
    lineToks = plainLine newText
    isPatchedContext r = rMark r == ' ' && rNewRow r == Just row
    touchedContext = any isPatchedContext (fvRows fv)
    patchTok (i, tl) = if i == row then lineToks else tl
    patchRow r =
      if rNewRow r == Just row
        then r {rText = newText, rToks = lineToks}
        else r
    patched = map patchRow (fvRows fv)
  in
    fv
      { fvToks = map patchTok (zip [0 ..] (fvToks fv))
      , fvRows = patched
      , -- the line just typed into is one of these rows, so the marks
        -- that were worked out against its old text no longer describe it
        fvEmph = emphFor cfg patched
      , fvStale = fvStale fv || touchedContext
      }

-- Verdict helpers -------------------------------------------------------------

-- Runs after every event (see the app wiring): if navigation just landed
-- on a file still carrying plain tokens, give it its real colors now.
ensureCurrentHighlighted :: EventM Name St ()
ensureCurrentHighlighted = do
  st <- get
  let
    i = stCurF st
    withTokensOf toks current =
      current
        { fvRows = threadTokens (fvRows current) toks
        , fvToks = toks
        , fvHighlighted = True
        }
  case safeIdx (stFiles st) i of
    Nothing -> pure ()
    Just fv
      | fvHighlighted fv -> pure ()
      | fvRowCount fv <= highlightInlineRows -> do
          toks <- liftIO (tokensFor fv (newSideLines fv))
          modify (updateFile i (withTokensOf toks))
      | stHighlighting st == Just (fvPath fv) -> pure ()
      | otherwise -> do
          modify (\s -> s {stHighlighting = Just (fvPath fv)})
          let
            before = newSideLines fv
            epoch = stEpoch st
            label = "highlighting " <> T.pack (takeFileName (fvPath fv))
          startJob label (withSlot (stWorkSlots st) (tokensFor fv before)) $ \outcome -> do
            latest <- get
            when (stEpoch latest == epoch) $ do
              modify (\s -> s {stHighlighting = Nothing})
              case (outcome, find ((== fvPath fv) . fvPath . snd) (zip [0 :: Int ..] (stFiles latest))) of
                (Left err, _) -> jobFailed label err
                (Right toks, Just (j, current))
                  | not (fvHighlighted current) && newSideLines current == before ->
                      modify (updateFile j (withTokensOf toks))
                _ -> pure ()

highlightInlineRows :: Int
highlightInlineRows = 4000

rebuildWith :: Int -> EdState -> EventM Name St ()
rebuildWith i ed = do
  modify (\s -> s {stKill = edKill ed})
  publishCopy ed
  st <- get
  let
    fv = (stFiles st !! i) {fvEd = ed}
    oldCur =
      if i == stCurF st then currentRunOf st i else Nothing
  (rebuilt, degraded) <- liftIO (rebuildFV (stConfig st) (repoText st) fv)
  modify (updateFile i (const rebuilt))
  when (i == stCurF st) $
    modify (\s -> s {stCurH = matchRun oldCur (fvRuns rebuilt)})
  when degraded $
    modify (\s -> s {stMsg = "diff too large — file shown as one hunk"})

-- A file changed on disk. When the buffer holds no unapplied edits it is
-- reloaded in place (cursor kept); mid-insert the local edit wins until
-- Esc, and the change is only announced.
data Reloaded
  = ReloadVanished
  | ReloadUnchanged
  | ReloadReady FileView Int

data ReloadPlan = ReloadPlan
  { rlPath :: FilePath
  , rlOldLines :: [Text]
  , rlCursor :: (Int, Int)
  , rlBase :: [Text]
  , rlEpoch :: Int
  }

reloadFromDisk :: FilePath -> EventM Name St ()
reloadFromDisk rel = do
  st <- get
  case find ((== rel) . fvPath) (stFiles st) of
    -- an unknown file changing may mean the change set just grew
    Nothing -> refreshGit
    Just fv
      | Map.member rel (stReloading st) ->
          modify (\s -> s {stReloading = Map.insert rel True (stReloading s)})
      | otherwise -> do
          modify (\s -> s {stReloading = Map.insert rel False (stReloading s)})
          let
            plan =
              ReloadPlan
                { rlPath = rel
                , rlOldLines = bufferLines (fvEd fv)
                , rlCursor = cursorRC (fvEd fv)
                , rlBase = fvBase fv
                , rlEpoch = stEpoch st
                }
          startJob
            ("reloading " <> T.pack rel)
            (withSlot (stWorkSlots st) (reloadWork st fv plan))
            (finishReload plan)

reloadWork :: St -> FileView -> ReloadPlan -> IO Reloaded
reloadWork st fv plan = do
  let
    path = stRoot st </> rlPath plan
    oldLs = rlOldLines plan
    (row, col) = rlCursor plan
  got <- try (TIO.readFile path) :: IO (Either IOException Text)
  case got of
    Left _ -> pure ReloadVanished
    Right diskTxt
      | T.lines diskTxt == oldLs -> pure ReloadUnchanged
      | otherwise -> do
          let
            diskLs = T.lines diskTxt
          stamped <- try (getModificationTime path) :: IO (Either IOException UTCTime)
          newRow <- mapRowThroughEdit oldLs diskLs row
          let
            newCol = min col (T.length (fromMaybe "" (safeIdx diskLs newRow)))
            fresh = newEdState newRow diskLs
            placed = fresh {edZipper = Z.moveCursor (newRow, newCol) (edZipper fresh)}
          (rebuilt, _) <- rebuildFV (stConfig st) (repoText st) fv {fvEd = placed}
          pure (ReloadReady rebuilt {fvMtime = either (const Nothing) Just stamped} newRow)

finishReload :: ReloadPlan -> Either Text Reloaded -> EventM Name St ()
finishReload plan outcome = do
  queued <- get
  let
    rel = rlPath plan
  when (stEpoch queued == rlEpoch plan) $ do
    let
      again = Map.findWithDefault False rel (stReloading queued)
    modify (\s -> s {stReloading = Map.delete rel (stReloading s)})
    latest <- get
    case (outcome, find ((== rel) . fvPath . snd) (zip [0 ..] (stFiles latest))) of
      (Left err, _) -> jobFailed "reloading" err
      (Right ReloadVanished, _) -> refreshGit
      (Right ReloadUnchanged, _) -> pure ()
      (Right (ReloadReady _ _), Nothing) -> pure ()
      (Right (ReloadReady rebuilt newRow), Just (i, current))
        | edMode (fvEd current) == Insert ->
            setMsg ("\9888 " <> T.pack rel <> " changed on disk; Esc applies your edit over it")
        | bufferLines (fvEd current) /= rlOldLines plan || fvBase current /= rlBase plan ->
            reloadFromDisk rel
        | otherwise -> installReload plan i current rebuilt newRow
    when again (reloadFromDisk rel)

installReload :: ReloadPlan -> Int -> FileView -> FileView -> Int -> EventM Name St ()
installReload plan i current rebuilt newRow = do
  latest <- get
  -- remember which screen row the cursor sat on, so the
  -- view doesn't jump when lines shift above it
  mvp <- lookupViewport BodyVP
  screenRows <- terminalRows
  let
    (liveRow, liveCol) = cursorRC (fvEd current)
    moved = (liveRow, liveCol) /= rlCursor plan
    landing =
      if moved
        then min liveRow (max 0 (bufferLineCount (fvEd rebuilt) - 1))
        else newRow
    installed =
      if moved
        then rebuilt {fvEd = (fvEd rebuilt) {edZipper = Z.moveCursor (landing, liveCol) (edZipper (fvEd rebuilt))}}
        else rebuilt
    oldOut = cursorDisplayRow (fvRows current) liveRow
    screenOff = case mvp of
      Just vp -> max 0 (oldOut - _vpTop vp)
      Nothing -> 0
    oldCur = if i == stCurF latest then currentRunOf latest i else Nothing
  modify (updateFile i (const installed))
  when (i == stCurF latest) $
    modify (\s -> s {stCurH = matchRun oldCur (fvRuns installed)})
  reloaded <- get
  let
    newOut = cursorDisplayRow (fvRows installed) landing
    origin = snd (bodyWindow screenRows reloaded installed)
  modify (updateFile i (\f -> f {fvOrigin = origin}))
  setTop (viewportScroll BodyVP) (max 0 (newOut - origin - screenOff))
  setMsg ("\8635 reloaded " <> T.pack (rlPath plan))

pollCurrentFile :: EventM Name St ()
pollCurrentFile = do
  st <- get
  when (stWatchMode st == PollingWatch) $
    case currentFileMay st of
      Nothing -> pure ()
      Just fv -> do
        got <-
          liftIO
            ( try (getModificationTime (stRoot st </> fvPath fv)) ::
                IO (Either IOException UTCTime)
            )
        case (got, fvMtime fv) of
          (Left _, _) -> pure ()
          (Right mtime, Nothing) ->
            modify (updateFile (stCurF st) (\f -> f {fvMtime = Just mtime}))
          (Right mtime, Just seen)
            | mtime /= seen && not (Map.member (fvPath fv) (stReloading st)) ->
                reloadFromDisk (fvPath fv)
            | otherwise -> pure ()

-- Where the cursor's line went: the old buffer is diffed against the new
-- one and the row mapped through it, so the cursor follows its content
-- when lines shift, and lands on the nearest surviving line when its own
-- line was edited away. Best effort — a degraded (budget-blown) diff
-- falls back toward the top.
mapRowThroughEdit :: [Text] -> [Text] -> Int -> IO Int
mapRowThroughEdit oldLs newLs target = do
  -- tracking where a line went, not what counts as a change: spacing
  -- is part of the line's identity here whatever the setting says
  (rows, _) <- diffIO WhitespaceMatters oldLs newLs
  let
    clamp r = max 0 (min r (length newLs - 1))
    bufferRowOf (_, mnr, _) = mnr
    nextSurviving rest lastNew =
      case mapMaybe bufferRowOf rest of
        (nr : _) -> nr
        [] -> fromMaybe (max 0 (length newLs - 1)) lastNew
    go _ lastNew [] = fromMaybe 0 lastNew
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
  | Replaced FileView

data Refresh = Refresh
  { rfRebase :: Maybe RebaseStatus
  , rfBranch :: Maybe Text
  , rfHead :: Maybe (Text, Text)
  , rfResults :: [Refreshed]
  }

refreshGit :: EventM Name St ()
refreshGit = do
  st <- get
  now <- liftIO getMonotonicTime
  if stRefreshing st
    then modify (\s -> s {stGitQueued = True})
    else
      if now - stLastGit st <= 0.5
        then unless (stGitQueued st) (queueRefresh st)
        else do
          modify (\s -> s {stLastGit = now, stGitQueued = False, stRefreshing = True})
          startJob "refreshing" (refreshWork st) (applyRefresh (stEpoch st))

queueRefresh :: St -> EventM Name St ()
queueRefresh st = do
  modify (\s -> s {stGitQueued = True})
  void . liftIO . forkIO $ do
    threadDelay 700000
    void (writeBChanNonBlocking (stChan st) GitStateChanged)

refreshWork :: St -> IO Refresh
refreshWork st = do
  let
    root = stRoot st
    existing = Map.fromList (map (\fv -> (fvPath fv, fv)) (stFiles st))
  rebaseNow <- rebaseStatus root
  branchNow <- currentBranch root
  headNow <- headCommit root
  let
    mode = rebaseEffMode rebaseNow (stMode st)
    guarded name = do
      got <- try (refreshOne st mode existing name)
      pure $ case got of
        Left e -> let _ = (e :: IOException) in Dropped
        Right r -> r
  ignores <- loadIgnores root
  names <-
    fmap
      (filter (not . ignoredPath ignores))
      (reviewFiles mode (cfgUntracked (stConfig st)) root)
  refreshed <- forBounded 8 guarded names
  extras <- forM (filter (`notElem` names) (Set.toList (stOpened st))) (refreshOpened st mode existing)
  pure (Refresh rebaseNow branchNow headNow (refreshed ++ extras))

refreshOne :: St -> ReviewMode -> Map.Map FilePath FileView -> FilePath -> IO Refreshed
refreshOne st mode existing name =
  let
    root = stRoot st
    counted = recountIn st
    reload = fmap (maybe Dropped (Replaced . counted)) (loadOne (stConfig st) (stHls st) mode root name)
  in
    case Map.lookup name existing of
      Nothing -> reload
      Just fv -> do
        -- a file that has come or gone since it was loaded is a
        -- different kind of view than the one in hand, so it is
        -- read afresh rather than patched
        present <- doesFileExist (root </> name)
        if present == fvDeleted fv
          then reload
          else do
            base <- baseLines mode root name
            if base == fvBase fv
              then pure (Unchanged name)
              else do
                (rebuilt, _) <- rebuildFV (stConfig st) (repoText st) fv {fvBase = base}
                pure (Replaced (counted rebuilt))

-- hand-opened files stay in the session even when the change set
-- doesn't claim them; once git stops seeing a difference (say the
-- file was committed), the base honestly rebases to the buffer so
-- stale hunks don't linger
refreshOpened :: St -> ReviewMode -> Map.Map FilePath FileView -> FilePath -> IO Refreshed
refreshOpened st mode existing path =
  case Map.lookup path existing of
    Nothing -> pure Dropped
    Just fv -> do
      base <- baseLines mode (stRoot st) path
      let
        buf = bufferLines (fvEd fv)
      if base == buf && fvBase fv /= buf
        then do
          (rebuilt, _) <- rebuildFV (stConfig st) (repoText st) fv {fvBase = buf}
          pure (Replaced (recountIn st rebuilt))
        else pure (Unchanged path)

applyRefresh :: Int -> Either Text Refresh -> EventM Name St ()
applyRefresh epoch outcome = do
  before <- get
  when (stEpoch before == epoch) $ do
    modify (\s -> s {stRefreshing = False})
    case outcome of
      Left err -> jobFailed "refreshing" err
      Right refresh -> installRefresh before refresh
    after <- get
    when (stGitQueued after) $ do
      modify (\s -> s {stGitQueued = False})
      refreshGit

installRefresh :: St -> Refresh -> EventM Name St ()
installRefresh before refresh = do
  let
    gitMoved =
      rfHead refresh /= stHead before
        || rfBranch refresh /= stBranch before
        || rfRebase refresh /= stRebase before
  modify $ \s ->
    s
      { stRebase = rfRebase refresh
      , stBranch = rfBranch refresh
      , stHead = rfHead refresh
      }
  latest <- get
  let
    current = Map.fromList (map (\fv -> (fvPath fv, fv)) (stFiles latest))
    resolve r = case r of
      Dropped -> Nothing
      Unchanged path -> Map.lookup path current
      Replaced fv -> Just (recountIn latest (mergeLive current fv))
    remaining = mapMaybe resolve (rfResults refresh)
  forM_ remaining (liftIO . stWatch latest . fileDir (stRoot latest) . fvPath)
  when gitMoved (liftIO (watchTracked (stWatch latest) (stRoot latest)))
  applyChangeSet remaining

mergeLive :: Map.Map FilePath FileView -> FileView -> FileView
mergeLive current fv =
  case Map.lookup (fvPath fv) current of
    Nothing -> fv
    Just live
      | bufferLines (fvEd live) == bufferLines (fvEd fv) ->
          fv {fvEd = fvEd live, fvOrigin = fvOrigin live}
      | otherwise -> live {fvBase = fvBase fv, fvStale = True}

applyChangeSet :: [FileView] -> EventM Name St ()
applyChangeSet remaining = do
  latest <- get
  let
    wasEmpty = null (stFiles latest)
    firstLoad = not (stLoadedOnce latest)
    pendingCount =
      length (filter (runPending (stVerdicts latest)) (concatMap fvRuns remaining))
    totalCount = length (concatMap fvRuns remaining)
  if null remaining
    -- the persistent session idles rather than exits: the watcher
    -- stays live and new work wakes the screen on its own
    then
      put
        latest
          { stFiles = []
          , stCurF = 0
          , stCurH = 0
          , stLoadedOnce = True
          , -- only on the way in: a later refresh that finds the
            -- session still idle must not talk over whatever the
            -- action that emptied it had to say
            stMsg =
              if (wasEmpty && not firstLoad) || (firstLoad && stWatchMode latest == PollingWatch)
                then stMsg latest
                else "change set is empty — watching for changes"
          }
    else do
      let
        curPath = case currentFileMay latest of
          Just fv -> fvPath fv
          Nothing -> ""
        oldCur = currentRunOf latest (stCurF latest)
        newF = fromMaybe 0 (findIndex ((== curPath) . fvPath) remaining)
      put
        latest
          { stFiles = remaining
          , stCurF = newF
          , stCurH = matchRun oldCur (fvRuns (remaining !! newF))
          , stLoadedOnce = True
          , stMsg =
              if firstLoad && stWatchMode latest == NativeWatch
                then tshow pendingCount <> " of " <> tshow totalCount <> " hunks pending"
                else stMsg latest
          }
      when wasEmpty $ do
        woken <- get
        case firstUnrespondedOrPending woken of
          Just (f, h) -> parkAnchor f h
          Nothing -> parkAnchor 0 0

-- The : command line ----------------------------------------------------------

-- Split knobs reshape every file's hunks; rebuild them all (bounded
-- workers) and keep the current hunk by content.
rebuildAllFiles :: EventM Name St ()
rebuildAllFiles = do
  st <- get
  let
    cfg = stConfig st
    oldCur = currentRunOf st (stCurF st)
  rebuilt <-
    liftIO
      ( forMaybeBounded
          (\fv -> fmap (Just . fst) (rebuildFV cfg (repoText st) fv))
          (stFiles st)
      )
  let
    files = map (recountIn st) (catMaybes rebuilt)
  unless (null files) $
    modify $ \s ->
      s
        { stFiles = files
        , stCurH =
            case safeIdx files (stCurF s) of
              Just fv -> matchRun oldCur (fvRuns fv)
              Nothing -> 0
        }

-- Reflect git state right now, bypassing the watcher throttle — for
-- commands whose whole point is an immediate view change.
forceGitRefresh :: EventM Name St ()
forceGitRefresh = do
  modify (\s -> s {stLastGit = 0, stGitQueued = False})
  refreshGit
