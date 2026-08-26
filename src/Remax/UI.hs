{-# LANGUAGE OverloadedStrings #-}

-- The review screen: the whole working file on screen, hunks decorated in
-- place by a live diff against the base, a collapsible tray of hunks
-- grouped by file, helix-flavored modal editing directly in the buffer,
-- and per-line verdicts recorded to the store as you go. Filesystem
-- events reload files and refresh the change set while the tool runs.
module Remax.UI
  ( runReview
  ) where

import Brick
  ( App (..)
  , AttrMap
  , AttrName
  , BrickEvent (..)
  , EventM
  , Location (..)
  , Padding (Max, Pad)
  , Viewport (..)
  , ViewportScroll (..)
  , ViewportType (Vertical)
  , Widget
  , attrMap
  , attrName
  , customMain
  , fill
  , getVtyHandle
  , hBox
  , hLimit
  , halt
  , lookupViewport
  , padBottom
  , padLeft
  , padRight
  , padTop
  , raw
  , showCursor
  , showFirstCursor
  , str
  , suspendAndResume
  , txt
  , txtWrap
  , vBox
  , vLimit
  , viewport
  , viewportScroll
  , visibleRegion
  , withAttr
  , withBorderStyle
  , withDefAttr
  , (<+>)
  )
import Brick.BChan (BChan, newBChan, writeBChanNonBlocking)
import Brick.Types (Context, Size (Fixed, Greedy), Widget (Widget), availHeight, availWidth, getContext, render)
import Brick.Util (fg)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Applicative ((<|>))
import Control.Concurrent
  ( forkIO
  , newEmptyMVar
  , newQSem
  , putMVar
  , signalQSem
  , takeMVar
  , threadDelay
  , waitQSem
  )
import Control.Exception (IOException, evaluate, finally, try)
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, modify, put)
import Data.Char (isAlphaNum, isSpace, toLower)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (find, findIndex, groupBy, isPrefixOf, maximumBy, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Database.SQLite.Simple (Connection)
import GHC.Clock (getMonotonicTime)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import qualified Remax.Zipper as Z
import System.Environment (lookupEnv)
import System.Exit (die)
import qualified System.FSNotify as FS
import System.FilePath
  ( dropTrailingPathSeparator
  , isAbsolute
  , makeRelative
  , normalise
  , splitDirectories
  , takeDirectory
  , takeFileName
  , (</>)
  )
import System.Process
  ( CreateProcess (delegate_ctlc)
  , createProcess
  , proc
  , readProcessWithExitCode
  , waitForProcess
  )
import System.Timeout (timeout)

import Remax.Config
import Remax.DiffModel
import Remax.Emacs (handleEmacs)
import Remax.Git
  ( PathChange (..)
  , PathStat (..)
  , RebaseStatus (..)
  , ReviewMode (..)
  , baseLines
  , branchRoot
  , changedFiles
  , commitBody
  , commitMessage
  , currentBranch
  , gitStateWatchDirs
  , hasStagedChanges
  , headCommit
  , listFiles
  , listRefs
  , localBranches
  , looksBinary
  , modeLabel
  , pathChangeLetter
  , pathStats
  , rebaseStatus
  , recentCommits
  , refExists
  , runGit
  , trackedChanges
  )
import Remax.Highlight
import Remax.Modal
import Remax.Store
import Remax.Theme
import Remax.Tree
import qualified Remax.TreeSitter as TS

data Name = BodyVP | TrayVP | TheCursor
  deriving (Eq, Ord, Show)

data REvent = FileChangedOnDisk FilePath | GitStateChanged | StoreChanged | Tick

-- The one-line prompt at the bottom of the screen: / search or : command.
data Prompt = SearchPrompt Text | CmdPrompt Text

-- The finder overlay: fuzzy file picking, or project-wide grep. Each
-- display row is aligned with a jump target (file, and a 1-based line
-- for grep hits; 0 = just open the file).
data PickerMode = PickFiles | PickGrep | PickBase | PickBranch

data Picker = Picker
  { pkMode :: PickerMode
  , pkInput :: Text
  , pkAll :: [FilePath]
  , pkMatches :: [Text]
  , pkTargets :: [(FilePath, Int)]
  , pkSel :: Int
  }

-- The note editor overlay: a small modal buffer over the lines it will
-- annotate (resolved when the popup opened).
data NotePopup = NotePopup
  { npEd :: EdState
  , npPurpose :: PopupPurpose
  }

-- What saving the popup buffer means: a post in a note conversation on
-- some lines, or the amended message of the commit under review.
data PopupPurpose
  = NoteFor [Text]
  | -- the paths that will be staged along with the message, so the popup
    -- can show them before you commit to them (empty at a rebase stop,
    -- where :continue owns the folding)
    AmendMessage [FilePath]

data FileView = FileView
  { fvPath :: FilePath
  , fvBase :: [Text]
  , fvEd :: EdState
  , fvRows :: [RowR]
  , fvRuns :: [Run]
  , fvToks :: [[Seg]]
  , fvStale :: Bool
  , fvHighlighted :: Bool -- real syntax colors computed yet? plain until shown
  , fvHl :: Maybe TS.Highlighter -- vendored tree-sitter grammar, when we have one
  , fvRunDecls :: [Text] -- per run: the top-level declaration it falls under
  }

data St = St
  { stFiles :: [FileView]
  , stCurF :: Int
  , stCurH :: Int
  , stVerdicts :: Map.Map Text Verdict
  , stMsg :: Text
  , stRoot :: FilePath
  , stMode :: ReviewMode
  , stConn :: Connection
  , stPassId :: Int64
  , stCounts :: Map.Map Verdict Int
  , stPunch :: [(FilePath, Int, Verdict)]
  , stTray :: Bool
  , stLastGit :: Double
  , stGitQueued :: Bool
  , stChan :: BChan REvent
  , stWatch :: FilePath -> IO () -- watch one directory, once
  , stPrompt :: Maybe Prompt -- the / search or : command prompt, while open
  , stLastSearch :: Text
  , stBorder :: V.Attr -- concrete attr for raw border fills
  , stHls :: TS.HighlighterTable
  , stOnlyPending :: Bool -- hide/skip hunks that already have verdicts
  , stRebase :: Maybe RebaseStatus -- a paused rebase the session is following
  , stWritten :: Set.Set FilePath -- files written since the last commit-shaped action
  , stLastMarked :: [Text] -- keys of the most recent verdict, for :note
  , stOpened :: Set.Set FilePath -- files opened by hand, pinned across refreshes
  , stPicker :: Maybe Picker
  , stNotePopup :: Maybe NotePopup
  , stThreads :: Map.Map Text [Post] -- note conversations by line key, posts in order
  , stSpace :: Bool -- the space menu prefix is pending
  , stConfig :: Config
  , stCmdFiles :: [Text] -- completion caches, filled when : opens
  , stCmdDirs :: [Text]
  , stCmdRefs :: [Text]
  , stPromptCycle :: Maybe (Text, Int) -- Tab cycling: typed base + applied index
  , stEmacsPrefix :: Maybe Char -- a pending C-c / C-x chord (emacs keymap)
  , stDeferAdvance :: Bool -- hold the post-mark jump until the note popup closes
  , stAltFile :: Maybe (FilePath, Int) -- where Space a bounces back to
  , stAnchor :: Maybe (FilePath, Int) -- the hunk under review; ' returns here
  , stGrepNav :: Maybe (Text, [(FilePath, Int)], Int) -- grep hits n/N walk
  , stInfoPopup :: Maybe (Text, Text) -- read-only text popup: title, body
  , stKill :: Text -- session-wide kill/yank slot, shared across files
  , stPolling :: Bool -- the native watcher was refused; polling instead
  , stBranch :: Maybe Text -- Nothing when HEAD is detached
  , stHead :: Maybe (Text, Text) -- (short sha, subject) of HEAD
  , stTree :: Maybe TreeView -- the project tree overlay, while it is open
  , stHelp :: Bool -- the keybinding cheat sheet is up
  }

repoText :: St -> Text
repoText = T.pack . stRoot

currentFile :: St -> FileView
currentFile st = stFiles st !! stCurF st

currentFileMay :: St -> Maybe FileView
currentFileMay st = safeIdx (stFiles st) (stCurF st)

-- The base the session actually diffs against right now: mid-rebase the
-- stopped commit is under review (its parent is the base) — except a
-- conflict stop, where the commit isn't made yet and the resolution work
-- in the tree is the story.
rebaseEffMode :: Maybe RebaseStatus -> ReviewMode -> ReviewMode
rebaseEffMode mRebase mode =
  case mRebase of
    Just rs
      | rebaseConflicted rs -> VsRef "HEAD"
      | otherwise -> VsRef "HEAD^"
    Nothing -> mode

effectiveMode :: St -> ReviewMode
effectiveMode st = rebaseEffMode (stRebase st) (stMode st)

-- The file watcher ---------------------------------------------------------

-- How often the fallback watcher re-reads the directories it is
-- watching, in microseconds. A review session cares about "did the file
-- I am looking at just change", so a second is plenty, and the watch
-- list is a handful of directories rather than the whole repository.
pollInterval :: Int
pollInterval = 1000000

-- The session's watcher, and whether it had to settle for polling.
--
-- inotify instances are a scarce per-user kernel resource: stock
-- fs.inotify.max_user_instances is 128, and a desktop with a few
-- editors, sync clients and language servers running can hold every one
-- of them. remax needs exactly one per session -- every directory it
-- follows is a watch descriptor on that single instance, and those are
-- capped orders of magnitude higher -- but one is still one more than
-- the kernel has to spare sometimes. Losing the native watcher is no
-- reason to refuse to review code, so fall back to polling and let the
-- caller say so on screen.
withWatcher :: (FS.WatchManager -> Bool -> IO a) -> IO a
withWatcher use = do
  let
    pollConfig =
      FS.defaultConfig {FS.confWatchMode = FS.WatchModePoll pollInterval}
    run mgr polling = use mgr polling `finally` FS.stopManager mgr
  native <-
    try (FS.startManagerConf FS.defaultConfig) ::
      IO (Either IOException FS.WatchManager)
  case native of
    Right mgr -> run mgr False
    Left nativeErr -> do
      polled <-
        try (FS.startManagerConf pollConfig) ::
          IO (Either IOException FS.WatchManager)
      case polled of
        Right mgr -> run mgr True
        Left polledErr -> die (watcherFailure nativeErr polledErr)

-- Shown once, in the status line, when the native watcher was refused.
-- Polling still notices everything; it just notices it a beat late, so
-- the message leads with the fix rather than the consequence.
watchPollWarning :: Text
watchPollWarning =
  "\9888 inotify full \8212 polling; sudo sysctl -w fs.inotify.max_user_instances=512"

-- Both watchers refused, which should not happen -- polling asks the
-- kernel for nothing. Say what was tried and what to do about it,
-- rather than letting an inotify errno reach the user on its own.
watcherFailure :: IOException -> IOException -> String
watcherFailure nativeErr polledErr =
  unlines
    [ "remax: no file watcher available."
    , ""
    , "  native : " ++ show nativeErr
    , "  polling: " ++ show polledErr
    , ""
    , "If the native error mentions inotify, the kernel is out of inotify"
    , "instances -- they are a per-user resource and a busy desktop can use"
    , "every one. Raise the cap:"
    , ""
    , "  sudo sysctl -w fs.inotify.max_user_instances=512"
    , ""
    , "and put it in /etc/sysctl.d/ so it survives a reboot."
    ]

-- Entry point ---------------------------------------------------------------

runReview :: ReviewMode -> FilePath -> IO ()
runReview mode root = do
  conn <- openStore
  verdicts <- loadVerdicts conn (T.pack root)
  hls <- TS.initHighlighters
  threads0 <- loadThreads conn (T.pack root)
  cfg <- loadConfig
  rebase0 <- rebaseStatus root
  branch0 <- currentBranch root
  head0 <- headCommit root
  files <- loadAll cfg hls (rebaseEffMode rebase0 mode) root
  passId <- beginPass conn (T.pack root) (modeLabel mode)
  theme <- loadHelixTheme
  chan <- newBChan 64
  final <- withWatcher $ \mgr polling -> do
    -- Watch only the directories that actually matter: where the
    -- changed files live, plus .git itself for index/HEAD updates.
    -- A recursive watch over the whole repository walks every build
    -- artifact directory and dominates startup on real projects.
    watchedRef <- newIORef Set.empty
    let
      watchDirOnce dir = do
        seen <- readIORef watchedRef
        unless (Set.member dir seen) $ do
          modifyIORef' watchedRef (Set.insert dir)
          void
            ( try (FS.watchDir mgr dir (const True) (dispatchFsEvent chan root)) ::
                IO (Either IOException FS.StopListening)
            )
    gitDirs <- gitStateWatchDirs root
    forM_ gitDirs watchDirOnce
    -- the verdict store lives outside the repository; watching it makes
    -- an agent's `remax reply` show up in the session as it lands
    storeDir <- storeDirectory
    void
      ( try
          ( FS.watchDir
              mgr
              storeDir
              (const True)
              (\_ -> void (writeBChanNonBlocking chan StoreChanged))
          ) ::
          IO (Either IOException FS.StopListening)
      )
    forM_ files (watchDirOnce . fileDir root . fvPath)
    let
      pendingCount =
        length
          (filter (runPending verdicts) (concatMap fvRuns files))
      totalCount = length (concatMap fvRuns files)
      st0 =
        St
          { stFiles = files
          , stCurF = 0
          , stCurH = 0
          , stVerdicts = verdicts
          , stMsg =
              if polling
                then watchPollWarning
                else
                  if null files
                    then "change set is empty — watching for changes"
                    else
                      tshow pendingCount
                        <> " of "
                        <> tshow totalCount
                        <> " hunks pending"
          , stRoot = root
          , stMode = mode
          , stConn = conn
          , stPassId = passId
          , stCounts = Map.empty
          , stPunch = []
          , stTray = True
          , stLastGit = 0
          , stGitQueued = False
          , stChan = chan
          , stWatch = watchDirOnce
          , stPrompt = Nothing
          , stLastSearch = ""
          , stBorder = fgOnly (scopeAttr theme "ui.linenr" (fg V.brightBlack))
          , stHls = hls
          , stOnlyPending = True
          , stRebase = rebase0
          , stWritten = Set.empty
          , stLastMarked = []
          , stOpened = Set.empty
          , stPicker = Nothing
          , stNotePopup = Nothing
          , stThreads = threads0
          , stSpace = False
          , stConfig = cfg
          , stCmdFiles = []
          , stCmdDirs = []
          , stCmdRefs = []
          , stPromptCycle = Nothing
          , stEmacsPrefix = Nothing
          , stDeferAdvance = False
          , stAltFile = Nothing
          , stAnchor = Nothing
          , stGrepNav = Nothing
          , stInfoPopup = Nothing
          , stKill = T.empty
          , stPolling = polling
          , stBranch = branch0
          , stHead = head0
          , stTree = Nothing
          , stHelp = False
          }
      st1 = case firstUnrespondedOrPending st0 of
        Just (f, h) ->
          let
            parked = (park f h st0) {stMsg = stMsg st0}
          in
            parked
              { stAnchor =
                  fmap (\fv -> (fvPath fv, stCurH parked)) (currentFileMay parked)
              }
        Nothing -> st0
    -- the first frame paints with plain tokens; this tick colorizes
    -- the file on screen immediately after, off the startup path
    void (writeBChanNonBlocking chan Tick)
    -- terminfo often claims only 256 colors even when the terminal
    -- speaks 24-bit; trust COLORTERM the way helix does, or theme
    -- colors get rounded to the nearest palette entry
    let
      buildVty = do
        colorterm <- lookupEnv "COLORTERM"
        let
          vtyCfg = case colorterm of
            Just v
              | v `elem` ["truecolor", "24bit"] ->
                  V.defaultConfig
                    { V.configPreferredColorMode = Just V.FullColor
                    }
            _ -> V.defaultConfig
        mkVty vtyCfg
    vty <- buildVty
    customMain vty buildVty (Just chan) (app theme) st1
  finishPass conn (stPassId final)
  printSummary final

fileDir :: FilePath -> FilePath -> FilePath
fileDir root path = normalise (root </> takeDirectory path)

-- Every (file, hunk) pair in display order.
allRunPairs :: St -> [(Int, Int)]
allRunPairs st =
  let
    filePairs (fi, fv) = map (\h -> (fi, h)) [0 .. length (fvRuns fv) - 1]
  in
    concatMap filePairs (zip [0 ..] (stFiles st))

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

-- Where a session begins: the first hunk nothing has been said about
-- yet; only when every hunk has some response does the first still-
-- pending (flagged/questioned) one win.
firstUnrespondedOrPending :: St -> Maybe (Int, Int)
firstUnrespondedOrPending st =
  find (pairUnvisited st) (allRunPairs st) <|> firstPending st

firstPending :: St -> Maybe (Int, Int)
firstPending st = find (pairPending st) (allRunPairs st)

printSummary :: St -> IO ()
printSummary st = do
  let
    count v = Map.findWithDefault 0 v (stCounts st)
  putStrLn
    ( "This pass: "
        ++ show (count Reviewed)
        ++ " reviewed, "
        ++ show (count Flagged)
        ++ " flagged, "
        ++ show (count Questioned)
        ++ " questioned."
    )
  forM_ (reverse (stPunch st)) $ \(path, line, v) ->
    putStrLn ("  " ++ T.unpack (verdictText v) ++ ": " ++ path ++ ":" ++ show line)

dispatchFsEvent :: BChan REvent -> FilePath -> FS.Event -> IO ()
dispatchFsEvent chan root ev = do
  let
    rel = makeRelative root (FS.eventPath ev)
    parts = splitDirectories rel
    noisy = any (`elem` ["dist-newstyle", ".stack-work", "node_modules"]) parts
  unless noisy $
    if ".git" `elem` parts
      then
        -- git updates the index by writing index.lock and renaming it, so
        -- the lock file is the reliably-delivered signal
        when
          ( takeFileName rel
              `elem` ["index", "index.lock", "HEAD", "HEAD.lock", "COMMIT_EDITMSG"]
              || "refs" `elem` parts
          )
          (void (writeBChanNonBlocking chan GitStateChanged))
      else void (writeBChanNonBlocking chan (FileChangedOnDisk rel))

-- Loading and rebuilding -----------------------------------------------------

loadAll :: Config -> TS.HighlighterTable -> ReviewMode -> FilePath -> IO [FileView]
loadAll cfg hls mode root = do
  ignores <- loadIgnores root
  names <- fmap (filter (not . ignoredPath ignores)) (changedFiles mode root)
  catMaybes <$> forMaybeBounded (loadOne cfg hls mode root) names

-- Load files on a few workers at once: each one costs a git spawn for
-- its base, and a rebase-stop commit can easily touch a hundred files —
-- serially that is seconds of startup, in parallel it is I/O-bound.
-- A worker's IOException counts as "file not loadable", like loadOne's
-- own error handling.
forMaybeBounded :: (a -> IO (Maybe b)) -> [a] -> IO [Maybe b]
forMaybeBounded f xs = do
  slots <- newQSem 8
  handles <- forM xs $ \x -> do
    slot <- newEmptyMVar
    _ <- forkIO $ do
      waitQSem slots
      got <- try (f x)
      signalQSem slots
      putMVar slot $ case got of
        Left e -> let _ = (e :: IOException) in Nothing
        Right r -> r
    pure slot
  forM handles takeMVar

loadOne ::
  Config ->
  TS.HighlighterTable ->
  ReviewMode ->
  FilePath ->
  FilePath ->
  IO (Maybe FileView)
loadOne cfg hls mode root path = do
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
          let
            newLs = T.lines newTxt
            fv0 =
              FileView
                { fvPath = path
                , fvBase = base
                , fvEd = newEdState 0 newLs
                , fvRows = []
                , fvRuns = []
                , fvToks = []
                , fvStale = False
                , fvHighlighted = False
                , -- the shebang is the fallback for a file with no
                  -- extension to go on, so the first line comes along
                  fvHl = TS.highlighterFor hls path (listToMaybe newLs)
                , fvRunDecls = []
                }
          (fv, _) <- rebuildFV (cfgSplit cfg) (T.pack root) fv0
          let
            anchor = case fvRuns fv of
              (r : _) -> runAnchor r
              [] -> 0
          pure (Just fv {fvEd = newEdState anchor newLs})

diffBudgetUs :: Int
diffBudgetUs = 400000

-- Real tokenization is paid only for files that have actually been shown;
-- everything else carries plain tokens until the cursor arrives. Shown
-- files use the vendored tree-sitter grammar when one claims the file,
-- and render plain when there is none or the parse blew its budget.
rebuildFV :: SplitConfig -> Text -> FileView -> IO (FileView, Bool)
rebuildFV sc repo fv = do
  let
    newLs = bufferLines (fvEd fv)
    plain = pure (plainLines newLs)
  toks <-
    case (fvHighlighted fv, fvHl fv) of
      (True, Just hl) -> do
        got <- TS.tsHighlight hl newLs
        case got of
          Just scoped -> do
            let
              toSeg (scope, t) = (scopeSeg scope, t)
              segs = map (map toSeg) scoped
            _ <- evaluate (forceTokLines segs `seq` segs)
            pure segs
          Nothing -> plain
      _ -> plain
  (rows3, degraded) <- diffIO (fvBase fv) newLs
  let
    (rows, runs) = assemble sc repo (T.pack (fvPath fv)) rows3 toks
    decls = map (fromMaybe "" . declAt newLs . runAnchor) runs
  pure
    ( fv
        { fvRows = rows
        , fvRuns = runs
        , fvToks = toks
        , fvStale = False
        , fvRunDecls = decls
        }
    , degraded
    )

-- The top-level declaration a buffer row falls under: the nearest line
-- at or above it whose first character sits in column zero and isn't a
-- comment or closing bracket — the boundary heuristic hunk splitting
-- already trusts. A tree-sitter walk could be exact; this is right
-- nearly always and costs one upward scan at rebuild time.
declAt :: [Text] -> Int -> Maybe Text
declAt ls row =
  let
    isDecl l = case T.uncons l of
      Just (c, _) ->
        not (isSpace c)
          && not
            ( any
                (`T.isPrefixOf` l)
                ["--", "//", "#", ";", "{-", "/*", "*", "<!--", "}", ")", "]"]
            )
      Nothing -> False
  in
    find isDecl (reverse (take (row + 1) ls))

-- Just the name from a declaration line: its first word, or two when
-- the first is a defining keyword (data Foo, def foo, class Bar ...).
declName :: Text -> Text
declName decl =
  case T.words decl of
    (w1 : w2 : _)
      | w1
          `elem` [ "data"
                 , "type"
                 , "newtype"
                 , "class"
                 , "instance"
                 , "module"
                 , "def"
                 , "fn"
                 , "func"
                 , "function"
                 , "impl"
                 , "pub"
                 , "let"
                 , "const"
                 , "var"
                 , "export"
                 ] ->
          w1 <> " " <> w2
    (w1 : _) -> w1
    [] -> ""

diffIO :: [Text] -> [Text] -> IO ([(Char, Maybe Int, Text)], Bool)
diffIO old new = do
  let
    rows = diffRows old new
    wholeFileFallback =
      map (\o -> ('-', Nothing, o)) old
        ++ zipWith (\i l -> ('+', Just i, l)) [0 ..] new
  got <- timeout diffBudgetUs (evaluate (forceRows rows `seq` rows))
  pure $ case got of
    Just r -> (r, False)
    Nothing -> (wholeFileFallback, True)

patchLine :: Int -> Text -> FileView -> FileView
patchLine row newText fv =
  let
    lineToks = plainLine newText
    isPatchedContext r = rMark r == ' ' && rNewRow r == Just row
    touchedContext = any isPatchedContext (fvRows fv)
    patchTok (i, tl) = if i == row then lineToks else tl
    patchRow r =
      if rNewRow r == Just row
        then r {rText = newText, rToks = lineToks}
        else r
  in
    fv
      { fvToks = map patchTok (zip [0 ..] (fvToks fv))
      , fvRows = map patchRow (fvRows fv)
      , fvStale = fvStale fv || touchedContext
      }

-- Verdict helpers -------------------------------------------------------------

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

clampH :: [Run] -> Int -> Int
clampH rs h = max 0 (min h (length rs - 1))

safeIdx :: [a] -> Int -> Maybe a
safeIdx xs i = if i >= 0 && i < length xs then Just (xs !! i) else Nothing

tshow :: Show a => a -> Text
tshow = T.pack . show

-- The app ----------------------------------------------------------------------

app :: Maybe HelixTheme -> App St REvent Name
app theme =
  App
    { appDraw = draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent =
        \ev -> handleEvent ev >> ensureCurrentHighlighted
    , appStartEvent = do
        vty <- getVtyHandle
        let
          out = V.outputIface vty
        when (V.supportsMode out V.BracketedPaste) $
          liftIO (V.setMode out V.BracketedPaste True)
    , appAttrMap = const (mkAttrMap theme)
    }

-- Drawing ------------------------------------------------------------------------

draw :: St -> [Widget Name]
draw st =
  let
    body = case currentFileMay st of
      Just fv -> viewport BodyVP Vertical (renderFile st fv)
      Nothing -> idleBody
    panes =
      if stTray st && not (null (stFiles st))
        then [tray st, body]
        else [body]
    overlays =
      catMaybes
        [ fmap (notePopupLayer st) (stNotePopup st)
        , fmap (treeLayer st) (stTree st)
        , fmap pickerLayer (stPicker st)
        , if stSpace st then Just spaceMenu else Nothing
        ]
    menuRows = case promptMenu st of
      Just menu -> [menu]
      Nothing -> []
    rebaseRows = case stRebase st of
      Just rb -> [rebaseBar rb]
      Nothing -> [gitBar st]
    helpRows = if stHelp st then [helpLayer] else []
    infoRows = case stInfoPopup st of
      Just (title, infoBody) -> [infoPopupLayer title infoBody]
      Nothing -> []
  in
    helpRows
      ++ infoRows
      ++ overlays
      ++ [ vBox
             ( [headerBar st]
                 ++ rebaseRows
                 ++ [hairline, hBox panes, hairline]
                 ++ menuRows
                 ++ [statusBar st]
             )
         ]

-- A quiet full-width rule; the chrome's only horizontal structure now
-- that the reverse-video slabs are gone.
hairline :: Widget Name
hairline = vLimit 1 (withAttr hairAttr (fill '\9472'))

-- Outside a rebase the same full-width slot answers "where am I in git":
-- the branch, and the commit HEAD is on. Same shape as the rebase bar,
-- because it is the same question.
gitBar :: St -> Widget Name
gitBar st =
  let
    place = case stBranch st of
      Just b ->
        hBox
          [ withAttr headerAttr (txt " on ")
          , withAttr headerFileAttr (txt b)
          ]
      Nothing -> withAttr conflictAttr (txt " detached")
    commitSeg = case stHead st of
      Nothing -> [withAttr hintAttr (txt "   no commits yet")]
      Just (sha, subject) ->
        [ txt "  "
        , withAttr rebaseShaAttr (txt sha)
        , txt "  "
        , txt subject
        , withAttr hintAttr (txt "   \9251c full message")
        ]
  in
    withDefAttr headerRebaseAttr . padRight Max $
      hBox (place : commitSeg)

-- The commit under review gets its own full-width bar during a rebase:
-- step, sha, and the whole subject line. Space c shows the full
-- message.
rebaseBar :: RebaseStatus -> Widget Name
rebaseBar rb =
  let
    conflictNote =
      if rebaseConflicted rb
        then withAttr conflictAttr (txt "  · conflict")
        else txt ""
    shaNote =
      if T.null (rebaseSha rb)
        then txt ""
        else withAttr rebaseShaAttr (txt (rebaseSha rb)) <+> txt "  "
  in
    withDefAttr headerRebaseAttr . padRight Max $
      hBox
        [ withAttr headerAttr $
            txt
              ( " \10227 commit "
                  <> tshow (rebaseStep rb)
                  <> "/"
                  <> tshow (rebaseTotal rb)
                  <> "  "
              )
        , shaNote
        , txt (rebaseSubject rb)
        , conflictNote
        , withAttr hintAttr (txt "   \9251c full message")
        ]

-- Shared float chrome: rounded frame with the title in the border, an
-- elevated surface, inner breathing room, and a dim hint as the last
-- interior row.
floatFrame :: Text -> Text -> Widget Name -> Widget Name
floatFrame title hint body =
  C.centerLayer
    . withDefAttr floatAttr
    . withBorderStyle BS.unicodeRounded
    . B.borderWithLabel (withAttr floatTitleAttr (txt (" " <> title <> " ")))
    . padTop (Pad 1)
    . padLeft (Pad 2)
    . padRight (Pad 2)
    $ vBox
      [ body
      , padTop (Pad 1) (withAttr hintAttr (txt hint))
      ]

-- A read-only text popup (full commit messages and kin); any key closes.
infoPopupLayer :: Text -> Text -> Widget Name
infoPopupLayer title body =
  Widget Greedy Greedy $ do
    ctx <- getContext
    let
      boxWidth = min 100 (max 50 (availWidth ctx - 20))
      bodyRows = max 4 (availHeight ctx - 14)
    render $
      floatFrame title "any key closes" $
        hLimit boxWidth (padRight Max (vLimit bodyRows (txtWrap (padT body))))

-- The pending-space infobox, helix style: bottom-right, listing what the
-- menu offers.
-- The cheat sheet: what a newcomer cannot otherwise find out. The :
-- commands document themselves — a bare : lists every one with its
-- description — but the keys had nowhere to be discovered, so this is
-- mostly keys, grouped by what you are trying to do rather than
-- alphabetically.
helpGroups :: [(Text, [(Text, Text)])]
helpGroups =
  [
    ( "review"
    ,
      [ ("\9166", "accept the selection as reviewed")
      , ("!", "flag it — come back to this")
      , ("?", "question it — explain or fix")
      , ("X", "select the whole hunk")
      , ("] [", "next / previous hunk")
      , ("'", "back to the hunk under review")
      , ("\9251 h", "make this hunk the current one")
      , ("\9251 p", "show only hunks still pending")
      ]
    )
  ,
    ( "saying something"
    ,
      [ (":note TEXT", "a one-line note on what you marked")
      , (":note", "the popup, with the thread so far")
      , ("\9251 u", "drop the conversation here")
      ]
    )
  ,
    ( "moving"
    ,
      [ ("h j k l", "left down up right")
      , ("w b e", "words; the crossed span is selected")
      , ("f F t T", "find / till a character")
      , ("gh gl gs", "line start / end / first non-blank")
      , ("gg ge", "file start / file end")
      , (":42", "go to line 42")
      , (") (", "next / previous file")
      , ("ga", "the alternate file")
      ]
    )
  ,
    ( "selecting"
    ,
      [ ("v", "every motion grows the selection")
      , ("x", "the line; again to extend")
      , ("%", "the whole file")
      , (";", "collapse it")
      ]
    )
  ,
    ( "editing"
    ,
      [ ("i a I A o O", "enter insert")
      , ("d c", "delete / change the selection")
      , ("u", "undo")
      , ("y p P", "yank / paste after / before")
      , ("> <", "indent / unindent")
      , ("J Q", "join lines / reflow to 80")
      , ("r ~", "replace character / toggle case")
      , (".", "repeat the last insert")
      ]
    )
  ,
    ( "finding"
    ,
      [ ("/ n N", "search this file, repeat fwd / back")
      , ("*", "search for the word under the cursor")
      , ("\9251 /", "search the whole project")
      ]
    )
  ,
    ( "getting around"
    ,
      [ ("Ctrl-p", "the file picker")
      , ("\9251 t", "the project tree, with diff stats")
      , ("\9251 b", "files already open in the session")
      , ("Tab", "collapse / expand the tray")
      , ("{ }", "scroll the tray")
      , ("E", "suspend into $EDITOR here")
      ]
    )
  ,
    ( "the : line"
    ,
      [ (":", "commands — a bare : lists them all")
      , (":base", "pick what to diff against")
      , (":rebase", "review commit by commit")
      , (":q", "quit")
      ]
    )
  ]

-- Two columns, split where the row counts come out even, so the sheet is
-- one glance rather than a scroll.
helpLayer :: Widget Name
helpLayer =
  Widget Greedy Greedy $ do
    ctx <- getContext
    let
      colWidth = max 36 (min 50 (div (availWidth ctx - 24) 2))
      keyWidth = 13
      entryRow (key, desc) =
        hBox
          [ hLimit keyWidth (padRight Max (withAttr noteInlineAttr (txt (" " <> key))))
          , withAttr statusAttr (txt desc)
          ]
      groupRows (name, entries) =
        (withAttr floatTitleAttr (txt (" " <> name)) : map entryRow entries)
          ++ [txt " "]
      height g = length (snd g) + 2
      half = div (sum (map height helpGroups)) 2
      -- take a group into the left column while doing so lands closer to
      -- the halfway mark than stopping would; a plain "stop once over
      -- half" split leaves the columns lopsided
      pack used groups =
        case groups of
          [] -> ([], [])
          (g : rest)
            | used == 0
                || abs (used + height g - half) <= abs (half - used) ->
                let
                  (a, b) = pack (used + height g) rest
                in
                  (g : a, b)
            | otherwise -> ([], g : rest)
      (leftGroups, rightGroups) = pack 0 helpGroups
      column groups = vBox (concatMap groupRows groups)
    render $
      floatFrame "keys" "any key closes" $
        hBox
          [ hLimit colWidth (padRight Max (column leftGroups))
          , hLimit colWidth (padRight Max (column rightGroups))
          ]

-- Space, then one key. This table is the only place a space key exists:
-- the infobox is rendered from it and the keystroke is dispatched through
-- it, so a key cannot be bound without being listed. :tree arrived bound
-- but invisible precisely because those used to be two lists.
spaceMenuEntries :: [(Text, Text, EventM Name St ())]
spaceMenuEntries =
  [ ("f", "file picker", openPicker)
  , ("b", "open files", openBufferPicker)
  , ("t", "project tree", openTree)
  , ("n", "note popup", openNotePopup)
  , ("e", "$EDITOR here", editCurrent)
  , ("p", "pending filter", togglePendingOnly)
  , ("u", "resolve note here", unnoteCmd)
  , ("h", "make this hunk current", setCurrentHunkHere)
  , ("r", "revert hunk to base", revertTarget)
  , ("a", "alternate file", gotoAltFile)
  , ("/", "project search", openGrepPicker "")
  , ("c", "commit message", showCommitPopup)
  , ("?", "keys", showHelp)
  ]

showHelp :: EventM Name St ()
showHelp = modify (\s -> s {stHelp = True})

spaceMenu :: Widget Name
spaceMenu =
  let
    item (key, label, _) =
      hBox
        [ withAttr noteInlineAttr (txt (" " <> key))
        , withAttr statusAttr (txt ("  " <> label))
        ]
  in
    floatFrame "space" "any other key cancels" . hLimit 26 . padRight Max $
      vBox (map item spaceMenuEntries)

-- The persistent session's resting state: nothing to review, but the
-- watcher is live and new work will wake the screen on its own.
idleBody :: Widget Name
idleBody =
  padBottom Max . padRight Max $
    vBox
      [ txt " "
      , withAttr doneMarkAttr (txt "  ✓ all clear — nothing to review")
      , txt " "
      , withAttr
          hintAttr
          (txt "  watching for changes · :base REF or :mode switches the view · :q quits")
      ]

headerBar :: St -> Widget Name
headerBar st =
  let
    lead = " remax · " <> modeLabel (stMode st) <> " · "
    rebaseSeg = case stRebase st of
      Nothing -> txt ""
      Just _ -> withAttr headerRebaseAttr (txt "  ·  rebasing")
  in
    withDefAttr headerAttr . padRight Max $
      case currentFileMay st of
        Nothing -> hBox [txt (lead <> "all clear"), rebaseSeg]
        Just fv ->
          let
            rs = fvRuns fv
            h = clampH rs (stCurH st)
            shownH = if null rs then 0 else h + 1
            pendingPairs = filter (pairPending st) (allRunPairs st)
            pendingAll = length pendingPairs
            pendingHere =
              length (filter (\(f, _) -> f == stCurF st) pendingPairs)
            pendingSeg =
              if length (stFiles st) > 1
                then
                  "  ·  "
                    <> tshow pendingHere
                    <> " pending here · "
                    <> tshow pendingAll
                    <> " overall"
                else "  ·  " <> tshow pendingAll <> " pending"
            doneBadge = case safeIdx rs h of
              Just r
                | not (runPending (stVerdicts st) r) ->
                    withAttr headerOkAttr (txt "  ✓ done")
              _ -> txt " "
          in
            hBox
              [ txt lead
              , withAttr headerFileAttr (txt (T.pack (fvPath fv)))
              , withAttr
                  headerCountAttr
                  ( txt
                      ( "  ·  file "
                          <> tshow (stCurF st + 1)
                          <> "/"
                          <> tshow (length (stFiles st))
                          <> "  ·  hunk "
                          <> tshow shownH
                          <> "/"
                          <> tshow (length rs)
                          <> pendingSeg
                      )
                  )
              , doneBadge
              , rebaseSeg
              ]

-- What the tray shows: one line per file (with a count of its hidden
-- hunks), one line per hunk still worth a row. Reviewed hunks collapse
-- under their file (in pending-only mode, all settled hunks do); the
-- current hunk is always shown regardless.
data TrayEntry
  = TrayFile Int Int -- file index, hidden hunk count
  | TrayHunk Int Int -- file index, hunk index

trayEntries :: St -> [TrayEntry]
trayEntries st =
  let
    curF = stCurF st
    curH = clampH (fvRuns (currentFile st)) (stCurH st)
    hiddenHunk fi (hi, r) =
      let
        mark = runMark (stVerdicts st) r
        isCurrent = fi == curF && hi == curH
      in
        not isCurrent && stOnlyPending st && mark == '\10003'
    fileGroup (fi, fv) =
      let
        numbered = zip [0 ..] (fvRuns fv)
        (hiddenOnes, shown) = splitHiddenShown fi numbered
      in
        TrayFile fi (length hiddenOnes)
          : map (\(hi, _) -> TrayHunk fi hi) shown
    splitHiddenShown fi numbered =
      ( filter (hiddenHunk fi) numbered
      , filter (not . hiddenHunk fi) numbered
      )
  in
    concatMap fileGroup (zip [0 ..] (stFiles st))

trayWidth :: Int
trayWidth = 32

tray :: St -> Widget Name
tray st =
  let
    entryWidget entry = case entry of
      TrayFile fi _hidden ->
        let
          fv = stFiles st !! fi
          total = length (fvRuns fv)
          done =
            length
              (filter (not . runPending (stVerdicts st)) (fvRuns fv))
          meterCols = meterTextWidth done total
          name =
            shortPath
              (trayWidth - 3 - meterCols)
              (T.pack (fvPath fv))
        in
          hBox
            [ withAttr
                (if fi == stCurF st then trayFileCurAttr else trayFileAttr)
                (txt (padT name))
            , padLeft Max (meterW done total)
            , txt " "
            ]
      TrayHunk fi hi ->
        let
          fv = stFiles st !! fi
          r = fvRuns fv !! hi
          decl = case safeIdx (fvRunDecls fv) hi of
            Just d
              | not (T.null d) ->
                  " " <> T.take 12 (declName d)
            _ -> ""
          current =
            fi == stCurF st
              && hi == clampH (fvRuns (currentFile st)) (stCurH st)
          mark = runMark (stVerdicts st) r
          noteMark =
            if any (`Map.member` stThreads st) (runKeys r)
              then withAttr noteInlineAttr (txt " \8801")
              else txt ""
          markAttr = case mark of
            '!' -> flagMarkAttr
            '?' -> questionMarkAttr
            '\10003' -> doneMarkAttr
            _ -> pendingMarkAttr
          lead =
            if current
              then withAttr floatSelBarAttr (txt "\9614 ")
              else txt "  "
          line =
            hBox
              [ lead
              , withAttr markAttr (str [mark])
              , txt (" :" <> tshow (runAnchor r + 1))
              , withAttr trayDeclAttr (txt decl)
              , noteMark
              ]
          w = padRight Max line
        in
          if current then withDefAttr floatSelAttr w else w
    rows = map entryWidget (trayEntries st)
  in
    hLimit trayWidth $
      padRight (Pad 1) $
        viewport TrayVP Vertical (vBox rows)

-- Long paths compress like s/R/Quote.hs: every directory shrinks to its
-- first character, the file name stays whole, and if even that is too
-- wide the front is elided so the end — the part that tells files apart —
-- always survives.
shortPath :: Int -> Text -> Text
shortPath w path =
  let
    parts = T.splitOn "/" path
    squeezed = case reverse parts of
      (file : dirs) ->
        T.intercalate "/" (reverse (file : map (T.take 1) dirs))
      [] -> path
  in
    if T.length path <= w
      then path
      else
        if T.length squeezed <= w
          then squeezed
          else "\8230" <> T.takeEnd (w - 1) squeezed

windowRows :: Int
windowRows = 300

-- Where each row's reviewer note (and any agent reply to it) should
-- render: annotations apply to every line of their target, so
-- consecutive rows carrying the same pair show it once, under the last
-- of them — a footnote to the block, not an echo on every line. One
-- entry per row, [] where nothing renders. Each rendered line knows its
-- post's author and whether it opens that post, so the conversation
-- reads in order with a sense of who said what.
shownNoteLines :: St -> [RowR] -> [[NoteLine]]
shownNoteLines st rows =
  let
    annOf r = do
      k <- rKey r
      Map.lookup k (stThreads st)
    anns = map annOf rows
    nexts = drop 1 anns ++ [Nothing]
    postLines post =
      case T.lines (postBody post) of
        [] -> []
        (first : rest) ->
          NoteLine (postAuthor post) True first
            : map (NoteLine (postAuthor post) False) rest
    pick (ma, mnext) = case ma of
      Just posts
        | mnext /= Just posts -> concatMap postLines posts
      _ -> []
  in
    map pick (zip anns nexts)

data NoteLine = NoteLine
  { nlAuthor :: Text
  , nlOpens :: Bool
  , nlText :: Text
  }
  deriving (Eq)

renderFile :: St -> FileView -> Widget Name
renderFile st fv =
  let
    rows = fvRows fv
    ed = fvEd fv
    curH = clampH (fvRuns fv) (stCurH st)
    (curRow, curCol) = cursorRC ed
    lineSel = selectionRows ed
    charSel = charSelection ed
    -- the column span of the char selection on one display row: start
    -- col, and Nothing for "through end of line"
    rowSpan r = do
      ((lr, lc), (hr, hc)) <- charSel
      nr <- rNewRow r
      if nr < lr || nr > hr
        then Nothing
        else
          Just
            ( if nr == lr then lc else 0
            , if nr == hr then Just hc else Nothing
            )
    cursorOut =
      fromMaybe 0 (findIndex (\r -> rNewRow r == Just curRow) rows)
    lo = max 0 (cursorOut - windowRows)
    hi = min (length rows) (cursorOut + windowRows)
    -- the buffer row each removed line now sits before, so a line-wise
    -- selection can honestly include the deletions it would verdict
    points =
      drop
        1
        ( scanr
            (\row acc -> fromMaybe acc (rNewRow row))
            (length (bufferLines ed))
            rows
        )
    noteRowsList = shownNoteLines st rows
    rowsWithPoints = zip3 rows points noteRowsList
    numW = length (show (max 1 (length (bufferLines ed))))
    -- padding rows carry the border too, so the tray/body divider runs
    -- unbroken through regions the render window skips
    pad k =
      if k <= 0
        then txt T.empty
        else raw (V.charFill (stBorder st) '\9614' (1 :: Int) k)
    lineWidget (r, point, shownNotes) =
      let
        isCurRun = case rRunIx r of
          Just runIx -> runIx == curH
          Nothing -> False
        verdict = rKey r >>= (`Map.lookup` stVerdicts st)
        isCursor = rNewRow r == Just curRow
        inSel = case lineSel of
          Nothing -> False
          Just (a, b) -> case rNewRow r of
            Just nr -> nr >= a && nr <= b
            -- a removed line counts when its resting point does,
            -- matching how verdicts scope it
            Nothing -> point >= a && point <= b
        -- the first gutter column doubles as the border between tray
        -- and body: drawn on every row, in the highlight color across
        -- the current hunk's rows; the second column carries the
        -- line's status, so neither ever hides the other
        curBar =
          if isCurRun
            then withAttr curRunAttr (str "\9612") -- wide: ▌
            else withAttr borderAttr (str "\9614") -- thin: ▎
            -- the cursor paints this cell yellow and renders the verdict
            -- glyph in black over it — shape carries the meaning where the
            -- colors would fight the background (• pending, ✓ reviewed,
            -- ! flagged, ? questioned)
        statusPick = case verdict of
          Just Flagged -> Just ('!', flagMarkAttr)
          Just Questioned -> Just ('?', questionMarkAttr)
          Just Reviewed -> Just ('\10003', doneMarkAttr)
          Nothing ->
            case rKey r of
              Just _ -> Just ('\8226', pendingMarkAttr)
              Nothing -> Nothing
        statusCol = case (isCursor, statusPick) of
          (True, Just (c, a)) ->
            withDefAttr cursorMarkAttr (withAttr a (str [c]))
          (True, Nothing) -> withAttr cursorMarkAttr (str " ")
          (False, Just (c, a)) -> withAttr a (str [c])
          (False, Nothing) -> str " "
        numCol = case rNewRow r of
          Just nr ->
            let
              shown = show (nr + 1)
              padded = replicate (numW - length shown) ' ' ++ shown
              numAttr =
                if isCursor
                  then lineNrCurAttr
                  else
                    if verdict == Just Reviewed
                      then lineNrDoneAttr
                      else lineNrAttr
            in
              withAttr numAttr (str (padded ++ " "))
          Nothing -> withAttr lineNrAttr (str (replicate numW ' ' ++ " "))
        markW = withAttr (markerAttrFor (rMark r)) (str [rMark r, ' '])
        -- content shifted hOff columns left, for reading long lines:
        -- the view follows the cursor horizontally while the gutter
        -- stays pinned
        contentFor hOff =
          if rMark r == '-'
            then withAttr removedTextAttr (txt (padT (T.drop hOff (rText r))))
            else
              let
                sliced = snd (splitAtCol hOff (effRuns r))
              in
                case rowSpan r of
                  Just (s, mEnd)
                    | fromMaybe maxBound mEnd >= hOff ->
                        selTokensWidget
                          sliced
                          (max 0 (s - hOff))
                          (fmap (subtract hOff) mEnd)
                  _ ->
                    case nonEmptySegs sliced of
                      [] -> txt " "
                      filled ->
                        hBox (map (\(a, t) -> withAttr a (txt t)) filled)
        -- reviewed lines keep their full syntax colors; the gutter dot
        -- and the tray checkmark already say "answered for", and dimming
        -- whole regions made the buffer feel murky
        buildLine hOff =
          let
            body1 = markW <+> contentFor hOff
            lineW0 = curBar <+> statusCol <+> numCol <+> body1
            lineW = if inSel then withDefAttr selAttr lineW0 else lineW0
          in
            if isCursor
              then
                showCursor
                  TheCursor
                  (Location (numW + 5 + curCol - hOff, 0))
                  lineW
              else lineW
        lineScrolled =
          Widget Greedy Fixed $ do
            ctx <- getContext
            render (buildLine (hScrollFor ctx numW curCol))
        -- the reviewer's note renders inline as virtual rows under
        -- the line it annotates — visible where the feedback lives
        noteRowW nl =
          let
            fromReviewer = nlAuthor nl == "reviewer"
            glyph
              | not (nlOpens nl) = "  "
              | fromReviewer = "\9492 \8801 "
              | otherwise = "\8618 "
            label =
              if nlOpens nl && not fromReviewer
                then nlAuthor nl <> " · "
                else ""
            noteAttr = if fromReviewer then noteInlineAttr else replyInlineAttr
          in
            curBar
              <+> withAttr
                noteAttr
                (txt (T.replicate (numW + 4) " " <> glyph <> label <> padT (nlText nl)))
      in
        case shownNotes of
          [] -> lineScrolled
          notes -> vBox (lineScrolled : map noteRowW notes)
    -- keep a margin of context visible around the cursor, so the view
    -- scrolls before the cursor reaches the top or bottom edge; virtual
    -- note rows above the cursor shift where it lands in the rendering
    notesAbove =
      sum (map length (take (cursorOut - lo) (drop lo noteRowsList)))
    cursorRender = cursorOut + notesAbove
    scrollOff = cfgScrolloff (stConfig st)
    marginTop = max 0 (cursorRender - scrollOff)
    marginHeight = cursorRender + scrollOff - marginTop + 1
  in
    visibleRegion (Location (0, marginTop)) (1, marginHeight) $
      vBox
        [ pad lo
        , vBox (map lineWidget (take (hi - lo) (drop lo rowsWithPoints)))
        , -- always leave border tail below the file so the divider spans
          -- the pane even when the content is shorter than the screen
          pad (max (length rows - hi) 60)
        ]

-- How far the view is shifted right: zero until the cursor closes in on
-- the pane's right edge, then just enough to keep a small margin ahead
-- of it. Depends only on the cursor column and the pane width, so every
-- line shifts in step.
hScrollFor :: Context Name -> Int -> Int -> Int
hScrollFor ctx numW curCol =
  let
    contentWidth = max 1 (availWidth ctx - (numW + 5))
    margin = 8
  in
    max 0 (curCol - (contentWidth - margin))

nonEmptySegs :: [Seg] -> [Seg]
nonEmptySegs = filter (not . T.null . snd)

-- Token runs with the char selection's columns highlighted.
effRuns :: RowR -> [Seg]
effRuns r =
  case nonEmptySegs (rToks r) of
    [] -> [(attrName "plain", rText r)]
    filled -> filled

selTokensWidget :: [Seg] -> Int -> Maybe Int -> Widget Name
selTokensWidget segs s mEnd =
  let
    (before, rest) = splitAtCol s segs
    (mid, after) = case mEnd of
      Nothing -> (rest, [])
      Just e -> splitAtCol (e + 1 - s) rest
    seg xs = hBox (map (\(a, t) -> withAttr a (txt t)) (nonEmptySegs xs))
    midW = case nonEmptySegs mid of
      [] -> txt " " -- keep the selection visible across empty stretches
      ms -> seg ms
  in
    seg before <+> withDefAttr selAttr midW <+> seg after

splitAtCol :: Int -> [Seg] -> ([Seg], [Seg])
splitAtCol =
  let
    go k xs =
      case xs of
        [] -> ([], [])
        ((a, t) : rest)
          | k <= 0 -> ([], xs)
          | T.length t <= k ->
              let
                (taken, left) = go (k - T.length t) rest
              in
                ((a, t) : taken, left)
          | otherwise -> ([(a, T.take k t)], (a, T.drop k t) : rest)
  in
    go

markerAttrFor :: Char -> AttrName
markerAttrFor mark =
  case mark of
    '+' -> addedMarkAttr
    '-' -> removedMarkAttr
    _ -> ctxMarkAttr

statusBar :: St -> Widget Name
statusBar st =
  let
    middle = case stPrompt st of
      Just (SearchPrompt input) -> txt ("/" <> input <> "\9612")
      Just (CmdPrompt input) -> txt (":" <> input <> "\9612")
      Nothing -> withAttr msgAttr (txt (padT (stMsg st)))
    chip dotAttr label =
      hBox
        [ txt " "
        , withAttr dotAttr (txt "\9679")
        , withAttr statusAttr (txt (" " <> label <> " "))
        ]
    modeChip =
      if stSpace st
        then chip modeDotNorAttr "spc\8230"
        else case stEmacsPrefix st of
          Just pfx -> chip modeDotNorAttr ("C-" <> T.singleton pfx <> "\8230")
          Nothing ->
            if cfgKeys (stConfig st) == KeysEmacs
              then chip modeDotNorAttr "emx"
              else case currentFileMay st of
                Nothing -> chip modeDotNorAttr "nor"
                Just fv -> case edMode (fvEd fv) of
                  Insert -> chip modeDotInsAttr "ins"
                  Normal -> case pendingLabel (fvEd fv) of
                    Just p -> chip modeDotNorAttr (p <> "\8230")
                    Nothing -> chip modeDotNorAttr "nor"
    -- the startup warning scrolls away with the next message; this keeps
    -- "the watcher is a second behind" on screen for the whole session
    pollChip =
      if stPolling st
        then withAttr watchPollAttr (txt " \9888 poll ")
        else txt ""
    declSeg = case currentFileMay st of
      Just fv ->
        case declAt (bufferLines (fvEd fv)) (fst (cursorRC (fvEd fv))) of
          Just d ->
            withAttr trayDeclAttr (txt (" " <> T.take 32 (declName d) <> " \183"))
          Nothing -> txt ""
      Nothing -> txt ""
    hint = case currentFileMay st of
      Nothing -> ": command · \9251? keys · :q quit "
      Just fv ->
        (if fvStale fv then "hunks refresh on Esc · " else "")
          <> "\9166 accept · ! flag · ? question · \9251 menu · \9251? keys "
    meterSeg =
      let
        pairs = allRunPairs st
        total = length pairs
        pending = length (filter (pairPending st) pairs)
      in
        if total == 0
          then txt ""
          else txt "  " <+> meterW (total - pending) total
  in
    withDefAttr statusAttr $
      modeChip
        <+> pollChip
        <+> declSeg
        <+> meterSeg
        <+> txt "  "
        <+> middle
        <+> padLeft Max (withAttr hintAttr (txt hint))

padT :: Text -> Text
padT t = if T.null t then " " else t

-- Progress as quiet blocks: one cell per hunk up to ten, then scaled.
-- The filled run only completes when the work does.
meterW :: Int -> Int -> Widget Name
meterW done total =
  let
    cells = min 10 (max 1 total)
    filled =
      if done >= total
        then cells
        else min (cells - 1) (div (done * cells) (max 1 total))
  in
    hBox
      [ withAttr meterOnAttr (txt (T.replicate filled "\9648"))
      , withAttr meterOffAttr (txt (T.replicate (cells - filled) "\9649"))
      , withAttr hintAttr (txt (" " <> tshow done <> "/" <> tshow total))
      ]

meterTextWidth :: Int -> Int -> Int
meterTextWidth done total =
  min 10 (max 1 total)
    + 2
    + T.length (tshow done)
    + T.length (tshow total)

-- Events --------------------------------------------------------------------------

handleEvent :: BrickEvent Name REvent -> EventM Name St ()
handleEvent brickEvent =
  case brickEvent of
    AppEvent (FileChangedOnDisk rel) -> reloadFromDisk rel
    AppEvent GitStateChanged -> refreshGit
    AppEvent StoreChanged -> do
      st <- get
      threads <- liftIO (loadThreads (stConn st) (repoText st))
      modify (\s -> s {stThreads = threads})
    AppEvent Tick -> scrollHunkIntoView
    VtyEvent ev -> do
      st <- get
      -- messages are transient: any keystroke clears the old one, and a
      -- handler that has something new to say sets it afresh
      modify (\s -> s {stMsg = ""})
      case (stNotePopup st, stPicker st, stPrompt st) of
        _
          | stHelp st -> modify (\s -> s {stHelp = False})
        _
          | stInfoPopup st /= Nothing ->
              modify (\s -> s {stInfoPopup = Nothing})
        (Just np, _, _) -> handleNotePopupKey ev np
        _
          | Just tv <- stTree st -> handleTreeKey ev tv
        (_, Just pk, _) -> handlePickerKey ev pk
        (_, _, Just p) -> handlePromptKey ev p
        _ ->
          if stSpace st
            then handleSpaceKey ev
            else case stEmacsPrefix st of
              Just pfx -> do
                modify (\s -> s {stEmacsPrefix = Nothing})
                if null (stFiles st)
                  then handleIdlePrefixed pfx ev
                  else handleEmacsPrefixed pfx ev
              Nothing ->
                if null (stFiles st)
                  then handleIdleKey ev
                  else handleModalKey ev
    _ -> pure ()

-- The helix-style space menu: Space, then one key; anything unlisted
-- just cancels. Dispatch reads spaceMenuEntries, which is also what the
-- infobox renders, so the menu and the bindings cannot disagree.
handleSpaceKey :: V.Event -> EventM Name St ()
handleSpaceKey ev = do
  modify (\s -> s {stSpace = False})
  case ev of
    V.EvKey (V.KChar c) [] ->
      case find (\(key, _, _) -> key == T.singleton c) spaceMenuEntries of
        Just (_, _, action) -> action
        Nothing -> pure ()
    _ -> pure ()

togglePendingOnly :: EventM Name St ()
togglePendingOnly = do
  modify $ \s ->
    let
      now = not (stOnlyPending s)
    in
      s
        { stOnlyPending = now
        , stMsg =
            if now
              then "pending hunks only"
              else "showing all hunks"
        }
  scrollTrayToCurrent

-- With nothing on screen the only meaningful keys are quitting and the
-- command line (to switch what the session is looking at).
handleIdleKey :: V.Event -> EventM Name St ()
handleIdleKey ev =
  case ev of
    V.EvKey (V.KChar ':') [] -> openCmdPrompt
    V.EvKey (V.KChar 'p') [V.MCtrl] -> openPicker
    V.EvKey (V.KChar ' ') [] -> modify (\s -> s {stSpace = True})
    V.EvKey (V.KChar 'x') [V.MMeta] -> openCmdPrompt
    V.EvKey (V.KChar 'x') [V.MCtrl] ->
      modify (\s -> s {stEmacsPrefix = Just 'x'})
    _ -> pure ()

-- With no files on screen, only the session chords mean anything.
handleIdlePrefixed :: Char -> V.Event -> EventM Name St ()
handleIdlePrefixed pfx ev =
  case (pfx, ev) of
    ('x', V.EvKey (V.KChar 'c') [V.MCtrl]) -> halt
    _ -> pure ()

handlePromptKey :: V.Event -> Prompt -> EventM Name St ()
handlePromptKey ev prompt =
  let
    input = case prompt of
      SearchPrompt t -> t
      CmdPrompt t -> t
    rewrap t = case prompt of
      SearchPrompt _ -> SearchPrompt t
      CmdPrompt _ -> CmdPrompt t
    isCmd = case prompt of
      CmdPrompt _ -> True
      SearchPrompt _ -> False
  in
    case ev of
      V.EvKey V.KEsc [] ->
        modify (\s -> s {stPrompt = Nothing, stPromptCycle = Nothing})
      V.EvKey V.KEnter [] -> do
        modify (\s -> s {stPrompt = Nothing, stPromptCycle = Nothing})
        case prompt of
          SearchPrompt t -> do
            modify (\s -> s {stLastSearch = t, stGrepNav = Nothing})
            unless (T.null t) (searchStep 1 t)
          CmdPrompt t -> runCommand t
      V.EvKey (V.KChar '\t') [] | isCmd -> promptComplete 1 input
      V.EvKey V.KBackTab [] | isCmd -> promptComplete (-1) input
      V.EvKey V.KBS [] ->
        modify
          ( \s ->
              s
                { stPrompt = Just (rewrap (T.dropEnd 1 input))
                , stPromptCycle = Nothing
                }
          )
      V.EvKey (V.KChar c) [] ->
        modify
          ( \s ->
              s
                { stPrompt = Just (rewrap (T.snoc input c))
                , stPromptCycle = Nothing
                }
          )
      _ -> pure ()

handleModalKey :: V.Event -> EventM Name St ()
handleModalKey ev = do
  st <- get
  let
    i = stCurF st
    fv = stFiles st !! i
    -- the kill slot is session state: whatever was yanked anywhere is
    -- pasteable here
    ed = (fvEd fv) {edKill = stKill st}
  if cfgKeys (stConfig st) == KeysEmacs
    then handleEmacsKey i ed ev
    else handleHelixKey i fv ed ev

-- Emacs keymap: editing chords go to Remax.Emacs; M-x and C-s open the
-- prompts; C-c prefixes the review verbs (same letters as the helix
-- keys), C-x the session chords (C-x C-c quits, C-x C-s saves).
handleEmacsKey :: Int -> EdState -> V.Event -> EventM Name St ()
handleEmacsKey i ed ev = do
  st <- get
  case stEmacsPrefix st of
    Just pfx -> do
      modify (\s -> s {stEmacsPrefix = Nothing})
      handleEmacsPrefixed pfx ev
    Nothing ->
      case ev of
        V.EvKey (V.KChar 'x') [V.MMeta] -> openCmdPrompt
        V.EvKey (V.KChar 's') [V.MCtrl] ->
          modify (\s -> s {stPrompt = Just (SearchPrompt "")})
        V.EvKey (V.KChar 'c') [V.MCtrl] ->
          modify (\s -> s {stEmacsPrefix = Just 'c'})
        V.EvKey (V.KChar 'x') [V.MCtrl] ->
          modify (\s -> s {stEmacsPrefix = Just 'x'})
        _ ->
          case handleEmacs ev ed of
            Just edNext ->
              if bufferLines edNext /= bufferLines ed
                then do
                  rebuildWith i edNext
                  applyToDisk i
                else setEd i edNext
            Nothing -> pure ()

handleEmacsPrefixed :: Char -> V.Event -> EventM Name St ()
handleEmacsPrefixed pfx ev = do
  st <- get
  case pfx of
    'x' ->
      case ev of
        V.EvKey (V.KChar 'c') [V.MCtrl] -> halt
        V.EvKey (V.KChar 's') [V.MCtrl] -> applyToDisk (stCurF st)
        _ -> pure ()
    _ ->
      case ev of
        V.EvKey V.KEnter [] -> acceptSelection
        V.EvKey (V.KChar '!') [] -> markThenNote Flagged
        V.EvKey (V.KChar '?') [] -> markThenNote Questioned
        V.EvKey (V.KChar 'r') [] -> clearVerdictBySelection
        V.EvKey (V.KChar 'R') [] -> clearVerdictBySelection
        V.EvKey (V.KChar ']') [] -> switchRun 1
        V.EvKey (V.KChar '[') [] -> switchRun (-1)
        V.EvKey (V.KChar 'x') [] -> selectCurrentHunk
        V.EvKey (V.KChar 'p') [] -> togglePendingOnly
        V.EvKey (V.KChar 'u') [] -> unnoteCmd
        V.EvKey (V.KChar 'h') [] -> setCurrentHunkHere
        V.EvKey (V.KChar 'v') [] -> revertTarget
        V.EvKey (V.KChar 'a') [] -> gotoAltFile
        V.EvKey (V.KChar '/') [] -> openGrepPicker ""
        V.EvKey (V.KChar 'c') [] -> showCommitPopup
        V.EvKey (V.KChar '\'') [] -> jumpToAnchor
        V.EvKey (V.KChar '*') [] -> searchWordUnderCursor
        V.EvKey (V.KChar 'n') [] -> searchNext 1
        V.EvKey (V.KChar 'N') [] -> searchNext (-1)
        V.EvKey (V.KChar 'f') [] -> openPicker
        V.EvKey (V.KChar 'b') [] -> openBufferPicker
        V.EvKey (V.KChar 'e') [] -> editCurrent
        V.EvKey (V.KChar ')') [] -> switchFile 1
        V.EvKey (V.KChar '(') [] -> switchFile (-1)
        V.EvKey (V.KChar '\t') [] ->
          modify (\s -> s {stTray = not (stTray s)})
        _ -> pure ()

handleHelixKey :: Int -> FileView -> EdState -> V.Event -> EventM Name St ()
handleHelixKey i fv ed ev = do
  st <- get
  case edMode ed of
    Insert ->
      case handleInsert ev ed of
        InsertHandled edNext -> do
          let
            oldLs = bufferLines ed
            newLs = bufferLines edNext
          if newLs == oldLs
            then setEd i edNext
            else
              if length newLs == length oldLs
                then
                  modify
                    ( updateFile
                        i
                        (patchLine (fst (cursorRC edNext)) (currentLineOf edNext) . withEd edNext)
                    )
                else rebuildWith i edNext
        InsertExit edNext -> do
          rebuildWith i edNext
          applyToDisk i
        InsertUnhandled -> pure ()
    -- a pending g/f/t prefix owns the next key outright; review keys
    -- must not steal its target character
    -- ga is review-level (goto alternate file), but the g prefix lives
    -- in the modal layer; catch it here and let the modal cancel its
    -- pending state
    Normal
      | pendingLabel ed == Just "g"
      , V.EvKey (V.KChar 'a') [] <- ev -> do
          deferToModal i ed ev
          gotoAltFile
    Normal | hasPending ed -> deferToModal i ed ev
    Normal ->
      case ev of
        V.EvKey (V.KChar '\t') [] ->
          modify (\s -> s {stTray = not (stTray s)})
        V.EvKey V.KEnter [] -> acceptSelection
        V.EvKey (V.KChar '!') [] -> markThenNote Flagged
        V.EvKey (V.KChar '?') [] -> markThenNote Questioned
        V.EvKey (V.KChar 'R') [] -> clearVerdictBySelection
        V.EvKey (V.KChar ']') [] -> switchRun 1
        V.EvKey (V.KChar '[') [] -> switchRun (-1)
        V.EvKey (V.KChar '/') [] ->
          modify (\s -> s {stPrompt = Just (SearchPrompt "")})
        V.EvKey (V.KChar ':') [] -> openCmdPrompt
        V.EvKey (V.KChar '*') [] -> searchWordUnderCursor
        V.EvKey (V.KChar 'n') [] -> searchNext 1
        V.EvKey (V.KChar 'N') [] -> searchNext (-1)
        V.EvKey (V.KChar '\'') [] -> jumpToAnchor
        V.EvKey (V.KChar 'X') [] -> selectCurrentHunk
        -- paste with an empty kill slot would be a silent no-op; the
        -- message also catches fingers trained on the old p toggle
        V.EvKey (V.KChar 'p') []
          | T.null (edKill ed) ->
              setMsg "kill slot is empty (y yanks) — the pending filter is Space p"
        V.EvKey (V.KChar 'P') []
          | T.null (edKill ed) ->
              setMsg "kill slot is empty (y yanks) — the pending filter is Space p"
        V.EvKey (V.KChar ' ') [] ->
          modify (\s -> s {stSpace = True})
        V.EvKey (V.KChar 'E') [] -> do
          let
            (row, _) = cursorRC ed
          launchEditor (stRoot st </> fvPath fv) (row + 1)
        V.EvKey (V.KChar ')') [] -> switchFile 1
        V.EvKey (V.KChar '(') [] -> switchFile (-1)
        V.EvKey (V.KChar 'p') [V.MCtrl] -> openPicker
        V.EvKey (V.KChar '{') [] ->
          vScrollBy (viewportScroll TrayVP) (-8)
        V.EvKey (V.KChar '}') [] ->
          vScrollBy (viewportScroll TrayVP) 8
        _ -> deferToModal i ed ev

deferToModal :: Int -> EdState -> V.Event -> EventM Name St ()
deferToModal i ed ev =
  case handleNormal ev ed of
    Just edNext ->
      if bufferLines edNext /= bufferLines ed
        then do
          rebuildWith i edNext
          when (edMode edNext == Normal) (applyToDisk i)
        else setEd i edNext
    Nothing -> pure ()

-- Runs after every event (see the app wiring): if navigation just landed
-- on a file still carrying plain tokens, give it its real colors now.
ensureCurrentHighlighted :: EventM Name St ()
ensureCurrentHighlighted = do
  st <- get
  let
    i = stCurF st
  case safeIdx (stFiles st) i of
    Nothing -> pure ()
    Just fv ->
      unless (fvHighlighted fv) $ do
        (colored, _) <-
          liftIO
            (rebuildFV (cfgSplit (stConfig st)) (repoText st) fv {fvHighlighted = True})
        modify (updateFile i (const colored))

currentLineOf :: EdState -> Text
currentLineOf ed =
  fromMaybe "" (safeIdx (bufferLines ed) (fst (cursorRC ed)))

withEd :: EdState -> FileView -> FileView
withEd ed fv = fv {fvEd = ed}

-- Which run in the rebuilt list is "the same hunk" as before: the one
-- sharing the most content keys, ties and misses settled by anchor
-- proximity. Content keys make this survive hunks appearing, vanishing,
-- and reshaping above the current one.
matchRun :: Maybe Run -> [Run] -> Int
matchRun mOld runs =
  case (mOld, runs) of
    (_, []) -> 0
    (Nothing, _) -> 0
    (Just old, _) ->
      let
        keySet = Set.fromList (runKeys old)
        score (ix, r) =
          ( length (filter (`Set.member` keySet) (runKeys r))
          , negate (abs (runAnchor r - runAnchor old))
          , ix
          )
        scored = map score (zip [0 :: Int ..] runs)
        (bestScore, _, bestIx) =
          maximumBy (comparing (\(sc, prox, _) -> (sc, prox))) scored
        (_, _, nearIx) =
          maximumBy (comparing (\(_, prox, _) -> prox)) scored
      in
        if bestScore > 0 then bestIx else nearIx

currentRunOf :: St -> Int -> Maybe Run
currentRunOf st i = do
  fv <- safeIdx (stFiles st) i
  let
    rs = fvRuns fv
  safeIdx rs (clampH rs (stCurH st))

rebuildWith :: Int -> EdState -> EventM Name St ()
rebuildWith i ed = do
  modify (\s -> s {stKill = edKill ed})
  st <- get
  let
    fv = (stFiles st !! i) {fvEd = ed}
    oldCur =
      if i == stCurF st then currentRunOf st i else Nothing
  (rebuilt, degraded) <- liftIO (rebuildFV (cfgSplit (stConfig st)) (repoText st) fv)
  modify (updateFile i (const rebuilt))
  when (i == stCurF st) $
    modify (\s -> s {stCurH = matchRun oldCur (fvRuns rebuilt)})
  when degraded $
    modify (\s -> s {stMsg = "diff too large — file shown as one hunk"})

setEd :: Int -> EdState -> EventM Name St ()
setEd i ed =
  modify (\s -> updateFile i (withEd ed) s {stKill = edKill ed})

updateFile :: Int -> (FileView -> FileView) -> St -> St
updateFile i f st =
  let
    patch (j, fv) = if j == i then f fv else fv
  in
    st {stFiles = map patch (zip [0 ..] (stFiles st))}

park :: Int -> Int -> St -> St
park f h st =
  case safeIdx (stFiles st) f of
    Nothing -> st
    Just fv
      | f /= stCurF st ->
          case currentFileMay st of
            Just prev ->
              parkGo f h (st {stAltFile = Just (fvPath prev, stCurH st)}) fv
            Nothing -> parkGo f h st fv
    Just fv -> parkGo f h st fv

parkGo :: Int -> Int -> St -> FileView -> St
parkGo f h st fv =
  let
    rs = fvRuns fv
    clamped = clampH rs h
    parked = st {stCurF = f, stCurH = clamped, stMsg = " "}
    -- land on the first line still needing attention, not merely
    -- the top of the hunk
    target r =
      fromMaybe (runAnchor r) (firstUnacceptedRow st fv clamped)
    moveTo r v =
      v
        { fvEd =
            (fvEd v)
              { edZipper = Z.moveCursor (target r, 0) (edZipper (fvEd v))
              , edSelAnchor = Nothing
              }
        }
  in
    case safeIdx rs clamped of
      Nothing -> parked
      Just r -> updateFile f (moveTo r) parked

-- The buffer row of the first line in a run whose verdict is not
-- Reviewed (removed lines borrow the row they rest before).
firstUnacceptedRow :: St -> FileView -> Int -> Maybe Int
firstUnacceptedRow st fv h =
  let
    rows = fvRows fv
    bufferLen = length (bufferLines (fvEd fv))
    points =
      drop 1 (scanr (\row acc -> fromMaybe acc (rNewRow row)) bufferLen rows)
    unaccepted (row, point) = do
      k <- rKey row
      if rRunIx row == Just h && Map.lookup k (stVerdicts st) /= Just Reviewed
        then Just (fromMaybe point (rNewRow row))
        else Nothing
  in
    case mapMaybe unaccepted (zip rows points) of
      (r : _) -> Just r
      [] -> Nothing

-- park, then ask the viewport to frame the whole hunk.
parkM :: Int -> Int -> EventM Name St ()
parkM f h = do
  modify (park f h)
  scrollHunkIntoView

-- Frame the current hunk: centered when it fits, top-aligned with a
-- small margin when it is taller than the screen, so as much of it as
-- possible is on screen rather than just its anchor line at the edge.
scrollHunkIntoView :: EventM Name St ()
scrollHunkIntoView = do
  st <- get
  case currentFileMay st of
    Nothing -> pure ()
    Just fv -> scrollHunkIntoViewOf fv

scrollHunkIntoViewOf :: FileView -> EventM Name St ()
scrollHunkIntoViewOf fv = do
  st <- get
  mvp <- lookupViewport BodyVP
  let
    rs = fvRuns fv
    h = clampH rs (stCurH st)
    inCurrentRun (_, row) = rRunIx row == Just h
    runRows = map fst (filter inCurrentRun (zip [0 :: Int ..] (fvRows fv)))
  case (mvp, runRows) of
    (Just vp, (start : _)) -> do
      let
        len = length runRows
        vpH = snd (_vpSize vp)
        -- best-effort correction for virtual note rows rendered above
        -- the hunk (exact when the render window covers them)
        renderStart =
          start
            + sum (map length (take start (shownNoteLines st (fvRows fv))))
        top =
          if len + 4 >= vpH
            then max 0 (renderStart - 2)
            else max 0 (renderStart - ((vpH - len) `div` 2))
      setTop (viewportScroll BodyVP) top
    _ -> pure ()
  -- follow in the tray too — an explicit scroll on hunk jumps only, so
  -- manual tray scrolling is never fought over
  scrollTrayToCurrent

-- Center the tray on the current hunk's entry. Also needed whenever the
-- entry list reshapes under the viewport (the pending filter toggling),
-- or the scroll position strands past the shrunken content.
scrollTrayToCurrent :: EventM Name St ()
scrollTrayToCurrent = do
  st <- get
  mtvp <- lookupViewport TrayVP
  case (mtvp, currentFileMay st) of
    (Just tvp, Just fv) -> do
      let
        h = clampH (fvRuns fv) (stCurH st)
        isCur entry = case entry of
          TrayHunk f hh -> f == stCurF st && hh == h
          _ -> False
        idx = fromMaybe 0 (findIndex isCur (trayEntries st))
      setTop (viewportScroll TrayVP) (max 0 (idx - (snd (_vpSize tvp) `div` 2)))
    _ -> pure ()

-- Walk hunks in (file, hunk) order, wrapping; with pending-only mode on,
-- hunks that already have verdicts are skipped entirely.
switchRun :: Int -> EventM Name St ()
switchRun d = do
  st <- get
  let
    eligible pair = not (stOnlyPending st) || pairPending st pair
    pairs = filter eligible (allRunPairs st)
    here =
      ( stCurF st
      , clampH (fvRuns (currentFile st)) (stCurH st)
      )
  case pairs of
    [] -> modify (\s -> s {stMsg = "no pending hunks left"})
    _ -> do
      let
        after = filter (> here) pairs
        before = filter (< here) pairs
        target =
          if d > 0
            then case after ++ pairs of
              (p : _) -> p
              [] -> here
            else case reverse before ++ reverse pairs of
              (p : _) -> p
              [] -> here
      parkAnchor (fst target) (snd target)

-- '*': prime the search with the selected text (single-line selections)
-- or the identifier under the cursor, then jump to its next occurrence.
searchWordUnderCursor :: EventM Name St ()
searchWordUnderCursor = do
  st <- get
  let
    ed = fvEd (currentFile st)
    ls = bufferLines ed
    lineAt r = fromMaybe "" (safeIdx ls r)
    -- only a deliberate v-extend selection overrides the cursor word;
    -- the incidental selections that w/e-style motions leave behind drag
    -- neighboring punctuation along and are not what * means
    selectionNeedle = do
      ((lr, lc), (hr, hc)) <- charSelection ed
      if not (edExtend ed) || (lr, lc) == (hr, hc) || lr /= hr
        then Nothing
        else
          let
            l = lineAt lr
          in
            Just (T.take (hc + 1 - lc) (T.drop lc l))
    isIdent ch = isAlphaNum ch || ch == '_' || ch == '\''
    wordNeedle =
      let
        (row, col) = cursorRC ed
        l = lineAt row
        -- on punctuation, scan forward to the next word on the line
        -- (vim's * does the same) rather than giving up
        effCol =
          if col < T.length l && isIdent (T.index l col)
            then Just col
            else fmap (col +) (T.findIndex isIdent (T.drop col l))
      in
        case effCol of
          Nothing -> Nothing
          Just c ->
            let
              start =
                T.length (T.dropWhileEnd isIdent (T.take c l))
              len =
                T.length (T.takeWhile isIdent (T.drop start l))
            in
              Just (T.take len (T.drop start l))
  case selectionNeedle <|> wordNeedle of
    Nothing -> modify (\s -> s {stMsg = "no word under cursor"})
    Just needle -> do
      modify (\s -> s {stLastSearch = needle, stGrepNav = Nothing})
      searchStep 1 needle

-- n/N: when the last search came from the grep picker, walk its hit
-- list across files without re-searching; otherwise repeat the in-file
-- search as always.
searchNext :: Int -> EventM Name St ()
searchNext dir = do
  st <- get
  case stGrepNav st of
    Just (q, hits, ix)
      | not (null hits) -> do
          let
            n = length hits
            ix2 = (((ix + dir) `mod` n) + n) `mod` n
          case safeIdx hits ix2 of
            Just (path, line) -> do
              modify (\s -> s {stGrepNav = Just (q, hits, ix2)})
              openGrepHit path line q
              setMsg ("grep hit " <> tshow (ix2 + 1) <> "/" <> tshow n)
            Nothing -> pure ()
    _ ->
      unless
        (T.null (stLastSearch st))
        (searchStep dir (stLastSearch st))

-- Literal search over the current file's buffer, helix-shaped: the match
-- becomes the selection, n/N walk matches, wrapping at either end.
searchStep :: Int -> Text -> EventM Name St ()
searchStep dir needle = do
  st <- get
  let
    i = stCurF st
    fv = stFiles st !! i
    ed = fvEd fv
    pos = cursorRC ed
    matchesInLine (r, l) =
      map (\(pre, _) -> (r, T.length pre)) (T.breakOnAll needle l)
    matches = concatMap matchesInLine (zip [0 ..] (bufferLines ed))
  case matches of
    [] -> modify (\s -> s {stMsg = "no match: " <> needle})
    _ -> do
      let
        after = filter (> pos) matches
        before = filter (< pos) matches
        (mr, mc) =
          if dir > 0
            then case after ++ matches of
              (m : _) -> m
              [] -> pos
            else case reverse before ++ reverse matches of
              (m : _) -> m
              [] -> pos
        moved =
          ed
            { edZipper = Z.moveCursor (mr, mc + T.length needle - 1) (edZipper ed)
            , edSelAnchor = Just (mr, mc)
            , edSelKind = CharSel
            }
      setEd i moved
      modify (\s -> s {stMsg = " "})

-- The buffer rows a selection covers, if one is active. A collapsed
-- char selection (just the cursor cell) does not count as "selected
-- lines" for verdicts.
activeSelectionRows :: EdState -> Maybe (Int, Int)
activeSelectionRows ed =
  case selectionRows ed of
    Just rowSpan -> Just rowSpan
    Nothing ->
      case charSelection ed of
        Just (loPos, hiPos)
          | loPos /= hiPos -> Just (fst loPos, fst hiPos)
        _ -> Nothing

-- Changed lines within a buffer-row span, as (key, marked line, 1-based
-- position) triples. A removed line has no buffer row of its own; it
-- counts when the row it now sits before falls inside the span, and
-- borrows that row's position.
selectedChanges :: FileView -> Int -> Int -> [(Text, Text, Int)]
selectedChanges fv lo hi =
  let
    rows = fvRows fv
    bufferLen = length (bufferLines (fvEd fv))
    points =
      drop 1 (scanr (\row acc -> fromMaybe acc (rNewRow row)) bufferLen rows)
    inSpan row point = case rNewRow row of
      Just nr -> nr >= lo && nr <= hi
      Nothing -> point >= lo && point <= hi
    pick (row, point) = do
      k <- rKey row
      if inSpan row point
        then Just (k, T.cons (rMark row) (rText row), fromMaybe point (rNewRow row) + 1)
        else Nothing
  in
    mapMaybe pick (zip rows points)

-- Every changed line of one run, same shape as selectedChanges.
runChanges :: FileView -> Int -> [(Text, Text, Int)]
runChanges fv h =
  let
    rows = fvRows fv
    bufferLen = length (bufferLines (fvEd fv))
    points =
      drop 1 (scanr (\row acc -> fromMaybe acc (rNewRow row)) bufferLen rows)
    pick (row, point) = do
      k <- rKey row
      if rRunIx row == Just h
        then Just (k, T.cons (rMark row) (rText row), fromMaybe point (rNewRow row) + 1)
        else Nothing
  in
    mapMaybe pick (zip rows points)

-- The current hunk as a line-wise selection — the review-unit motion.
-- Every verb then composes with it: X r reviews it, X d deletes it,
-- X > indents it.
-- Which hunk the cursor is standing in: the run of the cursor's display
-- row, or — for a delete-only hunk, which owns no buffer rows — the run
-- whose deletion rests at the cursor's line.
hunkUnderCursor :: St -> Maybe Int
hunkUnderCursor st = do
  fv <- currentFileMay st
  let
    (cur, _) = cursorRC (fvEd fv)
    rows = fvRows fv
    bufferLen = length (bufferLines (fvEd fv))
    points =
      drop 1 (scanr (\row acc -> fromMaybe acc (rNewRow row)) bufferLen rows)
    directHit row =
      if rNewRow row == Just cur then rRunIx row else Nothing
    restingHit (row, point) =
      if rNewRow row == Nothing && point == cur then rRunIx row else Nothing
  case mapMaybe directHit rows of
    (h : _) -> Just h
    [] -> case mapMaybe restingHit (zip rows points) of
      (h : _) -> Just h
      [] -> Nothing

-- The full message of the commit under review: the stopped commit's
-- own sha during a rebase (HEAD differs at a conflict stop), HEAD
-- otherwise.
showCommitPopup :: EventM Name St ()
showCommitPopup = do
  st <- get
  let
    ref = case stRebase st of
      Just rb
        | not (T.null (rebaseSha rb)) -> rebaseSha rb
      _ -> "HEAD"
  got <- liftIO (commitMessage (stRoot st) ref)
  case got of
    Left e -> setMsg e
    Right body -> modify (\s -> s {stInfoPopup = Just ("commit", body)})

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

-- Review-flow navigation parks AND drops the review anchor there, so '
-- can always come back; investigation moves (pickers, grep, ga, open)
-- park without touching it.
parkAnchor :: Int -> Int -> EventM Name St ()
parkAnchor f h = do
  parkM f h
  st <- get
  case currentFileMay st of
    Just fv ->
      modify $ \s ->
        s {stAnchor = Just (fvPath fv, clampH (fvRuns fv) (stCurH st))}
    Nothing -> pure ()

-- ': jump back to the hunk under review, wherever the investigation
-- wandered — across files included. With no anchor yet, it reframes
-- the current hunk as before.
jumpToAnchor :: EventM Name St ()
jumpToAnchor = do
  st <- get
  case stAnchor st of
    Just (path, h) ->
      case findIndex ((== path) . fvPath) (stFiles st) of
        Just f -> parkM f h
        Nothing -> setMsg ("anchored file left the session: " <> T.pack path)
    Nothing -> parkM (stCurF st) (stCurH st)

-- :N — park the cursor on line N of the current file, 1-based.
-- Z.moveCursor clamps, so :0 lands on the first line and a number past
-- the end lands on the last. This is an investigation move: the '
-- anchor and the hunk under review both stay where they are.
gotoLine :: Int -> EventM Name St ()
gotoLine n = do
  st <- get
  case currentFileMay st of
    Nothing -> setMsg "no file on screen"
    Just fv ->
      let
        ed = fvEd fv
        move = Z.moveCursor (n - 1, 0)
      in
        setEd (stCurF st) $
          if cfgKeys (stConfig st) == KeysEmacs
            -- emacs: point moves, the mark stays, so the region grows
            then ed {edZipper = move (edZipper ed), edInsertEntry = Nothing}
            else plainMove move ed

-- :revert / Space r — put the base text back for the hunk under the
-- cursor, or for every hunk a selection touches. The revert is an
-- ordinary buffer edit, so u undoes it and the diff re-derives at once.
revertTarget :: EventM Name St ()
revertTarget = do
  st <- get
  case currentFileMay st of
    Nothing -> setMsg "no file on screen"
    Just fv
      | fvStale fv -> setMsg "hunks are stale — press Esc first"
      | otherwise ->
          let
            ed = fvEd fv
            lineSpan = selectionRows ed
            charSpan =
              fmap (\((lr, _), (hr, _)) -> (lr, hr)) (charSelection ed)
            touchedBySpan (a, b) =
              let
                pick row = case (rRunIx row, rNewRow row) of
                  (Just ix, Just n)
                    | n >= a && n <= b -> Just ix
                  _ -> Nothing
              in
                Set.fromList (mapMaybe pick (fvRows fv))
            targetIxs = case lineSpan <|> charSpan of
              Just spanRows -> touchedBySpan spanRows
              Nothing ->
                maybe Set.empty Set.singleton (hunkUnderCursor st)
            inTarget row =
              maybe False (`Set.member` targetIxs) (rRunIx row)
            emit row =
              if inTarget row
                then case rMark row of
                  '-' -> [rText row]
                  '+' -> []
                  _ -> maybe [] (const [rText row]) (rNewRow row)
                else maybe [] (const [rText row]) (rNewRow row)
            newLines = concatMap emit (fvRows fv)
            keptLines = if null newLines then [""] else newLines
            parkRow =
              let
                plusRows = do
                  row <- fvRows fv
                  if inTarget row
                    then maybe [] pure (rNewRow row)
                    else []
              in
                case plusRows of
                  (r0 : _) -> min r0 (length keptLines - 1)
                  [] -> min (fst (cursorRC ed)) (length keptLines - 1)
            edNext =
              ed
                { edZipper =
                    Z.moveCursor
                      (max 0 parkRow, 0)
                      (Z.textZipper keptLines Nothing)
                , edUndo = take 100 (edZipper ed : edUndo ed)
                , edRedo = []
                , edTouched = True
                , edSelAnchor = Nothing
                , edMode = Normal
                }
          in
            if Set.null targetIxs
              then setMsg "nothing to revert here"
              else do
                rebuildWith (stCurF st) edNext
                applyToDisk (stCurF st)
                setMsg "\10226 reverted to base"

-- Space h: make the hunk the cursor is standing in the current one —
-- the current hunk being what a selection-less X, ! or ? acts on, and
-- what ' comes home to. The deliberate counterpart of ': that one goes
-- back to where you were reviewing, this one says review here instead.
setCurrentHunkHere :: EventM Name St ()
setCurrentHunkHere = do
  st <- get
  case hunkUnderCursor st of
    Nothing -> setMsg "cursor isn't in a hunk"
    Just h -> do
      modify (\s -> s {stCurH = h, stMsg = " "})
      moved <- get
      case currentFileMay moved of
        Just fv -> modify (\s -> s {stAnchor = Just (fvPath fv, h)})
        Nothing -> pure ()

selectCurrentHunk :: EventM Name St ()
selectCurrentHunk = do
  st <- get
  let
    fv = currentFile st
    inCurrentRun row =
      rRunIx row == Just (clampH (fvRuns fv) (stCurH st))
    hunkRows = mapMaybe rNewRow (filter inCurrentRun (fvRows fv))
    -- a delete-only hunk has no buffer rows of its own; its span is the
    -- anchor line the deletion sits at
    pickSpan =
      case (hunkRows, currentRunOf st (stCurF st)) of
        ((first : rest), _) ->
          case reverse (first : rest) of
            (lastRow : _) -> Just (first, lastRow)
            [] -> Nothing
        ([], Just r) -> Just (runAnchor r, runAnchor r)
        _ -> Nothing
  if hunkUnderCursor st /= Just (clampH (fvRuns fv) (stCurH st))
    then setMsg "cursor isn't in the current hunk — Space h makes this one current"
    else case pickSpan of
      Nothing -> pure ()
      Just (first, lastRow) -> do
        let
          ed = fvEd fv
          selected =
            ed
              { edZipper = Z.moveCursor (lastRow, 0) (edZipper ed)
              , edSelAnchor = Just (first, 0)
              , edSelKind = LineSel
              }
        setEd (stCurF st) selected

-- Accepting demands a visible object: Enter only acts on an active
-- selection, so what gets marked reviewed is always what was just seen
-- highlighted on screen. X puts the whole hunk one keystroke away.
acceptSelection :: EventM Name St ()
acceptSelection = do
  st <- get
  case activeSelectionRows (fvEd (currentFile st)) of
    Just _ -> void (applyVerdictLines Reviewed)
    Nothing ->
      modify
        (\s -> s {stMsg = "select what you're accepting — X selects the hunk"})

-- Selection-first, like every other verb: an active selection scopes the
-- verdict to those lines; no selection means the whole current hunk.
-- Reports whether anything was actually marked.
applyVerdict :: Verdict -> EventM Name St Bool
applyVerdict v = do
  st <- get
  case activeSelectionRows (fvEd (currentFile st)) of
    Just _ -> applyVerdictLines v
    Nothing -> applyVerdictHunk v

-- A flag or question wants its reason on record: the note popup opens
-- right away (config: note-on-flag = 0 turns this off), targeting the
-- lines just marked. Discarding keeps the mark, only the note is
-- skipped.
markThenNote :: Verdict -> EventM Name St ()
markThenNote v = do
  preSt <- get
  let
    noting = cfgNoteOnFlag (stConfig preSt)
  -- with the note popup coming, the post-mark jump waits until the
  -- popup closes — advancing first means writing the note while looking
  -- at the wrong hunk
  when noting (modify (\s -> s {stDeferAdvance = True}))
  marked <- applyVerdict v
  if marked && noting
    then do
      openNotePopup
      opened <- get
      case stNotePopup opened of
        Nothing -> runDeferredAdvance
        Just _ -> pure ()
    else modify (\s -> s {stDeferAdvance = False})

-- The advance a mark would have made, released when the note popup
-- closes.
runDeferredAdvance :: EventM Name St ()
runDeferredAdvance = do
  st <- get
  when (stDeferAdvance st) $ do
    modify (\s -> s {stDeferAdvance = False})
    advanceToPending

clearVerdictBySelection :: EventM Name St ()
clearVerdictBySelection = do
  st <- get
  case activeSelectionRows (fvEd (currentFile st)) of
    Just _ -> clearVerdict VerdictLines
    Nothing -> clearVerdict VerdictHunk

-- The commit a verdict describes, when the session knows it: at a rebase
-- stop, HEAD is the commit under review. Ambient reviews record no
-- commit — agents recover one from the line text with git blame.
verdictCommitContext :: EventM Name St (Maybe (Text, Text))
verdictCommitContext = do
  st <- get
  case stRebase st of
    Nothing -> pure Nothing
    Just rb
      | rebaseConflicted rb -> pure Nothing
      | otherwise -> liftIO (headCommit (stRoot st))

-- Mark the whole current hunk and move on to the next pending one.
applyVerdictHunk :: Verdict -> EventM Name St Bool
applyVerdictHunk v = do
  st <- get
  let
    fv = currentFile st
    rs = fvRuns fv
    path = T.pack (fvPath fv)
    -- with no selection, the verb needs the cursor to actually be in
    -- the hunk it would mark — acting on one out of view is how
    -- mistakes get recorded
    target = do
      here <- hunkUnderCursor st
      if here == clampH rs (stCurH st)
        then safeIdx rs (clampH rs (stCurH st))
        else Nothing
  case target of
    Nothing -> do
      setMsg "cursor isn't in the current hunk — Space h makes this one current"
      pure False
    Just r -> do
      let
        storeRow (k, l, n) = (k, path, l, n)
        rows = map storeRow (runChanges fv (clampH rs (stCurH st)))
      mCommit <- verdictCommitContext
      liftIO (recordVerdicts (stConn st) (repoText st) (stPassId st) v mCommit rows)
      when (v == Reviewed) (resolveNotesInMemory (runKeys r))
      modify $ \s ->
        s
          { stVerdicts =
              foldr (\k m -> Map.insert k v m) (stVerdicts s) (runKeys r)
          , stLastMarked = runKeys r
          , stCounts = Map.insertWith (+) v 1 (stCounts s)
          , stPunch =
              if v == Reviewed
                then stPunch s
                else (fvPath fv, runAnchor r + 1, v) : stPunch s
          }
      setEd (stCurF st) (collapseSelection (fvEd fv))
      deferred <- get
      unless (stDeferAdvance deferred) advanceToPending
      pure True

-- Mark just the selected lines (or the cursor's line); the hunk stays
-- pending until every one of its lines is settled, and the cursor stays
-- where it is.
applyVerdictLines :: Verdict -> EventM Name St Bool
applyVerdictLines v = do
  st <- get
  let
    fv = currentFile st
    path = T.pack (fvPath fv)
    (lo, hi) = verdictLineSpan (fvEd fv)
    picks = selectedChanges fv lo hi
    storeRow (k, l, n) = (k, path, l, n)
  if null picks
    then do
      modify (\s -> s {stMsg = "no changed lines here"})
      pure False
    else do
      mCommit <- verdictCommitContext
      liftIO
        ( recordVerdicts
            (stConn st)
            (repoText st)
            (stPassId st)
            v
            mCommit
            (map storeRow picks)
        )
      when (v == Reviewed) (resolveNotesInMemory (map (\(k, _, _) -> k) picks))
      modify $ \s ->
        s
          { stVerdicts =
              foldr (\(k, _, _) m -> Map.insert k v m) (stVerdicts s) picks
          , stLastMarked = map (\(k, _, _) -> k) picks
          , stPunch =
              if v == Reviewed
                then stPunch s
                else (fvPath fv, lo + 1, v) : stPunch s
          , stMsg = tshow (length picks) <> " line(s) marked"
          }
      -- auto-advance: a mark that finishes the hunk moves on to the next
      -- pending hunk, a partial mark steps to the line after the
      -- selection so line-by-line review flows
      settled <- get
      let
        fvSettled = currentFile settled
        rs = fvRuns fvSettled
        hunkDone =
          case safeIdx rs (clampH rs (stCurH settled)) of
            Just r -> not (runUnvisited (stVerdicts settled) r)
            Nothing -> True
      if hunkDone
        then do
          modify
            (\s -> s {stCounts = Map.insertWith (+) v 1 (stCounts s)})
          setEd (stCurF settled) (collapseSelection (fvEd fvSettled))
          unless (stDeferAdvance settled) advanceToPending
        else unless (stDeferAdvance settled) $ do
          let
            ed = fvEd fvSettled
            nextRow = min (hi + 1) (length (bufferLines ed) - 1)
            stepped =
              (collapseSelection ed)
                { edZipper = Z.moveCursor (nextRow, 0) (edZipper ed)
                }
          setEd (stCurF settled) stepped
      pure True

-- The line span a line-scoped verdict acts on: the selection when one is
-- active, the cursor's line otherwise.
verdictLineSpan :: EdState -> (Int, Int)
verdictLineSpan ed =
  case activeSelectionRows ed of
    Just rowSpan -> rowSpan
    Nothing ->
      let
        row = fst (cursorRC ed)
      in
        (row, row)

data VerdictScope = VerdictHunk | VerdictLines

collapseSelection :: EdState -> EdState
collapseSelection ed = ed {edSelAnchor = Nothing, edExtend = False}

-- Take the verdicts back off the hunk or the selected lines, so an
-- erroneous mark can be undone.
clearVerdict :: VerdictScope -> EventM Name St ()
clearVerdict scope = do
  st <- get
  let
    fv = currentFile st
    rs = fvRuns fv
    keys = case scope of
      VerdictLines ->
        let
          (lo, hi) = verdictLineSpan (fvEd fv)
        in
          map (\(k, _, _) -> k) (selectedChanges fv lo hi)
      VerdictHunk ->
        case safeIdx rs (clampH rs (stCurH st)) of
          Just r -> runKeys r
          Nothing -> []
    marked = filter (`Map.member` stVerdicts st) keys
  if null marked
    then modify (\s -> s {stMsg = "nothing marked here"})
    else do
      liftIO (deleteKeys (stConn st) (repoText st) marked)
      modify $ \s ->
        s
          { stVerdicts = foldr Map.delete (stVerdicts s) marked
          , stMsg = "unmarked " <> tshow (length marked) <> " line(s)"
          }
      setEd (stCurF st) (collapseSelection (fvEd fv))

advanceToPending :: EventM Name St ()
advanceToPending = do
  st <- get
  let
    here =
      ( stCurF st
      , clampH (fvRuns (currentFile st)) (stCurH st)
      )
    -- scan starts just after the current hunk and wraps all the way;
    -- hunks nothing has been said about come first, and the flagged/
    -- questioned ones only get circled back to once those run out
    wrapped =
      let
        (upToHere, afterHere) = break (> here) (allRunPairs st)
      in
        afterHere ++ upToHere
    unvisited = filter (pairUnvisited st) wrapped
    pending = filter (pairPending st) wrapped
  case unvisited ++ pending of
    ((f, h) : _) -> parkAnchor f h
    [] -> modify (\s -> s {stMsg = "All hunks have verdicts — :q to quit."})

applyToDisk :: Int -> EventM Name St ()
applyToDisk i = do
  st <- get
  let
    fv = stFiles st !! i
  when (edTouched (fvEd fv)) $ do
    got <-
      liftIO
        ( try (TIO.writeFile (stRoot st </> fvPath fv) (T.unlines (bufferLines (fvEd fv)))) ::
            IO (Either IOException ())
        )
    modify $ \s ->
      s
        { stMsg = case got of
            Right () -> "\9998 wrote " <> T.pack (fvPath fv)
            Left _ -> "! could not write " <> T.pack (fvPath fv)
        , stWritten = case got of
            Right () -> Set.insert (fvPath fv) (stWritten s)
            Left _ -> stWritten s
        }

-- A file changed on disk. When the buffer holds no unapplied edits it is
-- reloaded in place (cursor kept); mid-insert the local edit wins until
-- Esc, and the change is only announced.
reloadFromDisk :: FilePath -> EventM Name St ()
reloadFromDisk rel = do
  st <- get
  case find ((== rel) . fvPath . snd) (zip [0 ..] (stFiles st)) of
    -- an unknown file changing may mean the change set just grew
    Nothing -> refreshGit
    Just (i, fv) -> do
      got <- liftIO (try (TIO.readFile (stRoot st </> rel)) :: IO (Either IOException Text))
      case got of
        Left _ -> refreshGit
        Right diskTxt -> do
          let
            diskLs = T.lines diskTxt
          if diskLs == bufferLines (fvEd fv)
            then pure ()
            else
              if edMode (fvEd fv) == Insert
                then
                  modify
                    (\s -> s {stMsg = "\9888 " <> T.pack rel <> " changed on disk; Esc applies your edit over it"})
                else do
                  let
                    oldLs = bufferLines (fvEd fv)
                    (row, col) = cursorRC (fvEd fv)
                  -- remember which screen row the cursor sat on, so the
                  -- view doesn't jump when lines shift above it
                  mvp <- lookupViewport BodyVP
                  let
                    oldOut = cursorDisplayRow (fvRows fv) row
                    screenOff = case mvp of
                      Just vp -> max 0 (oldOut - _vpTop vp)
                      Nothing -> 0
                  newRow <- liftIO (mapRowThroughEdit oldLs diskLs row)
                  let
                    newCol =
                      min col (T.length (fromMaybe "" (safeIdx diskLs newRow)))
                    fresh = newEdState newRow diskLs
                    placed =
                      fresh {edZipper = Z.moveCursor (newRow, newCol) (edZipper fresh)}
                  rebuildWith i placed
                  reloaded <- get
                  let
                    newOut =
                      cursorDisplayRow (fvRows (stFiles reloaded !! i)) newRow
                  setTop (viewportScroll BodyVP) (max 0 (newOut - screenOff))
                  modify (\s -> s {stMsg = "\8635 reloaded " <> T.pack rel})

cursorDisplayRow :: [RowR] -> Int -> Int
cursorDisplayRow rows row =
  fromMaybe 0 (findIndex (\r -> rNewRow r == Just row) rows)

-- Where the cursor's line went: the old buffer is diffed against the new
-- one and the row mapped through it, so the cursor follows its content
-- when lines shift, and lands on the nearest surviving line when its own
-- line was edited away. Best effort — a degraded (budget-blown) diff
-- falls back toward the top.
mapRowThroughEdit :: [Text] -> [Text] -> Int -> IO Int
mapRowThroughEdit oldLs newLs target = do
  (rows, _) <- diffIO oldLs newLs
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
refreshGit :: EventM Name St ()
refreshGit = do
  st <- get
  now <- liftIO getMonotonicTime
  if now - stLastGit st <= 0.5
    then unless (stGitQueued st) $ do
      modify (\s -> s {stGitQueued = True})
      void . liftIO . forkIO $ do
        threadDelay 700000
        void (writeBChanNonBlocking (stChan st) GitStateChanged)
    else do
      modify (\s -> s {stLastGit = now, stGitQueued = False})
      rebaseNow <- liftIO (rebaseStatus (stRoot st))
      branchNow <- liftIO (currentBranch (stRoot st))
      headNow <- liftIO (headCommit (stRoot st))
      modify
        (\s -> s {stRebase = rebaseNow, stBranch = branchNow, stHead = headNow})
      stEff <- get
      let
        mode = effectiveMode stEff
      ignores <- liftIO (loadIgnores (stRoot stEff))
      names <-
        liftIO
          ( fmap
              (filter (not . ignoredPath ignores))
              (changedFiles mode (stRoot stEff))
          )
      let
        existing =
          Map.fromList (map (\fv -> (fvPath fv, fv)) (stFiles stEff))
      let
        refreshOne name =
          case Map.lookup name existing of
            Nothing -> loadOne (stConfig stEff) (stHls stEff) mode (stRoot stEff) name
            Just fv -> do
              base <- baseLines mode (stRoot stEff) name
              if base == fvBase fv
                then pure (Just fv)
                else do
                  (rebuilt, _) <-
                    rebuildFV (cfgSplit (stConfig stEff)) (repoText stEff) fv {fvBase = base}
                  pure (Just rebuilt)
      refreshed <- liftIO (forMaybeBounded refreshOne names)
      let
        inChangeSet = catMaybes refreshed
      -- hand-opened files stay in the session even when the change set
      -- doesn't claim them; once git stops seeing a difference (say the
      -- file was committed), the base honestly rebases to the buffer so
      -- stale hunks don't linger
      extras <-
        liftIO
          . forM
            (filter (`notElem` names) (Set.toList (stOpened stEff)))
          $ \path ->
            case Map.lookup path existing of
              Nothing -> pure Nothing
              Just fv -> do
                base <- baseLines mode (stRoot stEff) path
                let
                  buf = bufferLines (fvEd fv)
                if base == buf && fvBase fv /= buf
                  then do
                    (rebuilt, _) <-
                      rebuildFV (cfgSplit (stConfig stEff)) (repoText stEff) fv {fvBase = buf}
                    pure (Just rebuilt)
                  else pure (Just fv)
      let
        remaining = inChangeSet ++ catMaybes extras
      forM_ remaining (liftIO . stWatch stEff . fileDir (stRoot stEff) . fvPath)
      latest <- get
      let
        wasEmpty = null (stFiles latest)
      if null remaining
        -- the persistent session idles rather than exits: the watcher
        -- stays live and new work wakes the screen on its own
        then
          put
            latest
              { stFiles = []
              , stCurF = 0
              , stCurH = 0
              , -- only on the way in: a later refresh that finds the
                -- session still idle must not talk over whatever the
                -- action that emptied it had to say
                stMsg =
                  if wasEmpty
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
              }
          when wasEmpty $ do
            woken <- get
            case firstUnrespondedOrPending woken of
              Just (f, h) -> parkAnchor f h
              Nothing -> parkAnchor 0 0

-- The : command line ----------------------------------------------------------

-- What each command is called, what it takes, and what to tell a human
-- browsing the menu.
data ArgKind = NoArg | ArgRef | ArgMode | ArgPath | ArgDir | ArgText | ArgSet

commandTable :: [(Text, ArgKind, Text)]
commandTable =
  [ ("base", ArgRef, "diff against REF; bare :base picks from history")
  , ("branch", NoArg, "whole-branch view (merge-base base)")
  , ("mode", ArgMode, "head | unstaged | untracked")
  , ("rebase", ArgRef, "commit-by-commit review pass")
  , ("continue", NoArg, "fold edits into the stopped commit, advance")
  , ("abort", NoArg, "abort the rebase (asks first)")
  , ("abort!", NoArg, "abort the rebase, no questions")
  , ("amend", NoArg, "edit the commit message; saving amends (+ session edits)")
  , ("accept", ArgDir, "mark everything under a directory reviewed")
  , ("note", ArgText, "note popup (:note TEXT for a one-liner)")
  , ("unnote", NoArg, "remove the note from the target lines")
  , ("resolve", ArgText, "resolve the note here; 'all' sweeps answered ones")
  , ("revert", NoArg, "revert hunk under cursor (or selection) to base")
  , ("open", ArgPath, "open a file in the session")
  , ("close", NoArg, "drop the current file")
  , ("files", NoArg, "fuzzy file finder")
  , ("tree", NoArg, "project tree with per-file diff stats")
  , ("help", NoArg, "the keys, grouped (also Space ?)")
  , ("commit", NoArg, "full message of the commit under review / HEAD")
  , ("grep", ArgText, "project-wide search (picker)")
  , ("edit", ArgPath, "$EDITOR at the cursor (or on PATH)")
  , ("set", ArgSet, "change a setting (live + persisted)")
  , ("checkout", NoArg, "switch branches (picker)")
  , ("q", NoArg, "quit")
  ]

argKindOf :: Text -> Maybe ArgKind
argKindOf name =
  case filter (\(n, _, _) -> n == name) commandTable of
    ((_, kind, _) : _) -> Just kind
    [] -> Nothing

-- Candidates for the word being typed: command names before the first
-- space, that command's argument domain after it.
cmdCandidates :: St -> Text -> [Text]
cmdCandidates st input =
  let
    (done, word) = T.breakOnEnd " " input
    prefixed = filter (word `T.isPrefixOf`)
  in
    case T.words done of
      [] -> prefixed (map (\(n, _, _) -> n) commandTable)
      ["set"] -> prefixed settingNames
      ["set", "keys"] -> prefixed ["helix", "emacs"]
      ["set", "note-on-flag"] -> prefixed ["0", "1"]
      ["resolve"] -> prefixed ["all"]
      [cmd] -> case argKindOf cmd of
        Just ArgRef -> prefixed ("branch" : stCmdRefs st)
        Just ArgMode -> prefixed ["head", "unstaged", "untracked"]
        Just ArgPath -> prefixed (stCmdFiles st)
        Just ArgDir -> prefixed (stCmdDirs st ++ stCmdFiles st)
        _ -> []
      _ -> []

openCmdPrompt :: EventM Name St ()
openCmdPrompt = do
  st <- get
  files <- liftIO (listFiles (stRoot st))
  refs <- liftIO (listRefs (stRoot st))
  let
    dirs =
      Set.toList
        (Set.fromList (concatMap (parentDirs . takeDirectory) files))
    parentDirs d =
      if d == "." || d == "/" || null d
        then []
        else d : parentDirs (takeDirectory d)
  modify $ \s ->
    s
      { stPrompt = Just (CmdPrompt "")
      , stCmdFiles = map T.pack files
      , stCmdDirs = map T.pack dirs
      , stCmdRefs = refs
      , stPromptCycle = Nothing
      }

-- Complete the current word to a candidate; repeated Tab cycles. The
-- originally typed text stays the filter, so cycling never narrows.
promptComplete :: Int -> Text -> EventM Name St ()
promptComplete step input = do
  st <- get
  let
    (base, prevIx) = case stPromptCycle st of
      Just (b, i) -> (b, i + step)
      Nothing -> (input, if step > 0 then 0 else -1)
    cands = cmdCandidates st base
  case cands of
    [] -> pure ()
    _ -> do
      let
        n = length cands
        ix = ((prevIx `mod` n) + n) `mod` n
        cand = cands !! ix
        (done, _) = T.breakOnEnd " " base
        suffix =
          case (T.null done, argKindOf cand) of
            (True, Just NoArg) -> ""
            (True, Just _) -> " "
            _ -> ""
        completed = done <> cand <> suffix
        -- accepting a command that takes arguments moves to a fresh
        -- stage: the next Tab completes the argument, not the verb
        nextCycle =
          if T.null suffix then Just (base, ix) else Nothing
      modify $ \s ->
        s
          { stPrompt = Just (CmdPrompt completed)
          , stPromptCycle = nextCycle
          }

-- The live menu above the status line: bare ':' lists every command
-- with its description; typing narrows; the cycled candidate is lit.
promptMenu :: St -> Maybe (Widget Name)
promptMenu st =
  case stPrompt st of
    Just (CmdPrompt input) ->
      let
        base = case stPromptCycle st of
          Just (b, _) -> b
          Nothing -> input
        cands = cmdCandidates st base
        selIx = fmap snd (stPromptCycle st)
        isCmdStage = not (" " `T.isInfixOf` base)
        descOf n = case filter (\(c, _, _) -> c == n) commandTable of
          ((_, _, d) : _) -> d
          [] -> ""
        row (i, n) =
          let
            label =
              if isCmdStage
                then
                  " :"
                    <> T.justifyLeft 10 ' ' n
                    <> "  "
                    <> descOf n
                else " " <> n
            w = padRight Max (txt label)
          in
            if Just i == selIx then withDefAttr selAttr w else w
        shown = take 16 (zip [0 ..] cands)
        more = length cands - length shown
        moreRow =
          if more > 0
            then [withAttr hintAttr (txt ("   \8230 " <> tshow more <> " more"))]
            else []
      in
        case (cands, lineAddress base) of
          -- a bare number is a line address: no completions to offer,
          -- but say what it will do, like every other command does
          ([], Just n) ->
            Just
              ( txt
                  ( " :"
                      <> T.justifyLeft 10 ' ' base
                      <> "  go to line "
                      <> tshow (max 1 n)
                  )
              )
          ([], _) -> Nothing
          _ -> Just (vBox (map row shown ++ moreRow))
    _ -> Nothing

setMsg :: Text -> EventM Name St ()
setMsg t = modify (\s -> s {stMsg = t})

-- A bare number on the command line is a line address, ex-style.
-- Read as an Integer and saturated, so an absurdly wide number still
-- means "the end of the file" rather than wrapping around to the top.
lineAddress :: Text -> Maybe Int
lineAddress t = case TR.decimal t of
  Right (n, rest)
    | T.null rest ->
        Just (fromInteger (min n (fromIntegral (maxBound :: Int))))
  _ -> Nothing

runCommand :: Text -> EventM Name St ()
runCommand input =
  case T.words input of
    [] -> pure ()
    ["q"] -> halt
    ["base"] -> openBasePicker
    ["checkout"] -> openBranchPicker
    ["checkout", branch] -> applyCheckout branch
    ["base", "branch"] -> setBaseBranch
    ["branch"] -> setBaseBranch
    ["base", ref] -> setBase (VsRef ref)
    ["mode", "head"] -> setBase (VsRef "HEAD")
    ["mode", "unstaged"] -> setBase VsIndex
    ["mode", "untracked"] -> setBase Untracked
    ["mode", other] ->
      setMsg ("unknown mode " <> other <> " — head | unstaged | untracked")
    ["rebase"] -> startRebase Nothing
    ["rebase", ref] -> startRebase (Just ref)
    ["continue"] -> continueRebase
    ["abort"] -> setMsg "abort the rebase and lose its progress? :abort! confirms"
    ["abort!"] -> abortRebase
    ["amend"] -> amendHead
    ["note"] -> openNotePopup
    ("note" : rest) -> noteCmd (T.unwords rest)
    ["unnote"] -> unnoteCmd
    ["revert"] -> revertTarget
    ["resolve"] -> unnoteCmd
    ["resolve", "all"] -> resolveAllAnswered
    ["accept", path] -> acceptPath path
    ["set"] -> showSettings
    ["set", key] -> showSetting key
    ["set", key, val] -> applySet key val
    ["open", path] -> openFilePath path
    ["close"] -> closeCurrent
    ["files"] -> openPicker
    ["tree"] -> openTree
    ["help"] -> showHelp
    ["commit"] -> showCommitPopup
    ("grep" : rest)
      | not (null rest) -> openGrepPicker (T.unwords rest)
    ["grep"] -> openGrepPicker ""
    ["edit"] -> editCurrent
    ["edit", path] -> do
      st <- get
      launchEditor (stRoot st </> T.unpack path) 1
    [tok] | Just n <- lineAddress tok -> gotoLine n
    _ -> setMsg ("unknown command: " <> input)

-- Settings management: :set with no arguments lists everything, one
-- argument shows a value, two applies it — live and persisted to the
-- config file.
settingNames :: [Text]
settingNames =
  [ "keys"
  , "scrolloff"
  , "note-wrap"
  , "note-on-flag"
  , "split-threshold"
  , "piece-min"
  , "piece-max"
  ]

settingValue :: Config -> Text -> Text
settingValue cfg key =
  case key of
    "keys" -> if cfgKeys cfg == KeysEmacs then "emacs" else "helix"
    "scrolloff" -> tshow (cfgScrolloff cfg)
    "note-wrap" -> tshow (cfgNoteWrap cfg)
    "note-on-flag" -> if cfgNoteOnFlag cfg then "1" else "0"
    "split-threshold" -> tshow (splitThreshold (cfgSplit cfg))
    "piece-min" -> tshow (pieceMin (cfgSplit cfg))
    "piece-max" -> tshow (pieceMax (cfgSplit cfg))
    _ -> "?"

showSettings :: EventM Name St ()
showSettings = do
  st <- get
  let
    one k = k <> "=" <> settingValue (stConfig st) k
  setMsg (T.intercalate "  " (map one settingNames))

showSetting :: Text -> EventM Name St ()
showSetting key = do
  st <- get
  if key `elem` settingNames
    then setMsg (key <> " = " <> settingValue (stConfig st) key)
    else setMsg ("unknown setting: " <> key)

applySet :: Text -> Text -> EventM Name St ()
applySet key val = do
  st <- get
  let
    cfg = stConfig st
    asInt = case TR.signed TR.decimal val of
      Right (n, rest) | T.null rest -> Just (n :: Int)
      _ -> Nothing
    finish :: Config -> EventM Name St () -> EventM Name St ()
    finish newCfg after = do
      modify (\s -> s {stConfig = newCfg})
      liftIO (persistSetting key val)
      after
      setMsg (":set " <> key <> " = " <> val)
    withInt build after = case asInt of
      Just n -> finish (build n) after
      Nothing -> setMsg (key <> " wants a number")
    splitWith f = f (cfgSplit cfg)
  case key of
    "keys"
      | val == "helix" -> finish cfg {cfgKeys = KeysHelix} (pure ())
      | val == "emacs" -> finish cfg {cfgKeys = KeysEmacs} (pure ())
      | otherwise -> setMsg "keys = helix | emacs"
    "scrolloff" -> withInt (\n -> cfg {cfgScrolloff = max 0 n}) (pure ())
    "note-wrap" -> withInt (\n -> cfg {cfgNoteWrap = max 20 n}) (pure ())
    "note-on-flag" ->
      withInt (\n -> cfg {cfgNoteOnFlag = n /= 0}) (pure ())
    "split-threshold" ->
      withInt
        (\n -> cfg {cfgSplit = splitWith (\sc -> sc {splitThreshold = max 1 n})})
        rebuildAllFiles
    "piece-min" ->
      withInt
        (\n -> cfg {cfgSplit = splitWith (\sc -> sc {pieceMin = max 1 n})})
        rebuildAllFiles
    "piece-max" ->
      withInt
        (\n -> cfg {cfgSplit = splitWith (\sc -> sc {pieceMax = max 2 n})})
        rebuildAllFiles
    _ -> setMsg ("unknown setting: " <> key)

-- Split knobs reshape every file's hunks; rebuild them all (bounded
-- workers) and keep the current hunk by content.
rebuildAllFiles :: EventM Name St ()
rebuildAllFiles = do
  st <- get
  let
    sc = cfgSplit (stConfig st)
    oldCur = currentRunOf st (stCurF st)
  rebuilt <-
    liftIO
      ( forMaybeBounded
          (\fv -> fmap (Just . fst) (rebuildFV sc (repoText st) fv))
          (stFiles st)
      )
  let
    files = catMaybes rebuilt
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

-- Switch what the session reviews against. Each switch closes the pass
-- and opens a new one, so pass history records what was reviewed vs what.
setBase :: ReviewMode -> EventM Name St ()
setBase mode = do
  st <- get
  ok <- case mode of
    VsRef ref -> liftIO (refExists (stRoot st) ref)
    _ -> pure True
  case (ok, mode) of
    (False, VsRef ref) -> setMsg ("no such ref: " <> ref)
    _ -> do
      liftIO (finishPass (stConn st) (stPassId st))
      newPass <- liftIO (beginPass (stConn st) (repoText st) (modeLabel mode))
      modify (\s -> s {stMode = mode, stPassId = newPass})
      forceGitRefresh
      refreshed <- get
      unless
        (null (stFiles refreshed))
        (setMsg ("base: " <> modeLabel mode))

-- The whole-branch view: base = where this branch forked from the
-- mainline. Purely a diff base — no rebase involved.
setBaseBranch :: EventM Name St ()
setBaseBranch = do
  st <- get
  got <- liftIO (branchRoot (stRoot st))
  case got of
    Left e -> setMsg e
    Right sha -> setBase (VsRef (T.take 10 sha))

rebaseStopMsg :: RebaseStatus -> Text
rebaseStopMsg rb =
  let
    at = tshow (rebaseStep rb) <> "/" <> tshow (rebaseTotal rb)
  in
    if rebaseConflicted rb
      then "conflict at commit " <> at <> " — resolve, then :continue"
      else "commit " <> at <> " — review, edit, :continue (:abort! backs out)"

-- Start the surgical commit-by-commit pass: `git rebase --exec false`
-- pauses the sequencer after every commit (the failing exec is the stop
-- mechanism), and rebase awareness takes over from there.
startRebase :: Maybe Text -> EventM Name St ()
startRebase mRef = do
  st <- get
  case stRebase st of
    Just _ -> setMsg "a rebase is already in progress — :continue or :abort!"
    Nothing -> do
      dirty <- liftIO (trackedChanges (stRoot st))
      if dirty
        then setMsg "tracked changes in the tree — commit or stash before :rebase"
        else do
          target <- case mRef of
            Just ref -> do
              ok <- liftIO (refExists (stRoot st) ref)
              pure (if ok then Right ref else Left ("no such ref: " <> ref))
            Nothing -> do
              got <- liftIO (branchRoot (stRoot st))
              pure (fmap (T.take 10) got)
          case target of
            Left e -> setMsg e
            Right ref -> do
              res <- liftIO (runGit (stRoot st) ["rebase", "--exec", "false", T.unpack ref])
              rb <- liftIO (rebaseStatus (stRoot st))
              modify (\s -> s {stRebase = rb, stWritten = Set.empty})
              case rb of
                Just info -> do
                  forceGitRefresh
                  setMsg (rebaseStopMsg info)
                Nothing ->
                  setMsg $ case res of
                    Left e -> e
                    Right () -> "nothing to rebase — already at " <> ref

-- Fold this stop's edits into the stopped commit and advance. A conflict
-- stop has no commit yet, so --continue makes it; an exec stop already
-- committed, so the edits are amended in first.
continueRebase :: EventM Name St ()
continueRebase = do
  st <- get
  case stRebase st of
    Nothing -> setMsg "no rebase in progress"
    Just rb -> do
      let
        root = stRoot st
      -- a rebase stop starts from a clean tree, so everything dirty now
      -- is this stop's fix — including edits made through $EDITOR or by
      -- an agent, which never pass through remax's own write path
      folded <- liftIO $ do
        addRes <- runGit root ["add", "-A"]
        case addRes of
          Left e -> pure (Left e)
          Right () -> do
            staged <- hasStagedChanges root
            if not (rebaseConflicted rb) && staged
              then runGit root ["commit", "--amend", "--no-edit"]
              else pure (Right ())
      case folded of
        Left e -> setMsg e
        Right () -> do
          res <- liftIO (runGit root ["rebase", "--continue"])
          rbNext <- liftIO (rebaseStatus root)
          modify (\s -> s {stRebase = rbNext, stWritten = Set.empty})
          forceGitRefresh
          case rbNext of
            Nothing -> setMsg "rebase complete — base restored"
            Just next ->
              -- --continue itself exits nonzero when the next exec-false
              -- stop is reached; a stop that actually advanced is fine
              if rebaseStep next == rebaseStep rb && not (rebaseConflicted next)
                then
                  setMsg
                    ( case res of
                        Left e -> e
                        Right () -> rebaseStopMsg next
                    )
                else setMsg (rebaseStopMsg next)

abortRebase :: EventM Name St ()
abortRebase = do
  st <- get
  case stRebase st of
    Nothing -> setMsg "no rebase in progress"
    Just _ -> do
      res <- liftIO (runGit (stRoot st) ["rebase", "--abort"])
      rbNext <- liftIO (rebaseStatus (stRoot st))
      modify (\s -> s {stRebase = rbNext, stWritten = Set.empty})
      forceGitRefresh
      setMsg $ case res of
        Left e -> e
        Right () -> "rebase aborted"

-- The standalone fold-fixes-into-HEAD flow (pairs with :base HEAD^).
-- :amend — the commit message of the commit under review (HEAD; during
-- a rebase that is the stopped commit) opens for editing, and saving
-- amends it, folding in any files written this session outside a
-- rebase (a rebase stop leaves the folding to :continue, so the amend
-- there is message-only).
amendHead :: EventM Name St ()
amendHead = do
  st <- get
  got <- liftIO (commitBody (stRoot st) "HEAD")
  -- what folds in with the message: everything the session is showing as
  -- changed, asked of git rather than remembered, so an agent's edits and
  -- \$EDITOR's edits count the same as the ones typed in here. At a rebase
  -- stop nothing folds in -- :continue owns that.
  folding <- case stRebase st of
    Just _ -> pure []
    Nothing -> liftIO (changedFiles (effectiveMode st) (stRoot st))
  case got of
    Left e -> setMsg e
    Right msg -> do
      let
        ls = if T.null (T.strip msg) then [""] else T.lines msg
        ed = newEdState 0 ls
      modify
        (\s -> s {stNotePopup = Just (NotePopup ed (AmendMessage folding))})

applyAmendMessage :: [FilePath] -> Text -> EventM Name St ()
applyAmendMessage folding text = do
  st <- get
  if T.null text
    then setMsg "empty commit message — amend cancelled"
    else do
      res <- liftIO $ do
        addRes <-
          if null folding
            then pure (Right ())
            else runGit (stRoot st) (["add", "--"] ++ folding)
        case addRes of
          Left e -> pure (Left e)
          Right () ->
            runGit (stRoot st) ["commit", "--amend", "-m", T.unpack text]
      case res of
        Left e -> setMsg e
        Right () -> do
          unless (null folding) (modify (\s -> s {stWritten = Set.empty}))
          forceGitRefresh
          setMsg $
            if null folding
              then "amended the commit message"
              else
                "amended HEAD: message + "
                  <> fileCountLabel (length folding)
                  <> " ("
                  <> foldedNames 3 folding
                  <> ")"

fileCountLabel :: Int -> Text
fileCountLabel n = tshow n <> (if n == 1 then " file" else " files")

-- Names for the status line: a few, then a count for the rest, so the
-- message says what it staged without running off the bar.
foldedNames :: Int -> [FilePath] -> Text
foldedNames k paths =
  let
    shown = map (T.pack . takeFileName) (take k paths)
    more = length paths - length shown
  in
    T.intercalate ", " shown
      <> (if more > 0 then " +" <> tshow more <> " more" else "")

-- Attach a reviewer note to the selection's lines (or the current hunk),
-- so a ! or ? carries its reason into `remax list`.
-- :resolve all — sweep every answered note in the repository: anything
-- an agent has replied to is considered handled and both sides clear.
resolveAllAnswered :: EventM Name St ()
resolveAllAnswered = do
  st <- get
  n <- liftIO (resolveAnswered (stConn st) (repoText st))
  threads <- liftIO (loadThreads (stConn st) (repoText st))
  modify (\s -> s {stThreads = threads})
  setMsg $
    if n == 0
      then "no answered notes to resolve"
      else "resolved " <> tshow n <> " answered line(s)"

-- Acceptance closes the note conversation; the store upsert already
-- cleared the rows, this keeps the inline display in step.
resolveNotesInMemory :: [Text] -> EventM Name St ()
resolveNotesInMemory keys =
  modify $ \s ->
    s {stThreads = foldr Map.delete (stThreads s) keys}

-- What a note applies to: an active selection wins; then the line under
-- the cursor, when it already carries a verdict or a note (sitting on a
-- noted line and typing :note must open THAT note — a noted line's
-- whole group comes along, so the note stays one note); then whatever
-- was just marked (marking auto-advances, so the cursor has already
-- moved on); with none of those, the current hunk.
noteTargetKeys :: St -> [Text]
noteTargetKeys st =
  let
    selectionKeys = do
      fv <- currentFileMay st
      (lo, hi) <- activeSelectionRows (fvEd fv)
      pure (map (\(k, _, _) -> k) (selectedChanges fv lo hi))
    cursorKeys = do
      fv <- currentFileMay st
      let
        row = fst (cursorRC (fvEd fv))
        ks = map (\(k, _, _) -> k) (selectedChanges fv row row)
        relevant k =
          Map.member k (stVerdicts st) || Map.member k (stThreads st)
      let
        groupOf thread =
          let
            inGroup r = do
              k <- rKey r
              if Map.lookup k (stThreads st) == Just thread
                then Just k
                else Nothing
          in
            mapMaybe inGroup (fvRows fv)
        -- a conversation anywhere in the hunk under the cursor: from any
        -- line of the hunk, :note continues that thread rather than
        -- opening a second one a few lines away
        hunkThread = do
          h <- hunkUnderCursor st
          r <- safeIdx (fvRuns fv) h
          case mapMaybe (\k -> Map.lookup k (stThreads st)) (runKeys r) of
            (t : _) -> Just t
            [] -> Nothing
      noted <- case mapMaybe (\k -> Map.lookup k (stThreads st)) ks of
        (n : _) ->
          -- the whole group sharing this conversation, so editing keeps
          -- it a single thread rather than forking per line
          Just (Just (groupOf n))
        [] -> case hunkThread of
          Just t -> Just (Just (groupOf t))
          Nothing ->
            if any relevant ks
              then Just Nothing
              else Nothing
      pure $ case noted of
        Just groupKeys -> groupKeys
        Nothing -> filter relevant ks
    hunkKeys = do
      fv <- currentFileMay st
      r <- safeIdx (fvRuns fv) (clampH (fvRuns fv) (stCurH st))
      pure (runKeys r)
    lastKeys =
      if null (stLastMarked st) then Nothing else Just (stLastMarked st)
  in
    fromMaybe [] (selectionKeys <|> cursorKeys <|> lastKeys <|> hunkKeys)

-- Save the reviewer's side of the conversation (empty text clears the
-- whole thread instead), keep the in-memory view in step, and report
-- where it landed. While the reviewer's own post is the newest it stays
-- editable in place; once someone answered, saving appends the next
-- post.
applyNote :: Text -> [Text] -> EventM Name St ()
applyNote text keys = do
  st <- get
  if T.null text
    then do
      n <- liftIO (clearThread (stConn st) (repoText st) keys)
      modify $ \s ->
        s {stThreads = foldr Map.delete (stThreads s) keys}
      setMsg $
        if n == 0
          then "no notes here"
          else "removed the conversation from " <> tshow n <> " line(s)"
    else do
      n <- liftIO (saveReviewerPost (stConn st) (repoText st) text keys)
      let
        markedKeys = filter (`Map.member` stVerdicts st) keys
        place thread = case reverse thread of
          (Post "reviewer" _ : older) -> reverse (Post "reviewer" text : older)
          _ -> thread ++ [Post "reviewer" text]
        update k m = Map.insert k (place (Map.findWithDefault [] k m)) m
      modify $ \s ->
        s {stThreads = foldr update (stThreads s) markedKeys}
      setMsg $
        if n == 0
          then "no verdicts here yet — mark with ! or ? first"
          else "note on " <> tshow n <> " line(s)"

noteCmd :: Text -> EventM Name St ()
noteCmd text = do
  st <- get
  let
    keys = noteTargetKeys st
  if null keys
    then setMsg "nothing to note — mark lines with ! or ? first"
    else applyNote (T.strip text) keys

unnoteCmd :: EventM Name St ()
unnoteCmd = do
  st <- get
  let
    keys = noteTargetKeys st
  if null keys
    then setMsg "nothing here to unnote"
    else applyNote "" keys

-- The note popup: a small modal buffer with the full editing keys.
-- Enter in its normal mode saves, Ctrl-c discards; an empty note clears.
openNotePopup :: EventM Name St ()
openNotePopup = do
  st <- get
  let
    keys = noteTargetKeys st
  if null keys
    then setMsg "nothing to note — mark lines with ! or ? first"
    else do
      let
        existing = case mapMaybe (`Map.lookup` stThreads st) keys of
          (thread : _) -> case reverse thread of
            (Post "reviewer" body : _) -> body
            _ -> ""
          [] -> ""
        ls = if T.null existing then [""] else T.lines existing
        lastRow = length ls - 1
        lastCol = case reverse ls of
          (l : _) -> T.length l
          [] -> 0
        ed0 = newEdState lastRow ls
        ed1 =
          ed0
            { edZipper = Z.moveCursor (lastRow, lastCol) (edZipper ed0)
            , edMode = Insert
            }
      modify (\s -> s {stNotePopup = Just (NotePopup ed1 (NoteFor keys))})

setNotePopupEd :: EdState -> EventM Name St ()
setNotePopupEd ed =
  modify $ \s ->
    s
      { stNotePopup = fmap (\np -> np {npEd = ed}) (stNotePopup s)
      , stKill = edKill ed
      }

handleNotePopupKey :: V.Event -> NotePopup -> EventM Name St ()
handleNotePopupKey ev np0 = do
  st0 <- get
  let
    np = np0 {npEd = (npEd np0) {edKill = stKill st0}}
    discardMsg = case npPurpose np of
      NoteFor _ -> "note discarded"
      AmendMessage _ -> "amend cancelled"
    wrapCol = case npPurpose np of
      NoteFor _ -> cfgNoteWrap (stConfig st0)
      AmendMessage _ -> 72
    saveBuffer = do
      modify (\s -> s {stNotePopup = Nothing})
      let
        text = T.strip (T.intercalate "\n" (bufferLines (npEd np)))
      case npPurpose np of
        NoteFor keys -> do
          applyNote text keys
          runDeferredAdvance
        AmendMessage folding -> applyAmendMessage folding text
  case ev of
    -- discard works from either popup mode; nobody should need to leave
    -- insert just to bail out
    V.EvKey (V.KChar 'c') [V.MCtrl] -> do
      modify (\s -> s {stNotePopup = Nothing, stMsg = discardMsg})
      runDeferredAdvance
    _ ->
      case edMode (npEd np) of
        Insert ->
          case handleInsert ev (npEd np) of
            InsertHandled ed -> setNotePopupEd (wrapNoteLine wrapCol ed)
            InsertExit ed -> setNotePopupEd ed
            InsertUnhandled -> pure ()
        Normal ->
          case ev of
            V.EvKey V.KEnter [] -> saveBuffer
            -- Esc from insert lands here in the popup's normal mode; Esc
            -- again cancels the dialog outright
            V.EvKey V.KEsc [] -> do
              modify (\s -> s {stNotePopup = Nothing, stMsg = discardMsg})
              runDeferredAdvance
            _ ->
              case handleNormal ev (npEd np) of
                Just ed -> setNotePopupEd ed
                Nothing -> pure ()

-- Auto-fill for the note popup: typing past the popup width breaks the
-- line at the last space, and the cursor rides the remainder down.
-- A single overlong word is left alone.
wrapNoteLine :: Int -> EdState -> EdState
wrapNoteLine noteWrapCol ed =
  let
    (row, col) = cursorRC ed
    ls = bufferLines ed
    line = case drop row ls of
      (l : _) -> l
      [] -> ""
    breakAt =
      T.findIndex (== ' ') (T.reverse (T.take (noteWrapCol + 1) line))
  in
    if T.length line <= noteWrapCol
      then ed
      else case breakAt of
        Nothing -> ed
        Just fromEnd ->
          let
            at = noteWrapCol - fromEnd
            keep = T.take at line
            rest = T.drop (at + 1) line
            wrapped = take row ls ++ (keep : rest : drop (row + 1) ls)
            cursor =
              if col > at
                then (row + 1, col - at - 1)
                else (row, col)
          in
            ed {edZipper = Z.moveCursor cursor (Z.textZipper wrapped Nothing)}

-- What the popup is about to do. The old chrome never said: a fresh
-- note, a rewrite of your own latest post, and a follow-up to somebody
-- else's answer all looked exactly alike.
data NoteAct = NoteNew | NoteEdit | NoteReply Text

noteAct :: [Post] -> NoteAct
noteAct thread =
  case reverse thread of
    [] -> NoteNew
    (Post author _ : _)
      | author == "reviewer" -> NoteEdit
      | otherwise -> NoteReply author

-- The posts to show above the editor. The whole conversation, except
-- that when you are rewriting your own latest post that post is already
-- in the editor and would otherwise appear twice.
transcriptPosts :: [Post] -> [Post]
transcriptPosts thread =
  case noteAct thread of
    NoteEdit -> init thread
    _ -> thread

-- One post as rendered transcript rows: the same glyphs the inline
-- conversation uses, wrapped to the popup, continuations indented under
-- the text so the authorship stays scannable down the left edge.
postRows :: Int -> Post -> [Widget Name]
postRows width post =
  let
    mine = postAuthor post == "reviewer"
    lead =
      if mine
        then "\8801 you \183 "
        else "\8618 " <> postAuthor post <> " \183 "
    attr = if mine then noteInlineAttr else replyInlineAttr
    -- a stored post keeps the reviewer's own line breaks; the transcript
    -- is a quotation, so re-wrap it to whatever width the popup got
    packed =
      case greedyWrap
        (max 12 (width - T.length lead))
        (T.words (T.unwords (T.lines (postBody post)))) of
        [] -> [""]
        wss -> map T.unwords wss
  in
    case packed of
      [] -> []
      (first : rest) ->
        withAttr attr (txt (lead <> first))
          : map (\l -> withAttr attr (txt ("  " <> l))) rest

notePopupLayer :: St -> NotePopup -> Widget Name
notePopupLayer st np =
  let
    ed = npEd np
    (row, col) = cursorRC ed
    ls = bufferLines ed
    modeTag = case edMode ed of
      Insert -> "ins"
      Normal -> "nor"
    thread = case npPurpose np of
      AmendMessage _ -> []
      NoteFor keys ->
        case mapMaybe (`Map.lookup` stThreads st) keys of
          (t : _) -> t
          [] -> []
    act = noteAct thread
    popupName = case npPurpose np of
      AmendMessage _ -> "amend"
      NoteFor _ -> case act of
        NoteNew -> "note"
        NoteEdit -> "note \183 editing yours"
        NoteReply who -> "note \183 reply to " <> who
    popupHint = case npPurpose np of
      AmendMessage _ -> "\9166 amend (nor) \183 C-c cancel"
      NoteFor _ -> case act of
        NoteReply _ ->
          "\9166 post (nor) \183 C-c discard \183 empty clears the thread"
        _ -> "\9166 save (nor) \183 C-c discard \183 empty note clears"
    -- which line the conversation is about; the popup never used to say
    targetRows = case npPurpose np of
      -- an amend that quietly stages files is the surprise this is here
      -- to remove: say what folds in before ⏎ does it
      AmendMessage folding
        | null folding ->
            [ withAttr
                hintAttr
                ( txt
                    ( case stRebase st of
                        Just _ ->
                          "message only \183 :continue folds in this stop's edits"
                        Nothing -> "message only \183 nothing else has changed"
                    )
                )
            , txt " "
            ]
        | otherwise ->
            withAttr
              hintAttr
              (txt ("folding in " <> fileCountLabel (length folding) <> ":"))
              : map
                (\f -> withAttr noteInlineAttr (txt ("  " <> T.pack f)))
                (take 8 folding)
              ++ [ withAttr
                     hintAttr
                     (txt ("  \8230 and " <> tshow (length folding - 8) <> " more"))
                 | length folding > 8
                 ]
              ++ [txt " "]
      NoteFor _ -> case currentFileMay st of
        Nothing -> []
        Just fv ->
          let
            cur = fst (cursorRC (fvEd fv))
            decl = case declAt (bufferLines (fvEd fv)) cur of
              Just d -> " \183 " <> declName d
              Nothing -> ""
          in
            [ withAttr
                hintAttr
                (txt (T.pack (fvPath fv) <> ":" <> tshow (cur + 1) <> decl))
            , txt " "
            ]
    -- selections paint here the same way they do in the main buffer:
    -- whole rows for x, column spans for v
    lineSel = selectionRows ed
    charSel = charSelection ed
    spanFor i = do
      ((lr, lc), (hr, hc)) <- charSel
      if i < lr || i > hr
        then Nothing
        else
          Just
            ( if i == lr then lc else 0
            , if i == hr then Just hc else Nothing
            )
    inLineSel i = case lineSel of
      Just (a, b) -> i >= a && i <= b
      Nothing -> False
    bodyLine (i, l)
      | inLineSel i = withDefAttr selAttr (txt (padT l))
      | otherwise = case spanFor i of
          Nothing -> txt (padT l)
          Just (start, mEnd) ->
            let
              (pre, rest) = T.splitAt start l
              (mid, post) = case mEnd of
                Nothing -> (rest, T.empty)
                Just e -> T.splitAt (e + 1 - start) rest
              midW = if T.null mid then txt " " else txt mid
            in
              txt pre <+> withDefAttr selAttr midW <+> txt post
    bodyW =
      showCursor
        TheCursor
        (Location (col, row))
        (vBox (map bodyLine (zip [0 ..] ls)))
  in
    Widget Greedy Greedy $ do
      ctx <- getContext
      let
        boxWidth = min 90 (max 50 (availWidth ctx - 20))
        bodyRows = max 6 (availHeight ctx - 14)
        posts = transcriptPosts thread
        rowsFor = concatMap (postRows (boxWidth - 2))
        -- the editor keeps its room first; the transcript takes what is
        -- left, and gives up its oldest posts to fit rather than
        -- squeezing the box you are typing in
        editorRows = max 3 (min 10 (length ls + 1))
        budget = bodyRows - editorRows - length targetRows - 1
        (shownRows, elided)
          | null posts = ([], 0 :: Int)
          | length (rowsFor posts) <= budget = (rowsFor posts, 0)
          | otherwise =
              let
                drops ps
                  | null ps = ([], length posts)
                  | length (rowsFor ps) <= budget - 1 =
                      (rowsFor ps, length posts - length ps)
                  | otherwise = drops (drop 1 ps)
              in
                drops posts
        markerRows =
          if elided > 0
            then
              [ withAttr
                  hintAttr
                  ( txt
                      ( "\8230 "
                          <> tshow elided
                          <> " earlier post"
                          <> (if elided == 1 then "" else "s")
                      )
                  )
              ]
            else []
        ruleRows =
          if null shownRows && elided == 0
            then []
            else [withAttr hairAttr (vLimit 1 (fill '\9472'))]
      render $
        floatFrame (popupName <> " \183 " <> modeTag) popupHint $
          hLimit boxWidth $
            padRight Max $
              vLimit bodyRows $
                vBox (targetRows ++ markerRows ++ shownRows ++ ruleRows ++ [bodyW])

-- The project tree ------------------------------------------------------------

-- Browsing and shape in one overlay: the repository's files, decorated
-- per file the way git diff --stat is, and expandable a level further so
-- a file opens into its hunks. That last level makes this the tray's
-- file-to-hunk list with directories on top, which is why the model
-- lives in Remax.Tree and knows nothing about being drawn in a float.
data TreeView = TreeView
  { tvRoot :: TreeNode
  , tvAll :: [(FilePath, Maybe PathStat)] -- flat, for filtering
  , tvExpanded :: Set.Set FilePath -- directories and files alike
  , tvSel :: Int
  , tvInput :: Text
  }

openTree :: EventM Name St ()
openTree = do
  st <- get
  let
    root = stRoot st
  ignores <- liftIO (loadIgnores root)
  files <- liftIO (listFiles root)
  stats <- liftIO (pathStats (effectiveMode st) root)
  let
    keep = filter (not . ignoredPath ignores)
    paths = keep files
    statPaths = Map.filterWithKey (\p _ -> not (ignoredPath ignores p)) stats
    node = buildTree paths statPaths
    -- open the directories a change lives in, and the ones leading to
    -- the file under review, so the diff is on screen with no keystrokes
    changed = Map.keys statPaths
    here = maybe [] (\fv -> [fvPath fv]) (currentFileMay st)
    opened = Set.fromList (concatMap ancestorPaths (changed ++ here))
    flat =
      map (\p -> (p, Map.lookup p statPaths)) (Set.toList (Set.fromList (paths ++ changed)))
    view =
      TreeView
        { tvRoot = node
        , tvAll = flat
        , tvExpanded = opened
        , tvSel = 0
        , tvInput = ""
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
    treeRows treeFilter (tvExpanded view) (treeHunkCount st) (tvRoot view)

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

handleTreeKey :: V.Event -> TreeView -> EventM Name St ()
handleTreeKey ev view = do
  st <- get
  let
    visible = treeVisible st view
    bound = max 0 (length visible - 1)
    move d = setTree view {tvSel = max 0 (min bound (tvSel view + d))}
    -- land on the first file that matched, not on a directory kept only
    -- to hold it: the directories are structure, the file is the answer
    retype q =
      let
        next = view {tvInput = q, tvSel = 0}
        firstFile = findIndex rowIsFile (treeVisible st next)
      in
        setTree next {tvSel = fromMaybe 0 firstFile}
    close = modify (\s -> s {stTree = Nothing})
    expandable row = rowExpandable row
    fold path = setTree view {tvExpanded = Set.delete path (tvExpanded view)}
    unfold path = setTree view {tvExpanded = Set.insert path (tvExpanded view)}
    toParent row = case trParent row of
      Nothing -> pure ()
      Just parent ->
        let
          folded = view {tvExpanded = Set.delete parent (tvExpanded view)}
          at = findIndex ((== parent) . trPath) (treeVisible st folded)
        in
          setTree folded {tvSel = fromMaybe (tvSel view) at}
    act row =
      case trKind row of
        RowDir _ open ->
          if open then fold (trPath row) else unfold (trPath row)
        RowHunk ix -> do
          close
          case findIndex ((== trPath row) . fvPath) (stFiles st) of
            Just f -> parkAnchor f ix
            Nothing -> setMsg "that file has left the session"
        RowFile mStat hasHunks open ->
          if hasHunks && not open
            then unfold (trPath row)
            else case fmap psChange mStat of
              Just PathDeleted ->
                setMsg "deleted in this change — nothing to open"
              _ -> do
                close
                openFilePath (T.pack (trPath row))
  case ev of
    V.EvKey V.KEsc []
      | not (T.null (tvInput view)) -> retype ""
      | otherwise -> close
    V.EvKey V.KUp [] -> move (-1)
    V.EvKey (V.KChar 'p') [V.MCtrl] -> move (-1)
    V.EvKey V.KDown [] -> move 1
    V.EvKey (V.KChar 'n') [V.MCtrl] -> move 1
    V.EvKey V.KRight [] ->
      case treeSelected st view of
        Just row | expandable row -> unfold (trPath row)
        _ -> pure ()
    V.EvKey V.KLeft [] ->
      case treeSelected st view of
        Just row
          | expandable row && Set.member (trPath row) (tvExpanded view) ->
              fold (trPath row)
          | otherwise -> toParent row
        Nothing -> pure ()
    V.EvKey V.KEnter [] ->
      case treeSelected st view of
        Just row -> act row
        Nothing -> pure ()
    V.EvKey V.KBS [] -> retype (T.dropEnd 1 (tvInput view))
    V.EvKey (V.KChar c) [] -> retype (T.snoc (tvInput view) c)
    _ -> pure ()

-- Fixed-width columns, so one long name cannot shove the numbers out of
-- line. hLimit does what columnRow does for the pickers, but keeps the
-- cells as widgets so they can carry their own colors.
treeCell :: Int -> Widget Name -> Widget Name
treeCell w = hLimit w . padRight Max

treeCellR :: Int -> Widget Name -> Widget Name
treeCellR w = hLimit w . padLeft Max

treeNameW, treeStatW, treeNumW, treeMeterW :: Int
treeNameW = 34
treeStatW = 2
treeNumW = 6
treeMeterW = 13

clipT :: Int -> Text -> Text
clipT w t = if T.length t <= w then t else T.take (max 1 (w - 1)) t <> "\8230"

statusAttrFor :: PathChange -> AttrName
statusAttrFor change =
  case change of
    PathAdded -> addedMarkAttr
    PathModified -> pendingMarkAttr
    PathDeleted -> removedMarkAttr
    PathRenamed _ -> questionMarkAttr
    PathCopied _ -> questionMarkAttr
    PathUntracked -> hintAttr
    PathOtherChange -> hintAttr

verdictMarkAttr :: Char -> AttrName
verdictMarkAttr mark =
  case mark of
    '!' -> flagMarkAttr
    '?' -> questionMarkAttr
    '\10003' -> doneMarkAttr
    _ -> pendingMarkAttr

treeRowWidget :: St -> Bool -> TreeRow -> Widget Name
treeRowWidget st isSel row =
  let
    indent = T.replicate (trDepth row * 2) " "
    lead =
      if isSel
        then withAttr floatSelBarAttr (txt "\9614")
        else txt " "
    curPath = maybe "" fvPath (currentFileMay st)
    countCells mStat =
      case fmap psCounts mStat of
        Just (Just (a, r)) ->
          [ treeCellR
              treeNumW
              (if a > 0 then withAttr addedMarkAttr (txt ("+" <> tshow a)) else txt " ")
          , treeCellR
              treeNumW
              (if r > 0 then withAttr removedMarkAttr (txt ("\8722" <> tshow r)) else txt " ")
          ]
        Just Nothing ->
          case fmap psChange mStat of
            Just PathUntracked -> [treeCell (treeNumW * 2) (txt " ")]
            _ ->
              [ treeCellR treeNumW (withAttr trayDeclAttr (txt "bin"))
              , treeCell treeNumW (txt " ")
              ]
        Nothing -> [treeCell (treeNumW * 2) (txt " ")]
    meterCell path =
      case find ((== path) . fvPath) (stFiles st) of
        Just fv
          | not (null (fvRuns fv)) ->
              let
                total = length (fvRuns fv)
                done = length (filter (not . runPending (stVerdicts st)) (fvRuns fv))
              in
                treeCell treeMeterW (meterW done total)
        _ -> treeCell treeMeterW (txt " ")
    fromCell mStat =
      case fmap psChange mStat of
        Just (PathRenamed from) -> [renamedFrom from]
        Just (PathCopied from) -> [renamedFrom from]
        _ -> []
    renamedFrom from =
      withAttr hintAttr (txt (" \8592 " <> T.pack (takeFileName from)))
    body =
      case trKind row of
        RowDir roll open ->
          let
            marker = if open then "\9662 " else "\9656 "
            name = clipT treeNameW (indent <> marker <> trLabel row <> "/")
          in
            hBox
              ( treeCell treeNameW (withAttr hintAttr (txt name))
                  : if ruChanged roll > 0
                    then
                      [ treeCell treeStatW (txt " ")
                      , treeCellR
                          treeNumW
                          (withAttr addedMarkAttr (txt ("+" <> tshow (ruAdded roll))))
                      , treeCellR
                          treeNumW
                          (withAttr removedMarkAttr (txt ("\8722" <> tshow (ruRemoved roll))))
                      , txt "  "
                      , treeCell
                          treeMeterW
                          ( if open
                              then txt " "
                              else
                                withAttr
                                  hintAttr
                                  (txt (tshow (ruChanged roll) <> " chg"))
                          )
                      ]
                    else []
              )
        RowFile mStat hasHunks open ->
          let
            marker
              | not hasHunks = "  "
              | open = "\9662 "
              | otherwise = "\9656 "
            name = clipT treeNameW (indent <> marker <> trLabel row)
            nameAttr =
              if trPath row == curPath then trayFileCurAttr else trayFileAttr
            letter = case fmap psChange mStat of
              Just change ->
                withAttr (statusAttrFor change) (txt (pathChangeLetter change))
              Nothing -> txt " "
          in
            hBox
              ( [ treeCell treeNameW (withAttr nameAttr (txt name))
                , treeCell treeStatW letter
                ]
                  ++ countCells mStat
                  ++ [txt "  ", meterCell (trPath row)]
                  ++ fromCell mStat
              )
        RowHunk ix ->
          case treeHunkInfo st (trPath row) ix of
            Nothing -> txt " "
            Just (mark, ln, decl, hasNote) ->
              hBox
                [ txt (indent <> "  ")
                , withAttr (verdictMarkAttr mark) (str [mark])
                , txt (" :" <> tshow ln)
                , withAttr trayDeclAttr (txt decl)
                , if hasNote
                    then withAttr noteInlineAttr (txt " \8801")
                    else txt ""
                ]
    w = padRight Max (hBox [lead, txt " ", body])
  in
    if isSel then withDefAttr floatSelAttr w else w

treeLayer :: St -> TreeView -> Widget Name
treeLayer st view =
  Widget Greedy Greedy $ do
    ctx <- getContext
    let
      boxWidth = min 96 (max 60 (availWidth ctx - 16))
      -- as tall as the tree needs and no taller: a folded tree should not
      -- open a full-height box of nothing
      roomFor = max 6 (availHeight ctx - 14)
      visible = treeVisible st view
      visibleCount = max 6 (min roomFor (length visible))
      sel = max 0 (min (length visible - 1) (tvSel view))
      start =
        max 0 (min (sel - div visibleCount 2) (max 0 (length visible - visibleCount)))
      -- when the enclosing directory (or file, for a hunk) has scrolled
      -- off, pin it: otherwise a row loses all sense of whose it is
      shown = take visibleCount (drop start visible)
      sticky = do
        top <- safeIdx shown 0
        parent <- trParent top
        find ((== parent) . trPath) (reverse (take start visible))
      body = case sticky of
        Just parent ->
          withDefAttr floatSelAttr (treeRowWidget st False parent)
            : map row (drop 1 (zip [start ..] shown))
        Nothing -> map row (zip [start ..] shown)
      row (i, r) = treeRowWidget st (i == sel) r
      counter =
        if T.null (tvInput view)
          then tshow (length (tvAll view)) <> " files"
          else -- files that matched, not rows: the directories kept to
          -- hold them are structure, not results
            tshow (treeFileCount visible) <> "/" <> tshow (length (tvAll view))
      inputRow =
        hBox
          [ withAttr statusAttr (txt (" " <> tvInput view <> "\9612"))
          , padLeft Max (withAttr hintAttr (txt (counter <> " ")))
          ]
      filler = replicate (visibleCount - length body) (txt " ")
      hint =
        if T.null (tvInput view)
          then "\8593\8595 move \183 \8594\8592 fold \183 \9166 open \183 esc closes"
          else "\8593\8595 move \183 \9166 open \183 esc clears the filter"
    render $
      floatFrame "tree" hint $
        hLimit boxWidth (vBox (inputRow : txt " " : (body ++ filler)))

-- Accept everything under a directory (or one file) as reviewed — the
-- bulk verb for generated code nobody reads line by line. Matching is
-- by path component, so `gen` covers gen/a.txt but not generated.hs.
acceptPath :: Text -> EventM Name St ()
acceptPath rawPath = do
  st <- get
  let
    prefix =
      dropTrailingPathSeparator
        (normalise (makeRelative (stRoot st) (T.unpack rawPath)))
    matches fv =
      fvPath fv == prefix || (prefix ++ "/") `isPrefixOf` fvPath fv
    targets = filter matches (stFiles st)
    fileRows fv =
      let
        hunks = [0 .. length (fvRuns fv) - 1]
        perRun h =
          map
            (\(k, l, n) -> (k, T.pack (fvPath fv), l, n))
            (runChanges fv h)
      in
        concatMap perRun hunks
    rows = concatMap fileRows targets
    keys = map (\(k, _, _, _) -> k) rows
    hunkCount = sum (map (length . fvRuns) targets)
  if null targets
    then setMsg ("nothing in the change set under " <> T.pack prefix)
    else do
      mCommit <- verdictCommitContext
      liftIO
        ( recordVerdicts
            (stConn st)
            (repoText st)
            (stPassId st)
            Reviewed
            mCommit
            rows
        )
      resolveNotesInMemory keys
      modify $ \s ->
        s
          { stVerdicts =
              foldr (\k m -> Map.insert k Reviewed m) (stVerdicts s) keys
          , stCounts = Map.insertWith (+) Reviewed hunkCount (stCounts s)
          }
      advanceToPending
      setMsg
        ( "accepted "
            <> tshow hunkCount
            <> " hunk(s) in "
            <> tshow (length targets)
            <> " file(s) under "
            <> T.pack prefix
        )

-- Files beyond the change set ---------------------------------------------------

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
    rel =
      normalise
        (makeRelative rootP (if isAbsolute given then given else rootP </> given))
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
              { stFiles = stFiles s ++ [fv]
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
                , fvRuns = []
                , fvToks = []
                , fvStale = False
                , fvHighlighted = False
                , -- the shebang is the fallback for a file with no
                  -- extension to go on, so the first line comes along
                  fvHl = TS.highlighterFor hls path (listToMaybe newLs)
                , fvRunDecls = []
                }
          (fv, _) <- rebuildFV (cfgSplit cfg) (T.pack root) fv0
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

openPicker :: EventM Name St ()
openPicker = do
  st <- get
  files <- liftIO (listFiles (stRoot st))
  modify $ \s ->
    s {stPicker = Just (refreshPicker (Picker PickFiles "" files [] [] 0))}

-- The picker narrowed to what the session already has open.
openBufferPicker :: EventM Name St ()
openBufferPicker = do
  st <- get
  modify $ \s ->
    s
      { stPicker =
          Just (refreshPicker (Picker PickFiles "" (map fvPath (stFiles st)) [] [] 0))
      }

-- Lay cells out in real columns: each leading column as wide as its
-- widest cell up to a cap, anything longer clipped, so one long branch
-- name or ref decoration can never shove its neighbours out of line.
-- The final cell runs free.
columnRow :: [Int] -> [Text] -> Text
columnRow widths cells =
  let
    clip w t = if T.length t <= w then t else T.take (max 1 (w - 1)) t <> "\8230"
    lead = zipWith (\w c -> T.justifyLeft (w + 2) ' ' (clip w c)) widths cells
    rest = drop (length widths) cells
  in
    T.concat lead <> T.intercalate " " rest

-- Column widths measured from the data, each capped so a single outlier
-- cannot stretch the whole table.
columnWidths :: [Int] -> [[Text]] -> [Int]
columnWidths caps rows =
  let
    at i r = fromMaybe "" (safeIdx r i)
    widest i = maximum (1 : map (T.length . at i) rows)
  in
    zipWith (\cap i -> min cap (widest i)) caps [0 ..]

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

-- Case-insensitive subsequence match; lower scores first. Prefers
-- matches inside the file name, then tight spans, then short paths.
fuzzyScore :: Text -> FilePath -> Maybe (Int, Int, Int)
fuzzyScore q path
  | T.null q = Just (0, 0, length path)
  | otherwise =
      let
        ql = map toLower (T.unpack q)
        pl = map toLower path
        base = map toLower (takeFileName path)
        walk needle hay firstPos lastPos =
          case (needle, hay) of
            ([], _) -> Just (firstPos, lastPos)
            (_, []) -> Nothing
            (c : cs, (i, h) : hs) ->
              if c == h
                then walk cs hs (if firstPos < 0 then i else firstPos) i
                else walk needle hs firstPos lastPos
      in
        case walk ql (zip [0 ..] pl) (-1) (-1) of
          Nothing -> Nothing
          Just (firstPos, lastPos) ->
            Just
              ( if ql `isSubsequenceOfStr` base then 0 else 1
              , (lastPos - firstPos) - (length ql - 1)
              , length path
              )

isSubsequenceOfStr :: String -> String -> Bool
isSubsequenceOfStr needle hay =
  case needle of
    [] -> True
    (c : cs) ->
      case dropWhile (/= c) hay of
        [] -> False
        (_ : rest) -> isSubsequenceOfStr cs rest

handlePickerKey :: V.Event -> Picker -> EventM Name St ()
handlePickerKey ev pk =
  let
    move :: Int -> EventM Name St ()
    move d =
      let
        bound = max 0 (length (pkMatches pk) - 1)
        next = max 0 (min bound (pkSel pk + d))
      in
        modify (\s -> s {stPicker = Just pk {pkSel = next}})
    retype :: Text -> EventM Name St ()
    retype q =
      case pkMode pk of
        PickFiles ->
          modify
            (\s -> s {stPicker = Just (refreshPicker pk {pkInput = q})})
        PickBase ->
          modify
            (\s -> s {stPicker = Just (refreshRowPicker pk {pkInput = q})})
        PickBranch ->
          modify
            (\s -> s {stPicker = Just (refreshRowPicker pk {pkInput = q})})
        PickGrep -> do
          st <- get
          (matches, targets) <- liftIO (grepRepo (stRoot st) q)
          modify $ \s ->
            s
              { stPicker =
                  Just
                    pk
                      { pkInput = q
                      , pkMatches = matches
                      , pkTargets = targets
                      , pkSel = 0
                      }
              }
  in
    case ev of
      V.EvKey V.KEsc [] -> modify (\s -> s {stPicker = Nothing})
      V.EvKey V.KEnter [] -> do
        modify (\s -> s {stPicker = Nothing})
        case pkMode pk of
          PickBase ->
            case safeIdx (pkTargets pk) (pkSel pk) of
              Just (row, _) -> applyBaseChoice (T.pack row)
              Nothing -> pure ()
          PickBranch ->
            case safeIdx (pkTargets pk) (pkSel pk) of
              Just (row, _) -> applyCheckout (T.pack row)
              Nothing -> pure ()
          _ -> case safeIdx (pkTargets pk) (pkSel pk) of
            Just (path, 0) -> openFilePath (T.pack path)
            Just (path, line) -> do
              modify $ \s ->
                s {stGrepNav = Just (pkInput pk, pkTargets pk, pkSel pk)}
              openGrepHit path line (pkInput pk)
            Nothing -> pure ()
      V.EvKey V.KUp [] -> move (-1)
      V.EvKey (V.KChar 'p') [V.MCtrl] -> move (-1)
      V.EvKey V.KDown [] -> move 1
      V.EvKey (V.KChar 'n') [V.MCtrl] -> move 1
      V.EvKey V.KBS [] -> retype (T.dropEnd 1 (pkInput pk))
      V.EvKey (V.KChar c) [] -> retype (T.snoc (pkInput pk) c)
      _ -> pure ()

-- The overlay stretches to nearly the pane width, so grep hits keep
-- their line context instead of losing it to a narrow box.
pickerLayer :: Picker -> Widget Name
pickerLayer pk =
  Widget Greedy Greedy $ do
    ctx <- getContext
    let
      boxWidth = min 150 (max 50 (availWidth ctx - 16))
      visibleCount = max 5 (availHeight ctx - 14)
      start = max 0 (pkSel pk - (visibleCount - 1))
      shown = take visibleCount (drop start (zip [0 ..] (pkMatches pk)))
      matchRow (i, display) =
        let
          shownText = T.take (boxWidth - 4) display
          content = case pkMode pk of
            PickFiles -> highlightSubseq (pkInput pk) shownText
            PickBase -> highlightSubseq (pkInput pk) shownText
            PickBranch -> highlightSubseq (pkInput pk) shownText
            PickGrep -> highlightInfix (pkInput pk) shownText
          line = padRight Max (txt " " <+> content)
        in
          if i == pkSel pk
            then
              withDefAttr floatSelAttr $
                withAttr floatSelBarAttr (txt "\9614") <+> line
            else txt " " <+> line
      counter = case pkMode pk of
        PickFiles ->
          tshow (length (pkMatches pk)) <> "/" <> tshow (length (pkAll pk))
        PickBase ->
          tshow (length (pkMatches pk)) <> "/" <> tshow (length (pkAll pk))
        PickBranch ->
          tshow (length (pkMatches pk)) <> "/" <> tshow (length (pkAll pk))
        PickGrep -> tshow (length (pkMatches pk)) <> " hits"
      title = case pkMode pk of
        PickFiles -> "files"
        PickBase -> "base"
        PickBranch -> "checkout"
        PickGrep -> "grep"
      inputRow =
        hBox
          [ withAttr statusAttr (txt (" " <> pkInput pk <> "\9612"))
          , padLeft Max (withAttr hintAttr (txt (counter <> " ")))
          ]
      emptyRow = withAttr hintAttr (txt "  nothing matches")
      matchRows =
        if null shown
          then emptyRow : replicate (visibleCount - 1) (txt " ")
          else
            map matchRow shown
              ++ replicate (visibleCount - length shown) (txt " ")
      pickerHint = case pkMode pk of
        PickBase -> "\8593\8595 move · \9166 review against this · esc closes"
        PickBranch -> "\8593\8595 move · \9166 checks out · esc closes"
        _ -> "\8593\8595 move · \9166 open · esc closes"
    render $
      floatFrame title pickerHint $
        hLimit boxWidth (vBox (inputRow : txt " " : matchRows))

-- Where each query character lands in the shown text (greedy,
-- case-insensitive); Nothing when the text no longer contains the
-- subsequence, as with an elided path.
subseqPositions :: Text -> Text -> Maybe [Int]
subseqPositions q t =
  let
    walk needle hay acc =
      case (needle, hay) of
        ([], _) -> Just (reverse acc)
        (_, []) -> Nothing
        (c : cs, (i, h) : hs) ->
          if toLower c == toLower h
            then walk cs hs (i : acc)
            else walk (c : cs) hs acc
  in
    walk (T.unpack q) (zip [0 ..] (T.unpack t)) []

highlightSubseq :: Text -> Text -> Widget Name
highlightSubseq q t =
  case subseqPositions q t of
    Just ps
      | not (null ps) -> highlightAt (Set.fromList ps) t
    _ -> txt (padT t)

highlightInfix :: Text -> Text -> Widget Name
highlightInfix q t =
  let
    (pre, rest) = T.breakOn q t
  in
    if T.null q || T.null rest
      then txt (padT t)
      else
        hBox
          [ txt pre
          , withAttr matchCharAttr (txt (T.take (T.length q) rest))
          , txt (T.drop (T.length q) rest)
          ]

highlightAt :: Set.Set Int -> Text -> Widget Name
highlightAt ps t =
  let
    marked = zipWith (\i c -> (Set.member i ps, c)) [0 ..] (T.unpack t)
    groups = groupBy (\a b -> fst a == fst b) marked
    seg g = case g of
      [] -> txt ""
      ((hit, _) : _) ->
        let
          segText = T.pack (map snd g)
        in
          if hit
            then withAttr matchCharAttr (txt segText)
            else txt segText
  in
    hBox (map seg groups)

-- The editor escape hatch -----------------------------------------------------------

editCurrent :: EventM Name St ()
editCurrent = do
  st <- get
  case currentFileMay st of
    Nothing -> setMsg "no file on screen — :edit PATH opens one"
    Just fv -> do
      let
        (row, _) = cursorRC (fvEd fv)
      launchEditor (stRoot st </> fvPath fv) (row + 1)

-- Suspend the TUI, run the user's editor on the file at the given line,
-- and come back; the watcher reloads whatever the editor wrote.
launchEditor :: FilePath -> Int -> EventM Name St ()
launchEditor file line = do
  st <- get
  suspendAndResume $ do
    (cmd, args) <- editorInvocation file line
    got <-
      ( try $ do
          (_, _, _, ph) <- createProcess (proc cmd args) {delegate_ctlc = True}
          void (waitForProcess ph)
      ) ::
        IO (Either IOException ())
    pure $ case got of
      Left _ -> st {stMsg = "could not launch " <> T.pack cmd}
      Right () -> st

{- $EDITOR, hx-aware: helix takes file:line, the classic editors take
+line, anything unrecognized just gets the file.
-}
editorInvocation :: FilePath -> Int -> IO (String, [String])
editorInvocation file line = do
  mEd <- lookupEnv "EDITOR"
  let
    editorWords = words (fromMaybe "hx" mEd)
    (cmd, extraArgs) = case editorWords of
      (c : extra) -> (c, extra)
      [] -> ("hx", [])
    name = takeFileName cmd
    positional
      | name `elem` ["hx", "helix"] = [file ++ ":" ++ show line]
      | name `elem` ["vi", "vim", "nvim", "nano", "emacs", "kak"] =
          ["+" ++ show line, file]
      | otherwise = [file]
  pure (cmd, extraArgs ++ positional)

-- Attributes -----------------------------------------------------------------------

headerAttr
  , headerFileAttr
  , headerOkAttr
  , cursorMarkAttr
  , addedMarkAttr
  , removedMarkAttr
  , ctxMarkAttr
  , removedTextAttr
  , selAttr
  , dimAttr
  , curRunAttr
  , norAttr
  , insAttr
  , msgAttr
  , hintAttr
  , statusAttr
  , borderAttr
  , lineNrAttr
  , lineNrCurAttr
  , trayFileAttr
  , trayFileCurAttr
  , flagMarkAttr
  , questionMarkAttr
  , doneMarkAttr
  , pendingMarkAttr
  , lineNrDoneAttr
  , headerRebaseAttr
  , trayDeclAttr
  , headerCountAttr
  , rebaseShaAttr
  , conflictAttr
  , watchPollAttr
  , modeDotNorAttr
  , modeDotInsAttr
  , noteInlineAttr
  , replyInlineAttr
  , floatAttr
  , floatSelAttr
  , floatSelBarAttr
  , floatTitleAttr
  , matchCharAttr
  , meterOnAttr
  , meterOffAttr
  , hairAttr ::
    AttrName
headerAttr = attrName "header"
headerFileAttr = headerAttr <> attrName "file"
headerOkAttr = headerAttr <> attrName "ok"
headerRebaseAttr = headerAttr <> attrName "rebase"
trayDeclAttr = attrName "trayDecl"
headerCountAttr = headerAttr <> attrName "count"
rebaseShaAttr = attrName "rebaseSha"
conflictAttr = attrName "conflict"
watchPollAttr = attrName "watchPoll"
modeDotNorAttr = attrName "modeDotNor"
modeDotInsAttr = attrName "modeDotIns"
noteInlineAttr = attrName "noteInline"
replyInlineAttr = attrName "replyInline"
floatAttr = attrName "float"
floatSelAttr = floatAttr <> attrName "sel"
floatSelBarAttr = floatAttr <> attrName "selBar"
floatTitleAttr = floatAttr <> attrName "title"
matchCharAttr = attrName "matchChar"
meterOnAttr = attrName "meterOn"
meterOffAttr = attrName "meterOff"
hairAttr = attrName "hairline"
cursorMarkAttr = attrName "cursorMark"
addedMarkAttr = attrName "addedMark"
removedMarkAttr = attrName "removedMark"
ctxMarkAttr = attrName "ctxMark"
removedTextAttr = attrName "removedText"
selAttr = attrName "selection"
dimAttr = attrName "dim"
curRunAttr = attrName "currentRun"
norAttr = attrName "modeNor"
insAttr = attrName "modeIns"
msgAttr = attrName "message"
hintAttr = attrName "hint"
statusAttr = attrName "statusBar"
borderAttr = attrName "paneBorder"
lineNrAttr = attrName "lineNr"
lineNrCurAttr = lineNrAttr <> attrName "current"
lineNrDoneAttr = lineNrAttr <> attrName "reviewed"
trayFileAttr = attrName "trayFile"
trayFileCurAttr = trayFileAttr <> attrName "current"
flagMarkAttr = attrName "flagMark"
questionMarkAttr = attrName "questionMark"
doneMarkAttr = attrName "doneMark"
pendingMarkAttr = attrName "pendingMark"

-- A theme scope may carry a background; the border glyphs must not, or
-- the whole cell fills in and the thin bar reads as a thick one.
fgOnly :: V.Attr -> V.Attr
fgOnly a = a {V.attrBackColor = V.Default}

-- Helix-style scopes the tree-sitter path colors: top-level entries carry
-- built-in fallback colors so non-helix users get a sane palette, and
-- theme users additionally get every deeper scope their theme resolves
-- (brick's attribute hierarchy falls back through the dots on its own).
tsTopScopes :: [(Text, V.Color)]
tsTopScopes =
  [ ("keyword", V.magenta)
  , ("function", V.blue)
  , ("type", V.cyan)
  , ("constructor", V.yellow)
  , ("constant", V.brightCyan)
  , ("string", V.green)
  , ("comment", V.brightBlack)
  , ("operator", V.yellow)
  , ("namespace", V.brightMagenta)
  , ("label", V.yellow)
  , ("attribute", V.brightMagenta)
  , ("special", V.yellow)
  , ("tag", V.cyan)
  ]

tsDeepScopes :: [Text]
tsDeepScopes =
  [ "keyword.control"
  , "keyword.control.import"
  , "keyword.control.exception"
  , "keyword.operator"
  , "keyword.directive"
  , "function.method"
  , "function.macro"
  , "function.builtin"
  , "function.special"
  , "type.builtin"
  , "type.enum.variant"
  , "constant.builtin"
  , "constant.builtin.boolean"
  , "constant.character"
  , "constant.character.escape"
  , "constant.numeric"
  , "string.special"
  , "string.regexp"
  , "comment.block.documentation"
  , "variable"
  , "variable.builtin"
  , "variable.parameter"
  , "variable.other.member"
  , "punctuation"
  , "punctuation.bracket"
  , "punctuation.delimiter"
  , "punctuation.special"
  , "markup.heading"
  , "markup.bold"
  , "markup.italic"
  , "markup.list"
  , "markup.quote"
  , "markup.link.url"
  , "markup.link.text"
  , "markup.raw"
  ]

mkAttrMap :: Maybe HelixTheme -> AttrMap
mkAttrMap mth =
  let
    topPair (s, c) = (tsScopeAttr s, scopeAttr mth s (fg c))
    deepPairs = case mth of
      Nothing -> []
      Just th ->
        map
          (\s -> (tsScopeAttr s, scopeAttr mth s V.defAttr))
          (filter (hasScope th) tsDeepScopes)
    tsPairs = map topPair tsTopScopes ++ deepPairs
    baseAttr =
      let
        bgA = scopeAttr mth "ui.background" V.defAttr
        fgA = scopeAttr mth "ui.text" V.defAttr
      in
        V.defAttr
          { V.attrForeColor = V.attrForeColor fgA
          , V.attrBackColor = V.attrBackColor bgA
          }
    selection =
      scopeAttr mth "ui.selection.primary" (V.defAttr `V.withStyle` V.reverseVideo)
    uiPairs =
      [ (headerAttr, fg inkDim)
      , (headerRebaseAttr, fg inkText)
      , (headerCountAttr, fg inkText)
      , (rebaseShaAttr, fg inkMidBlue)
      , (conflictAttr, fg inkRose `V.withStyle` V.bold)
      , (watchPollAttr, fg inkGold)
      , (trayDeclAttr, fg inkMidBlue)
      , (headerFileAttr, fg inkFileGold)
      , (headerOkAttr, fg inkGreen)
      , (cursorMarkAttr, V.defAttr `V.withBackColor` cursorWash)
      , (addedMarkAttr, fg inkGreen)
      , (removedMarkAttr, fg inkRose)
      , (ctxMarkAttr, fg inkFaint)
      , (removedTextAttr, fg inkRose)
      , (selAttr, selection)
      , (dimAttr, fg inkDim)
      , (curRunAttr, fg inkMidBlue)
      , (norAttr, fg inkText)
      , (insAttr, fg inkText)
      , (modeDotNorAttr, fg inkLavender)
      , (modeDotInsAttr, fg inkGreen)
      , (msgAttr, fg inkText)
      , (hintAttr, fg inkFaint)
      , (statusAttr, fg inkText)
      , (lineNrAttr, scopeAttr mth "ui.linenr" (fg V.brightBlack))
      , (lineNrCurAttr, scopeAttr mth "ui.linenr.selected" (fg V.yellow))
      , (lineNrDoneAttr, scopeAttr mth "ui.linenr" (fg V.brightBlack) `V.withStyle` V.dim)
      , (borderAttr, fgOnly (scopeAttr mth "ui.linenr" (fg V.brightBlack)))
      , (trayFileAttr, fg trayFileInk `V.withStyle` V.bold)
      , (trayFileCurAttr, fg inkFileGold `V.withStyle` V.bold)
      , (flagMarkAttr, fg inkFlag)
      , (questionMarkAttr, fg inkGold)
      , (doneMarkAttr, fg inkGreen)
      , (pendingMarkAttr, fg inkBlue)
      , (noteInlineAttr, fg inkLavender)
      , (replyInlineAttr, fg inkMidBlue)
      , (floatAttr, V.defAttr `V.withBackColor` surfFloat)
      , (floatSelAttr, V.defAttr `V.withBackColor` surfSel)
      , (floatSelBarAttr, fg inkLavender `V.withBackColor` surfSel)
      , (floatTitleAttr, fg inkLavender `V.withBackColor` surfFloat)
      , -- every popup is framed in the same lavender as its title; floatFrame
        -- is the only bordered widget, so this reaches exactly the popups
        (B.borderAttr, fg inkLavender `V.withBackColor` surfFloat)
      , (matchCharAttr, fg matchGold)
      , (meterOnAttr, fg inkGreen)
      , (meterOffAttr, fg meterRail)
      , (hairAttr, fg hairInk)
      ]
  in
    attrMap baseAttr (tsPairs ++ uiPairs)

-- The Quiet Ink · Tinted palette: jellybeans-adjacent literals, chosen
-- in the temperaments mockups. linearColor keeps true 24-bit values;
-- vty approximates on lesser terminals.
inkRose, inkBlue, inkGold, inkGreen, inkLavender, inkMidBlue :: V.Color
inkFileGold, inkDim, inkFaint, inkText, inkFlag, trayFileInk :: V.Color
surfFloat, surfSel, cursorWash, hairInk, meterRail, matchGold :: V.Color
inkRose = V.linearColor (0xa2 :: Int) 0x70 0x7c
inkBlue = V.linearColor (0x7f :: Int) 0x9e 0xc7
inkGold = V.linearColor (0xd3 :: Int) 0xb0 0x78
inkGreen = V.linearColor (0x84 :: Int) 0xa7 0x75
inkLavender = V.linearColor (0xa3 :: Int) 0x90 0xf0
inkMidBlue = V.linearColor (0x81 :: Int) 0x97 0xbf
inkFileGold = V.linearColor (0xe8 :: Int) 0xd7 0xa7
inkDim = V.linearColor (0x8b :: Int) 0x8b 0x84
inkFaint = V.linearColor (0x6e :: Int) 0x6e 0x68
inkText = V.linearColor (0xb4 :: Int) 0xb4 0xaa
inkFlag = V.linearColor (0xcf :: Int) 0x8a 0x6a
trayFileInk = V.linearColor (0xd8 :: Int) 0xd3 0xc0
surfFloat = V.linearColor (0x1e :: Int) 0x1e 0x1e
surfSel = V.linearColor (0x24 :: Int) 0x24 0x30
cursorWash = V.linearColor (0x3a :: Int) 0x33 0x25
hairInk = V.linearColor (0x2c :: Int) 0x2c 0x2c
meterRail = V.linearColor (0x3a :: Int) 0x3f 0x3a
matchGold = V.linearColor (0xfa :: Int) 0xd0 0x7a
