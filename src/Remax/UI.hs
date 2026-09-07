{-# LANGUAGE BangPatterns #-}
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
  , attrMapLookup
  , attrName
  , customMain
  , fill
  , getVtyHandle
  , hBox
  , hLimit
  , halt
  , image
  , lookupViewport
  , padBottom
  , padLeft
  , padRight
  , padTop
  , raw
  , setDefaultAttr
  , showCursor
  , showFirstCursor
  , str
  , suspendAndResume
  , translateBy
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
import Brick.Types (Size (Fixed, Greedy), Widget (Widget), availHeight, availWidth, ctxAttrMap, ctxAttrName, getContext, render, windowHeight)
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
import Control.Monad (forM, forM_, forever, guard, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, modify, put)
import Data.Algorithm.Diff (PolyDiff (..), getGroupedDiff)
import Data.Char (chr, isAlphaNum, isSpace, ord, toLower)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List
  ( find
  , findIndex
  , groupBy
  , isPrefixOf
  , isSuffixOf
  , maximumBy
  , sortOn
  )
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
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , removeDirectoryRecursive
  , removeFile
  , renamePath
  )
import System.Environment (lookupEnv)
import System.Exit (die)
import qualified System.FSNotify as FS
import System.FilePath
  ( dropTrailingPathSeparator
  , isAbsolute
  , isPathSeparator
  , joinPath
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

import Remax.Clipboard
import Remax.Config
import Remax.DiffModel
import Remax.Emacs (handleEmacs)
import Remax.Git
  ( FileScope (..)
  , PathChange (..)
  , PathStat (..)
  , RebaseStatus (..)
  , ReviewMode (..)
  , baseLines
  , branchRoot
  , buildOutputDirs
  , changedFiles
  , childRepos
  , commitBody
  , commitMessage
  , currentBranch
  , gitStateWatchDirs
  , hasStagedChanges
  , headCommit
  , isTracked
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
  , reviewFiles
  , runGit
  , trackedChanges
  )
import Remax.Highlight
import Remax.Modal
import Remax.Store
import Remax.Theme
import Remax.Tree
import qualified Remax.TreeSitter as TS
import Remax.Vi (ViKeys, ViResult (..), handleVi, viBusy, viIdle, viLabel)

data Name = BodyVP | TrayVP | TheCursor
  deriving (Eq, Ord, Show)

-- FileChangedOnDisk carries an absolute path: the watcher callback is
-- created once and outlives any number of :repo switches, so it cannot
-- know which root to make a path relative to. The handler, which does
-- know, relativizes on arrival.
data REvent
  = FileChangedOnDisk FilePath
  | GitStateChanged
  | StoreChanged
  | -- repositories an agent is waiting on and working on, and a beat
    -- to animate by
    Attending ([Text], [Text])
  | Tick

-- The one-line prompt at the bottom of the screen: / search or : command.
data Prompt = SearchPrompt Text | CmdPrompt Text

-- The finder overlay: fuzzy file picking, or project-wide grep. Each
-- display row is aligned with a jump target (file, and a 1-based line
-- for grep hits; 0 = just open the file).
data PickerMode = PickFiles | PickGrep | PickBase | PickBranch | PickRepo

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
-- Your place in a repository you have stepped away from.
--
-- Buffers are deliberately not kept. Coming back reloads from disk,
-- because the reason you left may well have been to let an agent work
-- there, and a stale buffer would be worse than no memory at all. A
-- rebase in progress needs no remembering either — it lives in .git and
-- is read back on arrival. What is worth keeping is where you were
-- looking and what you had asked to see.
data RepoPlace = RepoPlace
  { rpMode :: ReviewMode
  , rpFile :: FilePath
  , rpHunk :: Int
  , rpOnlyPending :: Bool
  , rpRunFilter :: RunFilter
  , rpOpened :: Set.Set FilePath
  , rpAnchor :: Maybe (FilePath, Int)
  }

-- Which hunks the session is walking. Orthogonal to the pending filter,
-- so "the code hunks I have not looked at yet" is reachable.
data RunFilter = FilterAll | FilterCode | FilterComments
  deriving (Eq)

filterCycle :: [RunFilter]
filterCycle = [FilterAll, FilterCode, FilterComments]

runFilterLabel :: RunFilter -> Text
runFilterLabel f =
  case f of
    FilterAll -> "all"
    FilterCode -> "code"
    FilterComments -> "comments"

-- Cycles all → code → comments → all.
nextRunFilter :: RunFilter -> RunFilter
nextRunFilter f =
  case f of
    FilterAll -> FilterCode
    FilterCode -> FilterComments
    FilterComments -> FilterAll

runFilterAllows :: RunFilter -> Run -> Bool
runFilterAllows f r =
  case f of
    FilterAll -> True
    FilterCode -> runKind r == RunCode
    FilterComments -> runKind r == RunComment

data PopupPurpose
  = NoteFor [Text] NoteOrigin
  | -- the paths that will be staged along with the message, so the popup
    -- can show them before you commit to them (empty at a rebase stop,
    -- where :continue owns the folding)
    AmendMessage [FilePath]

-- Why a note popup is open, which decides what escaping out of it means.
--
-- Asked for by :note, the mark was already there and stays. Opened by !
-- or ?, the mark was made a moment ago by the same keystroke and has not
-- been thought about yet — backing out of the sentence backs out of the
-- verdict it was going to explain.
data NoteOrigin
  = NoteAsked
  | NoteFromMark MarkUndo

-- What a mark changed, kept so that escaping out of the note it opened
-- can put it back exactly.
data MarkUndo = MarkUndo
  { muRows :: [(Text, Text, Text, Int)] -- the rows it wrote
  , muBefore :: Map.Map Text (Maybe Verdict) -- what those keys were
  , muCounts :: Map.Map Verdict Int -- the session tallies before it
  , muPunch :: [(FilePath, Int, Verdict)]
  }

data FileView = FileView
  { fvPath :: FilePath
  , fvBase :: [Text]
  , fvEd :: EdState
  , fvRows :: [RowR]
  , -- Which columns of each row the word diff marks, one entry per row.
    -- Worked out for whole blocks rather than for the rows on screen:
    -- pairing needs the removals and the additions it chooses between,
    -- and a block cut off by the edge of the render window offers the
    -- tail of one against the head of the other, which pairs lines that
    -- have nothing to do with each other. Left lazy, so a file nothing
    -- has looked at pays nothing, and rebuilt with the rows it belongs
    -- to, which is what keeps it from going stale.
    fvEmph :: [[(Int, Int)]]
  , fvRowCount :: !Int -- length fvRows; wanted twice a frame, so carried
  , fvOrigin :: !Int -- first display row the body draws; see bodyWindow
  , fvRuns :: [Run]
  , fvToks :: [[Seg]]
  , fvStale :: Bool
  , fvDeleted :: Bool -- the change set removed this file; there is no working copy
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
  , stRunFilter :: RunFilter -- walk everything, only code, or only prose
  , stFileScope :: FileScope -- does the file dialog list ignored files too
  , stRebase :: Maybe RebaseStatus -- a paused rebase the session is following
  , stWritten :: Set.Set FilePath -- files written since the last commit-shaped action
  , stLastMarked :: [Text] -- keys of the most recent verdict, for :note
  , stLastRows :: [(Text, Text, Text, Int)] -- and the rows it wrote, to undo it
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
  , stIdleGo :: Bool -- a pending g with no file open to hold it
  , stComposing :: Maybe Text -- repository we told the store is mid-note
  , stAttending :: ([Text], [Text]) -- repositories agents wait on / work on
  , stThrob :: !Int -- which frame the watching indicator is on
  , stVi :: ViKeys -- a half-typed vi command: count, operator, what it awaits
  , stSearchDir :: Int -- which way / (or vi's ?) last searched: 1 or -1
  , stDeferAdvance :: Bool -- hold the post-mark jump until the note popup closes
  , stAltFile :: Maybe (FilePath, Int) -- where Space a bounces back to
  , stAnchor :: Maybe (FilePath, Int) -- the hunk under review; ' returns here
  , stGrepNav :: Maybe (Text, [(FilePath, Int)], Int) -- grep hits n/N walk
  , stInfoPopup :: Maybe (Text, Text) -- read-only text popup: title, body
  , stKill :: Text -- session-wide kill/yank slot, shared across files
  , stClip :: Clipboard -- how this machine reaches the system clipboard
  , stClipSent :: Text -- the text remax and the clipboard last agreed on
  , stPolling :: Bool -- the native watcher was refused; polling instead
  , stBranch :: Maybe Text -- Nothing when HEAD is detached
  , stHead :: Maybe (Text, Text) -- (short sha, subject) of HEAD
  , stTree :: Maybe TreeView -- the project tree overlay, while it is open
  , stHelp :: Bool -- the keybinding cheat sheet is up
  , stRoots :: [FilePath] -- repositories this session can switch between
  , stStory :: Maybe FilePath -- the folder they sit in, when this is a story
  , stPlaces :: Map.Map FilePath RepoPlace -- where you were in each
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

-- Watch every directory the repository keeps a file in, not only the
-- ones a file is changed in today.
--
-- Watching where the changes are cannot notice the first change anywhere
-- else: an edit in a directory nothing had changed in reached nobody, so
-- no refresh ran, so the directory never got watched, so the next edit
-- there reached nobody either. The file simply sat there until something
-- else made the session look.
--
-- Taken from git rather than by walking the tree, so build directories
-- and everything else ignored stay out of it — that walk is what makes a
-- recursive watch cost more than it is worth. In the background, because
-- a large repository is thousands of directories and none of them is
-- worth making somebody wait for.
watchTracked :: (FilePath -> IO ()) -> FilePath -> IO ()
watchTracked watch root =
  void . forkIO $ do
    tracked <- listFiles VisibleFiles root
    mapM_ (watch . fileDir root) tracked

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

runReview :: ReviewMode -> FilePath -> [FilePath] -> Maybe FilePath -> IO ()
runReview mode root roots story = do
  conn <- openStore
  -- a session killed with a note popup open leaves the marker behind;
  -- starting one is as good a moment as any to say it is not being
  -- written any more
  setComposing conn (T.pack root) False
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
          got <-
            try (FS.watchDir mgr dir (const True) (dispatchFsEvent chan root)) ::
              IO (Either IOException FS.StopListening)
          -- remembered only when it took: a directory whose watch was
          -- refused (inotify full, or gone between listing and watching)
          -- is worth trying again the next time it comes up, and marking
          -- it seen regardless meant it never was
          case got of
            Right _ -> modifyIORef' watchedRef (Set.insert dir)
            Left _ -> pure ()
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
    watchTracked watchDirOnce root
    -- probed once: doing it per yank would be a process spawn a keystroke
    clip <- discoverClipboard
    -- and read once on the way in, so whatever was already on the
    -- clipboard counts as seen. Without this the first paste of a
    -- session would treat months-old clipboard contents as newer than
    -- the line you just cut, and `dd p` would not put the line back.
    alreadyThere <- readClipboard clip
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
          , stRunFilter = FilterAll
          , stFileScope = VisibleFiles
          , stRebase = rebase0
          , stWritten = Set.empty
          , stLastMarked = []
          , stLastRows = []
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
          , stIdleGo = False
          , stComposing = Nothing
          , stAttending = ([], [])
          , stThrob = 0
          , stVi = viIdle
          , stSearchDir = 1
          , stDeferAdvance = False
          , stAltFile = Nothing
          , stAnchor = Nothing
          , stGrepNav = Nothing
          , stInfoPopup = Nothing
          , stKill = T.empty
          , stClip = clip
          , stClipSent = fromMaybe T.empty alreadyThere
          , stPolling = polling
          , stBranch = branch0
          , stHead = head0
          , stTree = Nothing
          , stHelp = False
          , stRoots = roots
          , stStory = story
          , stPlaces = Map.empty
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
    -- Who is waiting on this review, asked for on a beat.
    --
    -- The beat is the animation's as well as the question's, so it runs
    -- quickly while somebody is there and slowly while nobody is: an
    -- empty directory read every second and a half is not worth a
    -- watcher, and a session with no agent on it should not be waking up
    -- to redraw something that is not on screen.
    void . forkIO . forever $ do
      seen@(waiting, working) <- presenceNow
      void (writeBChanNonBlocking chan (Attending seen))
      threadDelay
        (if null waiting && null working then 1500000 else 420000)
    -- terminfo often claims only 256 colors even when the terminal
    -- speaks 24-bit; trust COLORTERM the way helix does, or theme
    -- colors get rounded to the nearest palette entry
    let
      buildVty = do
        full <- wantsFullColor
        let
          vtyCfg =
            if full
              then V.defaultConfig {V.configPreferredColorMode = Just V.FullColor}
              else V.defaultConfig
        mkVty vtyCfg
    vty <- buildVty
    customMain vty buildVty (Just chan) (app theme) st1
  finishPass conn (stPassId final)
  printSummary final

-- Whether to ask vty for 24-bit color.
--
-- COLORTERM alone is too thin a thread: it is a convention each shell
-- and multiplexer has to remember to pass along, and one that does not
-- (switching to a shell that manages its environment differently is
-- enough) drops the whole UI into palette mode. That is not a subtle
-- degradation — a popup's #151515 surface rounds to #5f5f5f, a mid-grey,
-- and the theme stops being the theme.
--
-- So ask the terminal as well. These emulators are all unconditionally
-- 24-bit, and a "-direct" terminfo entry says so by definition.
wantsFullColor :: IO Bool
wantsFullColor = do
  colorterm <- lookupEnv "COLORTERM"
  term <- lookupEnv "TERM"
  let
    saysColorterm = case colorterm of
      Just v -> v `elem` ["truecolor", "24bit"]
      Nothing -> False
    saysTerm = case term of
      Nothing -> False
      Just t ->
        t `elem` trueColorTerms
          || "-direct" `isSuffixOf` t
  pure (saysColorterm || saysTerm)

trueColorTerms :: [String]
trueColorTerms =
  [ "xterm-kitty"
  , "xterm-ghostty"
  , "wezterm"
  , "alacritty"
  , "foot"
  , "foot-extra"
  , "contour"
  , "rio"
  ]

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

-- Does the code/comment filter let this hunk through? Unlike the pending
-- filter, this one governs every way of arriving at a hunk — walking,
-- auto-advance, and where a session opens — because "walk just the
-- comment hunks" is the whole point of asking for it.
pairInFilter :: St -> (Int, Int) -> Bool
pairInFilter st pair =
  case runAt st pair of
    Just r -> runFilterAllows (stRunFilter st) r
    Nothing -> False

-- Every hunk the filter admits, in display order.
filteredRunPairs :: St -> [(Int, Int)]
filteredRunPairs st = filter (pairInFilter st) (allRunPairs st)

-- Where a session begins: the first hunk nothing has been said about
-- yet; only when every hunk has some response does the first still-
-- pending (flagged/questioned) one win.
firstUnrespondedOrPending :: St -> Maybe (Int, Int)
firstUnrespondedOrPending st =
  find (pairUnvisited st) (filteredRunPairs st) <|> firstPending st

firstPending :: St -> Maybe (Int, Int)
firstPending st = find (pairPending st) (filteredRunPairs st)

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
    absolute = FS.eventPath ev
    rel = makeRelative root absolute
    parts = splitDirectories rel
    -- the same directories the file listing declines to enumerate: a
    -- build writing thousands of files is not news about the review
    noisy = any (`elem` map dropTrailingPathSeparator buildOutputDirs) parts
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
      else void (writeBChanNonBlocking chan (FileChangedOnDisk absolute))

-- Loading and rebuilding -----------------------------------------------------

loadAll :: Config -> TS.HighlighterTable -> ReviewMode -> FilePath -> IO [FileView]
loadAll cfg hls mode root = do
  ignores <- loadIgnores root
  names <-
    fmap
      (filter (not . ignoredPath ignores))
      (reviewFiles mode (cfgUntracked cfg) root)
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
  let
    build deleted base newLs = do
      let
        fv0 =
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
            }
      (fv, _) <- rebuildFV cfg (T.pack root) fv0
      let
        anchor = case fvRuns fv of
          (r : _) -> runAnchor r
          [] -> 0
      pure (Just fv {fvEd = newEdState anchor newLs})
  present <- doesFileExist (root </> path)
  if not present
    then do
      -- No working copy, but git still has the base: the change set
      -- removed this file. It reviews as a file whose every line is a
      -- removal, so accepting those lines is accepting the deletion.
      -- Absent on both sides means there is nothing to say about it.
      base <- baseLines mode root path
      case base of
        [] -> pure Nothing
        _ -> build True base []
    else do
      readable <- try (looksBinary root path) :: IO (Either IOException Bool)
      case readable of
        Left _ -> pure Nothing
        Right True -> pure Nothing
        Right False -> do
          base <- baseLines mode root path
          gotNew <- try (TIO.readFile (root </> path)) :: IO (Either IOException Text)
          case gotNew of
            Left _ -> pure Nothing
            Right newTxt -> build False base (T.lines newTxt)

diffBudgetUs :: Int
diffBudgetUs = 400000

-- The new side of the diff, and the buffer every row calculation is
-- measured against. A deleted file has no working copy at all, but its
-- editor buffer still holds the single empty line a text zipper cannot
-- do without. Counting that line would diff it as an added blank and
-- would push every removed row's resting point one line past the end of
-- the buffer, where no cursor can ever reach it — so a deletion that
-- nothing has typed into answers with no lines whatsoever.
newSideLines :: FileView -> [Text]
newSideLines fv =
  let
    ls = bufferLines (fvEd fv)
  in
    case (fvDeleted fv, ls) of
      (True, [""]) -> []
      _ -> ls

-- length (newSideLines fv), without rebuilding the buffer to count it.
-- The one case that has to look at content is a single-line buffer,
-- where looking is free.
newSideLineCount :: FileView -> Int
newSideLineCount fv =
  let
    n = bufferLineCount (fvEd fv)
  in
    if fvDeleted fv && n == 1 && bufferLines (fvEd fv) == [""]
      then 0
      else n

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

-- The top-level declaration a buffer row falls under: the nearest line
-- at or above it whose first character sits in column zero and isn't a
-- comment or closing bracket — the boundary heuristic hunk splitting
-- already trusts. A tree-sitter walk could be exact; this is right
-- nearly always and costs one upward scan at rebuild time.
declAt :: [Text] -> Int -> Maybe Text
declAt ls row = find isDeclLine (reverse (take (row + 1) ls))

isDeclLine :: Text -> Bool
isDeclLine l =
  case T.uncons l of
    Just (c, _) ->
      not (isSpace c)
        && not
          ( any
              (`T.isPrefixOf` l)
              ["--", "//", "#", ";", "{-", "/*", "*", "<!--", "}", ")", "]"]
          )
    Nothing -> False

-- declAt for every run's anchor at once, in a single walk of the buffer
-- rather than one upward scan per run. A diff that blows its budget
-- falls back to marking every line in the file, which splits into tens
-- of thousands of runs; a scan apiece is quadratic in the file, and the
-- difference between a load that takes seconds and one that never
-- finishes at all. Anchors arrive in file order — buildRuns walks the
-- rows forward and every anchor is a buffer row at or after the last —
-- which is what lets one pass answer them all.
declsForAnchors :: [Text] -> [Int] -> [Text]
declsForAnchors ls anchors =
  let
    go _ _ [] _ = []
    go ix seen pending@(a : more) rest =
      if ix > a
        then seen : go ix seen more rest
        else case rest of
          [] -> map (const seen) pending
          (l : further) ->
            go (ix + 1) (if isDeclLine l then l else seen) pending further
  in
    go (0 :: Int) T.empty anchors ls

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
        \ev ->
          handleEvent ev
            >> ensureCurrentHighlighted
            >> reanchorBody
            >> syncComposing
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
      Just fv ->
        viewport BodyVP Vertical $
          Widget Greedy Fixed $ do
            ctx <- getContext
            render (renderFile (availWidth ctx) (windowHeight ctx) st fv)
      Nothing -> idleBody st
    panes =
      if stTray st && not (null (stFiles st))
        then [tray st, body]
        else [body]
    overlays =
      catMaybes
        [ fmap (notePopupLayer st) (stNotePopup st)
        , fileMenuLayer =<< stTree st
        , fmap (treeLayer st) (stTree st)
        , fmap pickerLayer (stPicker st)
        , if stSpace st then Just spaceMenu else Nothing
        , pendingMenuLayer st
        ]
    menuRows = case promptMenu st of
      Just menu -> [menu]
      Nothing -> []
    rebaseRows = case stRebase st of
      Just rb -> [rebaseBar rb]
      Nothing -> [gitBar st]
    helpRows =
      if stHelp st then [helpLayer (cfgKeys (stConfig st))] else []
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
    -- with more than one repository in reach, which one you are in is
    -- the first thing the bar has to answer
    repoSeg =
      if length (stRoots st) > 1
        then
          [ withAttr headerFileAttr (txt (" " <> T.pack (takeFileName (stRoot st))))
          , withAttr headerAttr (txt " ·")
          ]
        else []
    place = case stBranch st of
      Just b ->
        hBox
          ( repoSeg
              ++ [ withAttr headerAttr (txt " on ")
                 , withAttr headerFileAttr (txt b)
                 ]
          )
      Nothing -> hBox (repoSeg ++ [withAttr conflictAttr (txt " detached")])
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
-- The bordered box every popup is drawn in, without saying where it goes.
frameBox :: Text -> Text -> Widget Name -> Widget Name
frameBox title hint body =
  withDefAttr floatAttr
    . withBorderStyle BS.unicodeRounded
    . B.borderWithLabel (withAttr floatTitleAttr (txt (" " <> title <> " ")))
    . padTop (Pad 1)
    . padLeft (Pad 2)
    . padRight (Pad 2)
    $ vBox
      [ body
      , padTop (Pad 1) (withAttr hintAttr (txt hint))
      ]

floatFrame :: Text -> Text -> Widget Name -> Widget Name
floatFrame title hint = C.centerLayer . frameBox title hint

-- Where the key reminders go: down in the corner, out of the way of the
-- line being read. They answer "what can I press now", which is worth
-- glancing at and never worth reading over the top of the code — and
-- they all go to the same corner, so the eye learns one place.
cornerFrame :: Text -> Text -> Widget Name -> Widget Name
cornerFrame title hint body =
  -- Placed by translation rather than by padding it into the corner.
  -- Max padding is not empty space: it paints its own background, so a
  -- box pushed down there takes the status line with it.
  Widget Fixed Fixed $ do
    ctx <- getContext
    box <- render (frameBox title hint body)
    let
      img = image box
      -- one column in from the right, and one row up from the status line
      x = max 0 (availWidth ctx - V.imageWidth img - 1)
      y = max 0 (availHeight ctx - V.imageHeight img - 1)
    render (translateBy (Location (x, y)) (raw img))

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
-- The keys differ by family, so the sheet does too — an emacs or vi
-- session showing helix's keys would be worse than no sheet at all.
helpGroups :: KeyMode -> [(Text, [(Text, Text)])]
helpGroups mode =
  (reviewGroup mode : sayingGroup : editingGroups mode)
    ++ aroundGroups mode

sayingGroup :: (Text, [(Text, Text)])
sayingGroup =
  ( "saying something"
  ,
    [ (":note TEXT", "a one-line note on what you marked")
    , (":note", "the popup, with the thread so far")
    , ("\9251 u", "drop the conversation here")
    ]
  )

aroundGroups :: KeyMode -> [(Text, [(Text, Text)])]
aroundGroups mode =
  [
    ( "getting around"
    ,
      [ ("Ctrl-p", "find a file, flat and ranked")
      , ("\9251 t", "the same dialog, as a tree")
      , ("\8633", "in either: flat \8596 tree, filter kept")
      , ("Ctrl-a", "in either: show the ignored files too")
      , ("Ctrl-x", "in either: new / rename / duplicate / delete")
      , ("gr", "switch repository, starting on the next")
      , ("gn gp", "the next / previous file, as ) and ( do")
      , ("\9251 b", "files already open in the session")
      , ("Tab", "collapse / expand the tray")
      , ("{ }", "scroll the tray")
      , -- vi wants E for the WORD motion, so the leader carries $EDITOR
        (if mode == KeysVi then "\9251 e" else "E", "suspend into $EDITOR here")
      , ("\9251 a", "the alternate file")
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

reviewGroup :: KeyMode -> (Text, [(Text, Text)])
reviewGroup mode =
  let
    common =
      [ ("] [", "next / previous hunk")
      , ("'", "back to the hunk under review")
      , ("\9251 h", "make this hunk the current one")
      , ("\9251 p", "show only hunks still pending")
      , ("\9251 k", "walk all / code / comment hunks")
      ]
  in
    ( "review"
    , case mode of
        KeysVi ->
          [ ("\9251 \9166", "accept the selection as reviewed")
          , ("\9251 !", "flag it — come back to this")
          , ("\9251 q", "question it — explain or fix")
          , ("\9251 x", "select the whole hunk")
          ]
            ++ common
        KeysEmacs ->
          [ ("C-c \9166", "accept the selection as reviewed")
          , ("C-c !", "flag it — come back to this")
          , ("C-c ?", "question it — explain or fix")
          , ("C-c x", "select the whole hunk")
          , ("C-c ] [", "next / previous hunk")
          , ("C-c p", "show only hunks still pending")
          , ("C-c k", "walk all / code / comment hunks")
          ]
        KeysHelix ->
          [ ("\9166", "accept the selection as reviewed")
          , ("!", "flag it — come back to this")
          , ("?", "question it — explain or fix")
          , ("X", "select the whole hunk")
          ]
            ++ common
    )

editingGroups :: KeyMode -> [(Text, [(Text, Text)])]
editingGroups mode =
  case mode of
    KeysVi ->
      [
        ( "moving"
        ,
          [ ("h j k l", "left down up right")
          , ("w b e / W B E", "words / WORDs")
          , ("f F t T ; ,", "find / till a character, repeat")
          , ("0 ^ $", "line start / first non-blank / end")
          , ("gg G %", "file start / end / matching bracket")
          , (":42", "go to line 42")
          , (") (", "next / previous file")
          ]
        )
      ,
        ( "operators"
        ,
          [ ("d c y > <", "delete change yank indent unindent")
          , ("dw d2w dd", "operator, motion, count — or doubled")
          , ("ciw di( ya\"", "text objects: w W p ( { [ < \" ' `")
          , ("gu gU g~", "lower / upper / swap case")
          , ("x X D C s S Y", "the usual shorthands")
          ]
        )
      ,
        ( "editing"
        ,
          [ ("i a I A o O", "enter insert")
          , ("v V", "visual, visual-line")
          , ("p P J r ~", "paste / join / replace char / case")
          , ("u  Ctrl-r", "undo / redo")
          , (".", "repeat the last change")
          ]
        )
      ,
        ( "finding"
        ,
          [ ("/ ? n N", "search forward / back, repeat")
          , ("* #", "the word under the cursor, either way")
          , ("\9251 /", "search the whole project")
          ]
        )
      ]
    KeysEmacs ->
      [
        ( "moving"
        ,
          [ ("C-f C-b C-n C-p", "left right down up")
          , ("C-a C-e", "line start / end")
          , ("M-f M-b", "words")
          , ("M-< M->", "file start / end")
          , (":42", "go to line 42")
          , ("C-c ) (", "next / previous file")
          ]
        )
      ,
        ( "editing"
        ,
          [ ("C-space", "set the mark; the region is the selection")
          , ("C-w M-w", "kill / copy the region")
          , ("C-k C-y", "kill to end of line / yank")
          , ("C-_", "undo")
          , ("C-x C-s", "save (edits already write through)")
          ]
        )
      ,
        ( "finding"
        ,
          [ ("C-s", "search this file")
          , ("M-x", "the command line")
          , ("\9251 /", "search the whole project")
          ]
        )
      ]
    KeysHelix -> helixEditingGroups

helixEditingGroups :: [(Text, [(Text, Text)])]
helixEditingGroups =
  [
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
  ]

-- Two columns, split where the row counts come out even, so the sheet is
-- one glance rather than a scroll.
helpLayer :: KeyMode -> Widget Name
helpLayer mode =
  Widget Greedy Greedy $ do
    ctx <- getContext
    let
      groups = helpGroups mode
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
      half = div (sum (map height groups)) 2
      -- take a group into the left column while doing so lands closer to
      -- the halfway mark than stopping would; a plain "stop once over
      -- half" split leaves the columns lopsided
      pack used pending =
        case pending of
          [] -> ([], [])
          (g : rest)
            | used == 0
                || abs (used + height g - half) <= abs (half - used) ->
                let
                  (a, b) = pack (used + height g) rest
                in
                  (g : a, b)
            | otherwise -> ([], g : rest)
      (leftGroups, rightGroups) = pack 0 groups
      column shown = vBox (concatMap groupRows shown)
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
  [ ("\9166", "accept as reviewed", acceptSelection)
  , ("!", "flag it", markThenNote Flagged)
  , ("q", "question it", markThenNote Questioned)
  , ("x", "select the hunk", selectCurrentHunk)
  , ("f", "find a file", openFileFinder)
  , ("b", "open files", openBufferPicker)
  , ("t", "project tree", openTree)
  , ("R", "switch repository", openRepoPicker)
  , ("n", "note popup", openNotePopup NoteAsked)
  , ("e", "$EDITOR here", editCurrent)
  , ("p", "pending filter", togglePendingOnly)
  , ("k", "code / comment filter", cycleRunFilter)
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
    cornerFrame "space" "any other key cancels" . hLimit 26 . padRight Max $
      vBox (map item spaceMenuEntries)

-- What a half-typed prefix is waiting for.
--
-- The m menu reads its own dispatch table, so it cannot describe itself
-- wrongly. The g menu cannot: half its keys are the modal layer's
-- (handleG) and half are review-level interceptions in handleHelixKey,
-- so this list is written out and has to be kept beside them.
--
-- f, t and r are left out on purpose. They wait for any character at
-- all, and "type a character" is not worth a box in the corner.
pendingMenu :: Text -> Maybe (Text, [(Text, Text)])
pendingMenu label =
  let
    pairs =
      [ ("( ) [ ] { } < >", "brackets")
      , ("\" ' `", "quotes")
      ]
    objects = pairs ++ [("w W", "word under the cursor")]
    fromMatch = map (\(k, what, _) -> (T.singleton k, what)) matchMenu
  in
    case label of
      "g" ->
        Just
          ( "goto"
          ,
            [ ("g", "first line")
            , ("e", "last line")
            , ("h", "line start")
            , ("l", "line end")
            , ("s", "first non-blank")
            , ("a", "alternate file")
            , ("n", "next file")
            , ("p", "previous file")
            , ("r", "another repository")
            ]
          )
      "m" -> Just ("match", fromMatch)
      "ms" -> Just ("surround with", pairs)
      "md" -> Just ("delete surround", pairs)
      "mr" -> Just ("replace surround", pairs)
      "mi" -> Just ("select inside", objects)
      "ma" -> Just ("select around", objects)
      _ -> Nothing

pendingMenuLayer :: St -> Maybe (Widget Name)
pendingMenuLayer st = do
  fv <- currentFileMay st
  label <- pendingLabel (fvEd fv)
  (title, entries) <- pendingMenu label
  let
    -- the key column is padded to the widest of them, so the labels line
    -- up even where a key is a row of brackets rather than one letter
    keyW = maximum (map (T.length . fst) entries)
    item (key, what) =
      hBox
        [ withAttr noteInlineAttr (txt (" " <> T.justifyLeft keyW ' ' key))
        , withAttr statusAttr (txt ("  " <> what))
        ]
    width = 5 + keyW + maximum (map (T.length . snd) entries)
  pure
    ( cornerFrame title "any other key cancels" . hLimit width . padRight Max $
        vBox (map item entries)
    )

-- Ctrl-x's menu, drawn from the same table that dispatches it.
fileMenuLayer :: TreeView -> Maybe (Widget Name)
fileMenuLayer view =
  case tvOp view of
    Just OpMenu ->
      let
        item (key, label, _) =
          hBox
            [ withAttr noteInlineAttr (txt (" " <> key))
            , withAttr statusAttr (txt ("  " <> label))
            ]
      in
        Just
          ( cornerFrame "file" "any other key cancels" . hLimit 24 . padRight Max $
              vBox (map item fileMenuEntries)
          )
    _ -> Nothing

-- The persistent session's resting state: nothing to review, but the
-- watcher is live and new work will wake the screen on its own.
-- The resting screen names the way out that fits the session it is in:
-- with other checkouts in reach, a clean one is most often a cue to go
-- and look at the next, and gr is hard to guess from an empty pane.
idleBody :: St -> Widget Name
idleBody st =
  let
    hint =
      if length (stRoots st) > 1
        then "  watching for changes · gr switches repository · :base · :q quits"
        else
          "  watching for changes · :base REF or :mode switches the view · :q quits"
  in
    padBottom Max . padRight Max $
      vBox
        [ txt " "
        , withAttr doneMarkAttr (txt "  ✓ all clear — nothing to review")
        , txt " "
        , withAttr hintAttr (txt hint)
        ]

headerBar :: St -> Widget Name
headerBar st =
  let
    lead = " remax · " <> modeLabel (stMode st) <> " · "
    rebaseSeg = case stRebase st of
      Nothing -> txt ""
      Just _ -> withAttr headerRebaseAttr (txt "  ·  rebasing")
    -- Somebody is waiting on this review. Worth saying, because the
    -- alternative is flagging a line and wondering whether anyone is
    -- listening — and because when it stops, nothing else would say so.
    --
    -- It breathes rather than sits still: a mark that never moves is one
    -- you stop believing, and this one is a claim about right now.
    watchSeg =
      let
        (waiting, working) = stAttending st
        here = repoText st
        mine
          | here `elem` waiting = Just (throbGlyph (stThrob st), "watching")
          | here `elem` working = Just (busyGlyph (stThrob st), "working")
          | otherwise = Nothing
      in
        case mine of
          Nothing -> txt ""
          Just (glyph, label) ->
            hBox
              [ withAttr headerAttr (txt "  ·  ")
              , withAttr watchingAttr (txt glyph)
              , withAttr headerAttr (txt (" " <> label))
              ]
  in
    withDefAttr headerAttr . padRight Max $
      case currentFileMay st of
        Nothing -> hBox [txt (lead <> "all clear"), rebaseSeg, watchSeg]
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
            -- no working copy behind this screen: what is on it is the
            -- file as it was, every line of it on its way out
            deletedSeg =
              if fvDeleted fv
                then withAttr removedMarkAttr (txt "  ·  deleted")
                else txt ""
            -- a filtered session hides hunks; say so, or an empty-looking
            -- tray reads as a finished review
            filterSeg =
              if stRunFilter st == FilterAll
                then txt ""
                else
                  withAttr
                    headerRebaseAttr
                    (txt ("  ·  " <> runFilterLabel (stRunFilter st) <> " only"))
          in
            hBox
              [ txt lead
              , withAttr headerFileAttr (txt (T.pack (fvPath fv)))
              , deletedSeg
              , filterSeg
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
              , watchSeg
              ]

-- What the tray shows: one line per file (with a count of its hidden
-- hunks), one line per hunk still worth a row. Reviewed hunks collapse
-- under their file (in pending-only mode, all settled hunks do); the
-- current hunk is always shown regardless.
data TrayEntry
  = TrayFile Int Int -- file index, hidden hunk count
  | TrayHunk Int Int -- file index, hunk index
  | TrayRemark Int Int -- file index, index into that file's remarks

-- Remarks on lines nobody has changed: the buffer row to sit on, the key
-- they are recorded under, and the line itself.
--
-- Derived rather than carried on the FileView, because whether a line
-- bears one depends on the verdicts, which the rebuild that builds the
-- rows knows nothing about.
data Remark = Remark
  { rmRow :: !Int
  , rmKey :: !Text
  , rmText :: !Text
  }

fileRemarks :: St -> FileView -> [Remark]
fileRemarks st fv =
  let
    pick row = do
      k <- rRemarkKey row
      -- a removed row keeps its remark too, but by then the hunk it
      -- belongs to is what carries it through the tray
      guard (rMark row == ' ')
      guard (Map.member k (stVerdicts st) || Map.member k (stThreads st))
      n <- rNewRow row
      pure (Remark n k (rText row))
  in
    mapMaybe pick (fvRows fv)

trayEntries :: St -> [TrayEntry]
trayEntries st =
  let
    curF = stCurF st
    curH = clampH (fvRuns (currentFile st)) (stCurH st)
    hiddenHunk fi (hi, r) =
      let
        mark = runMark (stVerdicts st) r
        isCurrent = fi == curF && hi == curH
        settled = stOnlyPending st && mark == '\10003'
        wrongKind = not (runFilterAllows (stRunFilter st) r)
      in
        not isCurrent && (settled || wrongKind)
    fileGroup (fi, fv) =
      let
        numbered = zip [0 ..] (fvRuns fv)
        (hiddenOnes, shown) = splitHiddenShown fi numbered
      in
        TrayFile fi (length hiddenOnes)
          : map (\(hi, _) -> TrayHunk fi hi) shown
          ++ zipWith (\ri _ -> TrayRemark fi ri) [0 ..] (fileRemarks st fv)
    splitHiddenShown fi numbered =
      ( filter (hiddenHunk fi) numbered
      , filter (not . hiddenHunk fi) numbered
      )
  in
    concatMap fileGroup (zip [0 ..] (stFiles st))

trayWidth :: Int
trayWidth = 32

-- The tray is drawn as images for the same reason the body is: it holds
-- a row per hunk in the whole change set, and every one of them was a
-- box of boxes redrawn on each keystroke. See PenSet.
tray :: St -> Widget Name
tray st =
  let
    -- One walk per file rather than one per remark: a remark row asks
    -- for the remark at an index, and rebuilding the file's list to
    -- answer meant re-scanning every row of the file, once per remark
    -- on it, every frame.
    remarksByFile = map (fileRemarks st) (stFiles st)
    markAttrFor mark = case mark of
      '!' -> flagMarkAttr
      '?' -> questionMarkAttr
      '\10003' -> doneMarkAttr
      _ -> pendingMarkAttr
    entryInk pens width entry = case entry of
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
          nameImg =
            inkT
              width
              (penFor pens (if fi == stCurF st then trayFileCurAttr else trayFileAttr))
              (padT name)
          endImg = inkT (width - V.imageWidth nameImg) (penPlain pens) " "
          -- the meter is pushed to the right of what the name leaves,
          -- with the gap in front of it rather than behind
          gap = width - V.imageWidth nameImg - V.imageWidth endImg
          meterImg = meterInk pens gap done total
        in
          fitRow (penPlain pens) width $
            V.horizCat
              [ nameImg
              , V.charFill (penPlain pens) ' ' (max 0 (gap - V.imageWidth meterImg)) (1 :: Int)
              , meterImg
              , endImg
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
          -- the current row reads every attribute against the selection
          -- as its default, so the highlight runs the width of the tray
          rowPens = if current then penUnder floatSelAttr pens else pens
          mark = runMark (stVerdicts st) r
        in
          fitRow (penPlain rowPens) width $
            spanInk
              width
              [ \left ->
                  if current
                    then inkT left (penFor rowPens floatSelBarAttr) "\9614 "
                    else inkT left (penPlain rowPens) "  "
              , \left -> inkT left (penFor rowPens (markAttrFor mark)) (T.singleton mark)
              , \left -> inkT left (penPlain rowPens) (" :" <> tshow (runAnchor r + 1))
              , \left -> inkT left (penFor rowPens trayDeclAttr) decl
              , \left ->
                  if any (`Map.member` stThreads st) (runKeys r)
                    then inkT left (penFor rowPens noteInlineAttr) " \8801"
                    else V.emptyImage
              ]
      -- A remark sits between the hunks rather than among them: it is
      -- about a line the change never touched, so it carries the line
      -- itself where a hunk carries its declaration.
      TrayRemark fi ri ->
        let
          rm = (remarksByFile !! fi) !! ri
          mark = case Map.lookup (rmKey rm) (stVerdicts st) of
            Just Flagged -> '!'
            Just Questioned -> '?'
            Just Reviewed -> '\10003'
            Nothing -> '\183'
        in
          fitRow (penPlain pens) width $
            spanInk
              width
              [ \left -> inkT left (penPlain pens) "  "
              , \left -> inkT left (penFor pens (markAttrFor mark)) (T.singleton mark)
              , \left -> inkT left (penPlain pens) (" :" <> tshow (rmRow rm + 1))
              , \left ->
                  inkT
                    left
                    (penFor pens trayDeclAttr)
                    (" " <> T.take 12 (T.strip (rmText rm)))
              , \left ->
                  if Map.member (rmKey rm) (stThreads st)
                    then inkT left (penFor pens noteInlineAttr) " \8801"
                    else V.emptyImage
              ]
  in
    hLimit trayWidth $
      padRight (Pad 1) $
        viewport TrayVP Vertical $
          Widget Greedy Fixed $ do
            ctx <- getContext
            let
              pens = PenSet (ctxAttrName ctx) (ctxAttrMap ctx)
              width = availWidth ctx
            render . raw . V.vertCat $
              map (entryInk pens width) (trayEntries st)

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

-- How far the render window reaches past the cursor, measured in screens
-- rather than in a flat number of rows.
--
-- The window has to outrun the viewport in both directions or the pane
-- shows blank where the body ran out, and the slack below is exactly one
-- screen — see bodyWindow for why that is the bound. A constant big
-- enough for the tallest terminal is a constant far too big for an
-- ordinary one, and every row of it is drawn on every keystroke: six
-- hundred rows for a pane that shows forty was most of what a keypress
-- cost. The floor keeps a very short pane from re-anchoring constantly.
windowRowsFor :: Int -> Int
windowRowsFor screenRows = max 100 (2 * screenRows)

-- The terminal's height. Read afresh rather than remembered: the draw
-- and the handlers that move the window have to size it the same way or
-- the body slides under the cursor, and a resize must not be able to
-- reach one of them before the other.
terminalRows :: EventM Name St Int
terminalRows = do
  vty <- getVtyHandle
  (_, rows) <- liftIO (V.displayBounds (V.outputIface vty))
  pure rows

-- Tell the store whether a note is being written into this repository.
--
-- A flag reaches the store the instant the key is pressed, which is what
-- makes the tray and the meters answer at once — and means an agent
-- waiting on the reviewer hears about it seconds before the sentence
-- saying what is wrong with it. It cannot act on that, and waiting again
-- for the note is how it misses the note. So the store carries the fact
-- that one is being written, and the wait holds until it is.
--
-- Driven off the state after every event rather than from the places a
-- popup opens and closes, so a path that dismisses one some other way
-- cannot leave the marker standing. Only transitions are written.
syncComposing :: EventM Name St ()
syncComposing = do
  st <- get
  let
    writing = case stNotePopup st of
      Just np
        | NoteFor _ _ <- npPurpose np -> Just (repoText st)
      _ -> Nothing
  unless (writing == stComposing st) $ do
    -- cleared where it was set before being set anywhere else, so
    -- switching repositories cannot strand it on the old one
    forM_ (stComposing st) (\was -> liftIO (setComposing (stConn st) was False))
    forM_ writing (\now -> liftIO (setComposing (stConn st) now True))
    modify (\s -> s {stComposing = writing})

-- Write the window's origin back after an event, and move the viewport
-- by the same amount so nothing jumps.
--
-- The origin only changes when the cursor has drifted near an edge of
-- what is drawn; the rows on screen are the same rows either side of
-- that, so the view must not move. brick's scroll offset is in window
-- coordinates, and shifting the window by n rows moves everything on
-- screen by n unless the offset is shifted with it.
--
-- Deliberate jumps (a hunk, a reload) set the origin themselves before
-- placing the viewport, so by the time this runs there is nothing left
-- to reconcile and it does nothing.
reanchorBody :: EventM Name St ()
reanchorBody = do
  st <- get
  case currentFileMay st of
    Nothing -> pure ()
    Just fv -> do
      screenRows <- terminalRows
      let
        (_, origin) = bodyWindow screenRows st fv
        moved = origin - fvOrigin fv
      unless (moved == 0) $ do
        modify (updateFile (stCurF st) (\f -> f {fvOrigin = origin}))
        mvp <- lookupViewport BodyVP
        case mvp of
          Just vp -> setTop (viewportScroll BodyVP) (max 0 (_vpTop vp - moved))
          Nothing -> pure ()

-- How many display rows the body draws at once, and how close the cursor
-- may come to an edge before the window is moved.
--
-- The slack is a whole screen because that is what it takes. The
-- viewport shows one screen containing the cursor, and it may sit
-- anywhere in that screen, so the rows drawn have to reach a screen
-- either side of the cursor for the pane to be full whichever way the
-- reader arrived at the line.
windowSpanFor :: Int -> Int
windowSpanFor screenRows = 2 * windowRowsFor screenRows

windowSlackFor :: Int -> Int
windowSlackFor screenRows = windowRowsFor screenRows `div` 2

-- The display row the body is framed on, and the first row of the render
-- window.
--
-- That first row is also the body's coordinate origin. brick renders a
-- vertical viewport's contents with a fixed available height of 100000
-- rows (Brick.Widgets.Core.unrestricted), so a body that padded its way
-- down to an absolute display row fell off the end of that budget: the
-- padding consumed the whole allowance and the real rows were cropped
-- away, leaving a blank pane on any file whose current hunk sat past
-- line 100000. Everything the body renders is therefore in window
-- coordinates, and the places that scroll the viewport subtract the same
-- origin so they agree with what was drawn.
--
-- The origin has to hold still while the cursor moves, which is why it
-- is carried on the FileView rather than derived from the cursor. An
-- origin that slid with the cursor kept it a fixed distance into the
-- window, so the visibility request never changed, brick never scrolled
-- again, and the file slid upward under a cursor nailed to one screen
-- line. So a stored origin is kept as long as the cursor stays clear of
-- the window's edges, and only when it does not is the window moved to
-- sit around the cursor again.
bodyWindow :: Int -> St -> FileView -> (Int, Int)
bodyWindow screenRows st fv =
  let
    windowRows = windowRowsFor screenRows
    windowSpan = windowSpanFor screenRows
    windowSlack = windowSlackFor screenRows
    rows = fvRows fv
    curH = clampH (fvRuns fv) (stCurH st)
    curRow = fst (cursorRC (fvEd fv))
    -- Normally the cursor's row, but an all-removed file owns no buffer
    -- rows at all, so nothing carries the cursor and the view would be
    -- nailed to the top: there the hunk under review is what the reader
    -- is looking at, and ] and [ are how they move.
    cursorOut =
      case findIndex (\r -> rNewRow r == Just curRow) rows of
        Just out -> out
        Nothing -> fromMaybe 0 (findIndex (\r -> rRunIx r == Just curH) rows)
    held = fvOrigin fv
    inside =
      cursorOut >= held + windowSlack
        && cursorOut <= held + windowSpan - windowSlack
    -- clear of both edges, or already showing the top of the file
    settled = inside || (held == 0 && cursorOut <= windowSpan - windowSlack)
  in
    (cursorOut, if settled then held else max 0 (cursorOut - windowRows))

-- Where each row's reviewer note (and any agent reply to it) should
-- render: annotations apply to every line of their target, so
-- consecutive rows carrying the same pair show it once, under the last
-- of them — a footnote to the block, not an echo on every line. One
-- entry per row, [] where nothing renders. Each rendered line knows its
-- post's author and whether it opens that post, so the conversation
-- reads in order with a sense of who said what.
-- The line-number gutter's width, and with it the indent every note row
-- carries. The render and the scroll correction both have to reckon a
-- note's height the same way or their idea of what is above the cursor
-- drifts apart, so both ask here.
gutterWidth :: FileView -> Int
gutterWidth fv = length (show (max 1 (bufferLineCount (fvEd fv))))

noteTextWidth :: Int -> FileView -> Int
noteTextWidth paneWidth fv = max 12 (paneWidth - (gutterWidth fv + 4))

-- Where a row's feedback lives. A changed row carries what was said
-- about the change; a line nobody has changed carries a remark, keyed
-- against the base; and a removed row can answer to either, because a
-- remark made while it was unchanged follows it here when somebody
-- changes it.
rowFeedbackKeys :: RowR -> [Text]
rowFeedbackKeys r = catMaybes [rKey r, rRemarkKey r]

shownNoteLines :: Int -> St -> [RowR] -> [[NoteLine]]
shownNoteLines width st rows =
  let
    annOf r =
      listToMaybe (mapMaybe (`Map.lookup` stThreads st) (rowFeedbackKeys r))
    anns = map annOf rows
    nexts = drop 1 anns ++ [Nothing]
    -- A note is prose and the pane is narrow, so it is folded to fit
    -- rather than run off the edge where nobody can read it. The
    -- author's own line breaks are kept and each of their lines folded
    -- in turn, so a deliberate paragraph survives while a long one still
    -- lands on screen.
    --
    -- The opening row carries the glyph, and an agent's name besides;
    -- folding to what that leaves means no row overflows, and the
    -- continuations simply have more room than they use.
    postLines post =
      let
        author = postAuthor post
        lead = if author == "reviewer" then 4 else 2 + T.length author + 3
        room = max 12 (width - lead)
        fold ln = case greedyWrap room (T.words ln) of
          [] -> [""]
          wss -> map T.unwords wss
        flowed = concatMap fold (T.lines (postBody post))
      in
        case flowed of
          [] -> []
          (first : rest) ->
            NoteLine author True first : map (NoteLine author False) rest
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

renderFile :: Int -> Int -> St -> FileView -> Widget Name
renderFile paneWidth screenRows st fv =
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
    -- Which display row the view keeps in sight, and where the window
    -- that carries it starts. lo is the body's origin: nothing above it
    -- is drawn, so the pane never asks brick for a row it cannot reach.
    (cursorOut, lo) = bodyWindow screenRows st fv
    hi = min (fvRowCount fv) (lo + windowSpanFor screenRows)
    -- one walk to the window, shared by everything that reads it
    fromLo = drop lo rows
    windowed = take (hi - lo) fromLo
    beyond = drop (hi - lo) fromLo
    -- the buffer row each removed line now sits before, so a line-wise
    -- selection can honestly include the deletions it would verdict.
    -- The scan only has to cover the window, seeded with the answer for
    -- the row just past it: the first real row that follows, or the end
    -- of the buffer when nothing does.
    seedPoint =
      case mapMaybe rNewRow beyond of
        (p : _) -> p
        [] -> newSideLineCount fv
    points =
      drop
        1
        (scanr (\row acc -> fromMaybe acc (rNewRow row)) seedPoint windowed)
    noteRowsList =
      take (hi - lo) (drop lo (shownNoteLines (noteTextWidth paneWidth fv) st rows))
    -- worked out once for the whole file when the rows were built, so
    -- the marks are the same wherever the window happens to sit
    emphList = take (hi - lo) (drop lo (fvEmph fv))
    rowsWithPoints = zip (zip3 windowed points noteRowsList) emphList
    numW = gutterWidth fv
    -- how far the whole window is shifted right; every line shifts in
    -- step, so it is worked out once rather than per row
    hOff = hScrollFor paneWidth numW curCol
    -- One row's images: the line itself, then any note rows folded under
    -- it, paired with whether the cursor sits on this line. Counting the
    -- cursor's screen row off the images that precede it is how the
    -- cursor stays with its line whatever the notes above it do.
    rowInk pens ((r, point, shownNotes), emph) =
      let
        isCurRun = case rRunIx r of
          Just runIx -> runIx == curH
          Nothing -> False
        verdict =
          listToMaybe (mapMaybe (`Map.lookup` stVerdicts st) (rowFeedbackKeys r))
        isCursor = rNewRow r == Just curRow
        inSel = case lineSel of
          Nothing -> False
          Just (a, b) -> case rNewRow r of
            Just nr -> nr >= a && nr <= b
            -- a removed line counts when its resting point does,
            -- matching how verdicts scope it
            Nothing -> point >= a && point <= b
        -- a selected line reads every attribute it draws against a map
        -- whose default is the selection, so tokens that set only a
        -- foreground pick the highlight up behind them
        rowPens = if inSel then penUnder selAttr pens else pens
        -- the first gutter column doubles as the border between tray
        -- and body: drawn on every row, in the highlight color across
        -- the current hunk's rows; the second column carries the
        -- line's status, so neither ever hides the other
        barName = if isCurRun then curRunAttr else borderAttr
        barGlyph = if isCurRun then '\9612' else '\9614' -- wide ▌, thin ▎
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
        statusInk left = case (isCursor, statusPick) of
          (True, Just (c, a)) ->
            inkT left (penFor (penUnder cursorMarkAttr rowPens) a) (T.singleton c)
          (True, Nothing) -> inkT left (penFor rowPens cursorMarkAttr) " "
          (False, Just (c, a)) -> inkT left (penFor rowPens a) (T.singleton c)
          (False, Nothing) -> inkT left (penPlain rowPens) " "
        numInk left = case rNewRow r of
          Just nr ->
            let
              numAttr =
                if isCursor
                  then lineNrCurAttr
                  else
                    if verdict == Just Reviewed
                      then lineNrDoneAttr
                      else lineNrAttr
            in
              inkT
                left
                (penFor rowPens numAttr)
                (T.justifyRight numW ' ' (tshow (nr + 1)) <> " ")
          Nothing ->
            inkT left (penFor rowPens lineNrAttr) (T.replicate (numW + 1) " ")
        -- content shifted hOff columns left, for reading long lines:
        -- the view follows the cursor horizontally while the gutter
        -- stays pinned
        contentInk left =
          let
            marks = shiftSpans hOff emph
          in
            if rMark r == '-'
              then
                let
                  shown = padT (T.drop hOff (rText r))
                in
                  if null marks
                    then inkT left (penFor rowPens removedTextAttr) shown
                    else emphInk rowPens left marks [(removedTextAttr, shown)]
              else
                let
                  sliced = snd (splitAtCol hOff (effRuns r))
                in
                  case rowSpan r of
                    -- a live selection outranks the marks: it is what the
                    -- reader is pointing at this instant
                    Just (s, mEnd)
                      | fromMaybe maxBound mEnd >= hOff ->
                          selInk
                            rowPens
                            left
                            sliced
                            (max 0 (s - hOff))
                            (fmap (subtract hOff) mEnd)
                    _
                      | not (null marks) -> emphInk rowPens left marks sliced
                    _ ->
                      case nonEmptySegs sliced of
                        [] -> inkT left (penPlain rowPens) " "
                        filled -> segsInk rowPens left filled
        -- reviewed lines keep their full syntax colors; the gutter dot
        -- and the tray checkmark already say "answered for", and dimming
        -- whole regions made the buffer feel murky
        lineImg =
          spanInk
            paneWidth
            [ \left -> inkT left (penFor rowPens barName) (T.singleton barGlyph)
            , statusInk
            , numInk
            , \left ->
                inkT
                  left
                  (penFor rowPens (markerAttrFor (rMark r)))
                  (T.pack [rMark r, ' '])
            , contentInk
            ]
        -- the reviewer's note renders inline as virtual rows under
        -- the line it annotates — visible where the feedback lives
        noteImg nl =
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
            spanInk
              paneWidth
              [ \left -> inkT left (penFor pens barName) (T.singleton barGlyph)
              , \left ->
                  inkT
                    left
                    (penFor pens noteAttr)
                    ( T.replicate (numW + 4) " "
                        <> glyph
                        <> label
                        <> padT (nlText nl)
                    )
              ]
      in
        (lineImg : map noteImg shownNotes, isCursor)
    -- keep a margin of context visible around the cursor, so the view
    -- scrolls before the cursor reaches the top or bottom edge; virtual
    -- note rows above the cursor shift where it lands in the rendering
    notesAbove = sum (map length (take (cursorOut - lo) noteRowsList))
    cursorRender = (cursorOut - lo) + notesAbove
    scrollOff = cfgScrolloff (stConfig st)
    marginTop = max 0 (cursorRender - scrollOff)
    marginHeight = cursorRender + scrollOff - marginTop + 1
  in
    Widget Greedy Fixed $ do
      ctx <- getContext
      let
        pens = PenSet (ctxAttrName ctx) (ctxAttrMap ctx)
        blocks = map (rowInk pens) rowsWithPoints
        -- the cursor's screen row, or nothing at all when the window
        -- carries no row for the cursor's line — an all-removed file
        -- owns no buffer rows, and nothing there can wear a cursor
        cursorAt =
          let
            findY _ [] = Nothing
            findY y ((imgs, isCursor) : rest) =
              if isCursor
                then Just y
                else findY (y + length imgs) rest
          in
            findY (0 :: Int) blocks
        -- always leave border tail below the window so the divider
        -- spans the pane even when the content is shorter than the
        -- screen. Rows past the window are never addressed, so the
        -- tail only has to outlast the pane, not the file.
        tailRows =
          max
            (min (fvRowCount fv - hi) (windowRowsFor screenRows))
            (max 60 screenRows)
        bodyRows =
          concatMap fst blocks
            ++ [V.charFill (stBorder st) '\9614' (1 :: Int) tailRows]
        -- every row filled out to the width of the widest, so no
        -- attribute runs on past the text it belongs to
        fullWidth = maximum (0 : map V.imageWidth bodyRows)
        squared img =
          let
            short = fullWidth - V.imageWidth img
          in
            if short <= 0
              then img
              else
                V.horizJoin
                  img
                  (V.charFill (penPlain pens) ' ' short (V.imageHeight img))
        bodyImg = raw (V.vertCat (map squared bodyRows))
        placed = case cursorAt of
          Just y ->
            showCursor
              TheCursor
              (Location (numW + 5 + curCol - hOff, y))
              bodyImg
          Nothing -> bodyImg
      render (visibleRegion (Location (0, marginTop)) (1, marginHeight) placed)

-- How far the view is shifted right: zero until the cursor closes in on
-- the pane's right edge, then just enough to keep a small margin ahead
-- of it. Depends only on the cursor column and the pane width, so every
-- line shifts in step.
hScrollFor :: Int -> Int -> Int -> Int
hScrollFor paneWidth numW curCol =
  let
    contentWidth = max 1 (paneWidth - (numW + 5))
    margin = 8
  in
    max 0 (curCol - (contentWidth - margin))

nonEmptySegs :: [Seg] -> [Seg]
nonEmptySegs = filter (not . T.null . snd)

-- Words, for the purpose of telling two versions of a line apart: runs of
-- identifier characters, runs of space, runs of operator characters, and
-- brackets and separators one at a time. Each carries the column it
-- starts at, so a difference can be pointed at on screen.
--
-- An operator is a word in its own right — >>= and <$> are read as units
-- and change as units — but the brackets around one are not part of it.
-- Lumping all punctuation together made ")+(" a single token, so
-- changing the operator between two parenthesised terms marked the
-- parentheses along with it.
wordTokens :: Text -> [(Int, Text)]
wordTokens t =
  let
    kind c
      | isAlphaNum c || c == '_' = 0 :: Int
      | isSpace c = 1
      | c `elem` ("()[]{},;" :: String) = 2
      | otherwise = 3
    -- kind 2 never joins anything, itself included: "))" is two brackets
    sameWord a b = kind a == kind b && kind a /= 2
    groups = T.groupBy sameWord t
    starts = scanl (\n g -> n + T.length g) 0 groups
  in
    zip starts groups

-- What two lines have in common, counted two ways, because the two
-- questions asked of it want different answers.
--
-- Whether to mark at all is about words: a line where one long
-- identifier was renamed has little of its text left in common and is
-- still plainly an edit, so counting characters there would call it a
-- rewrite and say nothing.
--
-- Which of several candidates to pair is about characters: two lines can
-- share the same number of words and not be equally alike. Choosing
-- between "keeps <$>" and "keeps createdAtField" is a tie by the word,
-- and not remotely a tie to a reader.
data WordMatch = WordMatch
  { wmKeptWords :: !Int
  , wmWords :: !Int
  , wmKeptChars :: !Int
  , wmChars :: !Int
  , wmSpans :: ([(Int, Int)], [(Int, Int)])
  }

-- Below half their words in common two lines are a rewrite rather than an
-- edit, and pointing at every word says no more than the + and - already
-- did. Half rather than a third: a line whose binding name and = survive
-- already has a third of itself in common.
worthMarking :: WordMatch -> Bool
worthMarking wm = wmWords wm > 0 && wmKeptWords wm * 2 >= wmWords wm

-- How alike two lines are, for choosing between candidates. Nothing
-- compares this across different pairs of lines except bestPairs.
matchShare :: WordMatch -> Int
matchShare wm = (wmKeptChars wm * 1000) `div` max 1 (wmChars wm)

-- How much two lines have in common, and where they differ: the words
-- shared, the length of the longer, and the columns to mark on each side.
--
-- Whole words rather than characters: a renamed identifier should light
-- up as a name, not as a scatter of letters inside one. Under
-- WhitespaceIgnored the blanks are dropped before the comparison rather
-- than merely discounted — having said spacing is not a difference,
-- pointing at it anyway would be the noise that setting exists to avoid.
wordMatch :: WhitespaceMode -> Text -> Text -> WordMatch
wordMatch mode oldLine newLine =
  let
    tokensOf line = case mode of
      WhitespaceMatters -> wordTokens line
      WhitespaceIgnored ->
        filter (not . T.all isSpace . snd) (wordTokens line)
    olds = tokensOf oldLine
    news = tokensOf newLine
    grouped = getGroupedDiff (map snd olds) (map snd news)
    -- measured on words, not on the blanks between them: two lines with
    -- nothing whatever in common still share their spaces, and counting
    -- those as common was enough to call a rewrite an edit
    solid = filter (not . T.all isSpace)
    shared = solid (concatMap (\g -> case g of Both bs _ -> bs; _ -> []) grouped)
    oldWords = solid (map snd olds)
    newWords = solid (map snd news)
    chars = sum . map T.length
    spanOf toks i n =
      case (drop i toks, drop (i + n - 1) toks) of
        ((start, _) : _, (end, endText) : _) ->
          [(start, end + T.length endText - 1)]
        _ -> []
    walk _ _ [] = ([], [])
    walk oi ni (g : gs) =
      case g of
        Both bs _ -> walk (oi + length bs) (ni + length bs) gs
        First os ->
          let
            (a, b) = walk (oi + length os) ni gs
          in
            (spanOf olds oi (length os) ++ a, b)
        Second ns ->
          let
            (a, b) = walk oi (ni + length ns) gs
          in
            (a, spanOf news ni (length ns) ++ b)
  in
    WordMatch
      { wmKeptWords = length shared
      , wmWords = max (length oldWords) (length newWords)
      , wmKeptChars = chars shared
      , wmChars = max (chars oldWords) (chars newWords)
      , wmSpans = walk 0 0 grouped
      }

-- Which columns to mark on each row of the window. Rows arrive as a block
-- of removals followed by a block of additions, and the nth removal is
-- read against the nth addition.
--
-- Blocks of unequal size are paired too, as far as the shorter one goes.
-- Requiring them to match one for one looked like the careful choice and
-- was not: editing a line and adding another beside it makes a block of
-- one against two, which is an ordinary thing to do and left the edited
-- line with no marks at all. Nothing is risked by pairing anyway, because
-- a pair that does not resemble each other is dropped by wordSpans on the
-- way — the guard against marking the wrong words already lives there.
-- Above this many candidate pairings a block goes unmarked.
--
-- Matching is quadratic in the size of the block, and a block that large
-- is a wholesale rewrite where the marks would say nothing anyway. It
-- also cannot be matched honestly: only the rows in the render window
-- are on hand, so a block running past both ends of it offers the tail
-- of the removals against the head of the additions, and pairing those
-- produces marks that are confident and wrong.
pairingBudget :: Int
pairingBudget = 2500

-- Which added line each removed line was edited into. Not the one in the
-- same position: removing a line and editing the one below it is an
-- everyday shape, and by position that reads the deleted line against
-- the edited one's replacement, finds no resemblance, and leaves the
-- edit unmarked.
--
-- So every pairing is scored and the best taken first, then the best of
-- what is left, and so on. A line with no partner it resembles keeps
-- none, which is the honest answer for one that was purely added or
-- purely deleted.
bestPairs ::
  WhitespaceMode ->
  [RowR] ->
  [RowR] ->
  [(Int, Int, ([(Int, Int)], [(Int, Int)]))]
bestPairs mode removed added =
  let
    scored =
      [ (matchShare wm, i, j, wmSpans wm)
      | (i, o) <- zip [0 :: Int ..] removed
      , (j, n) <- zip [0 :: Int ..] added
      , let
          wm = wordMatch mode (rText o) (rText n)
      , worthMarking wm
      ]
    -- most text in common first; ties go to the earlier rows, which keeps
    -- equal-sized blocks pairing straight down as they read
    ranked = sortOn (\(share, i, j, _) -> (negate share, i, j)) scored
    take' _ _ [] = []
    take' usedO usedN ((_, i, j, spans) : rest)
      | Set.member i usedO || Set.member j usedN = take' usedO usedN rest
      | otherwise =
          (i, j, spans) : take' (Set.insert i usedO) (Set.insert j usedN) rest
  in
    take' Set.empty Set.empty ranked

intralineSpans :: WhitespaceMode -> [RowR] -> [[(Int, Int)]]
intralineSpans mode =
  let
    go [] = []
    go rows@(r : _)
      | rMark r == '-' =
          let
            (removed, afterRemoved) = span ((== '-') . rMark) rows
            (added, rest) = span ((== '+') . rMark) afterRemoved
            chosen =
              if length removed * length added > pairingBudget
                then []
                else bestPairs mode removed added
            olds = Map.fromList [(i, fst sp) | (i, _, sp) <- chosen]
            news = Map.fromList [(j, snd sp) | (_, j, sp) <- chosen]
            spansFor m n = [Map.findWithDefault [] k m | k <- [0 .. n - 1]]
          in
            spansFor olds (length removed)
              ++ spansFor news (length added)
              ++ go rest
      | otherwise = [] : go (drop 1 rows)
  in
    go

-- Ranges moved into the coordinates of a horizontally scrolled line, and
-- those scrolled off the left dropped.
shiftSpans :: Int -> [(Int, Int)] -> [(Int, Int)]
shiftSpans off =
  mapMaybe
    ( \(s, e) ->
        if e < off then Nothing else Just (max 0 (s - off), e - off)
    )

effRuns :: RowR -> [Seg]
effRuns r =
  case nonEmptySegs (rToks r) of
    [] -> [(attrName "plain", rText r)]
    filled -> filled

-- Drawing the body ---------------------------------------------------------------

-- The body is drawn as vty images rather than composed out of brick
-- widgets. Brick's boxes carry border-map bookkeeping through every
-- composition — merging, translating and cropping the border segments a
-- box might have to join — and a six-hundred-row window whose every row
-- was a dozen nested boxes paid for that thousands of times a frame, for
-- borders the body does not have. It was three quarters of the time
-- spent between a keystroke and the screen.
--
-- What a widget did at render time is done here instead, and the pens
-- are how: an attribute name resolves against exactly the map a withAttr
-- would have consulted, so the colors are the ones the widgets produced.
data PenSet = PenSet
  { penBaseName :: AttrName -- the ambient name, what plain text renders in
  , penAttrs :: AttrMap
  }

-- The attribute a withAttr of this name would have resolved to.
penFor :: PenSet -> AttrName -> V.Attr
penFor pens name = attrMapLookup name (penAttrs pens)

-- The attribute unnamed text renders in: the map's default, which is
-- what withDefAttr replaces.
penPlain :: PenSet -> V.Attr
penPlain pens = penFor pens (penBaseName pens)

-- withDefAttr: the named attribute becomes the default everything under
-- it inherits from, so a token that sets only a foreground picks up the
-- selection or the word-change wash behind it.
penUnder :: AttrName -> PenSet -> PenSet
penUnder name pens =
  pens {penAttrs = setDefaultAttr (penFor pens name) (penAttrs pens)}

-- Text clipped to a column budget, the way a nested box clipped it
-- against the width its siblings had left. Empty text draws nothing,
-- which is what brick's txt does with it.
inkT :: Int -> V.Attr -> Text -> V.Image
inkT width attr t =
  if width <= 0 || T.null t
    then V.emptyImage
    else
      let
        -- a column is never narrower than a character, so taking that
        -- many characters cannot come up short of the budget
        img = V.text' attr (T.take width t)
      in
        if V.imageWidth img > width
          then V.cropRight width img
          else img

-- A row filled out to the pane's width, or cropped back to it: what a
-- padRight Max did, and what the box around it did afterwards.
fitRow :: V.Attr -> Int -> V.Image -> V.Image
fitRow attr width img =
  let
    short = width - V.imageWidth img
  in
    if short > 0
      then V.horizJoin img (V.charFill attr ' ' short (1 :: Int))
      else
        if short < 0
          then V.cropRight width img
          else img

-- Pieces laid left to right against one shared budget, each offered only
-- what the pieces before it did not use.
spanInk :: Int -> [Int -> V.Image] -> V.Image
spanInk width pieces =
  let
    go _ [] = []
    go left (piece : rest) =
      if left <= 0
        then []
        else
          let
            img = piece left
          in
            img : go (left - V.imageWidth img) rest
  in
    V.horizCat (go width pieces)

-- One run of segments, each drawn in the attribute its name resolves to.
segsInk :: PenSet -> Int -> [Seg] -> V.Image
segsInk pens width segs =
  spanInk width (map (\(a, t) left -> inkT left (penFor pens a) t) segs)

-- Runs of segments that read their attributes against different pens,
-- sharing one budget across the lot.
runsInk :: Int -> [(PenSet, [Seg])] -> V.Image
runsInk width parts =
  spanInk width (map (\(pens, segs) left -> segsInk pens left segs) parts)

-- Segments with the given columns wearing the word-change wash. The wash
-- sets only a background, so a token keeps whatever colour the grammar
-- gave it and gains the mark on top.
emphInk :: PenSet -> Int -> [(Int, Int)] -> [Seg] -> V.Image
emphInk pens width spans segs =
  let
    washed = penUnder wordChangeAttr pens
    go _ [] rest = [(pens, rest)]
    go col ((s, e) : more) rest =
      let
        (before, fromStart) = splitAtCol (s - col) rest
        (mid, after) = splitAtCol (e + 1 - s) fromStart
      in
        (pens, before) : (washed, mid) : go (e + 1) more after
  in
    runsInk width (go 0 spans segs)

-- Token runs with the char selection's columns highlighted.
selInk :: PenSet -> Int -> [Seg] -> Int -> Maybe Int -> V.Image
selInk pens width segs s mEnd =
  let
    (before, rest) = splitAtCol s segs
    (mid, after) = case mEnd of
      Nothing -> (rest, [])
      Just e -> splitAtCol (e + 1 - s) rest
    selPens = penUnder selAttr pens
    midSegs = case nonEmptySegs mid of
      -- keep the selection visible across empty stretches
      [] -> [(penBaseName pens, " ")]
      ms -> ms
  in
    runsInk width [(pens, before), (selPens, midSegs), (pens, after)]

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
      Just (SearchPrompt input) ->
        txt (searchSigil st <> input <> "\9612")
      Just (CmdPrompt input) -> txt (":" <> input <> "\9612")
      Nothing -> withAttr msgAttr (txt (padT (stMsg st)))
    chip dotAttr label =
      hBox
        [ txt " "
        , withAttr dotAttr (txt "\9679")
        , withAttr statusAttr (txt (" " <> label <> " "))
        ]
    -- what the buffer is waiting for, most specific first: the space
    -- menu, an emacs chord, a half-typed vi command, insert, then plain
    -- normal — with vi's own resting label saying which keymap you are in
    modeChip =
      if stSpace st
        then chip modeDotNorAttr "spc\8230"
        else
          if stIdleGo st
            then chip modeDotNorAttr "g\8230"
            else case stEmacsPrefix st of
              Just pfx -> chip modeDotNorAttr ("C-" <> T.singleton pfx <> "\8230")
              Nothing ->
                case cfgKeys (stConfig st) of
                  KeysEmacs -> chip modeDotNorAttr "emx"
                  KeysVi -> case currentFileMay st of
                    Nothing -> chip modeDotNorAttr "vi"
                    Just fv -> case edMode (fvEd fv) of
                      Insert -> chip modeDotInsAttr "ins"
                      Normal -> case viLabel (stVi st) of
                        Just p -> chip modeDotNorAttr (p <> "\8230")
                        Nothing ->
                          if edExtend (fvEd fv)
                            then chip modeDotNorAttr "vis"
                            else chip modeDotNorAttr "vi"
                  KeysHelix -> case currentFileMay st of
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
        -- nearest decl at or above the cursor, found by walking up
        -- from it rather than rebuilding the buffer every frame
        case find isDeclLine (bufferUpward (fvEd fv)) of
          Just d ->
            withAttr trayDeclAttr (txt (" " <> T.take 32 (declName d) <> " \183"))
          Nothing -> txt ""
      Nothing -> txt ""
    -- in vi the verbs sit under the leader, so the hint has to say so
    verbHint = case cfgKeys (stConfig st) of
      KeysVi -> "\9251\9166 accept · \9251! flag · \9251q question · \9251? keys "
      _ -> "\9166 accept · ! flag · ? question · \9251 menu · \9251? keys "
    hint = case currentFileMay st of
      Nothing -> ": command · \9251? keys · :q quit "
      Just fv ->
        (if fvStale fv then "hunks refresh on Esc · " else "") <> verbHint
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

-- Waiting on you: a slow breath, four frames to a cycle.
throbGlyph :: Int -> Text
throbGlyph n =
  case n `mod` 4 of
    0 -> "\183" -- ·
    1 -> "\8226" -- •
    2 -> "\9679" -- ●
    _ -> "\8226"

-- Busy on your behalf: two frames, so it reads as quicker than the
-- breath even though both are redrawn on the same beat.
busyGlyph :: Int -> Text
busyGlyph n =
  if even n
    then "\10022" -- ✦
    else "\10023" -- ✧

padT :: Text -> Text
padT t = if T.null t then " " else t

-- Progress as quiet blocks: one cell per hunk up to ten, then scaled.
-- The filled run only completes when the work does.
-- How many cells the meter has, and how many of them are filled. The
-- filled run only completes when the work does.
meterCells :: Int -> Int -> (Int, Int)
meterCells done total =
  let
    cells = min 10 (max 1 total)
    filled =
      if done >= total
        then cells
        else min (cells - 1) (div (done * cells) (max 1 total))
  in
    (cells, filled)

meterW :: Int -> Int -> Widget Name
meterW done total =
  let
    (cells, filled) = meterCells done total
  in
    hBox
      [ withAttr meterOnAttr (txt (T.replicate filled "\9648"))
      , withAttr meterOffAttr (txt (T.replicate (cells - filled) "\9649"))
      , withAttr hintAttr (txt (" " <> tshow done <> "/" <> tshow total))
      ]

meterInk :: PenSet -> Int -> Int -> Int -> V.Image
meterInk pens width done total =
  let
    (cells, filled) = meterCells done total
  in
    spanInk
      width
      [ \left -> inkT left (penFor pens meterOnAttr) (T.replicate filled "\9648")
      , \left -> inkT left (penFor pens meterOffAttr) (T.replicate (cells - filled) "\9649")
      , \left ->
          inkT left (penFor pens hintAttr) (" " <> tshow done <> "/" <> tshow total)
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
    AppEvent (FileChangedOnDisk absolute) -> do
      st <- get
      -- relativize here, against whichever repository is current now
      let
        rel = makeRelative (stRoot st) absolute
      unless (isAbsolute rel) (reloadFromDisk rel)
    AppEvent GitStateChanged -> refreshGit
    AppEvent StoreChanged -> do
      st <- get
      threads <- liftIO (loadThreads (stConn st) (repoText st))
      modify (\s -> s {stThreads = threads})
    AppEvent (Attending seen) ->
      modify (\s -> s {stAttending = seen, stThrob = stThrob s + 1})
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
              Nothing
                | stIdleGo st -> do
                    modify (\s -> s {stIdleGo = False})
                    handleIdleGo ev
                | null (stFiles st) -> handleIdleKey ev
                | otherwise -> handleModalKey ev
    _ -> pure ()

-- The helix-style space menu: Space, then one key; anything unlisted
-- just cancels. Dispatch reads spaceMenuEntries, which is also what the
-- infobox renders, so the menu and the bindings cannot disagree.
handleSpaceKey :: V.Event -> EventM Name St ()
handleSpaceKey ev = do
  modify (\s -> s {stSpace = False})
  let
    -- the table is keyed by what the menu prints, so Enter has to become
    -- its own glyph to find its row
    typed = case ev of
      V.EvKey (V.KChar c) [] -> Just (T.singleton c)
      V.EvKey V.KEnter [] -> Just "\9166"
      _ -> Nothing
  case typed of
    Nothing -> pure ()
    Just key ->
      case find (\(k, _, _) -> k == key) spaceMenuEntries of
        Just (_, _, action) -> action
        Nothing -> pure ()

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

-- Space k cycles all → code → comments; :filter names one outright.
-- Independent of the pending filter, so "the code hunks nobody has
-- looked at yet" is one combination of the two.
cycleRunFilter :: EventM Name St ()
cycleRunFilter = do
  st <- get
  setRunFilter (nextRunFilter (stRunFilter st))

setRunFilter :: RunFilter -> EventM Name St ()
setRunFilter chosen = do
  modify (\s -> s {stRunFilter = chosen, stMsg = filterMsg chosen})
  -- the entry list reshapes under the viewport, so the current hunk has
  -- to be found again
  scrollTrayToCurrent

filterMsg :: RunFilter -> Text
filterMsg chosen =
  case chosen of
    FilterAll -> "walking every hunk"
    FilterCode -> "walking code hunks only"
    FilterComments -> "walking comment hunks only"

-- With nothing on screen the only meaningful keys are quitting and the
-- command line (to switch what the session is looking at).
handleIdleKey :: V.Event -> EventM Name St ()
handleIdleKey ev =
  case ev of
    V.EvKey (V.KChar ':') [] -> openCmdPrompt
    V.EvKey (V.KChar 'p') [V.MCtrl] -> openFileFinder
    V.EvKey (V.KChar 'g') [] -> modify (\s -> s {stIdleGo = True})
    V.EvKey (V.KChar ' ') [] -> modify (\s -> s {stSpace = True})
    V.EvKey (V.KChar 'x') [V.MMeta] -> openCmdPrompt
    V.EvKey (V.KChar 'x') [V.MCtrl] ->
      modify (\s -> s {stEmacsPrefix = Just 'x'})
    _ -> pure ()

-- The g menu with nothing on screen. Only gr means anything here: there
-- is no file to move around in, and switching repository is most of what
-- an empty session is for — a story's first checkout being clean is
-- exactly when the next one is wanted.
handleIdleGo :: V.Event -> EventM Name St ()
handleIdleGo ev =
  case ev of
    V.EvKey (V.KChar 'r') [] -> openRepoPicker
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
            st <- get
            unless (T.null t) (searchStep (stSearchDir st) t)
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
  case cfgKeys (stConfig st) of
    KeysEmacs -> handleEmacsKey i ed ev
    KeysVi -> handleViKey i ed ev
    KeysHelix -> handleHelixKey i fv ed ev

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
        V.EvKey (V.KChar 's') [V.MCtrl] -> openSearchPrompt 1
        V.EvKey (V.KChar 'c') [V.MCtrl] ->
          modify (\s -> s {stEmacsPrefix = Just 'c'})
        V.EvKey (V.KChar 'x') [V.MCtrl] ->
          modify (\s -> s {stEmacsPrefix = Just 'x'})
        -- C-y is emacs's paste, so it consults the clipboard the way
        -- p does in the modal keymaps
        V.EvKey (V.KChar 'y') [V.MCtrl] -> do
          pullClipboard
          refreshed <- get
          emacsEdit i ed {edKill = stKill refreshed} ev
        _ -> emacsEdit i ed ev

emacsEdit :: Int -> EdState -> V.Event -> EventM Name St ()
emacsEdit i ed ev =
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
        V.EvKey (V.KChar 'k') [] -> cycleRunFilter
        V.EvKey (V.KChar 'u') [] -> unnoteCmd
        V.EvKey (V.KChar 'h') [] -> setCurrentHunkHere
        V.EvKey (V.KChar 'v') [] -> revertTarget
        V.EvKey (V.KChar 'a') [] -> gotoAltFile
        V.EvKey (V.KChar '/') [] -> openGrepPicker ""
        V.EvKey (V.KChar 'c') [] -> showCommitPopup
        V.EvKey (V.KChar '\'') [] -> jumpToAnchor
        V.EvKey (V.KChar '*') [] -> searchWordUnderCursor
        V.EvKey (V.KChar 'n') [] -> searchRepeat 1
        V.EvKey (V.KChar 'N') [] -> searchRepeat (-1)
        V.EvKey (V.KChar 'f') [] -> openFileFinder
        V.EvKey (V.KChar 'b') [] -> openBufferPicker
        V.EvKey (V.KChar 'e') [] -> editCurrent
        V.EvKey (V.KChar ')') [] -> switchFile 1
        V.EvKey (V.KChar '(') [] -> switchFile (-1)
        V.EvKey (V.KChar '\t') [] ->
          modify (\s -> s {stTray = not (stTray s)})
        _ -> pure ()

-- Insert mode is the same in every modal keymap: type, and land the
-- result back in the file view as cheaply as the edit allows — a
-- same-shape edit patches one line rather than rebuilding the diff.
handleInsertKey :: Int -> EdState -> V.Event -> EventM Name St ()
handleInsertKey i ed ev =
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
                ( \s ->
                    updateFile
                      i
                      ( patchLine
                          (stConfig s)
                          (fst (cursorRC edNext))
                          (currentLineOf edNext)
                          . withEd edNext
                      )
                      s
                )
            else rebuildWith i edNext
    InsertExit edNext -> do
      rebuildWith i edNext
      applyToDisk i
    InsertUnhandled -> pure ()

handleHelixKey :: Int -> FileView -> EdState -> V.Event -> EventM Name St ()
handleHelixKey i fv ed ev = do
  st <- get
  case edMode ed of
    Insert -> handleInsertKey i ed ev
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
    -- gr: go to another repository. Free in every keymap remax has, and
    -- the g menu is where going somewhere already lives. Space R is the
    -- same door for the keymaps that have no g of their own.
    Normal
      | pendingLabel ed == Just "g"
      , V.EvKey (V.KChar 'r') [] <- ev -> do
          deferToModal i ed ev
          openRepoPicker
    -- gn/gp: the next and previous file. helix spends these on buffers,
    -- and a file under review is what remax has instead of one, so they
    -- keep the meaning a reader arrives with. ) and ( are the same move
    -- in one keystroke; these are for the hand that already knows helix.
    Normal
      | pendingLabel ed == Just "g"
      , V.EvKey (V.KChar 'n') [] <- ev -> do
          deferToModal i ed ev
          switchFile 1
    Normal
      | pendingLabel ed == Just "g"
      , V.EvKey (V.KChar 'p') [] <- ev -> do
          deferToModal i ed ev
          switchFile (-1)
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
        V.EvKey (V.KChar '/') [] -> openSearchPrompt 1
        V.EvKey (V.KChar ':') [] -> openCmdPrompt
        V.EvKey (V.KChar '*') [] -> searchWordUnderCursor
        V.EvKey (V.KChar 'n') [] -> searchRepeat 1
        V.EvKey (V.KChar 'N') [] -> searchRepeat (-1)
        V.EvKey (V.KChar '\'') [] -> jumpToAnchor
        V.EvKey (V.KChar 'X') [] -> selectCurrentHunk
        V.EvKey (V.KChar 'p') [] -> pasteKey deferToModal i ev
        V.EvKey (V.KChar 'P') [] -> pasteKey deferToModal i ev
        V.EvKey (V.KChar ' ') [] ->
          modify (\s -> s {stSpace = True})
        V.EvKey (V.KChar 'E') [] -> do
          let
            (row, _) = cursorRC ed
          launchEditor (stRoot st </> fvPath fv) (row + 1)
        V.EvKey (V.KChar ')') [] -> switchFile 1
        V.EvKey (V.KChar '(') [] -> switchFile (-1)
        V.EvKey (V.KChar 'p') [V.MCtrl] -> openFileFinder
        V.EvKey (V.KChar '{') [] ->
          vScrollBy (viewportScroll TrayVP) (-8)
        V.EvKey (V.KChar '}') [] ->
          vScrollBy (viewportScroll TrayVP) 8
        _ -> deferToModal i ed ev

-- Vi keymap: the buffer belongs to vi, and the review verbs live behind
-- the Space leader the way the emacs keymap puts them behind C-c. What
-- keeps its remax key is structural navigation — hunks, files, the tray,
-- the anchor back to what is under review — because that is the tool's
-- own vocabulary rather than the editor's, and vi's sentence and
-- paragraph motions are a small price for it. Everything else is vi's:
-- `?` searches backward, `X` deletes leftward, `E` is the WORD motion
-- (Space e still opens $EDITOR here).
handleViKey :: Int -> EdState -> V.Event -> EventM Name St ()
handleViKey i ed ev = do
  st <- get
  case edMode ed of
    Insert -> handleInsertKey i ed ev
    -- a half-typed vi command owns the next keystroke outright, so `f]`
    -- finds a bracket instead of walking to the next hunk, and `2` then
    -- `}` counts rather than scrolling the tray
    Normal | viBusy (stVi st) -> deferToVi i ed ev
    Normal ->
      case ev of
        V.EvKey (V.KChar '\t') [] ->
          modify (\s -> s {stTray = not (stTray s)})
        V.EvKey (V.KChar ']') [] -> switchRun 1
        V.EvKey (V.KChar '[') [] -> switchRun (-1)
        V.EvKey (V.KChar ')') [] -> switchFile 1
        V.EvKey (V.KChar '(') [] -> switchFile (-1)
        V.EvKey (V.KChar '{') [] ->
          vScrollBy (viewportScroll TrayVP) (-8)
        V.EvKey (V.KChar '}') [] ->
          vScrollBy (viewportScroll TrayVP) 8
        V.EvKey (V.KChar '\'') [] -> jumpToAnchor
        V.EvKey (V.KChar ':') [] -> openCmdPrompt
        V.EvKey (V.KChar '/') [] -> openSearchPrompt 1
        V.EvKey (V.KChar '?') [] -> openSearchPrompt (-1)
        V.EvKey (V.KChar 'n') [] -> searchRepeat 1
        V.EvKey (V.KChar 'N') [] -> searchRepeat (-1)
        V.EvKey (V.KChar '*') [] -> searchWordUnderCursor
        V.EvKey (V.KChar '#') [] -> searchWordBackward
        V.EvKey (V.KChar 'p') [V.MCtrl] -> openFileFinder
        V.EvKey (V.KChar ' ') [] ->
          modify (\s -> s {stSpace = True})
        V.EvKey (V.KChar 'p') [] -> pasteKey deferToVi i ev
        V.EvKey (V.KChar 'P') [] -> pasteKey deferToVi i ev
        _ -> deferToVi i ed ev

-- The vi layer's own state lives in St, so the result carries both a new
-- key state and (when the command completed) a new buffer.
deferToVi :: Int -> EdState -> V.Event -> EventM Name St ()
deferToVi i ed ev = do
  st <- get
  case handleVi (stVi st) ev ed of
    ViUnhandled -> pure ()
    ViPending keys -> modify (\s -> s {stVi = keys})
    ViDone keys edNext -> do
      modify (\s -> s {stVi = keys})
      if bufferLines edNext /= bufferLines ed
        then do
          rebuildWith i edNext
          when (edMode edNext == Normal) (applyToDisk i)
        else setEd i edNext

-- A paste key, whichever keymap asked: consult the system clipboard,
-- then hand the key to the editor with the slot already refreshed. An
-- empty slot after that really is empty, and the message says which of
-- the two reasons applies.
pasteKey ::
  (Int -> EdState -> V.Event -> EventM Name St ()) ->
  Int ->
  V.Event ->
  EventM Name St ()
pasteKey deliver i ev = do
  pullClipboard
  st <- get
  let
    ed = (fvEd (stFiles st !! i)) {edKill = stKill st}
  if T.null (edKill ed)
    then
      setMsg
        ( if clipboardReadable (stClip st)
            then "nothing to paste — y yanks (the pending filter is Space p)"
            else "nothing to paste — no clipboard helper here (wl-copy, xclip, xsel, pbcopy)"
        )
    else deliver i ed ev

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
            (rebuildFV (stConfig st) (repoText st) fv {fvHighlighted = True})
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

setEd :: Int -> EdState -> EventM Name St ()
setEd i ed = do
  modify (\s -> updateFile i (withEd (ed {edCopied = False})) s {stKill = edKill ed})
  publishCopy ed

-- Every keymap's copy lands in the same slot, so this is the one place
-- the system clipboard has to be told: whatever was just deliberately
-- copied goes out. Deletes never set the flag, so pulling a line out of
-- a file cannot clobber what you copied from somewhere else.
publishCopy :: EdState -> EventM Name St ()
publishCopy ed = do
  st <- get
  let
    copied = edKill ed
  when (edCopied ed && not (T.null copied) && copied /= stClipSent st) $ do
    vty <- getVtyHandle
    ok <-
      liftIO
        (writeClipboard (stClip st) (V.outputByteBuffer (V.outputIface vty)) copied)
    when ok (modify (\s -> s {stClipSent = copied}))

-- The other direction, run just before a paste: if the clipboard holds
-- something remax did not put there, that is what you meant to paste.
-- Text arriving from outside carries its own line-wise sense — a copy of
-- whole lines ends in a newline, which is exactly what makes p put them
-- on their own lines.
pullClipboard :: EventM Name St ()
pullClipboard = do
  st <- get
  arrived <- liftIO (readClipboard (stClip st))
  case arrived of
    Just text
      | not (T.null text)
      , text /= stClipSent st ->
          modify (\s -> s {stKill = text, stClipSent = text})
    _ -> pure ()

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
    bufferLen = newSideLineCount fv
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
  screenRows <- terminalRows
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
        -- the body draws from its window's first row, so the viewport's
        -- coordinates are relative to that same origin. A jump can land
        -- anywhere, so the window is moved with it and written back here
        -- rather than left for the drift check to discover.
        origin = snd (bodyWindow screenRows st fv)
        noteRoom = noteTextWidth (fst (_vpSize vp)) fv
        startIn = max 0 (start - origin)
        -- best-effort correction for virtual note rows rendered above
        -- the hunk (exact when the render window covers them)
        renderStart =
          startIn
            + sum
              ( map
                  length
                  ( take
                      startIn
                      ( drop
                          origin
                          (shownNoteLines noteRoom st (fvRows fv))
                      )
                  )
              )
        top =
          if len + 4 >= vpH
            then max 0 (renderStart - 2)
            else max 0 (renderStart - ((vpH - len) `div` 2))
      modify (updateFile (stCurF st) (\f -> f {fvOrigin = origin}))
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
-- hunks that already have verdicts are skipped entirely, and the
-- code/comment filter skips whichever kind you are not reading.
switchRun :: Int -> EventM Name St ()
switchRun d = do
  st <- get
  let
    eligible pair =
      (not (stOnlyPending st) || pairPending st pair) && pairInFilter st pair
    pairs = filter eligible (allRunPairs st)
    here =
      ( stCurF st
      , clampH (fvRuns (currentFile st)) (stCurH st)
      )
    -- name whichever filter emptied the list, so an empty walk is never
    -- mistaken for a finished review
    emptyMsg = case stRunFilter st of
      FilterCode -> "no code hunks left"
      FilterComments -> "no comment hunks left"
      FilterAll -> "no pending hunks left"
  case pairs of
    [] -> modify (\s -> s {stMsg = emptyMsg})
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
searchWordUnderCursor = searchWordDir 1

-- vi's '#': the same needle, walked the other way.
searchWordBackward :: EventM Name St ()
searchWordBackward = searchWordDir (-1)

searchWordDir :: Int -> EventM Name St ()
searchWordDir dir = do
  st <- get
  let
    ed = fvEd (currentFile st)
    ls = bufferLines ed
    rowText r = fromMaybe "" (safeIdx ls r)
    -- only a deliberate v-extend selection overrides the cursor word;
    -- the incidental selections that w/e-style motions leave behind drag
    -- neighboring punctuation along and are not what * means
    selectionNeedle = do
      ((lr, lc), (hr, hc)) <- charSelection ed
      if not (edExtend ed) || (lr, lc) == (hr, hc) || lr /= hr
        then Nothing
        else
          let
            l = rowText lr
          in
            Just (T.take (hc + 1 - lc) (T.drop lc l))
    isIdent ch = isAlphaNum ch || ch == '_' || ch == '\''
    wordNeedle =
      let
        (row, col) = cursorRC ed
        l = rowText row
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
      modify
        (\s -> s {stLastSearch = needle, stGrepNav = Nothing, stSearchDir = dir})
      searchStep dir needle

-- `/` searches forward and vi's `?` backward; the prompt wears whichever
-- sigil opened it, and n/N repeat along that grain rather than always
-- downward. In the keymaps with no `?`, the direction is always forward
-- and this is exactly the old behavior.
openSearchPrompt :: Int -> EventM Name St ()
openSearchPrompt dir =
  modify (\s -> s {stPrompt = Just (SearchPrompt ""), stSearchDir = dir})

searchSigil :: St -> Text
searchSigil st = if stSearchDir st < 0 then "?" else "/"

-- n keeps the search's own direction, N reverses it.
searchRepeat :: Int -> EventM Name St ()
searchRepeat sense = do
  st <- get
  searchNext (sense * stSearchDir st)

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
selectedChanges :: Bool -> FileView -> Int -> Int -> [(Text, Text, Int)]
selectedChanges withRemarks fv lo hi =
  let
    rows = fvRows fv
    bufferLen = newSideLineCount fv
    points =
      drop 1 (scanr (\row acc -> fromMaybe acc (rNewRow row)) bufferLen rows)
    inSpan row point = case rNewRow row of
      Just nr -> nr >= lo && nr <= hi
      Nothing -> point >= lo && point <= hi
    pick (row, point) = do
      k <-
        if rMark row == ' '
          then if withRemarks then rRemarkKey row else Nothing
          else rKey row
      if inSpan row point
        then Just (k, T.cons (rMark row) (rText row), fromMaybe point (rNewRow row) + 1)
        else Nothing
  in
    mapMaybe pick (zip rows points)

-- Which hunks a line span actually touches, by the same reckoning
-- selectedChanges uses — a removed line counts against the buffer row it
-- now rests before, so a deletion is selectable at all.
selectedRuns :: FileView -> Int -> Int -> Set.Set Int
selectedRuns fv lo hi =
  let
    rows = fvRows fv
    bufferLen = newSideLineCount fv
    points =
      drop 1 (scanr (\row acc -> fromMaybe acc (rNewRow row)) bufferLen rows)
    inSpan row point = case rNewRow row of
      Just nr -> nr >= lo && nr <= hi
      Nothing -> point >= lo && point <= hi
    pick (row, point) = if inSpan row point then rRunIx row else Nothing
  in
    Set.fromList (mapMaybe pick (zip rows points))

-- Every changed line of one run, same shape as selectedChanges.
runChanges :: FileView -> Int -> [(Text, Text, Int)]
runChanges fv h =
  let
    rows = fvRows fv
    bufferLen = newSideLineCount fv
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
-- Where each row sits in the buffer: its own line, or — for a removed
-- line, which has none — the line it rests before.
--
-- A hunk that only removes occupies no buffer line whatsoever, so
-- anything that reasons about rows by buffer position has to consult
-- this or it will simply not see such a hunk.
rowRestPoints :: FileView -> [(RowR, Int)]
rowRestPoints fv =
  let
    rows = fvRows fv
    bufferLen = newSideLineCount fv
    points =
      drop 1 (scanr (\row acc -> fromMaybe acc (rNewRow row)) bufferLen rows)
  in
    zip rows points

-- The buffer line a row answers to, whether it owns one or rests before
-- one.
rowAt :: (RowR, Int) -> Int
rowAt (row, point) = fromMaybe point (rNewRow row)

hunkUnderCursor :: St -> Maybe Int
hunkUnderCursor st = do
  fv <- currentFileMay st
  let
    (cur, _) = cursorRC (fvEd fv)
    resting = rowRestPoints fv
    directHit (row, _) =
      if rNewRow row == Just cur then rRunIx row else Nothing
    restingHit (row, point) =
      if rNewRow row == Nothing && point == cur then rRunIx row else Nothing
    -- Several delete-only hunks can rest at one line — an all-removed
    -- file rests every one of them at line 0 — and then the cursor
    -- cannot say which is meant. The hunk under review is the honest
    -- answer: it is the one ] and [ moved to and the one the bar marks.
    restingOn = mapMaybe restingHit resting
    onReview = clampH (fvRuns fv) (stCurH st)
  case mapMaybe directHit resting of
    (h : _) -> Just h
    [] ->
      case find (== onReview) restingOn of
        Just h -> Just h
        Nothing -> listToMaybe restingOn

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
            -- by where a row answers, not by where it owns a line: a
            -- removal-only hunk has no line of its own, so matching on
            -- rNewRow alone made it unselectable and left X-then-revert
            -- reporting nothing to do
            touchedBySpan (a, b) =
              let
                pick entry@(row, _) =
                  let
                    n = rowAt entry
                  in
                    case rRunIx row of
                      Just ix | n >= a && n <= b -> Just ix
                      _ -> Nothing
              in
                Set.fromList (mapMaybe pick (rowRestPoints fv))
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
      after <- get
      let
        undoing =
          MarkUndo
            { muRows = stLastRows after
            , muBefore =
                Map.fromList
                  [ (k, Map.lookup k (stVerdicts preSt))
                  | k <- stLastMarked after
                  ]
            , muCounts = stCounts preSt
            , muPunch = stPunch preSt
            }
      openNotePopup (NoteFromMark undoing)
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
      when (v == Reviewed) (closeConversation (runKeys r))
      modify $ \s ->
        s
          { stVerdicts =
              foldr (\k m -> Map.insert k v m) (stVerdicts s) (runKeys r)
          , stLastMarked = runKeys r
          , stLastRows = rows
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
    -- accepting says a change has been read, and a line nobody changed
    -- has none; ! and ? are remarks and reach any line
    picks = selectedChanges (v /= Reviewed) fv lo hi
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
      when (v == Reviewed) (closeConversation (map (\(k, _, _) -> k) picks))
      modify $ \s ->
        s
          { stVerdicts =
              foldr (\(k, _, _) m -> Map.insert k v m) (stVerdicts s) picks
          , stLastMarked = map (\(k, _, _) -> k) picks
          , stLastRows = map storeRow picks
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
        curIx = clampH rs (stCurH settled)
        -- A verdict says something only about the hunks its lines belong
        -- to. Marking a line somewhere else leaves the current hunk
        -- exactly as unreviewed as it was, so asking whether that hunk is
        -- finished is asking about the wrong thing: it would move the
        -- review on off the back of a verdict that never touched it.
        onCurrent = Set.member curIx (selectedRuns fvSettled lo hi)
        hunkDone =
          case safeIdx rs curIx of
            Just r -> not (runUnvisited (stVerdicts settled) r)
            Nothing -> True
      -- including the advance a flag would otherwise have deferred until
      -- its note was written
      unless onCurrent (modify (\s -> s {stDeferAdvance = False}))
      let
        deferring = stDeferAdvance settled && onCurrent
      if onCurrent && hunkDone
        then do
          modify
            (\s -> s {stCounts = Map.insertWith (+) v 1 (stCounts s)})
          setEd (stCurF settled) (collapseSelection (fvEd fvSettled))
          unless deferring advanceToPending
        else unless deferring $ do
          let
            ed = fvEd fvSettled
            nextRow = min (hi + 1) (bufferLineCount ed - 1)
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
          -- clearing reaches remarks too: a remark you have thought
          -- better of should come off the same way a verdict does
          map (\(k, _, _) -> k) (selectedChanges True fv lo hi)
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

-- Put back what a mark changed, in the store and on screen.
--
-- Not simply an unmark: a line that already carried a verdict had it
-- overwritten the instant the key was pressed, so cancelling has to
-- restore that rather than leave the line bare.
cancelMark :: MarkUndo -> EventM Name St ()
cancelMark undoing = do
  st <- get
  let
    before = muBefore undoing
    fresh = [k | (k, Nothing) <- Map.toList before]
    replaced = Map.fromListWith (++) [(v, [k]) | (k, Just v) <- Map.toList before]
    rowsFor ks = filter (\(k, _, _, _) -> k `elem` ks) (muRows undoing)
  unless (null fresh) $
    liftIO (deleteKeys (stConn st) (repoText st) fresh)
  mCommit <- verdictCommitContext
  forM_ (Map.toList replaced) $ \(v, ks) ->
    liftIO
      ( recordVerdicts
          (stConn st)
          (repoText st)
          (stPassId st)
          v
          mCommit
          (rowsFor ks)
      )
  modify $ \s ->
    s
      { stVerdicts =
          Map.foldrWithKey
            (\k mv m -> maybe (Map.delete k m) (\v -> Map.insert k v m) mv)
            (stVerdicts s)
            before
      , -- the tallies the summary prints, as they stood before the mark
        stCounts = muCounts undoing
      , stPunch = muPunch undoing
      , stLastMarked = []
      , stLastRows = []
      }

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
        (upToHere, afterHere) = break (> here) (filteredRunPairs st)
      in
        afterHere ++ upToHere
    unvisited = filter (pairUnvisited st) wrapped
    pending = filter (pairPending st) wrapped
  case unvisited ++ pending of
    ((f, h) : _) -> parkAnchor f h
    [] -> modify (\s -> s {stMsg = "All hunks have verdicts — :q to quit."})

-- Write the buffer through to the working file. remax writes eagerly —
-- a normal-mode edit and Esc from insert both land here — which a
-- deleted file cannot tolerate: its buffer is empty, so any write at all
-- would recreate on disk the very file whose removal is under review,
-- and call that an edit. Nothing but content deliberately put back
-- (:revert restoring the base, or typing into the buffer) may resurrect
-- it, and that write is what stops the file being deleted.
applyToDisk :: Int -> EventM Name St ()
applyToDisk i = do
  st <- get
  let
    fv = stFiles st !! i
    outLines = bufferLines (fvEd fv)
    wouldRecreate = fvDeleted fv && all T.null outLines
  when (edTouched (fvEd fv) && not wouldRecreate) $ do
    got <-
      liftIO
        ( try (TIO.writeFile (stRoot st </> fvPath fv) (T.unlines outLines)) ::
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
    case got of
      Right () -> modify (updateFile i (\v -> v {fvDeleted = False}))
      Left _ -> pure ()

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
                  screenRows <- terminalRows
                  let
                    reloadedFv = stFiles reloaded !! i
                    newOut = cursorDisplayRow (fvRows reloadedFv) newRow
                    origin = snd (bodyWindow screenRows reloaded reloadedFv)
                  modify (updateFile i (\f -> f {fvOrigin = origin}))
                  setTop
                    (viewportScroll BodyVP)
                    (max 0 (newOut - origin - screenOff))
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
              (reviewFiles mode (cfgUntracked (stConfig stEff)) (stRoot stEff))
          )
      let
        existing =
          Map.fromList (map (\fv -> (fvPath fv, fv)) (stFiles stEff))
      let
        reload name = loadOne (stConfig stEff) (stHls stEff) mode (stRoot stEff) name
        refreshOne name =
          case Map.lookup name existing of
            Nothing -> reload name
            Just fv -> do
              -- a file that has come or gone since it was loaded is a
              -- different kind of view than the one in hand, so it is
              -- read afresh rather than patched
              present <- doesFileExist (stRoot stEff </> name)
              if present == fvDeleted fv
                then reload name
                else do
                  base <- baseLines mode (stRoot stEff) name
                  if base == fvBase fv
                    then pure (Just fv)
                    else do
                      (rebuilt, _) <-
                        rebuildFV (stConfig stEff) (repoText stEff) fv {fvBase = base}
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
                      rebuildFV (stConfig stEff) (repoText stEff) fv {fvBase = buf}
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
data ArgKind
  = NoArg
  | ArgRef
  | ArgMode
  | ArgPath
  | ArgDir
  | ArgText
  | ArgSet
  | ArgFilter

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
  , ("filter", ArgFilter, "walk all | code | comments (also Space k)")
  , ("open", ArgPath, "open a file in the session")
  , ("close", NoArg, "drop the current file")
  , ("files", NoArg, "fuzzy file finder")
  , ("tree", NoArg, "project tree with per-file diff stats")
  , ("repo", NoArg, "switch to another repository here (picker)")
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
      ["set", "keys"] -> prefixed keyModeNames
      ["set", "note-on-flag"] -> prefixed ["0", "1"]
      ["resolve"] -> prefixed ["all"]
      [cmd] -> case argKindOf cmd of
        Just ArgRef -> prefixed ("branch" : stCmdRefs st)
        Just ArgMode -> prefixed ["head", "unstaged", "untracked"]
        Just ArgPath -> prefixed (stCmdFiles st)
        Just ArgDir -> prefixed (stCmdDirs st ++ stCmdFiles st)
        Just ArgFilter -> prefixed (map runFilterLabel filterCycle)
        _ -> []
      _ -> []

openCmdPrompt :: EventM Name St ()
openCmdPrompt = do
  st <- get
  files <- liftIO (listFiles VisibleFiles (stRoot st))
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
    ["note"] -> openNotePopup NoteAsked
    ("note" : rest) -> noteCmd (T.unwords rest)
    ["unnote"] -> unnoteCmd
    ["revert"] -> revertTarget
    ["filter"] -> cycleRunFilter
    ["filter", which] ->
      case filter ((== which) . runFilterLabel) filterCycle of
        (chosen : _) -> setRunFilter chosen
        [] ->
          setMsg
            ("filter = " <> T.intercalate " | " (map runFilterLabel filterCycle))
    ["resolve"] -> unnoteCmd
    ["resolve", "all"] -> resolveAllAnswered
    ["accept", path] -> acceptPath path
    ["set"] -> showSettings
    ["set", key] -> showSetting key
    ["set", key, val] -> applySet key val
    ["open", path] -> openFilePath path
    ["close"] -> closeCurrent
    ["files"] -> openFileFinder
    ["tree"] -> openTree
    ["help"] -> showHelp
    ["repo"] -> openRepoPicker
    ["repo", name] -> do
      rescanRepos
      st <- get
      case filter ((== T.unpack name) . takeFileName) (stRoots st) of
        (root : _) -> switchRepo root
        [] -> setMsg ("no repository called " <> name <> " here")
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
  , "ignore-whitespace"
  , "word-diff"
  , "untracked"
  , "split-threshold"
  , "piece-min"
  , "piece-max"
  ]

settingValue :: Config -> Text -> Text
settingValue cfg key =
  case key of
    "keys" -> keyModeToText (cfgKeys cfg)
    "scrolloff" -> tshow (cfgScrolloff cfg)
    "note-wrap" -> tshow (cfgNoteWrap cfg)
    "note-on-flag" -> if cfgNoteOnFlag cfg then "1" else "0"
    "ignore-whitespace" -> if cfgIgnoreWs cfg then "1" else "0"
    "word-diff" -> if cfgWordDiff cfg then "1" else "0"
    "untracked" -> if cfgUntracked cfg then "1" else "0"
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
      | val `elem` keyModeNames || val == "vim" ->
          -- switching family mid-command would strand a half-typed one
          finish cfg {cfgKeys = keyModeFromText val} (modify (\s -> s {stVi = viIdle}))
      | otherwise -> setMsg ("keys = " <> T.intercalate " | " keyModeNames)
    "scrolloff" -> withInt (\n -> cfg {cfgScrolloff = max 0 n}) (pure ())
    "note-wrap" -> withInt (\n -> cfg {cfgNoteWrap = max 20 n}) (pure ())
    "note-on-flag" ->
      withInt (\n -> cfg {cfgNoteOnFlag = n /= 0}) (pure ())
    -- both change what the diff says, so every file is decorated again
    "ignore-whitespace" ->
      withInt (\n -> cfg {cfgIgnoreWs = n /= 0}) rebuildAllFiles
    "word-diff" ->
      withInt (\n -> cfg {cfgWordDiff = n /= 0}) (pure ())
    -- this one changes which files are in the review at all, so the
    -- change set is asked for again rather than merely redecorated
    "untracked" ->
      withInt (\n -> cfg {cfgUntracked = n /= 0}) refreshGit
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
    cfg = stConfig st
    oldCur = currentRunOf st (stCurF st)
  rebuilt <-
    liftIO
      ( forMaybeBounded
          (\fv -> fmap (Just . fst) (rebuildFV cfg (repoText st) fv))
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
        ( \s ->
            s
              { stNotePopup = Just (NotePopup ed (AmendMessage folding))
              , stVi = viIdle
              }
        )

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

-- Every key carrying the same conversation as one of these.
--
-- A note is one conversation however many lines it was written across,
-- and those lines need not all be part of the change: a note made over a
-- selection that reaches past it is recorded against the unchanged lines
-- too, under the key a remark uses. Closing it from the hunk's own keys
-- leaves that half alive, keyed to a line no hunk owns, where nothing
-- will ever resolve it — and it reappears beside whichever hunk that
-- line happens to sit next to.
-- Deliberately the rows beside the accepted ones, not every key whose
-- posts happen to match. Two hunks flagged with the same sentence carry
-- equal conversations while being nothing to do with each other, and
-- retiring one would take the other with it — which is a worse bug than
-- the one this is here to fix.
conversationKeys :: St -> FileView -> [Text] -> [Text]
conversationKeys st fv keys =
  let
    rows = fvRows fv
    threadAt i = do
      r <- safeIdx rows i
      listToMaybe (mapMaybe (`Map.lookup` stThreads st) (rowFeedbackKeys r))
    keysAt i = maybe [] rowFeedbackKeys (safeIdx rows i)
    seeds =
      [ i
      | (i, r) <- zip [0 ..] rows
      , any (`elem` keys) (rowFeedbackKeys r)
      ]
    -- the run of neighbours carrying the same conversation, which is the
    -- block the note is drawn under
    grow step i posts =
      let
        j = i + step
      in
        if threadAt j == Just posts then j : grow step j posts else []
    block i = case threadAt i of
      Nothing -> []
      Just posts -> (i : grow (-1) i posts) ++ grow 1 i posts
  in
    Set.toList (Set.fromList (concatMap keysAt (concatMap block seeds)))

-- Retire the conversation the accepted lines belonged to, wherever the
-- rest of it lives.
--
-- One keystroke made the mark and one retires it: the lines outside the
-- hunk lose their verdict as well as their share of the conversation.
-- Closing the note but leaving them flagged would leave a mark standing
-- with nothing left to say why, and a punch list asking for work that
-- has just been done.
--
-- Cleared rather than marked reviewed, because reviewed says a change
-- has been read and a line nobody changed has none.
closeConversation :: [Text] -> EventM Name St ()
closeConversation keys = do
  st <- get
  let
    wider = case currentFileMay st of
      Nothing -> keys
      Just fv -> conversationKeys st fv keys
    beyond = filter (`notElem` keys) wider
  unless (null beyond) $ do
    void (liftIO (clearThread (stConn st) (repoText st) beyond))
    liftIO (deleteKeys (stConn st) (repoText st) beyond)
    modify $ \s ->
      s {stVerdicts = foldr Map.delete (stVerdicts s) beyond}
  resolveNotesInMemory wider

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
      pure (map (\(k, _, _) -> k) (selectedChanges True fv lo hi))
    cursorKeys = do
      fv <- currentFileMay st
      let
        row = fst (cursorRC (fvEd fv))
        ks = map (\(k, _, _) -> k) (selectedChanges True fv row row)
        relevant k =
          Map.member k (stVerdicts st) || Map.member k (stThreads st)
      let
        groupOf thread =
          let
            inGroup r =
              listToMaybe
                ( filter
                    (\k -> Map.lookup k (stThreads st) == Just thread)
                    (rowFeedbackKeys r)
                )
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
-- Discarding one that ! or ? opened cancels that mark too.
openNotePopup :: NoteOrigin -> EventM Name St ()
openNotePopup origin = do
  st <- get
  let
    -- A note that ! or ? opened is about what that keystroke just
    -- marked, all of it. Asking noteTargetKeys instead lands it on the
    -- line under the cursor alone, because marking collapses the
    -- selection on its way out and the cursor is left on whichever line
    -- the selection happened to end at — so a note written across four
    -- lines was recorded against one of them, and if that one was
    -- unchanged it belonged to no hunk and nothing could ever resolve it.
    keys = case origin of
      NoteFromMark _ | not (null (stLastMarked st)) -> stLastMarked st
      _ -> noteTargetKeys st
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
      modify
        (\s -> s {stNotePopup = Just (NotePopup ed1 (NoteFor keys origin)), stVi = viIdle})

setNotePopupEd :: EdState -> EventM Name St ()
setNotePopupEd ed = do
  modify $ \s ->
    s
      { stNotePopup =
          fmap (\np -> np {npEd = ed {edCopied = False}}) (stNotePopup s)
      , stKill = edKill ed
      }
  -- the popup shares the session's yank slot, so a copy made in a note
  -- reaches the clipboard the same way one made in the buffer does
  publishCopy ed

handleNotePopupKey :: V.Event -> NotePopup -> EventM Name St ()
handleNotePopupKey ev np0 = do
  st0 <- get
  let
    np = np0 {npEd = (npEd np0) {edKill = stKill st0}}
    discardMsg = case npPurpose np of
      NoteFor _ NoteAsked -> "note discarded"
      NoteFor _ (NoteFromMark _) -> "mark cancelled"
      AmendMessage _ -> "amend cancelled"
    -- Backing out of a note that ! or ? opened takes the mark with it.
    -- The verdict was made by that same keystroke and has not been
    -- explained yet, so abandoning the sentence abandons what it was
    -- going to be about — and the advance the mark would have made never
    -- happens either, since there is nothing to move on from.
    --
    -- A note asked for by :note is the other case: that mark was already
    -- there and had nothing to do with this popup.
    discard = do
      modify
        (\s -> s {stNotePopup = Nothing, stMsg = discardMsg, stVi = viIdle})
      case npPurpose np of
        NoteFor _ (NoteFromMark undoing) -> do
          cancelMark undoing
          modify (\s -> s {stDeferAdvance = False})
        _ -> runDeferredAdvance
    wrapCol = case npPurpose np of
      NoteFor _ _ -> cfgNoteWrap (stConfig st0)
      AmendMessage _ -> 72
    saveBuffer = do
      modify (\s -> s {stNotePopup = Nothing, stVi = viIdle})
      let
        text = T.strip (T.intercalate "\n" (bufferLines (npEd np)))
      case npPurpose np of
        NoteFor keys _ -> do
          applyNote text keys
          runDeferredAdvance
        AmendMessage folding -> applyAmendMessage folding text
  case ev of
    -- discard works from either popup mode; nobody should need to leave
    -- insert just to bail out
    V.EvKey (V.KChar 'c') [V.MCtrl] -> discard
    _ ->
      case edMode (npEd np) of
        Insert ->
          case handleInsert ev (npEd np) of
            InsertHandled ed -> setNotePopupEd (wrapNoteLine wrapCol ed)
            InsertExit ed -> setNotePopupEd ed
            InsertUnhandled -> pure ()
        -- a half-typed vi command owns its next key here too, so `f⏎`
        -- looks for a character instead of posting the note
        Normal
          | cfgKeys (stConfig st0) == KeysVi
          , viBusy (stVi st0) ->
              notePopupVi st0 np ev
        Normal ->
          case ev of
            V.EvKey V.KEnter [] -> saveBuffer
            -- Esc from insert lands here in the popup's normal mode; Esc
            -- again cancels the dialog outright
            V.EvKey V.KEsc [] -> discard
            -- the popup's normal mode follows the session's keymap, so a
            -- vi user gets vi here too; emacs is modeless and never
            -- reaches this branch
            _ ->
              case cfgKeys (stConfig st0) of
                KeysVi -> notePopupVi st0 np ev
                _ ->
                  case handleNormal ev (npEd np) of
                    Just ed -> setNotePopupEd ed
                    Nothing -> pure ()

notePopupVi :: St -> NotePopup -> V.Event -> EventM Name St ()
notePopupVi st np ev =
  case handleVi (stVi st) ev (npEd np) of
    ViUnhandled -> pure ()
    ViPending keys -> modify (\s -> s {stVi = keys})
    ViDone keys ed -> do
      modify (\s -> s {stVi = keys})
      setNotePopupEd ed

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
      NoteFor keys _ ->
        case mapMaybe (`Map.lookup` stThreads st) keys of
          (t : _) -> t
          [] -> []
    act = noteAct thread
    popupName = case npPurpose np of
      AmendMessage _ -> "amend"
      NoteFor _ _ -> case act of
        NoteNew -> "note"
        NoteEdit -> "note \183 editing yours"
        NoteReply who -> "note \183 reply to " <> who
    popupHint = case npPurpose np of
      AmendMessage _ -> "\9166 amend (nor) \183 C-c cancel"
      -- what backing out costs is worth saying, since it differs: a note
      -- the mark opened takes the mark with it, one asked for does not
      NoteFor _ (NoteFromMark _) ->
        "\9166 save (nor) \183 C-c cancels the mark \183 empty note clears"
      NoteFor _ NoteAsked -> case act of
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
      NoteFor _ _ -> case currentFileMay st of
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

-- Switching repositories --------------------------------------------------------

-- Remember where we are before stepping away.
currentPlace :: St -> Maybe (FilePath, RepoPlace)
currentPlace st = do
  fv <- currentFileMay st
  pure
    ( stRoot st
    , RepoPlace
        { rpMode = stMode st
        , rpFile = fvPath fv
        , rpHunk = stCurH st
        , rpOnlyPending = stOnlyPending st
        , rpRunFilter = stRunFilter st
        , rpOpened = stOpened st
        , rpAnchor = stAnchor st
        }
    )

-- Point the session at another repository, keeping the one we leave in
-- stPlaces so coming back lands where we left off. Everything else is
-- read fresh: the verdicts and threads are that repository's, the rebase
-- state comes back out of its .git, and the buffers are re-read from
-- disk because an agent may have been working here in the meantime.
switchRepo :: FilePath -> EventM Name St ()
switchRepo target = do
  st <- get
  if target == stRoot st
    then setMsg ("already in " <> T.pack (takeFileName target))
    else do
      let
        remembered = Map.lookup target (stPlaces st)
        mode = maybe (VsRef "HEAD") rpMode remembered
        conn = stConn st
        saved = case currentPlace st of
          Just (from, place) -> Map.insert from place (stPlaces st)
          Nothing -> stPlaces st
      liftIO (finishPass conn (stPassId st))
      loaded <- liftIO $ do
        verdicts <- loadVerdicts conn (T.pack target)
        threads <- loadThreads conn (T.pack target)
        rebase <- rebaseStatus target
        branch <- currentBranch target
        headAt <- headCommit target
        files <-
          loadAll (stConfig st) (stHls st) (rebaseEffMode rebase mode) target
        passId <- beginPass conn (T.pack target) (modeLabel mode)
        -- the watch manager is shared, so following a second repository
        -- costs watch descriptors rather than another inotify instance
        gitDirs <- gitStateWatchDirs target
        mapM_ (stWatch st) gitDirs
        mapM_ (stWatch st . fileDir target . fvPath) files
        -- and the rest of it in the background, so the first change in a
        -- directory nothing is changed in yet is noticed here too
        watchTracked (stWatch st) target
        pure (verdicts, threads, rebase, branch, headAt, files, passId)
      let
        (verdicts, threads, rebase, branch, headAt, files, passId) = loaded
      modify $ \s ->
        s
          { stRoot = target
          , stFiles = files
          , stVerdicts = verdicts
          , stThreads = threads
          , stRebase = rebase
          , stBranch = branch
          , stHead = headAt
          , stPassId = passId
          , stMode = mode
          , stPlaces = saved
          , stCurF = 0
          , stCurH = 0
          , stWritten = Set.empty
          , stAltFile = Nothing
          , stAnchor = maybe Nothing rpAnchor remembered
          , stOpened = maybe Set.empty rpOpened remembered
          , stOnlyPending = maybe (stOnlyPending s) rpOnlyPending remembered
          , stRunFilter = maybe (stRunFilter s) rpRunFilter remembered
          , stGrepNav = Nothing
          , stCmdFiles = []
          , stCmdDirs = []
          , stCmdRefs = []
          }
      case remembered of
        Just place -> restorePlace place
        Nothing -> do
          arrived <- get
          case firstUnrespondedOrPending arrived of
            Just (f, h) -> parkAnchor f h
            Nothing -> pure ()
      setMsg (repoArrivalMsg target rebase files)

-- Land back on the file and hunk we left, when they are still there;
-- otherwise fall back to wherever a fresh session would have opened.
restorePlace :: RepoPlace -> EventM Name St ()
restorePlace place = do
  st <- get
  case findIndex ((== rpFile place) . fvPath) (stFiles st) of
    Just f -> parkM f (rpHunk place)
    Nothing ->
      case firstUnrespondedOrPending st of
        Just (f, h) -> parkAnchor f h
        Nothing -> pure ()

repoArrivalMsg :: FilePath -> Maybe RebaseStatus -> [FileView] -> Text
repoArrivalMsg target rebase files =
  let
    name = T.pack (takeFileName target)
    hunks = length (concatMap fvRuns files)
    what
      | null files = "nothing to review"
      | otherwise = tshow hunks <> " hunk(s) in " <> tshow (length files) <> " file(s)"
    mid = case rebase of
      Just rb -> " · rebasing, " <> rebaseStopMsg rb
      Nothing -> ""
  in
    name <> " · " <> what <> mid

-- The project tree ------------------------------------------------------------

-- Browsing and shape in one overlay: the repository's files, decorated
-- per file the way git diff --stat is, and expandable a level further so
-- a file opens into its hunks. That last level makes this the tray's
-- file-to-hunk list with directories on top, which is why the model
-- lives in Remax.Tree and knows nothing about being drawn in a float.
-- The same dialog, seen two ways. Flat is the file finder: every file on
-- one level, ranked best-match-first. Hierarchy is the project tree:
-- structure kept, order meaningful. They were two commands answering one
-- question, so Tab now moves between them without dropping the filter —
-- a flat row is just a tree row whose label happens to be the whole path.
data TreeMode = TreeHierarchy | TreeFlat
  deriving (Eq)

data TreeView = TreeView
  { tvMode :: TreeMode
  , tvRoot :: TreeNode
  , tvAll :: [(FilePath, Maybe PathStat)] -- every file, for flat mode
  , tvExpanded :: Set.Set FilePath -- directories and files alike
  , tvSel :: Int
  , tvInput :: Text
  , tvOp :: Maybe TreeOp -- the file menu, mid-operation
  }

-- Making, moving and unmaking files, from the list that already shows
-- them. Reviewing turns up work — a file wants splitting, a scratch file
-- has served its purpose — and dropping to a shell to do it loses the
-- session's place.
--
-- OpMenu is the leader's list. OpNaming carries the action and the path
-- typed so far. OpConfirming is the one irreversible action asking
-- first: everything else can be undone by doing the opposite, and a
-- delete cannot.
data TreeOp
  = OpMenu
  | OpNaming FileAction Text
  | OpConfirming FilePath

-- Each carries the path it works from: the directory a new thing lands
-- in, or the path being renamed, copied or removed.
data FileAction
  = NewFile FilePath
  | NewDir FilePath
  | RenamePath FilePath
  | DuplicatePath FilePath

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
    query = tvInput view
    nd = needleFor query
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
    scored =
      map
        (\(Ranked sc p ms) -> (sc, (p, ms)))
        (Set.toAscList (bestOf Set.empty (tvAll view)))
    emit (path, mStat) =
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
  in
    concatMap (emit . snd) (sortOn fst scored)

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

-- The dialog is modal while an operation is under way: the menu, then a
-- path to type or a deletion to confirm. Browsing keys only apply when
-- none of that is open.
handleTreeKey :: V.Event -> TreeView -> EventM Name St ()
handleTreeKey ev view =
  case tvOp view of
    Just OpMenu -> fileMenuKey ev view
    Just (OpNaming action typed) -> namingKey ev view action typed
    Just (OpConfirming path) -> confirmKey ev view path
    Nothing -> browseTreeKey ev view

-- Any key not in the table cancels, which is what the frame promises.
fileMenuKey :: V.Event -> TreeView -> EventM Name St ()
fileMenuKey ev view =
  case ev of
    V.EvKey (V.KChar c) [] ->
      case find (\(key, _, _) -> key == T.singleton c) fileMenuEntries of
        Just (_, _, act) -> act view
        Nothing -> setTree view {tvOp = Nothing}
    _ -> setTree view {tvOp = Nothing}

namingKey :: V.Event -> TreeView -> FileAction -> Text -> EventM Name St ()
namingKey ev view action typed = do
  st <- get
  let
    retype next = setTree view {tvOp = Just (OpNaming action next)}
  case ev of
    V.EvKey V.KEsc [] -> setTree view {tvOp = Nothing}
    V.EvKey V.KBS [] -> retype (T.dropEnd 1 typed)
    V.EvKey (V.KChar c) [] -> retype (T.snoc typed c)
    V.EvKey V.KEnter [] ->
      case resolveInRepo (stRoot st) typed of
        Left err -> do
          setTree view {tvOp = Nothing}
          setMsg err
        Right rel -> do
          outcome <- liftIO (runFileAction (stRoot st) action rel)
          -- a rename takes the old path out of the session; the other
          -- three leave what is already open alone
          let
            gone = case action of
              RenamePath src -> Just src
              _ -> Nothing
          finishFileOp view gone outcome
    _ -> pure ()

confirmKey :: V.Event -> TreeView -> FilePath -> EventM Name St ()
confirmKey ev view path = do
  st <- get
  case ev of
    V.EvKey (V.KChar 'y') [] -> do
      outcome <- liftIO (runDelete (stRoot st) path)
      finishFileOp view (Just path) outcome
    _ -> setTree view {tvOp = Nothing}

browseTreeKey :: V.Event -> TreeView -> EventM Name St ()
browseTreeKey ev view = do
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
        -- flat rows lead with their file, so the answer is nought and
        -- asking costs a whole pass over the candidates — which the
        -- render is about to make anyway, and which on a large
        -- repository is seconds rather than milliseconds
        firstFile = case tvMode view of
          TreeFlat -> Just 0
          TreeHierarchy -> findIndex rowIsFile (treeVisible st next)
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
        -- a deletion opens like anything else now: the change set
        -- carries it as a file whose every line is removed
        RowFile _ hasHunks open ->
          if hasHunks && not open
            then unfold (trPath row)
            else do
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
    -- Tab moves between flat and hierarchical without losing the filter,
    -- and lands on the same file it was pointing at. (Tab is safe to
    -- spend here: a path containing one is a problem no keybinding can
    -- fix.)
    V.EvKey (V.KChar '\t') [] -> switchTreeMode view
    -- the ignored files, and back. A modified key because every
    -- printable one belongs to the filter.
    V.EvKey (V.KChar 'a') [V.MCtrl] -> toggleTreeScope view
    V.EvKey (V.KChar 'x') [V.MCtrl] -> setTree view {tvOp = Just OpMenu}
    V.EvKey V.KBS [] -> retype (T.dropEnd 1 (tvInput view))
    V.EvKey (V.KChar c) [] -> retype (T.snoc (tvInput view) c)
    _ -> pure ()

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

-- Ctrl-x's menu. One table, rendered and dispatched from the same place,
-- so a key cannot be bound here without appearing in the list — the
-- lesson the space menu already learned.
fileMenuEntries :: [(Text, Text, TreeView -> EventM Name St ())]
fileMenuEntries =
  [ ("n", "new file", startNaming NewFile)
  , ("d", "new directory", startNaming NewDir)
  , ("r", "rename / move", startActing RenamePath)
  , ("c", "duplicate", startActing DuplicatePath)
  , ("x", "delete", startDelete)
  ]

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

-- The two creating actions start from a directory and a trailing slash,
-- so the path is already where the cursor was and only the name is left
-- to type.
startNaming :: (FilePath -> FileAction) -> TreeView -> EventM Name St ()
startNaming build view = do
  st <- get
  let
    dir = treeTargetDir st view
    seeded = if null dir then "" else T.pack dir <> "/"
  setTree view {tvOp = Just (OpNaming (build dir) seeded)}

-- The two acting-on-a-path actions start from the path itself, so
-- renaming is editing what is already written.
startActing :: (FilePath -> FileAction) -> TreeView -> EventM Name St ()
startActing build view = do
  st <- get
  case treeSelected st view of
    Nothing -> setTree view {tvOp = Nothing} >> setMsg "nothing selected"
    Just row ->
      setTree
        view {tvOp = Just (OpNaming (build (trPath row)) (T.pack (trPath row)))}

startDelete :: TreeView -> EventM Name St ()
startDelete view = do
  st <- get
  case treeSelected st view of
    Nothing -> setTree view {tvOp = Nothing} >> setMsg "nothing selected"
    Just row -> setTree view {tvOp = Just (OpConfirming (trPath row))}

namingLabel :: FileAction -> Text
namingLabel action =
  case action of
    NewFile _ -> "new file"
    NewDir _ -> "new directory"
    RenamePath _ -> "rename to"
    DuplicatePath _ -> "duplicate to"

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

-- Filesystem work fails in the ordinary ways, and the dialog would
-- rather say why on its status line than take the session down.
tryFileIO :: IO Text -> IO (Either Text Text)
tryFileIO act = do
  got <- try act :: IO (Either IOException Text)
  pure $ case got of
    Left e -> Left (T.pack (show e))
    Right msg -> Right msg

-- A tracked path moves and disappears through git, so a rename stays a
-- rename in the diff instead of arriving as an unrelated delete and add.
-- Everything else — new files, anything untracked, anything ignored —
-- is plain filesystem work.
runFileAction :: FilePath -> FileAction -> FilePath -> IO (Either Text Text)
runFileAction root action rel = do
  let
    full = root </> rel
    named = T.pack rel
  taken <- (||) <$> doesFileExist full <*> doesDirectoryExist full
  case action of
    NewFile _
      | taken -> pure (Left ("already there: " <> named))
      | otherwise -> tryFileIO $ do
          createDirectoryIfMissing True (takeDirectory full)
          TIO.writeFile full ""
          pure ("new file " <> named)
    NewDir _
      | taken -> pure (Left ("already there: " <> named))
      | otherwise -> tryFileIO $ do
          createDirectoryIfMissing True full
          pure ("new directory " <> named)
    RenamePath src
      | src == rel -> pure (Left "that is where it already is")
      | taken -> pure (Left ("already there: " <> named))
      | otherwise -> do
          tracked <- isTracked root src
          let
            done = "renamed to " <> named
          if tracked
            then do
              createDirectoryIfMissing True (takeDirectory full)
              fmap (fmap (const done)) (runGit root ["mv", "--", src, rel])
            else tryFileIO $ do
              createDirectoryIfMissing True (takeDirectory full)
              renamePath (root </> src) full
              pure done
    DuplicatePath src
      | src == rel -> pure (Left "give the copy a different name")
      | taken -> pure (Left ("already there: " <> named))
      | otherwise -> do
          srcIsDir <- doesDirectoryExist (root </> src)
          if srcIsDir
            then pure (Left "only files can be duplicated")
            else tryFileIO $ do
              createDirectoryIfMissing True (takeDirectory full)
              copyFile (root </> src) full
              pure ("copied to " <> named)

runDelete :: FilePath -> FilePath -> IO (Either Text Text)
runDelete root rel = do
  let
    full = root </> rel
    done = "deleted " <> T.pack rel
  tracked <- isTracked root rel
  isDir <- doesDirectoryExist full
  if tracked
    then fmap (fmap (const done)) (runGit root ["rm", "-r", "-f", "--", rel])
    else tryFileIO $ do
      if isDir then removeDirectoryRecursive full else removeFile full
      pure done

-- A path that has just been renamed away or deleted is not worth holding
-- open: its buffer describes a file that is no longer there. A directory
-- takes everything under it.
dropFromSession :: FilePath -> EventM Name St ()
dropFromSession path = do
  let
    under p = p == path || (path ++ "/") `isPrefixOf` p
  modify $ \s ->
    let
      kept = filter (not . under . fvPath) (stFiles s)
    in
      s
        { stFiles = kept
        , stOpened = Set.filter (not . under) (stOpened s)
        , stCurF = max 0 (min (stCurF s) (length kept - 1))
        , stCurH = 0
        }

-- Run it, then let everything catch up: the dialog rebuilds from disk,
-- a path that went away stops being held open, and git is asked again so
-- the change set reflects what just happened.
finishFileOp :: TreeView -> Maybe FilePath -> Either Text Text -> EventM Name St ()
finishFileOp view gone outcome = do
  setTree view {tvOp = Nothing}
  case outcome of
    Left err -> setMsg err
    Right msg -> do
      mapM_ dropFromSession gone
      setMsg msg
      forceGitRefresh
      grown <- get
      case stTree grown of
        Just current -> refreshTreeView current
        Nothing -> pure ()

-- Flip the view, keeping the filter and the row. Coming back to the tree
-- opens the ancestors of wherever flat mode left you, so the file you
-- were on is on screen rather than folded away.
switchTreeMode :: TreeView -> EventM Name St ()
switchTreeMode view = do
  st <- get
  let
    was = treeSelected st view
    onPath = fmap trPath was
    flipped = case tvMode view of
      TreeFlat -> TreeHierarchy
      TreeHierarchy -> TreeFlat
    revealed = case (flipped, onPath) of
      (TreeHierarchy, Just path) ->
        Set.union (tvExpanded view) (Set.fromList (ancestorPaths path))
      _ -> tvExpanded view
    next = view {tvMode = flipped, tvExpanded = revealed}
    landed = case onPath of
      Just path -> findIndex ((== path) . trPath) (treeVisible st next)
      Nothing -> Nothing
  setTree next {tvSel = fromMaybe 0 landed}

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

-- Which characters of a row's own label the filter matched.
--
-- Positions come from the row's witness — the full path the whole query
-- actually matches — and are then shifted onto the slice of it this row
-- displays. A directory has to be measured against a descendant's path
-- rather than its own: "remaxgit" matches src/Remax/Git.hs, but no part
-- of that query matches src/Remax alone, so asking the directory by
-- itself finds nothing and lights nothing. Every row on the way down to
-- a match now shows its share of it.
rowMatchPositions :: Text -> TreeRow -> Set.Set Int
rowMatchPositions query row
  | T.null query = Set.empty
  | otherwise =
      let
        witness = T.pack (trWitness row)
        label = T.dropWhileEnd (== '/') (trLabel row)
        -- the label is the tail of trPath, and trPath is a prefix of the
        -- witness, so this span is the same in both
        offset = length (trPath row) - T.length label
        inLabel p = p >= offset && p < offset + T.length label
      in
        case subseqPositions query witness of
          Just ps -> Set.fromList (map (subtract offset) (filter inLabel ps))
          Nothing -> Set.empty

treeRowWidget :: St -> Text -> Int -> Bool -> TreeRow -> Widget Name
treeRowWidget st query rowWidth isSel row =
  let
    indent = T.replicate (trDepth row * 2) " "
    -- the prefix (indent and fold marker) is never part of the path, so
    -- it sits outside the highlighted span
    --
    -- A row with nothing to say about git has no columns to line up
    -- with, so its name takes the whole row rather than being clipped at
    -- the width the decorated rows need.
    nameCellIn width attr prefix label =
      let
        room = max 1 (width - T.length prefix)
        shown = clipT room label
      in
        treeCell
          width
          ( hBox
              [ withAttr attr (txt prefix)
              , withAttr attr (highlightAt (rowMatchPositions query row) shown)
              ]
          )
    nameCell = nameCellIn treeNameW
    wideName = nameCellIn (max treeNameW rowWidth)
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
            changed = ruChanged roll > 0
            cell = if changed then nameCell else wideName
          in
            hBox
              ( cell hintAttr (indent <> marker) (trLabel row <> "/")
                  : if changed
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
            nameAttr =
              if trPath row == curPath then trayFileCurAttr else trayFileAttr
            letter = case fmap psChange mStat of
              Just change ->
                withAttr (statusAttrFor change) (txt (pathChangeLetter change))
              Nothing -> txt " "
            -- untouched and not in the session: nothing to the right of
            -- the name, so the name gets the room
            bare = mStat == Nothing && not hasHunks
          in
            if bare
              then wideName nameAttr (indent <> marker) (trLabel row)
              else
                hBox
                  ( [ nameCell nameAttr (indent <> marker) (trLabel row)
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
          withDefAttr floatSelAttr (treeRowWidget st query rowWidth False parent)
            : map row (drop 1 (zip [start ..] shown))
        Nothing -> map row (zip [start ..] shown)
      row (i, r) = treeRowWidget st query rowWidth (i == sel) r
      query = tvInput view
      -- what a row has to play with once the frame and the selection
      -- bar have taken theirs
      rowWidth = max treeNameW (boxWidth - 6)
      -- the scope is worth saying outright: an empty-looking search is
      -- a different thing when the ignored files are in the haystack
      scoped = case stFileScope st of
        AllFiles -> " (all)"
        VisibleFiles -> ""
      counter =
        ( if T.null (tvInput view)
            then tshow (length (tvAll view)) <> " files"
            else -- files that matched, not rows: the directories kept to
            -- hold them are structure, not results
              tshow (treeFileCount visible) <> "/" <> tshow (length (tvAll view))
        )
          <> scoped
      inputRow = case tvOp view of
        Just (OpNaming action typed) ->
          hBox
            [ withAttr hintAttr (txt (" " <> namingLabel action <> " "))
            , withAttr statusAttr (txt (typed <> "\9612"))
            ]
        Just (OpConfirming path) ->
          withAttr
            statusAttr
            (txt (" delete " <> T.pack path <> " ?"))
        _ ->
          hBox
            [ withAttr statusAttr (txt (" " <> tvInput view <> "\9612"))
            , padLeft Max (withAttr hintAttr (txt (counter <> " ")))
            ]
      filler = replicate (visibleCount - length body) (txt " ")
      flat = tvMode view == TreeFlat
      title = if flat then "files" else "tree"
      folding = if flat then "" else "\8594\8592 fold \183 "
      leaving =
        if T.null (tvInput view)
          then "esc closes"
          else "esc clears the filter"
      other = if flat then "tree" else "flat"
      showing = case stFileScope st of
        AllFiles -> "^a hide ignored \183 "
        VisibleFiles -> "^a all \183 "
      hint = case tvOp view of
        Just (OpNaming _ _) -> "\9166 does it \183 esc cancels"
        Just (OpConfirming _) -> "y deletes it \183 any other key cancels"
        _ ->
          "\8593\8595 move \183 "
            <> folding
            <> "\9166 open \183 \8633 "
            <> other
            <> " \183 "
            <> showing
            <> "^x file \183 "
            <> leaving
    render $
      floatFrame title hint $
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

-- Pick which repository the session is looking at. Each row costs two
-- git calls, which is fine for the handful of checkouts a story
-- directory holds and is worth it: choosing between five repos by name
-- alone tells you nothing about which one needs you.
-- Which checkouts the story holds, asked again. A story directory is a
-- working set rather than a fixed one — a repository gets cloned into it
-- partway through a review, or tidied away — and the session was going on
-- believing the list it was handed at startup.
--
-- Asked at the two moments the list is about to be used rather than kept
-- current in the background: a directory listing is cheap, it cannot go
-- stale between here and the answer, and nothing else in the session
-- consults the list unprompted.
rescanRepos :: EventM Name St ()
rescanRepos = do
  st <- get
  case stStory st of
    Nothing -> pure ()
    Just dir -> do
      found <- liftIO (childRepos dir)
      unless (found == stRoots st) (modify (\s -> s {stRoots = found}))

openRepoPicker :: EventM Name St ()
openRepoPicker = do
  rescanRepos
  st <- get
  case stRoots st of
    [] -> setMsg "no other repositories here"
    [_] -> setMsg "this is the only repository here"
    roots -> do
      described <- liftIO (mapM (describeRepo st) roots)
      let
        widths = columnWidths [24, 28, 16] (map snd described)
        -- the row shows the basename; the ref carries the whole path, so
        -- a clipped name can never become the thing we switch to
        row (root, cells) =
          pickerRow
            (T.pack root)
            (columnRow widths cells)
            (T.pack root <> " " <> T.unwords cells)
        rows = map row described
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

-- How much of a repository is left to read, for the switcher. Knowing a
-- repository has changes says nothing about whether they have been
-- looked at, which is the thing worth knowing before switching to one.
--
-- Verdicts are keyed by line content, so answering this for a repository
-- the session is not in means actually diffing it. That is affordable
-- for the handful of checkouts a story holds and not for something the
-- size of a vendored tree, so past a point the count is not attempted
-- and the file count stands in for it.
repoStatusMaxFiles :: Int
repoStatusMaxFiles = 40

repoHunkCount :: St -> FilePath -> IO (Int, Int)
repoHunkCount st root
  -- the session already has this one loaded, down to its verdicts
  | root == stRoot st =
      let
        runs = concatMap fvRuns (stFiles st)
      in
        pure
          ( length (filter (not . runPending (stVerdicts st)) runs)
          , length runs
          )
  | otherwise = do
      verdicts <- loadVerdicts (stConn st) (T.pack root)
      files <- loadAll (stConfig st) (stHls st) (VsRef "HEAD") root
      let
        runs = concatMap fvRuns files
      pure (length (filter (not . runPending verdicts) runs), length runs)

describeRepo :: St -> FilePath -> IO (FilePath, [Text])
describeRepo st root = do
  branch <- currentBranch root
  rebase <- rebaseStatus root
  changed <- reviewFiles (VsRef "HEAD") (cfgUntracked (stConfig st)) root
  counted <-
    if null changed || length changed > repoStatusMaxFiles
      then pure Nothing
      else fmap Just (repoHunkCount st root)
  let
    here = if root == stRoot st then "  \8592 current" else ""
    onBranch = case branch of
      Just b -> b
      Nothing -> "detached"
    reviewed = case counted of
      Just (done, total)
        | total > 0 ->
            tshow done <> "/" <> tshow total <> " hunks"
      _ -> tshow (length changed) <> " changed"
    state = case rebase of
      Just rb
        | rebaseConflicted rb -> "CONFLICT"
        | otherwise -> "rebasing " <> tshow (rebaseStep rb) <> "/" <> tshow (rebaseTotal rb)
      Nothing
        | null changed -> "clean"
        | otherwise -> reviewed
  pure (root, [T.pack (takeFileName root), onBranch, state <> here])

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
-- A query, lowercased and measured once, so that scoring a candidate
-- against it costs nothing but the walk.
data Needle = Needle
  { ndChars :: String
  , ndLen :: !Int
  , ndBlank :: !Bool
  }

needleFor :: Text -> Needle
needleFor q =
  let
    cs = map lowerFast (T.unpack q)
  in
    Needle cs (length cs) (T.null q)

-- Case folding for matching paths, which are very nearly always ASCII.
--
-- Data.Char.toLower is a Unicode table lookup, and the scorer calls it
-- once per character of every candidate — at a third of a million paths
-- that is tens of millions of table lookups for each character typed,
-- and it is most of what the finder spends its time on. Anything outside
-- ASCII still goes the long way round.
lowerFast :: Char -> Char
lowerFast c
  | c >= 'A' && c <= 'Z' = chr (ord c + 32)
  | c < '\128' = c
  | otherwise = toLower c

fuzzyScore :: Text -> FilePath -> Maybe (Int, Int, Int)
fuzzyScore q = fuzzyScoreOn (needleFor q)

-- How well a path matches, or Nothing if it does not.
--
-- Walks the path where it lies, lowercasing each character as it is
-- compared. The obvious version builds a lowercased copy of the
-- candidate and pairs every character of it with an index — two lists
-- per candidate, per keystroke. Against a few thousand paths that is
-- invisible; against the three hundred thousand a large repository has
-- once ignored files are shown, it is well over a gigabyte of garbage
-- for every character typed, and the session goes to its knees.
fuzzyScoreOn :: Needle -> FilePath -> Maybe (Int, Int, Int)
fuzzyScoreOn nd path
  | ndBlank nd = Just (0, 0, length path)
  | otherwise =
      let
        -- One walk of the path, which yields everything the score wants:
        -- whether the query is a subsequence of it and where, how long
        -- it is, and where its file name begins.
        --
        -- Separately these are three more walks of the same string —
        -- matching, then finding the last separator, then counting — and
        -- against a few hundred thousand candidates a walk is not
        -- something to spend four times. The file name comes back as the
        -- tail of the path itself rather than a copy of it, which a list
        -- gives away for nothing.
        go needle hay !i !firstAt !lastAt tailAfterSep =
          case hay of
            [] -> (needle, i, firstAt, lastAt, tailAfterSep)
            (h : hs) ->
              let
                nextTail = if isPathSeparator h then hs else tailAfterSep
              in
                case needle of
                  (c : cs)
                    | c == lowerFast h ->
                        go
                          cs
                          hs
                          (i + 1)
                          (if firstAt < 0 then i else firstAt)
                          i
                          nextTail
                  _ -> go needle hs (i + 1) firstAt lastAt nextTail
        (leftover, len, firstPos, lastPos, base) =
          go (ndChars nd) path 0 (-1) (-1) path
      in
        if not (null leftover)
          then Nothing
          else
            Just
              ( if subsequenceLower (ndChars nd) base then 0 else 1
              , (lastPos - firstPos) - (ndLen nd - 1)
              , len
              )

subsequenceLower :: String -> String -> Bool
subsequenceLower needle hay =
  case (needle, hay) of
    ([], _) -> True
    (_, []) -> False
    (c : cs, h : hs) ->
      if c == lowerFast h
        then subsequenceLower cs hs
        else subsequenceLower needle hs

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
        PickRepo ->
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
          PickRepo ->
            case safeIdx (pkTargets pk) (pkSel pk) of
              Just (row, _) -> switchRepo row
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
            PickRepo -> highlightSubseq (pkInput pk) shownText
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
        PickRepo ->
          tshow (length (pkMatches pk)) <> "/" <> tshow (length (pkAll pk))
        PickGrep -> tshow (length (pkMatches pk)) <> " hits"
      title = case pkMode pk of
        PickFiles -> "files"
        PickBase -> "base"
        PickBranch -> "checkout"
        PickRepo -> "repo"
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
        PickRepo -> "\8593\8595 move · \9166 switch to this repository · esc closes"
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
  , watchingAttr
  , wordChangeAttr
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
watchingAttr = attrName "watching"
wordChangeAttr = attrName "wordChange"
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

-- Nudge a colour up a little, for surfaces that should read as the same
-- material under slightly more light. Only truecolor values can be
-- lifted; a palette entry has no channels to add to, so it is left be.
lighten :: Int -> V.Color -> V.Color
lighten by c =
  case c of
    V.RGBColor r g b ->
      let
        bump v = min 255 (fromIntegral v + by)
      in
        V.linearColor (bump r) (bump g) (bump b)
    _ -> c

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
    bgA = scopeAttr mth "ui.background" V.defAttr
    baseAttr =
      let
        fgA = scopeAttr mth "ui.text" V.defAttr
      in
        V.defAttr
          { V.attrForeColor = V.attrForeColor fgA
          , V.attrBackColor = V.attrBackColor bgA
          }
    -- A popup sits a hair above the body, and "a hair" has to be
    -- measured from the body rather than fixed: the body colour comes
    -- from the theme's ui.background, so a hardcoded surface drifts as
    -- far from it as the theme happens to be dark. Against a #080808
    -- background the old fixed #1e1e1e read as a different material
    -- rather than the same one lit slightly.
    floatBack = case V.attrBackColor bgA of
      V.SetTo c -> V.SetTo (lighten 0x0d c)
      _ -> V.SetTo surfFloat
    onFloat a = a {V.attrBackColor = floatBack}
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
      , (watchingAttr, fg inkGreen)
      , -- only a background, so the syntax colour underneath survives
        (wordChangeAttr, V.defAttr `V.withBackColor` wordWash)
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
      , (floatAttr, onFloat V.defAttr)
      , (floatSelAttr, V.defAttr `V.withBackColor` surfSel)
      , (floatSelBarAttr, fg inkLavender `V.withBackColor` surfSel)
      , (floatTitleAttr, onFloat (fg inkLavender))
      , -- every popup is framed in the same lavender as its title; floatFrame
        -- is the only bordered widget, so this reaches exactly the popups
        (B.borderAttr, onFloat (fg inkLavender))
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
surfFloat, surfSel, cursorWash, wordWash, hairInk, meterRail, matchGold :: V.Color
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

-- Only used when the theme names no background at all, so there is
-- nothing to measure a lift from.
surfFloat = V.linearColor (0x15 :: Int) 0x15 0x15
surfSel = V.linearColor (0x24 :: Int) 0x24 0x30
cursorWash = V.linearColor (0x3a :: Int) 0x33 0x25
wordWash = V.linearColor (0x3d :: Int) 0x35 0x4a
hairInk = V.linearColor (0x2c :: Int) 0x2c 0x2c
meterRail = V.linearColor (0x3a :: Int) 0x3f 0x3a
matchGold = V.linearColor (0xfa :: Int) 0xd0 0x7a
