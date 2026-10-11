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
  , customMain
  , getVtyHandle
  , showFirstCursor
  )
import Brick.BChan
  ( BChan
  , newBChan
  , writeBChanNonBlocking
  )
import Brick.Util (fg)
import Control.Concurrent
  ( QSem
  , forkIO
  , newQSem
  , threadDelay
  )
import Control.Monad
  ( forM_
  , forever
  , void
  , when
  )
import Control.Monad.IO.Class (liftIO)
import Data.IORef
  ( IORef
  , newIORef
  )
import Data.Int (Int64)
import Data.List
  ( isSuffixOf
  )
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( fromMaybe
  )
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Remax.Clipboard
import Remax.Config
import Remax.Git
  ( FileScope (..)
  , RebaseStatus (..)
  , ReviewMode (..)
  , currentBranch
  , headCommit
  , modeLabel
  , rebaseStatus
  )
import Remax.Store
import Remax.Theme
import qualified Remax.TreeSitter as TS
import Remax.UI.Attrs
import Remax.UI.Draw
import Remax.UI.Events
import Remax.UI.Load
import Remax.UI.Nav
import Remax.UI.Types
import Remax.UI.Verdict
import Remax.UI.Watch
import Remax.Vi
  ( viIdle
  )
import System.Environment (lookupEnv)

data Startup = Startup
  { suConn :: Connection
  , suVerdicts :: Map.Map Text Verdict
  , suHighlighters :: TS.HighlighterTable
  , suThreads :: Map.Map Text [Post]
  , suNoteGroups :: Map.Map Text Text
  , suConfig :: Config
  , suRebase :: Maybe RebaseStatus
  , suBranch :: Maybe Text
  , suHead :: Maybe (Text, Text)
  , suPassId :: Int64
  , suTheme :: Maybe HelixTheme
  , suChan :: BChan REvent
  , suJobs :: IORef Int
  , suSlots :: QSem
  }

runReview :: ReviewMode -> FilePath -> [FilePath] -> Maybe FilePath -> IO ()
runReview mode root roots story = do
  startup <- loadStartup mode root
  let
    chan = suChan startup
  final <- withWatcher $ \mgr watchMode -> do
    dirWatch <- installWatches mgr chan root watchMode
    -- probed once: doing it per yank would be a process spawn a keystroke
    clip <- discoverClipboard
    -- and read once on the way in, so whatever was already on the
    -- clipboard counts as seen. Without this the first paste of a
    -- session would treat months-old clipboard contents as newer than
    -- the line you just cut, and `dd p` would not put the line back.
    alreadyThere <- readClipboard clip
    let
      st0 = initialState startup mode root roots story watchMode dirWatch clip alreadyThere
    startBeats chan
    vty <- buildVty
    customMain vty buildVty (Just chan) (app (suTheme startup)) (parkAtStart st0)
  finishPass (suConn startup) (stPassId final)
  printSummary final

loadStartup :: ReviewMode -> FilePath -> IO Startup
loadStartup mode root = do
  conn <- openStore
  -- a session killed with a note popup open leaves the marker behind;
  -- starting one is as good a moment as any to say it is not being
  -- written any more
  setComposing conn (T.pack root) False
  verdicts <- loadVerdicts conn (T.pack root)
  hls <- TS.initHighlighters
  threads0 <- loadThreads conn (T.pack root)
  noteGroups0 <- loadNoteGroups conn (T.pack root)
  cfg <- loadConfig
  rebase0 <- rebaseStatus root
  branch0 <- currentBranch root
  head0 <- headCommit root
  passId <- beginPass conn (T.pack root) (modeLabel mode)
  theme <- loadHelixTheme
  chan <- newBChan 64
  jobs <- newIORef (0 :: Int)
  slots <- newQSem 2
  pure
    Startup
      { suConn = conn
      , suVerdicts = verdicts
      , suHighlighters = hls
      , suThreads = threads0
      , suNoteGroups = noteGroups0
      , suConfig = cfg
      , suRebase = rebase0
      , suBranch = branch0
      , suHead = head0
      , suPassId = passId
      , suTheme = theme
      , suChan = chan
      , suJobs = jobs
      , suSlots = slots
      }

initialState ::
  Startup ->
  ReviewMode ->
  FilePath ->
  [FilePath] ->
  Maybe FilePath ->
  WatchMode ->
  DirWatch ->
  Clipboard ->
  Maybe Text ->
  St
initialState startup mode root roots story watchMode dirWatch clip alreadyThere =
  St
    { stFiles = []
    , stCurF = 0
    , stCurH = 0
    , stVerdicts = suVerdicts startup
    , stMsg = if watchMode == PollingWatch then watchPollWarning else "loading\8230"
    , stRoot = root
    , stMode = mode
    , stConn = suConn startup
    , stPassId = suPassId startup
    , stCounts = Map.empty
    , stPunch = []
    , stTray = True
    , stLastGit = 0
    , stGitQueued = False
    , stChan = suChan startup
    , stWatch = dirWatchFollow dirWatch
    , stPrompt = Nothing
    , stLastSearch = ""
    , stBorder = fgOnly (scopeAttr (suTheme startup) "ui.linenr" (fg V.brightBlack))
    , stHls = suHighlighters startup
    , stOnlyPending = True
    , stRunFilter = FilterAll
    , stFileScope = VisibleFiles
    , stRebase = suRebase startup
    , stWritten = Set.empty
    , stLastMarked = []
    , stLastRows = []
    , stOpened = Set.empty
    , stPicker = Nothing
    , stLastSelector = Nothing
    , stTraySeen = (0, 0)
    , stNotePopup = Nothing
    , stThreads = suThreads startup
    , stNoteGroups = suNoteGroups startup
    , stSpace = False
    , stConfig = suConfig startup
    , stCmdFiles = []
    , stCmdDirs = []
    , stCmdRefs = []
    , stPromptCycle = Nothing
    , stEmacsPrefix = Nothing
    , stIdleGo = False
    , stComposing = Nothing
    , stAttending = ([], [])
    , stThrob = 0
    , stBusy = []
    , stRepoInfo = Map.empty
    , stCountedVerdicts = suVerdicts startup
    , stCountedThreads = suThreads startup
    , stHighlighting = Nothing
    , stJobs = suJobs startup
    , stWorkSlots = suSlots startup
    , stEpoch = 0
    , stReloading = Map.empty
    , stRefreshing = False
    , stLoadedOnce = False
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
    , stWatchMode = watchMode
    , stBranch = suBranch startup
    , stHead = suHead startup
    , stTree = Nothing
    , stHelp = False
    , stRoots = roots
    , stStory = story
    , stPlaces = Map.empty
    }

parkAtStart :: St -> St
parkAtStart st0 =
  case firstUnrespondedOrPending st0 of
    Just (f, h) ->
      let
        parked = (park f h st0) {stMsg = stMsg st0}
      in
        parked
          { stAnchor =
              fmap (\fv -> (fvPath fv, stCurH parked)) (currentFileMay parked)
          }
    Nothing -> st0

startBeats :: BChan REvent -> IO ()
startBeats chan = do
  -- the first frame paints with plain tokens; this tick colorizes
  -- the file on screen immediately after, off the startup path
  void (writeBChanNonBlocking chan Tick)
  void (writeBChanNonBlocking chan GitStateChanged)
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
buildVty :: IO V.Vty
buildVty = do
  full <- wantsFullColor
  let
    vtyCfg =
      if full
        then V.defaultConfig {V.configPreferredColorMode = Just V.FullColor}
        else V.defaultConfig
  mkVty vtyCfg

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

app :: Maybe HelixTheme -> App St REvent Name
app theme =
  App
    { appDraw = draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent =
        \ev ->
          handleEvent ev
            >> recountIfChanged
            >> ensureCurrentHighlighted
            >> reanchorBody
            >> syncComposing
            >> rememberSelector
            >> noteTrayView
    , appStartEvent = do
        vty <- getVtyHandle
        let
          out = V.outputIface vty
        when (V.supportsMode out V.BracketedPaste) $
          liftIO (V.setMode out V.BracketedPaste True)
    , appAttrMap = const (mkAttrMap theme)
    }

-- Drawing ------------------------------------------------------------------------
