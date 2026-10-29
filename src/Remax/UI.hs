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

import Brick qualified as Brick
import Brick.BChan qualified as BChan
import Brick.Util qualified as BrickUtil
import Control.Concurrent qualified as Concurrent
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.IORef qualified as IORef
import Data.Int qualified as DataInt
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Clock qualified as Clock
import Database.SQLite.Simple qualified as SQLite
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform qualified as VtyPlatform
import Remax.Clipboard qualified as Clipboard
import Remax.Config qualified as Config
import Remax.Conversation qualified as Conv
import Remax.Git qualified as Git
import Remax.GitHub qualified as GitHub
import Remax.Store qualified as Store
import Remax.Theme qualified as Theme
import Remax.TreeSitter qualified as TS
import Remax.UI.Action.Pull qualified as PullAction
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Tour qualified as Tour
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Verdict qualified as Verdict
import Remax.UI.Draw qualified as Draw
import Remax.UI.Events qualified as Events
import Remax.UI.Model.Load qualified as Load
import Remax.UI.Model.Nav qualified as Nav
import Remax.UI.Model.Watch qualified as Watch
import Remax.UI.Model.Work qualified as WorkModel
import Remax.Vi qualified as Vi
import System.Environment qualified as Env

data Startup = Startup
  { suConn :: SQLite.Connection
  , suVerdicts :: Map.Map T.Text Store.Verdict
  , suHighlighters :: TS.HighlighterTable
  , suThreads :: Map.Map T.Text [Store.Post]
  , suWork :: Conv.OwnWork
  , suNow :: Clock.UTCTime
  , suNoteGroups :: Map.Map T.Text T.Text
  , suTour :: [Store.TourStep]
  , suConfig :: Config.Config
  , suRebase :: Maybe Git.RebaseStatus
  , suBranch :: Maybe T.Text
  , suHead :: Maybe (T.Text, T.Text)
  , suPassId :: DataInt.Int64
  , suTheme :: Maybe Theme.HelixTheme
  , suChan :: BChan.BChan Types.REvent
  , suJobs :: IORef.IORef Int
  , suSettleBeat :: IORef.IORef Int
  , suSlots :: Concurrent.QSem
  }

runReview :: Git.ReviewMode -> Maybe GitHub.PullMeta -> FilePath -> [FilePath] -> Maybe FilePath -> IO ()
runReview mode pullMeta root roots story = do
  startup <- loadStartup mode root
  let
    chan = suChan startup
  final <- Watch.withWatcher $ \mgr watchMode -> do
    dirWatch <- Watch.installWatches mgr chan root watchMode
    -- probed once: doing it per yank would be a process spawn a keystroke
    clip <- Clipboard.discoverClipboard
    -- and read once on the way in, so whatever was already on the
    -- clipboard counts as seen. Without this the first paste of a
    -- session would treat months-old clipboard contents as newer than
    -- the line you just cut, and `dd p` would not put the line back.
    alreadyThere <- Clipboard.readClipboard clip
    let
      st0 = initialState startup mode pullMeta root roots story watchMode dirWatch clip alreadyThere
    startBeats chan
    vty <- buildVty
    Brick.customMain vty buildVty (Just chan) (app (suTheme startup)) (parkAtStart (Tour.installTour (suTour startup) st0))
  Store.finishPass (suConn startup) (Types.stPassId final)
  printSummary final

loadStartup :: Git.ReviewMode -> FilePath -> IO Startup
loadStartup mode root = do
  conn <- Store.openStore
  -- a session killed with a note popup open leaves the marker behind;
  -- starting one is as good a moment as any to say it is not being
  -- written any more
  Store.setComposing conn (T.pack root) False
  verdicts <- Store.loadVerdicts conn (T.pack root)
  hls <- TS.initHighlighters
  threads0 <- Store.loadThreads conn (T.pack root)
  work0 <- WorkModel.loadFor conn root mode
  now0 <- Clock.getCurrentTime
  noteGroups0 <- Store.loadNoteGroups conn (T.pack root)
  cfg <- Config.loadConfig
  rebase0 <- Git.rebaseStatus root
  Tour.recordView conn root (Types.rebaseEffMode rebase0 mode)
  tour0 <- Tour.loadTourFor conn root (Types.rebaseEffMode rebase0 mode)
  (branch0, head0) <- Git.subjectPlace (Types.rebaseEffMode rebase0 mode) root
  passId <- Store.beginPass conn (T.pack root) (Git.modeLabel mode)
  theme <- Theme.loadHelixTheme
  chan <- BChan.newBChan 64
  jobs <- IORef.newIORef (0 :: Int)
  settleBeat <- IORef.newIORef (0 :: Int)
  slots <- Concurrent.newQSem 2
  pure
    Startup
      { suConn = conn
      , suVerdicts = verdicts
      , suHighlighters = hls
      , suThreads = threads0
      , suWork = work0
      , suNow = now0
      , suNoteGroups = noteGroups0
      , suTour = tour0
      , suConfig = cfg
      , suRebase = rebase0
      , suBranch = branch0
      , suHead = head0
      , suPassId = passId
      , suTheme = theme
      , suChan = chan
      , suJobs = jobs
      , suSettleBeat = settleBeat
      , suSlots = slots
      }

treeModeOf :: Git.ReviewMode -> Git.ReviewMode
treeModeOf mode =
  case Git.subjectOf mode of
    Git.WorkingTree -> mode
    _ -> Git.VsRef "HEAD"

initialState ::
  Startup ->
  Git.ReviewMode ->
  Maybe GitHub.PullMeta ->
  FilePath ->
  [FilePath] ->
  Maybe FilePath ->
  Types.WatchMode ->
  Watch.DirWatch ->
  Clipboard.Clipboard ->
  Maybe T.Text ->
  Types.St
initialState startup mode pullMeta root roots story watchMode dirWatch clip alreadyThere =
  Types.St
    { Types.stFiles = []
    , Types.stCurF = 0
    , Types.stCurH = 0
    , Types.stVerdicts = suVerdicts startup
    , Types.stMsg = if watchMode == Types.PollingWatch then Watch.watchPollWarning else "loading\8230"
    , Types.stRoot = root
    , Types.stMode = mode
    , Types.stTreeMode = treeModeOf mode
    , Types.stConn = suConn startup
    , Types.stPassId = suPassId startup
    , Types.stCounts = Map.empty
    , Types.stPunch = []
    , Types.stTray = Config.cfgTray (suConfig startup) == Config.TrayOpen
    , Types.stLastGit = 0
    , Types.stLastRefPoll = 0
    , Types.stGitQueued = False
    , Types.stChan = suChan startup
    , Types.stWatch = Watch.dirWatchFollow dirWatch
    , Types.stPrompt = Nothing
    , Types.stLastSearch = ""
    , Types.stBorder = Attrs.fgOnly (Theme.scopeAttr (suTheme startup) "ui.linenr" (BrickUtil.fg V.brightBlack))
    , Types.stHls = suHighlighters startup
    , Types.stWalk = Types.WalkPending
    , Types.stFileScope = Git.VisibleFiles
    , Types.stRebase = suRebase startup
    , Types.stWritten = Set.empty
    , Types.stLastMarked = []
    , Types.stLastRows = []
    , Types.stOpened = Set.empty
    , Types.stPicker = Nothing
    , Types.stLastSelector = Nothing
    , Types.stTraySeen = (0, 0)
    , Types.stNotePopup = Nothing
    , Types.stThreads = suThreads startup
    , Types.stWork = suWork startup
    , Types.stBand = Nothing
    , Types.stFoldFlipped = Set.empty
    , Types.stOpenDetails = Set.empty
    , Types.stOpenCommits = Set.empty
    , Types.stConfirm = Nothing
    , Types.stCiting = Nothing
    , Types.stZPending = False
    , Types.stReacting = Nothing
    , Types.stSubmit = Nothing
    , Types.stSending = Nothing
    , Types.stInbox = Nothing
    , Types.stPullView = fmap (const Types.emptyPullView) pullMeta
    , Types.stNow = suNow startup
    , Types.stConversations = Conv.conversations Conv.emptyRemote (suWork startup)
    , Types.stOutdated = Map.empty
    , Types.stTour = suTour startup
    , Types.stTourKeys = Map.empty
    , Types.stTourMarks = Map.empty
    , Types.stTourLast = Nothing
    , Types.stNoteGroups = suNoteGroups startup
    , Types.stSpace = False
    , Types.stConfig = suConfig startup
    , Types.stCmdFiles = []
    , Types.stCmdDirs = []
    , Types.stCmdRefs = []
    , Types.stPromptCycle = Nothing
    , Types.stEmacsPrefix = Nothing
    , Types.stIdleGo = False
    , Types.stComposing = Nothing
    , Types.stAttending = ([], [])
    , Types.stThrob = 0
    , Types.stSettling = Map.empty
    , Types.stSettleFile = Nothing
    , Types.stBusy = []
    , Types.stRepoInfo = Map.empty
    , Types.stCountedVerdicts = suVerdicts startup
    , Types.stCountedThreads = suThreads startup
    , Types.stHighlighting = Nothing
    , Types.stFindingUses = Nothing
    , Types.stJobs = suJobs startup
    , Types.stSettleBeat = suSettleBeat startup
    , Types.stWorkSlots = suSlots startup
    , Types.stEpoch = 0
    , Types.stReloading = Map.empty
    , Types.stRefreshing = False
    , Types.stLoadedOnce = False
    , Types.stVi = Vi.viIdle
    , Types.stSearchDir = 1
    , Types.stDeferAdvance = False
    , Types.stPullMeta = pullMeta
    , Types.stPullPoll = Types.noPullPoll
    , Types.stBaseAt = Nothing
    , Types.stPullLocal = Nothing
    , Types.stAltFile = Nothing
    , Types.stAnchor = Nothing
    , Types.stGrepNav = Nothing
    , Types.stInfoPopup = Nothing
    , Types.stKill = T.empty
    , Types.stClip = clip
    , Types.stClipSent = Maybe.fromMaybe T.empty alreadyThere
    , Types.stWatchMode = watchMode
    , Types.stBranch = suBranch startup
    , Types.stHead = suHead startup
    , Types.stTree = Nothing
    , Types.stHelp = False
    , Types.stRoots = roots
    , Types.stStory = story
    , Types.stPlaces = Map.empty
    }

parkAtStart :: Types.St -> Types.St
parkAtStart st0 =
  case Verdict.firstUnrespondedOrPending st0 of
    Just (f, h) ->
      let
        parked = (Nav.park f h st0) {Types.stMsg = Types.stMsg st0}
      in
        parked
          { Types.stAnchor =
              fmap (\fv -> Types.anchorAt fv (Types.stCurH parked)) (Types.currentFileMay parked)
          }
    Nothing -> st0

startBeats :: BChan.BChan Types.REvent -> IO ()
startBeats chan = do
  -- the first frame paints with plain tokens; this tick colorizes
  -- the file on screen immediately after, off the startup path
  Monad.void (BChan.writeBChanNonBlocking chan Types.Tick)
  Monad.void (BChan.writeBChanNonBlocking chan Types.GitStateChanged)
  -- Who is waiting on this review, asked for on a beat.
  --
  -- The beat is the animation's as well as the question's, so it runs
  -- quickly while somebody is there and slowly while nobody is: an
  -- empty directory read every second and a half is not worth a
  -- watcher, and a session with no agent on it should not be waking up
  -- to redraw something that is not on screen.
  Monad.void . Concurrent.forkIO . Monad.forever $ do
    seen@(waiting, working) <- Store.presenceNow
    Monad.void (BChan.writeBChanNonBlocking chan (Types.Attending seen))
    Concurrent.threadDelay
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
  VtyPlatform.mkVty vtyCfg

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
  colorterm <- Env.lookupEnv "COLORTERM"
  term <- Env.lookupEnv "TERM"
  let
    saysColorterm = case colorterm of
      Just v -> v `elem` ["truecolor", "24bit"]
      Nothing -> False
    saysTerm = case term of
      Nothing -> False
      Just t ->
        t `elem` trueColorTerms
          || "-direct" `List.isSuffixOf` t
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

printSummary :: Types.St -> IO ()
printSummary st = do
  let
    count v = Map.findWithDefault 0 v (Types.stCounts st)
  putStrLn
    ( "This pass: "
        ++ show (count Store.Reviewed)
        ++ " reviewed, "
        ++ show (count Store.Flagged)
        ++ " flagged, "
        ++ show (count Store.Questioned)
        ++ " questioned."
    )
  Monad.forM_ (reverse (Types.stPunch st)) $ \(path, line, v) ->
    putStrLn ("  " ++ T.unpack (Store.verdictText v) ++ ": " ++ path ++ ":" ++ show line)

app :: Maybe Theme.HelixTheme -> Brick.App Types.St Types.REvent Types.Name
app theme =
  Brick.App
    { Brick.appDraw = Draw.draw
    , Brick.appChooseCursor = Brick.showFirstCursor
    , Brick.appHandleEvent =
        \ev ->
          Events.handleEvent ev
            >> Verdict.recountIfChanged
            >> Load.ensureCurrentHighlighted
            >> Events.reanchorBody
            >> Events.syncComposing
            >> Events.rememberSelector
            >> Events.noteTrayView
    , Brick.appStartEvent = do
        vty <- Brick.getVtyHandle
        let
          out = V.outputIface vty
        Monad.when (V.supportsMode out V.BracketedPaste) $
          MonadIO.liftIO (V.setMode out V.BracketedPaste True)
        Monad.when (V.supportsMode out V.Mouse) $
          MonadIO.liftIO (V.setMode out V.Mouse True)
        Monad.when (V.supportsMode out V.Hyperlink) $
          MonadIO.liftIO (V.setMode out V.Hyperlink True)
        seeded <- State.gets Types.stPullMeta
        Monad.when (Maybe.isNothing seeded) PullAction.refreshPullMeta
        PullAction.pollPullNow
    , Brick.appAttrMap = const (Attrs.mkAttrMap theme)
    }

-- Drawing ------------------------------------------------------------------------
