{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Core.Types
  ( WatchMode (..)
  , Name (..)
  , REvent (..)
  , Prompt (..)
  , PickerMode (..)
  , Picker (..)
  , Selector (..)
  , NotePopup (..)
  , RepoPlace (..)
  , WalkFilter (..)
  , TourMark (..)
  , Anchor (..)
  , anchorAt
  , anchorHunkIn
  , walkCycle
  , walkLabel
  , nextWalkFilter
  , PopupPurpose (..)
  , AmendTarget (..)
  , NoteOrigin (..)
  , MarkUndo (..)
  , FileView (..)
  , St (..)
  , repoText
  , currentFile
  , currentFileMay
  , rebaseEffMode
  , effectiveMode
  , rebaseShown
  , whenWritable
  , settleLevel
  , readOnlyMsg
  , fileDir
  , clampH
  , safeIdx
  , tshow
  , showHelp
  , quitKeys
  , promptKeys
  , TrayEntry (..)
  , Remark (..)
  , NoteLine (..)
  , searchSigil
  , VerdictScope (..)
  , setMsg
  , TreeMode (..)
  , TreeView (..)
  , TreeOp (..)
  , FileAction (..)
  , PollFlight (..)
  , PullPoll (..)
  , noPullPoll
  ) where

import Brick qualified as Brick
import Brick.BChan qualified as BChan
import Control.Concurrent qualified as Concurrent
import Control.Monad.State qualified as State
import Data.IORef qualified as IORef
import Data.Int qualified as DataInt
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Clock qualified as Clock
import Database.SQLite.Simple qualified as SQLite
import Graphics.Vty qualified as V
import Remax.Clipboard qualified as Clipboard
import Remax.Config qualified as Config
import Remax.DiffModel qualified as DiffModel
import Remax.Git qualified as Git
import Remax.GitHub qualified as GitHub
import Remax.Highlight qualified as Highlight
import Remax.Modal qualified as Modal
import Remax.PullPoll qualified as PullPoll
import Remax.Store qualified as Store
import Remax.Tree qualified as Tree
import Remax.TreeSitter qualified as TS
import Remax.Vi qualified as Vi
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

data Name = BodyVP | TrayVP | TheCursor
  deriving (Eq, Ord, Show)

-- FileChangedOnDisk carries an absolute path: the watcher callback is
-- created once and outlives any number of :repo switches, so it cannot
-- know which root to make a path relative to. The handler, which does
-- know, relativizes on arrival.
data WatchMode = NativeWatch | PollingWatch
  deriving (Eq, Show)

data REvent
  = FileChangedOnDisk FilePath
  | GitStateChanged
  | StoreChanged
  | -- repositories an agent is waiting on and working on, and a beat
    -- to animate by
    Attending ([T.Text], [T.Text])
  | Tick
  | Pulse
  | Settle
  | JobDone T.Text (Brick.EventM Name St ())

-- The one-line prompt at the bottom of the screen: / search or : command.
data Prompt = SearchPrompt T.Text | CmdPrompt T.Text

-- The finder overlay: fuzzy file picking, or project-wide grep. Each
-- display row is aligned with a jump target (file, and a 1-based line
-- for grep hits; 0 = just open the file).
data PickerMode = PickFiles | PickGrep | PickBase | PickBranch | PickRef | PickCommit | PickParent T.Text | PickRepo | PickPull

data Picker = Picker
  { pkMode :: PickerMode
  , pkInput :: T.Text
  , pkAll :: [FilePath]
  , pkMatches :: [T.Text]
  , pkTargets :: [(FilePath, Int)]
  , pkSel :: Int
  , pkExpanded :: Set.Set T.Text
  }

-- The selector that was last on screen, kept whole so it can be brought
-- back exactly as it was left — the query typed into it, what that
-- matched, and which row was under the cursor.
--
-- Two overlays answer to this. The tree is the file finder, flat or
-- hierarchical; the picker is buffers, commits, branches, repositories
-- and project search. Which of the two it was is part of what gets
-- remembered, because coming back to the other one is not coming back.
data Selector
  = LastTree TreeView
  | LastPicker Picker

-- The note editor overlay: a small modal buffer over the lines it will
-- annotate (resolved when the popup opened).
data NotePopup = NotePopup
  { npEd :: Modal.EdState
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
  { rpMode :: Git.ReviewMode
  , rpFile :: FilePath
  , rpHunk :: Int
  , rpWalk :: WalkFilter
  , rpOpened :: Set.Set FilePath
  , rpAnchor :: Maybe Anchor
  }

-- The hunk under review, remembered by content as well as by position:
-- a refresh can re-split a file and move hunk numbers about, and the
-- keys are what find the same lines again afterwards.
data Anchor = Anchor
  { anPath :: FilePath
  , anHunk :: Int
  , anKeys :: [T.Text]
  }

anchorAt :: FileView -> Int -> Anchor
anchorAt fv h =
  let
    clamped = clampH (fvRuns fv) h
  in
    Anchor
      { anPath = fvPath fv
      , anHunk = clamped
      , anKeys = maybe [] DiffModel.runKeys (safeIdx (fvRuns fv) clamped)
      }

-- Where the anchored hunk sits now: the run sharing the most of its
-- keys, falling back to the remembered position when none do.
anchorHunkIn :: Anchor -> FileView -> Int
anchorHunkIn an fv =
  let
    wanted = Set.fromList (anKeys an)
    overlap r = length (filter (`Set.member` wanted) (DiffModel.runKeys r))
    scored = zip (map overlap (fvRuns fv)) [0 ..]
  in
    case filter ((> 0) . fst) scored of
      [] -> clampH (fvRuns fv) (anHunk an)
      hits -> snd (maximum hits)

data TourMark = TourMark
  { tmStop :: Int
  , tmTotal :: Int
  , tmTitle :: Maybe T.Text
  , tmNote :: Maybe T.Text
  }
  deriving (Eq)

data WalkFilter = WalkAll | WalkPending | WalkUnresolved | WalkTour
  deriving (Eq)

walkCycle :: [WalkFilter]
walkCycle = [WalkPending, WalkUnresolved, WalkTour, WalkAll]

walkLabel :: WalkFilter -> T.Text
walkLabel w =
  case w of
    WalkAll -> "all"
    WalkPending -> "pending"
    WalkUnresolved -> "unresolved"
    WalkTour -> "tour"

nextWalkFilter :: WalkFilter -> WalkFilter
nextWalkFilter w =
  case w of
    WalkPending -> WalkUnresolved
    WalkUnresolved -> WalkTour
    WalkTour -> WalkAll
    WalkAll -> WalkPending

data PopupPurpose
  = NoteFor [T.Text] NoteOrigin
  | AmendMessage AmendTarget

data AmendTarget
  = {- | the paths that will be staged along with the message, so the popup
    can show them before you commit to them (empty at a rebase stop,
    where :continue owns the folding)
    -}
    AmendHead [FilePath]
  | -- | a conflict stop, whose commit :continue has yet to make
    AmendStoppedCommit

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
  { muRows :: [(T.Text, T.Text, T.Text, Int)] -- the rows it wrote
  , muBefore :: Map.Map T.Text (Maybe Store.Verdict) -- what those keys were
  , muCounts :: Map.Map Store.Verdict Int -- the session tallies before it
  , muPunch :: [(FilePath, Int, Store.Verdict)]
  }

data FileView = FileView
  { fvPath :: FilePath
  , fvBase :: [T.Text]
  , fvEd :: Modal.EdState
  , fvRows :: [DiffModel.RowR]
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
  , fvRuns :: [DiffModel.Run]
  , fvToks :: [[Highlight.Seg]]
  , fvStale :: Bool
  , fvDeleted :: Bool -- the change set removed this file; there is no working copy
  , fvHighlighted :: Bool -- real syntax colors computed yet? plain until shown
  , fvHl :: Maybe TS.Highlighter -- vendored tree-sitter grammar, when we have one
  , fvRunDecls :: [T.Text] -- per run: the top-level declaration it falls under
  , fvRemarks :: [Remark]
  , fvPending :: !Int
  , fvMtime :: Maybe Clock.UTCTime
  }

data St = St
  { stFiles :: [FileView]
  , stCurF :: Int
  , stCurH :: Int
  , stVerdicts :: Map.Map T.Text Store.Verdict
  , stMsg :: T.Text
  , stRoot :: FilePath
  , stMode :: Git.ReviewMode
  , stTreeMode :: Git.ReviewMode
  , stConn :: SQLite.Connection
  , stPassId :: DataInt.Int64
  , stCounts :: Map.Map Store.Verdict Int
  , stPunch :: [(FilePath, Int, Store.Verdict)]
  , stTray :: Bool
  , stLastGit :: Double
  , stLastRefPoll :: Double
  , stGitQueued :: Bool
  , stChan :: BChan.BChan REvent
  , stWatch :: FilePath -> IO () -- watch one directory, once
  , stPrompt :: Maybe Prompt -- the / search or : command prompt, while open
  , stLastSearch :: T.Text
  , stBorder :: V.Attr -- concrete attr for raw border fills
  , stHls :: TS.HighlighterTable
  , stWalk :: WalkFilter -- which hunks ]/[ and the tray admit
  , stFileScope :: Git.FileScope -- does the file dialog list ignored files too
  , stRebase :: Maybe Git.RebaseStatus -- a paused rebase the session is following
  , stWritten :: Set.Set FilePath -- files written since the last commit-shaped action
  , stLastMarked :: [T.Text] -- keys of the most recent verdict, for :note
  , stLastRows :: [(T.Text, T.Text, T.Text, Int)] -- and the rows it wrote, to undo it
  , stOpened :: Set.Set FilePath -- files opened by hand, pinned across refreshes
  , stPicker :: Maybe Picker
  , stLastSelector :: Maybe Selector -- brought back by ␣', as it was
  , -- where the tray viewport was sitting as of the last event: the
    -- first row on screen and how many fit. Only the band around it is
    -- actually drawn; see tray.
    stTraySeen :: (Int, Int)
  , stNotePopup :: Maybe NotePopup
  , stThreads :: Map.Map T.Text [Store.Post] -- note conversations by line key, posts in order
  , stTour :: [Store.TourStep]
  , stTourKeys :: Map.Map T.Text Int
  , stTourMarks :: Map.Map T.Text TourMark
  , stTourLast :: Maybe (Int, Int)
  , -- which named group each noted key belongs to. Two notes worded the
    -- same are two notes, and only the name says so.
    stNoteGroups :: Map.Map T.Text T.Text
  , stSpace :: Bool -- the space menu prefix is pending
  , stConfig :: Config.Config
  , stCmdFiles :: [T.Text] -- completion caches, filled when : opens
  , stCmdDirs :: [T.Text]
  , stCmdRefs :: [T.Text]
  , stPromptCycle :: Maybe (T.Text, Int) -- Tab cycling: typed base + applied index
  , stEmacsPrefix :: Maybe Char -- a pending C-c / C-x chord (emacs keymap)
  , stIdleGo :: Bool -- a pending g with no file open to hold it
  , stComposing :: Maybe T.Text -- repository we told the store is mid-note
  , stAttending :: ([T.Text], [T.Text]) -- repositories agents wait on / work on
  , stThrob :: !Int -- which frame the watching indicator is on
  , stSettling :: Map.Map T.Text Int -- accepted keys still glowing, frames left
  , stSettleFile :: Maybe FilePath -- the file those keys belong to
  , stBusy :: [T.Text]
  , stRepoInfo :: Map.Map FilePath [T.Text]
  , stCountedVerdicts :: Map.Map T.Text Store.Verdict
  , stCountedThreads :: Map.Map T.Text [Store.Post]
  , stHighlighting :: Maybe FilePath
  , stJobs :: IORef.IORef Int
  , stSettleBeat :: IORef.IORef Int -- frames the settle timer still owes
  , stWorkSlots :: Concurrent.QSem
  , stEpoch :: !Int
  , stReloading :: Map.Map FilePath Bool
  , stRefreshing :: Bool
  , stLoadedOnce :: Bool
  , stVi :: Vi.ViKeys -- a half-typed vi command: count, operator, what it awaits
  , stSearchDir :: Int -- which way / (or vi's ?) last searched: 1 or -1
  , stDeferAdvance :: Bool -- hold the post-mark jump until the note popup closes
  , stAltFile :: Maybe (FilePath, Int) -- where Space a bounces back to
  , stAnchor :: Maybe Anchor -- the hunk under review; ' returns here
  , stGrepNav :: Maybe (T.Text, [(FilePath, Int)], Int) -- grep hits n/N walk
  , stInfoPopup :: Maybe (T.Text, T.Text) -- read-only text popup: title, body
  , stKill :: T.Text -- session-wide kill/yank slot, shared across files
  , stClip :: Clipboard.Clipboard -- how this machine reaches the system clipboard
  , stClipSent :: T.Text -- the text remax and the clipboard last agreed on
  , stWatchMode :: WatchMode -- the native watcher was refused; polling instead
  , stBranch :: Maybe T.Text -- Nothing when HEAD is detached
  , stHead :: Maybe (T.Text, T.Text) -- (short sha, subject) of HEAD
  , stTree :: Maybe TreeView -- the project tree overlay, while it is open
  , stHelp :: Bool -- the keybinding cheat sheet is up
  , stRoots :: [FilePath] -- repositories this session can switch between
  , stStory :: Maybe FilePath -- the folder they sit in, when this is a story
  , stPlaces :: Map.Map FilePath RepoPlace -- where you were in each
  , stPullMeta :: Maybe GitHub.PullMeta
  , stPullPoll :: PullPoll
  , stBaseAt :: Maybe T.Text -- short sha of a pull request's diff base
  }

data PollFlight = PollIdle | PollRunning
  deriving (Eq, Show)

data PullPoll = PullPoll
  { ppCache :: Maybe PullPoll.PollCache
  , ppSnapshot :: Maybe PullPoll.Snapshot
  , ppFlight :: PollFlight
  , ppStarted :: Double
  , ppLastOk :: Maybe Double
  , ppFailure :: Maybe T.Text
  , ppNow :: Double
  }

noPullPoll :: PullPoll
noPullPoll =
  PullPoll
    { ppCache = Nothing
    , ppSnapshot = Nothing
    , ppFlight = PollIdle
    , ppStarted = 0
    , ppLastOk = Nothing
    , ppFailure = Nothing
    , ppNow = 0
    }

repoText :: St -> T.Text
repoText = T.pack . stRoot

currentFile :: St -> FileView
currentFile st = stFiles st !! stCurF st

currentFileMay :: St -> Maybe FileView
currentFileMay st = safeIdx (stFiles st) (stCurF st)

-- The base the session actually diffs against right now: mid-rebase the
-- stopped commit is under review (its parent is the base) — except a
-- conflict stop, where the commit isn't made yet and the resolution work
-- in the tree is the story.
rebaseEffMode :: Maybe Git.RebaseStatus -> Git.ReviewMode -> Git.ReviewMode
rebaseEffMode mRebase mode =
  case (Git.subjectOf mode, mRebase) of
    (Git.WorkingTree, Just rs)
      | Git.rebaseConflicted rs -> Git.VsRef "HEAD"
      | otherwise -> Git.VsRef "HEAD^"
    _ -> mode

effectiveMode :: St -> Git.ReviewMode
effectiveMode st = rebaseEffMode (stRebase st) (stMode st)

rebaseShown :: St -> Maybe Git.RebaseStatus
rebaseShown st =
  case Git.subjectOf (stMode st) of
    Git.WorkingTree -> stRebase st
    _ -> Nothing

whenWritable :: (Git.Writable -> Brick.EventM Name St ()) -> Brick.EventM Name St ()
whenWritable act = do
  st <- State.get
  case Git.writable (effectiveMode st) of
    Right grant -> act grant
    Left tip -> setMsg (readOnlyMsg tip)

settleLevel :: St -> Maybe Int
settleLevel st =
  if Map.null (stSettling st) then Nothing else Just (maximum (Map.elems (stSettling st)))

readOnlyMsg :: T.Text -> T.Text
readOnlyMsg tip =
  "reviewing " <> tip <> " read-only \8212 :mode tree returns to the working tree"

-- The file watcher ---------------------------------------------------------

fileDir :: FilePath -> FilePath -> FilePath
fileDir root path = FilePath.normalise (root </> FilePath.takeDirectory path)

clampH :: [DiffModel.Run] -> Int -> Int
clampH rs h = max 0 (min h (length rs - 1))

-- The element at an index, if the list reaches that far.
--
-- Counted to rather than measured first: asking `i < length xs` walks
-- the whole list before the index does, so reading one hunk out of a
-- file that has thousands cost a walk over all of them twice. That is
-- charged per lookup, and the frame asks per hunk.
safeIdx :: [a] -> Int -> Maybe a
safeIdx xs i =
  if i < 0
    then Nothing
    else case drop i xs of
      (x : _) -> Just x
      [] -> Nothing

tshow :: Show a => a -> T.Text
tshow = T.pack . show

-- The app ----------------------------------------------------------------------

showHelp :: Brick.EventM Name St ()
showHelp = State.modify (\s -> s {stHelp = True})

-- The persistent session's resting state: nothing to review, but the
-- watcher is live and new work will wake the screen on its own.
-- The resting screen names the way out that fits the session it is in:
-- with other checkouts in reach, a clean one is most often a cue to go
-- and look at the next, and gr is hard to guess from an empty pane.
-- How to leave, and how to reach the command line, in the words of the
-- keymap the reader chose.
--
-- The command is q whatever they are using, but the way to it is not.
-- Emacs has no : — it opens the command line with M-x, so telling an
-- emacs reader to press : is telling them to type a colon into their
-- buffer — and it has its own chord for leaving outright, which is the
-- one that hand will reach for.
quitKeys :: Config.KeyMode -> T.Text
quitKeys mode = case mode of
  Config.KeysEmacs -> "C-x C-c"
  _ -> ":q"

promptKeys :: Config.KeyMode -> T.Text
promptKeys mode = case mode of
  Config.KeysEmacs -> "M-x"
  _ -> ":"

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
  , rmKey :: !T.Text
  , rmText :: !T.Text
  }

data NoteLine = NoteLine
  { nlAuthor :: T.Text
  , nlOpens :: Bool
  , nlText :: T.Text
  }
  deriving (Eq)

searchSigil :: St -> T.Text
searchSigil st = if stSearchDir st < 0 then "?" else "/"

data VerdictScope = VerdictHunk | VerdictLines

setMsg :: T.Text -> Brick.EventM Name St ()
setMsg t = State.modify (\s -> s {stMsg = t})

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
  , tvRoot :: Tree.TreeNode
  , tvAll :: [(FilePath, Maybe Git.PathStat)] -- every file, for flat mode
  , tvExpanded :: Set.Set FilePath -- directories and files alike
  , tvSel :: Int
  , tvInput :: T.Text
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
  | OpNaming FileAction T.Text
  | OpConfirming FilePath

-- Each carries the path it works from: the directory a new thing lands
-- in, or the path being renamed, copied or removed.
data FileAction
  = NewFile FilePath
  | NewDir FilePath
  | RenamePath FilePath
  | DuplicatePath FilePath
