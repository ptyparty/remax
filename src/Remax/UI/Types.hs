{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Types
  ( WatchMode (..)
  , Name (..)
  , REvent (..)
  , Prompt (..)
  , PickerMode (..)
  , Picker (..)
  , Selector (..)
  , NotePopup (..)
  , RepoPlace (..)
  , RunFilter (..)
  , filterCycle
  , runFilterLabel
  , nextRunFilter
  , runFilterAllows
  , PopupPurpose (..)
  , NoteOrigin (..)
  , MarkUndo (..)
  , FileView (..)
  , St (..)
  , repoText
  , currentFile
  , currentFileMay
  , rebaseEffMode
  , effectiveMode
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
  ) where

import Brick
  ( EventM
  )
import Brick.BChan
  ( BChan
  )
import Control.Concurrent
  ( QSem
  )
import Control.Monad.State
  ( modify
  )
import Data.IORef
  ( IORef
  )
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple (Connection)
import qualified Graphics.Vty as V
import Remax.Clipboard
import Remax.Config
import Remax.DiffModel
import Remax.Git
  ( FileScope (..)
  , PathStat (..)
  , RebaseStatus (..)
  , ReviewMode (..)
  )
import Remax.Highlight
import Remax.Modal
import Remax.Store
import Remax.Tree
import qualified Remax.TreeSitter as TS
import Remax.Vi
  ( ViKeys
  )
import System.FilePath
  ( normalise
  , takeDirectory
  , (</>)
  )

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
    Attending ([Text], [Text])
  | Tick
  | Pulse
  | JobDone Text (EventM Name St ())

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
  , fvRemarks :: [Remark]
  , fvPending :: !Int
  , fvMtime :: Maybe UTCTime
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
  , stLastSelector :: Maybe Selector -- brought back by ␣', as it was
  , -- where the tray viewport was sitting as of the last event: the
    -- first row on screen and how many fit. Only the band around it is
    -- actually drawn; see tray.
    stTraySeen :: (Int, Int)
  , stNotePopup :: Maybe NotePopup
  , stThreads :: Map.Map Text [Post] -- note conversations by line key, posts in order
  , -- which named group each noted key belongs to. Two notes worded the
    -- same are two notes, and only the name says so.
    stNoteGroups :: Map.Map Text Text
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
  , stBusy :: [Text]
  , stRepoInfo :: Map.Map FilePath [Text]
  , stCountedVerdicts :: Map.Map Text Verdict
  , stCountedThreads :: Map.Map Text [Post]
  , stHighlighting :: Maybe FilePath
  , stJobs :: IORef Int
  , stWorkSlots :: QSem
  , stEpoch :: !Int
  , stReloading :: Map.Map FilePath Bool
  , stRefreshing :: Bool
  , stLoadedOnce :: Bool
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
  , stWatchMode :: WatchMode -- the native watcher was refused; polling instead
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

fileDir :: FilePath -> FilePath -> FilePath
fileDir root path = normalise (root </> takeDirectory path)

clampH :: [Run] -> Int -> Int
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

tshow :: Show a => a -> Text
tshow = T.pack . show

-- The app ----------------------------------------------------------------------

showHelp :: EventM Name St ()
showHelp = modify (\s -> s {stHelp = True})

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
quitKeys :: KeyMode -> Text
quitKeys mode = case mode of
  KeysEmacs -> "C-x C-c"
  _ -> ":q"

promptKeys :: KeyMode -> Text
promptKeys mode = case mode of
  KeysEmacs -> "M-x"
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
  , rmKey :: !Text
  , rmText :: !Text
  }

data NoteLine = NoteLine
  { nlAuthor :: Text
  , nlOpens :: Bool
  , nlText :: Text
  }
  deriving (Eq)

searchSigil :: St -> Text
searchSigil st = if stSearchDir st < 0 then "?" else "/"

data VerdictScope = VerdictHunk | VerdictLines

setMsg :: Text -> EventM Name St ()
setMsg t = modify (\s -> s {stMsg = t})

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
