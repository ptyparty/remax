{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent qualified as Concurrent
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Data.IORef qualified as IORef
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Database.SQLite.Simple qualified as SQLite
import Remax.Conversation qualified as Conv
import Remax.Feedback qualified as Feedback
import Remax.Json qualified as Json
import Remax.NoteArchive qualified as NoteArchive
import Remax.NoteExport qualified as NoteExport
import Remax.Store qualified as Store
import Remax.WorkStore qualified as WorkStore
import System.Directory qualified as Directory
import System.Environment qualified as Env
import System.Exit qualified as Exit
import System.FilePath ((</>))
import System.Posix.Process qualified as Posix
import Test.QuickCheck qualified as QC

data Op
  = AddNote Int (NEL.NonEmpty Int)
  | Reply Int
  | Accept [Int]
  | RetireOne Int
  | Forget
  | Unmark [Int]
  deriving (Show)

instance QC.Arbitrary Op where
  arbitrary =
    QC.frequency
      [ (4, AddNote <$> noteIndex <*> fmap (NEL.fromList . List.nub) (QC.listOf1 lineIndex))
      , (4, Reply <$> noteIndex)
      , (2, Accept <$> QC.listOf lineIndex)
      , (2, RetireOne <$> noteIndex)
      , (1, pure Forget)
      , (1, Unmark <$> QC.listOf lineIndex)
      ]
   where
    noteIndex = QC.choose (0, 2)
    lineIndex = QC.choose (0, 4)

data ModelNote = ModelNote
  { modelLines :: [Int]
  , modelPosts :: [T.Text]
  , modelRetired :: Bool
  }
  deriving (Eq, Show)

type Model = Map.Map Int ModelNote

archiving :: NoteArchive.Retention
archiving = NoteArchive.ArchiveNotes

lineKey :: Int -> T.Text
lineKey i = "k" <> T.pack (show i)

lineText :: Int -> T.Text
lineText i = "+line " <> T.pack (show i)

filePath :: T.Text
filePath = "src/a.hs"

noteName :: T.Text -> Int -> T.Text
noteName repo i = "note" <> T.pack (show i) <> "@" <> repo

scopeOf :: T.Text -> WorkStore.Scope
scopeOf repo = WorkStore.Scope {WorkStore.scopeRepo = repo, WorkStore.scopePull = Nothing}

noteTarget :: T.Text -> Int -> [Int] -> Conv.Target
noteTarget repo i ls =
  let
    spanLines = NEL.fromList [Conv.SpanLine (Store.LineKey (lineKey l)) (l + 1) (lineText l) | l <- ls]
    lineSpan =
      Conv.LineSpan
        { Conv.spanPath = filePath
        , Conv.spanSide = Conv.HeadSide
        , Conv.spanLines = spanLines
        , Conv.spanAt = Nothing
        }
  in
    Conv.OnLinesOnly (Conv.LocalId (noteName repo i)) lineSpan Store.Flagged

recordLines :: SQLite.Connection -> T.Text -> IO ()
recordLines conn repo = do
  passId <- Store.beginPass conn repo "test"
  Store.recordVerdicts
    conn
    NoteArchive.ArchiveNotes
    repo
    passId
    Store.Flagged
    Store.VerdictOrigin {Store.originCommit = Just ("abc123", "a subject"), Store.originRef = Just "feature"}
    [(lineKey i, filePath, lineText i, i + 1) | i <- [0 .. 4]]

data Actions = Actions
  { actRetireOn :: [Int] -> IO ()
  , actRetireOne :: Int -> IO ()
  , actForget :: IO ()
  , actUnmark :: [Int] -> IO ()
  }

implActions :: SQLite.Connection -> NoteArchive.Retention -> T.Text -> Actions
implActions conn retention repo =
  Actions
    { actRetireOn = \ls -> WorkStore.retireNotesOn conn retention WorkStore.LinesAccepted repo (map (Store.LineKey . lineKey) ls)
    , actRetireOne = \i -> WorkStore.retireNotes conn retention repo [Conv.LocalId (noteName repo i)]
    , actForget = WorkStore.forgetWork conn retention repo "" [filePath]
    , actUnmark = \ls -> Store.deleteKeys conn retention repo (map lineKey ls)
    }

keysJson :: [Int] -> T.Text
keysJson ls = "[" <> T.intercalate "," ["\"" <> lineKey l <> "\"" | l <- ls] <> "]"

mainActions :: SQLite.Connection -> T.Text -> Actions
mainActions conn repo =
  Actions
    { actRetireOn = \ls -> do
        let
          onKeys = ("SELECT local_id FROM work_target_line WHERE repo = ? AND key IN (SELECT value FROM json_each(?))" :: T.Text)
        SQLite.execute conn ("DELETE FROM work WHERE repo = ? AND target_kind = 'note' AND target_id IN (" <> SQLite.Query onKeys <> ")") (repo, repo, keysJson ls)
        SQLite.execute conn ("DELETE FROM work_target WHERE repo = ? AND kind = 'note' AND local_id IN (" <> SQLite.Query onKeys <> ")") (repo, repo, keysJson ls)
        SQLite.execute conn "DELETE FROM work_target_line WHERE repo = ? AND local_id NOT IN (SELECT local_id FROM work_target WHERE repo = ?)" (repo, repo)
    , actRetireOne = \i -> do
        let
          lid = noteName repo i
        SQLite.execute conn "DELETE FROM work WHERE repo = ? AND target_kind = 'note' AND target_id = ?" (repo, lid)
        SQLite.execute conn "DELETE FROM work_target WHERE repo = ? AND local_id = ?" (repo, lid)
        SQLite.execute conn "DELETE FROM work_target_line WHERE repo = ? AND local_id = ?" (repo, lid)
        SQLite.execute conn "DELETE FROM note_post WHERE repo = ? AND group_id = ?" (repo, lid)
    , actForget = do
        SQLite.execute conn "DELETE FROM note_post WHERE repo = ? AND key IN (SELECT key FROM line_verdict WHERE repo = ? AND file = ?)" (repo, repo, filePath)
        SQLite.execute conn "DELETE FROM line_verdict WHERE repo = ? AND file = ?" (repo, filePath)
        SQLite.execute conn "DELETE FROM work WHERE repo = ?" (SQLite.Only repo)
        SQLite.execute conn "DELETE FROM work_target_line WHERE repo = ?" (SQLite.Only repo)
        SQLite.execute conn "DELETE FROM work_target WHERE repo = ?" (SQLite.Only repo)
    , actUnmark = \ls -> do
        SQLite.execute conn "DELETE FROM line_verdict WHERE repo = ? AND key IN (SELECT value FROM json_each(?))" (repo, keysJson ls)
        SQLite.execute conn "DELETE FROM note_post WHERE repo = ? AND key IN (SELECT value FROM json_each(?))" (repo, keysJson ls)
    }

apply :: SQLite.Connection -> T.Text -> Actions -> Model -> (Int, Op) -> IO Model
apply conn repo actions model (n, op) =
  let
    body = "<post-" <> T.pack (show n) <> ">"
    post i ls author = Monad.void (WorkStore.addAside conn (scopeOf repo) (noteTarget repo i ls) author (Conv.Markdown body))
    retireWhere live = Map.map (\m -> if live m then m {modelRetired = True} else m)
    isLive = not . modelRetired
  in
    case op of
      AddNote i ls
        | Map.notMember i model -> do
            let
              sorted = List.sort (NEL.toList ls)
            post i sorted Conv.Viewer
            pure (Map.insert i (ModelNote sorted [body] False) model)
      Reply i
        | Just m <- Map.lookup i model
        , isLive m -> do
            post i (modelLines m) (Conv.Agent (Conv.AgentName "claude"))
            pure (Map.insert i m {modelPosts = modelPosts m ++ [body]} model)
      Accept ls -> do
        actRetireOn actions ls
        pure (retireWhere (\m -> isLive m && any (`elem` ls) (modelLines m)) model)
      RetireOne i -> do
        actRetireOne actions i
        pure (Map.adjust (\m -> m {modelRetired = True}) i model)
      Forget -> do
        actForget actions
        pure (retireWhere isLive model)
      Unmark ls -> do
        actUnmark actions ls
        pure model
      _ -> pure model

liveDump :: SQLite.Connection -> T.Text -> IO ([(T.Text, T.Text, T.Text, T.Text)], [(T.Text, T.Text, T.Text, T.Text, Maybe T.Text)], [(T.Text, Int, T.Text, Int, T.Text)], [(T.Text, Int, T.Text, T.Text)])
liveDump conn repo = do
  let
    suffix = "@" <> repo
  works <- SQLite.query conn "SELECT REPLACE(target_id, ?, ''), kind, author, body FROM work WHERE repo = ? ORDER BY target_id, id" (suffix, repo)
  targets <- SQLite.query conn "SELECT REPLACE(local_id, ?, ''), kind, path, side, verdict FROM work_target WHERE repo = ? ORDER BY local_id" (suffix, repo)
  targetLines <- SQLite.query conn "SELECT REPLACE(local_id, ?, ''), seq, key, line_no, line FROM work_target_line WHERE repo = ? ORDER BY local_id, seq" (suffix, repo)
  posts <- SQLite.query conn "SELECT key, seq, author, body FROM note_post WHERE repo = ? ORDER BY key, seq" (SQLite.Only repo)
  pure (works, targets, targetLines, posts)

archiveRows :: SQLite.Connection -> T.Text -> IO Int
archiveRows conn repo = do
  [SQLite.Only n] <- SQLite.query conn "SELECT COUNT(*) FROM note_archive WHERE repo = ?" (SQLite.Only repo)
  pure n

postBodies :: [(Conv.WorkId, Conv.Work)] -> [T.Text]
postBodies = map (Conv.markdownText . Conv.workBody . snd)

openNotesRead :: SQLite.Connection -> T.Text -> IO (Map.Map T.Text [T.Text])
openNotesRead conn repo = do
  own <- WorkStore.loadWork conn (scopeOf repo)
  pure (Map.fromList [(lid, postBodies items) | (Conv.OnLinesOnly (Conv.LocalId lid) _ _, items) <- Map.toList (Conv.workItems own)])

listedText :: SQLite.Connection -> T.Text -> IO T.Text
listedText conn repo = do
  listed <- Feedback.listFor conn (T.unpack repo)
  pure (T.concat (map (Json.encode . Feedback.listedJson) listed))

exportedIn :: SQLite.Connection -> T.Text -> NoteExport.Which -> IO (Map.Map T.Text NoteArchive.ExportedNote)
exportedIn conn repo which = do
  notes <- NoteExport.exportNotes conn which
  pure (Map.fromList [(NoteArchive.noteId note, note) | note <- notes, NoteArchive.noteRepo note == repo])

isRetired :: NoteArchive.ExportedNote -> Bool
isRetired note =
  case NoteArchive.noteState note of
    NoteArchive.Retired _ -> True
    NoteArchive.Open -> False

summary :: NoteArchive.ExportedNote -> (Bool, [T.Text], [Maybe Int], Maybe T.Text)
summary note =
  ( isRetired note
  , map NoteArchive.postBody (NoteArchive.notePosts note)
  , map NoteArchive.lineNumber (NoteArchive.noteLines note)
  , NoteArchive.noteFile note
  )

propRetiredNotesStayOutOfReadsAndInTheExport :: SQLite.Connection -> NoteArchive.Retention -> IORef.IORef Int -> [Op] -> QC.Property
propRetiredNotesStayOutOfReadsAndInTheExport conn retention counter ops =
  QC.ioProperty $ do
    n <- IORef.atomicModifyIORef' counter (\c -> (c + 1, c))
    let
      repo = "/remax-test/repo" <> T.pack (show n)
      reference = "/remax-test/main" <> T.pack (show n)
    recordLines conn repo
    model <- Monad.foldM (apply conn repo (implActions conn retention repo)) Map.empty (zip [0 ..] ops)
    recordLines conn reference
    Monad.foldM_ (apply conn reference (mainActions conn reference)) Map.empty (zip [0 ..] ops)
    ours <- liveDump conn repo
    mains <- liveDump conn reference
    archived <- archiveRows conn repo
    open <- openNotesRead conn repo
    leftovers <- liveNoteTables conn repo
    listing <- listedText conn repo
    everything <- exportedIn conn repo NoteExport.AllNotes
    retired <- exportedIn conn repo NoteExport.RetiredOnly
    let
      live = Map.filter (not . modelRetired) model
      kept = if retention == NoteArchive.ArchiveNotes then model else live
      gone = Map.filter modelRetired model
      byName = Map.mapKeys (noteName repo)
      said ms = concatMap modelPosts (Map.elems ms)
      expectedSummary m = (modelRetired m, modelPosts m, map (Just . (+ 1)) (modelLines m), Just filePath)
    pure $
      QC.conjoin
        [ QC.counterexample "live tables are what main leaves" (ours QC.=== mains)
        , QC.counterexample "archive rows" (archived QC.=== (if retention == NoteArchive.ArchiveNotes then Map.size gone else 0))
        , QC.counterexample "live tables hold only open notes" (leftovers QC.=== (Set.fromList (Map.keys (byName live)), Set.fromList (Map.keys (byName live)), sum (map (length . modelPosts) (Map.elems live))))
        , QC.counterexample "open notes read back" (open QC.=== Map.map modelPosts (byName live))
        , QC.counterexample "list shows live posts" (all (`T.isInfixOf` listing) (said live))
        , QC.counterexample "list hides retired posts" (not (any (`T.isInfixOf` listing) (said gone)))
        , QC.counterexample "export keeps every note the setting keeps" (Map.map summary everything QC.=== Map.map expectedSummary (byName kept))
        , QC.counterexample "retired export" (Map.keys retired QC.=== Map.keys (byName (if retention == NoteArchive.ArchiveNotes then gone else Map.empty)))
        , QC.counterexample "retired are stamped" (all isRetired retired)
        ]

liveNoteTables :: SQLite.Connection -> T.Text -> IO (Set.Set T.Text, Set.Set T.Text, Int)
liveNoteTables conn repo = do
  targets <- SQLite.query conn "SELECT local_id FROM work_target WHERE repo = ? AND kind = 'note'" (SQLite.Only repo)
  lineOwners <- SQLite.query conn "SELECT DISTINCT local_id FROM work_target_line WHERE repo = ?" (SQLite.Only repo)
  [SQLite.Only works] <- SQLite.query conn "SELECT COUNT(*) FROM work WHERE repo = ? AND target_kind = 'note'" (SQLite.Only repo)
  pure (Set.fromList (map SQLite.fromOnly targets), Set.fromList (map SQLite.fromOnly lineOwners), works)

data Check = Check String Bool

legacyPost :: SQLite.Connection -> T.Text -> T.Text -> T.Text -> Int -> T.Text -> T.Text -> IO ()
legacyPost conn repo group key seq_ author body =
  SQLite.execute
    conn
    "INSERT INTO note_post (repo, key, seq, author, body, group_id) VALUES (?, ?, ?, ?, ?, ?)"
    (repo, key, seq_, author, body, group)

exampleContextSurvivesUnmarking :: SQLite.Connection -> IO [Check]
exampleContextSurvivesUnmarking conn = do
  let
    repo = "/remax-test/context"
  recordLines conn repo
  Monad.void (WorkStore.addAside conn (scopeOf repo) (noteTarget repo 0 [0, 1]) Conv.Viewer (Conv.Markdown "why"))
  WorkStore.retireNotesOn conn archiving WorkStore.LinesAccepted repo [Store.LineKey (lineKey 0)]
  Store.deleteKeys conn archiving repo [lineKey 0, lineKey 1]
  notes <- exportedIn conn repo NoteExport.RetiredOnly
  pure $ case Map.elems notes of
    [note] ->
      [ Check "context: file" (NoteArchive.noteFile note == Just filePath)
      , Check "context: ref" (NoteArchive.noteRef note == Just "feature")
      , Check "context: commit" (NoteArchive.noteCommit note == Just "abc123")
      , Check "context: line text" (map NoteArchive.lineText (NoteArchive.noteLines note) == [lineText 0, lineText 1])
      , Check "context: post" (map NoteArchive.postBody (NoteArchive.notePosts note) == ["why"])
      ]
    other -> [Check ("context: one retired note, got " <> show (length other)) False]

exampleForgetKeepsContext :: SQLite.Connection -> IO [Check]
exampleForgetKeepsContext conn = do
  let
    repo = "/remax-test/forget-context"
  recordLines conn repo
  Monad.void (WorkStore.addAside conn (scopeOf repo) (noteTarget repo 0 [0, 1]) Conv.Viewer (Conv.Markdown "why"))
  WorkStore.forgetWork conn archiving repo "" [filePath]
  notes <- exportedIn conn repo NoteExport.RetiredOnly
  pure $ case Map.elems notes of
    [note] ->
      [ Check "forget: ref kept" (NoteArchive.noteRef note == Just "feature")
      , Check "forget: commit kept" (NoteArchive.noteCommit note == Just "abc123")
      ]
    other -> [Check ("forget: one retired note, got " <> show (length other)) False]

exampleRetirementLeavesATombstone :: SQLite.Connection -> NoteArchive.Retention -> T.Text -> IO [Check]
exampleRetirementLeavesATombstone conn retention name = do
  let
    repo = "/remax-test/tombstone-" <> name
    tombstones prefix = fmap (map (\r -> (WorkStore.retiredId r, retiredReasonName (WorkStore.retiredReason r)))) (WorkStore.retiredNotes conn repo prefix)
  recordLines conn repo
  Monad.void (WorkStore.addAside conn (scopeOf repo) (noteOnFile repo "ts-a" "src/a.hs" [0]) Conv.Viewer (Conv.Markdown "a"))
  Monad.void (WorkStore.addAside conn (scopeOf repo) (noteOnFile repo "ts-b" "src/b.hs" [1]) Conv.Viewer (Conv.Markdown "b"))
  WorkStore.retireNotesOn conn retention WorkStore.LinesAccepted repo [Store.LineKey "k0"]
  WorkStore.retireNotes conn retention repo [Conv.LocalId "ts-b"]
  before <- tombstones "ts-"
  WorkStore.forgetWork conn retention repo "" [filePath]
  after <- tombstones "ts-"
  pure
    [ Check (T.unpack name <> ": accepted and resolved notes leave tombstones") (List.sort before == [("ts-a", "accepted"), ("ts-b", "resolved")])
    , Check (T.unpack name <> ": forgetting the repository clears them") (null after)
    ]

retiredReasonName :: WorkStore.RetiredReason -> T.Text
retiredReasonName reason =
  case reason of
    WorkStore.LinesAccepted -> "accepted"
    WorkStore.NoteResolved -> "resolved"

exampleArchivingOffDeletes :: SQLite.Connection -> IO [Check]
exampleArchivingOffDeletes conn = do
  let
    repo = "/remax-test/off"
    off = NoteArchive.DeleteNotes
  recordLines conn repo
  legacyPost conn repo "g-off" "k4" 0 "reviewer" "ask"
  Monad.void (WorkStore.addAside conn (scopeOf repo) (noteOnFile repo "on-a" "src/a.hs" [0]) Conv.Viewer (Conv.Markdown "a"))
  Monad.void (WorkStore.addAside conn (scopeOf repo) (noteOnFile repo "on-b" "src/b.hs" [1]) Conv.Viewer (Conv.Markdown "b"))
  Monad.void (WorkStore.addAside conn (scopeOf repo) (noteOnFile repo "on-c" "src/a.hs" [2]) Conv.Viewer (Conv.Markdown "c"))
  WorkStore.retireNotesOn conn off WorkStore.LinesAccepted repo [Store.LineKey "k2"]
  _ <- Store.clearThread conn off repo ["k4"]
  WorkStore.retireNotes conn off repo [Conv.LocalId "on-b"]
  Monad.void (WorkStore.addAside conn (scopeOf repo) (noteOnFile repo "on-d" "src/a.hs" [3]) Conv.Viewer (Conv.Markdown "d"))
  Monad.void (WorkStore.addAside conn (scopeOf repo) (noteOnFile repo "on-e" "src/e.hs" [3]) Conv.Viewer (Conv.Markdown "e"))
  WorkStore.forgetWork conn off repo "src/a.hs" ["src/a.hs"]
  open <- openNotesRead conn repo
  archived <- archiveRows conn repo
  notes <- exportedIn conn repo NoteExport.AllNotes
  pure
    [ Check "off: nothing archived" (archived == 0)
    , Check "off: only the note outside the path is left, and it is open" (Map.keys open == ["on-e"] && Map.keys notes == ["on-e"])
    , Check "off: legacy posts deleted" (Map.null notes || all (\n -> NoteArchive.noteId n /= "g-off") (Map.elems notes))
    ]

exampleLegacyNotesAreRetiredNotDeleted :: SQLite.Connection -> IO [Check]
exampleLegacyNotesAreRetiredNotDeleted conn = do
  let
    repo = "/remax-test/legacy"
    thread group keys = Monad.forM_ keys $ \key -> do
      legacyPost conn repo group key 0 "reviewer" ("ask " <> group)
      legacyPost conn repo group key 1 "agent" ("answer " <> group)
  recordLines conn repo
  thread "g-clear" ["k0"]
  thread "g-unmark" ["k1", "k2"]
  thread "g-accept" ["k3"]
  thread "g-open" ["k4"]
  cleared <- Store.clearThread conn archiving repo ["k0"]
  Store.deleteKeys conn archiving repo ["k1", "k2"]
  passId <- Store.beginPass conn repo "test"
  Store.recordVerdicts conn archiving repo passId Store.Reviewed Store.noOrigin [("k3", filePath, lineText 3, 4)]
  threads <- Store.loadThreads conn repo
  groups <- Store.loadNoteGroups conn repo
  everything <- exportedIn conn repo NoteExport.AllNotes
  let
    stateOf group = fmap isRetired (Map.lookup group everything)
    postsOf group = fmap (map NoteArchive.postBody . NoteArchive.notePosts) (Map.lookup group everything)
    fileOf group = Map.lookup group everything >>= NoteArchive.noteFile
  WorkStore.forgetWork conn archiving repo "" [filePath]
  afterForget <- exportedIn conn repo NoteExport.AllNotes
  threadsAfterForget <- Store.loadThreads conn repo
  pure
    [ Check "legacy: forget retires the open thread" (fmap isRetired (Map.lookup "g-open" afterForget) == Just True && Map.null threadsAfterForget)
    , Check "legacy: forget keeps where it was" ((Map.lookup "g-open" afterForget >>= NoteArchive.noteFile) == Just filePath)
    , Check "legacy: clearThread counts the line" (cleared == 1)
    , Check "legacy: only the open thread is read" (Map.keys threads == ["k4"])
    , Check "legacy: only the open group is read" (Map.keys groups == ["k4"])
    , Check "legacy: states" (map stateOf ["g-clear", "g-unmark", "g-accept", "g-open"] == map Just [True, True, True, False])
    , Check "legacy: posts kept" (postsOf "g-clear" == Just ["ask g-clear", "answer g-clear"])
    , Check "legacy: file kept after verdicts are deleted" (fileOf "g-unmark" == Just filePath)
    , Check "legacy: lines kept after verdicts are deleted" (fmap (length . NoteArchive.noteLines) (Map.lookup "g-unmark" everything) == Just 2)
    ]

noteOnFile :: T.Text -> T.Text -> T.Text -> [Int] -> Conv.Target
noteOnFile repo name path ls =
  case noteTarget repo 0 ls of
    Conv.OnLinesOnly _ lineSpan verdict -> Conv.OnLinesOnly (Conv.LocalId name) lineSpan {Conv.spanPath = path} verdict
    other -> other

legacyPostAt :: SQLite.Connection -> T.Text -> T.Text -> T.Text -> Int -> T.Text -> T.Text -> T.Text -> IO ()
legacyPostAt conn repo group key seq_ author body at =
  SQLite.execute
    conn
    "INSERT INTO note_post (repo, key, seq, author, body, group_id, created_at) VALUES (?, ?, ?, ?, ?, ?, ?)"
    (repo, key, seq_, author, body, group, at)

repliedLegacyGroup :: SQLite.Connection -> T.Text -> T.Text -> IO ()
repliedLegacyGroup conn repo group = do
  legacyPostAt conn repo group "k1" 0 "reviewer" "ask" "2026-01-01 10:00:00"
  legacyPostAt conn repo group "k2" 0 "reviewer" "ask" "2026-01-01 10:00:00"
  legacyPostAt conn repo group "k1" 1 "agent" "legacy answer" "2026-01-01 10:00:04"
  legacyPostAt conn repo group "k2" 1 "agent" "legacy answer" "2026-01-01 10:00:04"
  Monad.void
    ( WorkStore.addAside
        conn
        (scopeOf repo)
        (noteOnFile repo group filePath [1, 2])
        (Conv.Agent (Conv.AgentName "claude"))
        (Conv.Markdown "reply")
    )
  SQLite.execute conn "UPDATE work SET created_at = '2026-01-01 10:00:02' WHERE repo = ? AND target_id = ?" (repo, group)

exampleRepliedLegacyGroupIsOneNote :: SQLite.Connection -> IO [Check]
exampleRepliedLegacyGroupIsOneNote conn = do
  let
    repo = "/remax-test/replied"
  recordLines conn repo
  repliedLegacyGroup conn repo "g-replied"
  notes <- exportedIn conn repo NoteExport.AllNotes
  pure
    [ Check "replied legacy: one note" (Map.keys notes == ["g-replied"])
    , Check
        "replied legacy: posts in time order"
        (fmap (map NoteArchive.postBody . NoteArchive.notePosts) (Map.lookup "g-replied" notes) == Just ["ask", "reply", "legacy answer"])
    ]

data OpenNote = OpenNote
  { openNoteId :: T.Text
  , openNoteLines :: [Maybe Int]
  , openNotePosts :: [T.Text]
  }
  deriving (Eq, Show)

openNotesOf :: SQLite.Connection -> T.Text -> IO [OpenNote]
openNotesOf conn repo = do
  notes <- NoteExport.exportNotes conn NoteExport.AllNotes
  pure
    [ OpenNote (NoteArchive.noteId n) (map NoteArchive.lineNumber (NoteArchive.noteLines n)) (map NoteArchive.postBody (NoteArchive.notePosts n))
    | n <- notes
    , NoteArchive.noteRepo n == repo
    , not (isRetired n)
    ]

archivedOf :: SQLite.Connection -> T.Text -> IO [NoteArchive.ExportedNote]
archivedOf conn repo = do
  notes <- NoteExport.exportNotes conn NoteExport.RetiredOnly
  pure (filter ((== repo) . NoteArchive.noteRepo) notes)

data LegacyUnmark = LegacyUnmark
  { unmarkName :: String
  , unmarkRun :: SQLite.Connection -> NoteArchive.Retention -> T.Text -> IO ()
  }

legacyUnmarks :: [LegacyUnmark]
legacyUnmarks =
  [ LegacyUnmark "deleteKeys" (\conn retention repo -> Store.deleteKeys conn retention repo ["k1"])
  , LegacyUnmark "clearThread" (\conn retention repo -> Monad.void (Store.clearThread conn retention repo ["k1"]))
  , LegacyUnmark
      "recordVerdicts"
      ( \conn retention repo -> do
          passId <- Store.beginPass conn repo "test"
          Store.recordVerdicts conn retention repo passId Store.Reviewed Store.noOrigin [("k1", filePath, lineText 1, 2)]
      )
  , LegacyUnmark
      "accept the lines"
      (\conn retention repo -> WorkStore.retireNotesOn conn retention WorkStore.LinesAccepted repo [Store.LineKey "k1"])
  ]

exampleLegacyPartialUnmarkKeepsTheNoteOnItsOtherLines :: SQLite.Connection -> IO [Check]
exampleLegacyPartialUnmarkKeepsTheNoteOnItsOtherLines conn =
  fmap concat . Monad.forM legacyUnmarks $ \unmark -> do
    let
      run retention label = do
        let
          repo = "/remax-test/legacy-partial-" <> T.pack (unmarkName unmark) <> label
        recordLines conn repo
        repliedLegacyGroup conn repo ("g-" <> T.pack (unmarkName unmark) <> label)
        unmarkRun unmark conn retention repo
        open <- openNotesOf conn repo
        archived <- archivedOf conn repo
        pure (map (\n -> n {openNoteId = T.dropEnd (T.length label) (openNoteId n)}) open, archived)
    (deleted, _) <- run NoteArchive.DeleteNotes "-delete"
    (open, archived) <- run archiving "-archive"
    pure
      [ Check (unmarkName unmark <> ": archive mode leaves the same notes open as delete mode") (open == deleted)
      , Check (unmarkName unmark <> ": the note stays open on the line still marked") (map openNoteId open == ["g-" <> T.pack (unmarkName unmark)])
      , Check (unmarkName unmark <> ": what left is archived") (not (null archived))
      ]

exampleRepliedLegacyGroupIsArchivedOnce :: SQLite.Connection -> IO [Check]
exampleRepliedLegacyGroupIsArchivedOnce conn = do
  let
    repo = "/remax-test/replied-archive"
  recordLines conn repo
  repliedLegacyGroup conn repo "g-once"
  passId <- Store.beginPass conn repo "test"
  Store.recordVerdicts conn archiving repo passId Store.Reviewed Store.noOrigin [("k1", filePath, lineText 1, 2), ("k2", filePath, lineText 2, 3)]
  WorkStore.retireNotesOn conn archiving WorkStore.LinesAccepted repo [Store.LineKey "k1", Store.LineKey "k2"]
  archived <- archivedOf conn repo
  open <- openNotesOf conn repo
  pure
    [ Check "replied legacy archive: one entry" (map NoteArchive.noteId archived == ["g-once"])
    , Check
        "replied legacy archive: every post in it"
        (fmap (map NoteArchive.postBody . NoteArchive.notePosts) (Maybe.listToMaybe archived) == Just ["ask", "reply", "legacy answer"])
    , Check "replied legacy archive: nothing left open" (null open)
    ]

exampleLegacyGroupIsArchivedOnceAcrossRetirements :: SQLite.Connection -> IO [Check]
exampleLegacyGroupIsArchivedOnceAcrossRetirements conn = do
  let
    repo = "/remax-test/legacy-once"
    bigRepo = "/remax-test/legacy-big"
    bigKeys = ["c" <> T.pack (show i) | i <- [0 .. 449 :: Int]]
  recordLines conn repo
  Monad.forM_ ["k0", "k1", "k2"] $ \key -> do
    legacyPost conn repo "g-three" key 0 "reviewer" "ask"
    legacyPost conn repo "g-three" key 1 "agent" "answer"
  passId <- Store.beginPass conn repo "test"
  Store.recordVerdicts conn archiving repo passId Store.Reviewed Store.noOrigin [("k0", filePath, lineText 0, 1)]
  _ <- Store.clearThread conn archiving repo ["k1"]
  Store.deleteKeys conn archiving repo ["k2"]
  threeArchived <- archivedOf conn repo
  bigPass <- Store.beginPass conn bigRepo "test"
  Store.recordVerdicts conn archiving bigRepo bigPass Store.Flagged Store.noOrigin [(key, filePath, key, i) | (i, key) <- zip [1 ..] bigKeys]
  Monad.forM_ bigKeys $ \key -> legacyPost conn bigRepo "g-big" key 0 "reviewer" "ask"
  Store.deleteKeys conn archiving bigRepo bigKeys
  bigArchived <- archivedOf conn bigRepo
  pure
    [ Check "legacy group retired in steps: one entry" (map NoteArchive.noteId threeArchived == ["g-three"])
    , Check "legacy group retired in steps: every line" (fmap (length . NoteArchive.noteLines) (Maybe.listToMaybe threeArchived) == Just 3)
    , Check "legacy group retired in steps: each post once" (fmap (map NoteArchive.postBody . NoteArchive.notePosts) (Maybe.listToMaybe threeArchived) == Just ["ask", "answer"])
    , Check "legacy group across chunks: one entry" (map NoteArchive.noteId bigArchived == ["g-big"])
    , Check "legacy group across chunks: every line" (fmap (length . NoteArchive.noteLines) (Maybe.listToMaybe bigArchived) == Just 450)
    ]

exampleExportSeesEachNoteOnceWhileNotesRetire :: SQLite.Connection -> IO [Check]
exampleExportSeesEachNoteOnceWhileNotesRetire conn = do
  let
    repo = "/remax-test/export-race"
    names = ["race" <> T.pack (show i) | i <- [0 .. 149 :: Int]]
    countsIn notes = Map.fromListWith (+) [(NoteArchive.noteId n, 1 :: Int) | n <- notes, NoteArchive.noteRepo n == repo]
    whole counts = Map.size counts == length names && all (== 1) (Map.elems counts) && all (`Map.member` counts) names
  recordLines conn repo
  Monad.forM_ names $ \name ->
    Monad.void (WorkStore.addAside conn (scopeOf repo) (noteOnFile repo name filePath [0]) Conv.Viewer (Conv.Markdown name))
  writer <- Store.openStore
  finished <- Concurrent.newEmptyMVar
  _ <-
    Concurrent.forkIO $ do
      Monad.forM_ names $ \name -> WorkStore.retireNotes writer archiving repo [Conv.LocalId name]
      Concurrent.putMVar finished ()
  let
    watch torn = do
      done <- Concurrent.tryReadMVar finished
      notes <- NoteExport.exportNotes conn NoteExport.AllNotes
      let
        torn' = torn + (if whole (countsIn notes) then 0 else 1 :: Int)
      case done of
        Just () -> pure torn'
        Nothing -> watch torn'
  torn <- watch 0
  SQLite.close writer
  pure [Check ("export while notes retire: every export had each note once, " <> show torn <> " did not") (torn == 0)]

exampleReadOnlyOpenTakesNoWriteLock :: IO [Check]
exampleReadOnlyOpenTakesNoWriteLock = do
  stateHome <- Env.lookupEnv "XDG_STATE_HOME"
  dir <- maybe (pure "") pure stateHome
  let
    lockedDir = dir </> "locked"
  Directory.createDirectoryIfMissing True (lockedDir </> "remax")
  Env.setEnv "XDG_STATE_HOME" lockedDir
  old <- SQLite.open (lockedDir </> "remax" </> "review.db")
  mapM_
    (SQLite.execute_ old)
    [ "CREATE TABLE note_post (repo TEXT NOT NULL, key TEXT NOT NULL, seq INTEGER NOT NULL, author TEXT NOT NULL, body TEXT NOT NULL, created_at TEXT NOT NULL DEFAULT (datetime('now')), group_id TEXT, retired_at TEXT, file TEXT, line TEXT, line_no INTEGER, ref TEXT, commit_sha TEXT, PRIMARY KEY (repo, key, seq))"
    , "CREATE TABLE work_target (repo TEXT NOT NULL, local_id TEXT PRIMARY KEY, kind TEXT NOT NULL CHECK (kind IN ('new','note')), anchor TEXT NOT NULL CHECK (anchor IN ('lines','pull')), path TEXT NOT NULL, side TEXT NOT NULL, commit_sha TEXT, verdict TEXT, created_at TEXT NOT NULL DEFAULT (datetime('now')), retired_at TEXT, ref TEXT)"
    ]
  SQLite.close old
  conn <- Store.openStore
  holder <- Store.openStore
  SQLite.execute_ holder "BEGIN IMMEDIATE"
  opened <- Exception.try (NoteArchive.absorbRetiredRows conn)
  SQLite.execute_ holder "ROLLBACK"
  SQLite.close holder
  SQLite.close conn
  Env.setEnv "XDG_STATE_HOME" dir
  pure [Check "migrated store: looking for retired rows takes no write lock" (either (\e -> const False (e :: Exception.SomeException)) (const True) opened)]

exampleResolveAllRetiresAnsweredNotes :: SQLite.Connection -> NoteArchive.Retention -> T.Text -> IO [Check]
exampleResolveAllRetiresAnsweredNotes conn retention name = do
  let
    repo = "/remax-test/resolve-all-" <> name
    claude = Conv.Agent (Conv.AgentName "claude")
    say note author = Monad.void (WorkStore.addAside conn (scopeOf repo) (noteOnFile repo (note <> "-" <> name) filePath [0]) author (Conv.Markdown (note <> " " <> Conv.authorText author)))
  recordLines conn repo
  say "answered" Conv.Viewer
  say "answered" claude
  say "waiting" Conv.Viewer
  say "replied-to" Conv.Viewer
  say "replied-to" claude
  say "replied-to" Conv.Viewer
  resolved <- WorkStore.resolveAnsweredNotes conn retention repo
  open <- openNotesRead conn repo
  retired <- archivedOf conn repo
  tombstones <- fmap (map (\r -> (WorkStore.retiredId r, retiredReasonName (WorkStore.retiredReason r)))) (WorkStore.retiredNotes conn repo "")
  let
    named note = note <> "-" <> name
    expectedRetired = if retention == NoteArchive.ArchiveNotes then [named "answered"] else []
  pure
    [ Check (T.unpack name <> " resolve all: counts the answered note") (resolved == 1)
    , Check (T.unpack name <> " resolve all: only unanswered notes stay") (List.sort (Map.keys open) == [named "replied-to", named "waiting"])
    , Check (T.unpack name <> " resolve all: archive follows the setting") (List.sort (map NoteArchive.noteId retired) == expectedRetired)
    , Check (T.unpack name <> " resolve all: tombstone says resolved") (tombstones == [(named "answered", "resolved")])
    ]

exampleUnnoteResolvesAndLeavesNoLegacyPosts :: SQLite.Connection -> IO [Check]
exampleUnnoteResolvesAndLeavesNoLegacyPosts conn = do
  let
    repo = "/remax-test/unnote"
  recordLines conn repo
  repliedLegacyGroup conn repo "g-unnote"
  WorkStore.retireNotesOn conn archiving WorkStore.NoteResolved repo [Store.LineKey "k1", Store.LineKey "k2"]
  tombstones <- fmap (map (\r -> (WorkStore.retiredId r, retiredReasonName (WorkStore.retiredReason r)))) (WorkStore.retiredNotes conn repo "g-")
  threads <- Store.loadThreads conn repo
  open <- openNotesOf conn repo
  pure
    [ Check "unnote: tombstone says resolved" (tombstones == [("g-unnote", "resolved")])
    , Check "unnote: no legacy posts stay live" (Map.null threads && null open)
    ]

exampleLegacyAuthorsAreNormalised :: SQLite.Connection -> IO [Check]
exampleLegacyAuthorsAreNormalised conn = do
  let
    repo = "/remax-test/authors"
  recordLines conn repo
  legacyPost conn repo "g-authors" "k1" 0 "reviewer" "ask"
  legacyPost conn repo "g-authors" "k1" 1 "claude" "answer"
  before <- exportedIn conn repo NoteExport.AllNotes
  Store.deleteKeys conn archiving repo ["k1"]
  after <- exportedIn conn repo NoteExport.RetiredOnly
  let
    authorsIn = fmap (map NoteArchive.postAuthor . NoteArchive.notePosts) . Map.lookup "g-authors"
  pure
    [ Check "authors: open export" (authorsIn before == Just ["viewer", "agent:claude"])
    , Check "authors: archive" (authorsIn after == Just ["viewer", "agent:claude"])
    ]

draftOn :: T.Text -> T.Text -> [Int] -> Conv.Target
draftOn name path ls =
  case noteOnFile "" name path ls of
    Conv.OnLinesOnly lid lineSpan _ -> Conv.NewThread lid (Conv.OnLines lineSpan)
    other -> other

exampleScopedForgetKeepsDraftsOutsideThePath :: SQLite.Connection -> IO [Check]
exampleScopedForgetKeepsDraftsOutsideThePath conn = do
  let
    repo = "/remax-test/scoped-drafts"
    draftIds = fmap (map SQLite.fromOnly) (SQLite.query conn "SELECT local_id FROM work_target WHERE repo = ? AND kind = 'new' ORDER BY local_id" (SQLite.Only repo)) :: IO [T.Text]
    draftWork = fmap (map SQLite.fromOnly) (SQLite.query conn "SELECT target_id FROM work WHERE repo = ? AND target_kind = 'new' ORDER BY target_id" (SQLite.Only repo)) :: IO [T.Text]
    draftLines = fmap (map SQLite.fromOnly) (SQLite.query conn "SELECT DISTINCT local_id FROM work_target_line WHERE repo = ? ORDER BY local_id" (SQLite.Only repo)) :: IO [T.Text]
    submitted = fmap (map SQLite.fromOnly) (SQLite.query conn "SELECT local_id FROM work_submitted WHERE repo = ? ORDER BY local_id" (SQLite.Only repo)) :: IO [T.Text]
  recordLines conn repo
  Monad.forM_ [("d-a", "src/a.hs"), ("d-ab", "src/ab.hs"), ("d-b", "src/b/c.hs")] $ \(name, path) -> do
    Monad.void (WorkStore.addPending conn (scopeOf repo) (draftOn name path [0]) Conv.Viewer (Conv.Markdown name))
    WorkStore.recordSubmitted conn repo (Conv.LocalId name) (Store.CommentId 1)
  WorkStore.forgetWork conn archiving repo "src/a.hs" ["src/a.hs"]
  scopedIds <- draftIds
  scopedWork <- draftWork
  scopedLines <- draftLines
  scopedSubmitted <- submitted
  WorkStore.forgetWork conn archiving repo "" [filePath]
  everywhere <- draftIds
  pure
    [ Check "scoped forget: drafts outside the path stay" (scopedIds == ["d-ab", "d-b"])
    , Check "scoped forget: their work stays" (scopedWork == ["d-ab", "d-b"])
    , Check "scoped forget: their lines stay" (scopedLines == ["d-ab", "d-b"])
    , Check "scoped forget: their submissions stay" (scopedSubmitted == ["d-ab", "d-b"])
    , Check "unscoped forget: every draft goes" (null everywhere)
    ]

exampleScopedForgetRetiresOnlyNotesUnderThePath :: SQLite.Connection -> IO [Check]
exampleScopedForgetRetiresOnlyNotesUnderThePath conn = do
  let
    repo = "/remax-test/scoped"
  recordLines conn repo
  passId <- Store.beginPass conn repo "test"
  Store.recordVerdicts conn archiving repo passId Store.Flagged Store.noOrigin [("kb", "src/b.hs", "+b", 1)]
  Monad.void (WorkStore.addAside conn (scopeOf repo) (noteOnFile repo "on-a" "src/a.hs" [0]) Conv.Viewer (Conv.Markdown "a"))
  Monad.void (WorkStore.addAside conn (scopeOf repo) (noteOnFile repo "on-ab" "src/b.hs" [1]) Conv.Viewer (Conv.Markdown "b"))
  WorkStore.forgetWork conn archiving repo "src/a.hs" ["src/a.hs"]
  open <- openNotesRead conn repo
  notes <- exportedIn conn repo NoteExport.AllNotes
  pure
    [ Check "scoped forget: note outside the path stays open" (Map.keys open == ["on-ab"])
    , Check "scoped forget: note under the path retired" (fmap isRetired (Map.lookup "on-a" notes) == Just True)
    ]

exampleExportOrdersByStartAndPostsLast :: SQLite.Connection -> IO [Check]
exampleExportOrdersByStartAndPostsLast conn = do
  let
    repo = "/remax-test/order"
    started name at = SQLite.execute conn "UPDATE work_target SET created_at = ? WHERE repo = ? AND local_id = ?" (at :: T.Text, repo, name :: T.Text)
  recordLines conn repo
  Monad.void (WorkStore.addAside conn (scopeOf repo) (noteOnFile repo "late" filePath [0]) Conv.Viewer (Conv.Markdown "late"))
  Monad.void (WorkStore.addAside conn (scopeOf repo) (noteOnFile repo "early" filePath [1]) Conv.Viewer (Conv.Markdown "early"))
  Monad.void (WorkStore.addAside conn (scopeOf repo) (noteOnFile repo "bare" filePath [2]) Conv.Viewer (Conv.Markdown "bare"))
  SQLite.execute conn "DELETE FROM work WHERE repo = ? AND target_id = 'bare'" (SQLite.Only repo)
  started "late" "2026-01-03 00:00:00"
  started "early" "2026-01-02 00:00:00"
  started "bare" "2026-01-01 00:00:00"
  notes <- NoteExport.exportNotes conn NoteExport.AllNotes
  pure [Check "export order: by start, notes without posts last" (map NoteArchive.noteId (filter ((== repo) . NoteArchive.noteRepo) notes) == ["early", "late", "bare"])]

exampleBranchStoreOpens :: IO [Check]
exampleBranchStoreOpens = do
  stateHome <- Env.lookupEnv "XDG_STATE_HOME"
  dir <- maybe (pure "") pure stateHome
  let
    branchDir = dir </> "branch"
    repo = "/branch"
  Directory.createDirectoryIfMissing True (branchDir </> "remax")
  Env.setEnv "XDG_STATE_HOME" branchDir
  old <- SQLite.open (branchDir </> "remax" </> "review.db")
  mapM_
    (SQLite.execute_ old)
    [ "CREATE TABLE note_post (repo TEXT NOT NULL, key TEXT NOT NULL, seq INTEGER NOT NULL, author TEXT NOT NULL, body TEXT NOT NULL, created_at TEXT NOT NULL DEFAULT (datetime('now')), group_id TEXT, retired_at TEXT, file TEXT, line TEXT, line_no INTEGER, ref TEXT, commit_sha TEXT, PRIMARY KEY (repo, key, seq))"
    , "CREATE TABLE work_target (repo TEXT NOT NULL, local_id TEXT PRIMARY KEY, kind TEXT NOT NULL CHECK (kind IN ('new','note')), anchor TEXT NOT NULL CHECK (anchor IN ('lines','pull')), path TEXT NOT NULL, side TEXT NOT NULL, commit_sha TEXT, verdict TEXT, created_at TEXT NOT NULL DEFAULT (datetime('now')), retired_at TEXT, ref TEXT)"
    , "CREATE TABLE work_target_line (repo TEXT NOT NULL, local_id TEXT NOT NULL, seq INTEGER NOT NULL, key TEXT NOT NULL, line_no INTEGER NOT NULL, line TEXT NOT NULL, PRIMARY KEY (local_id, seq))"
    , "CREATE TABLE work (id INTEGER PRIMARY KEY AUTOINCREMENT, repo TEXT NOT NULL, pull INTEGER, target_kind TEXT NOT NULL CHECK (target_kind IN ('thread','comment','new','note')), target_id TEXT NOT NULL, kind TEXT NOT NULL CHECK (kind IN ('proposed','pending','aside','edit')), author TEXT NOT NULL, body TEXT NOT NULL, comment_id INTEGER, created_at TEXT NOT NULL DEFAULT (datetime('now')))"
    , "INSERT INTO work_target (repo, local_id, kind, anchor, path, side, ref, retired_at) VALUES ('/branch', 'gone', 'note', 'lines', 'a.hs', 'head', 'feature', '2026-01-05 00:00:00')"
    , "INSERT INTO work_target (repo, local_id, kind, anchor, path, side) VALUES ('/branch', 'kept', 'note', 'lines', 'a.hs', 'head')"
    , "INSERT INTO work_target_line (repo, local_id, seq, key, line_no, line) VALUES ('/branch', 'gone', 0, 'kg', 3, '+gone')"
    , "INSERT INTO work_target_line (repo, local_id, seq, key, line_no, line) VALUES ('/branch', 'kept', 0, 'kk', 4, '+kept')"
    , "INSERT INTO work (repo, target_kind, target_id, kind, author, body) VALUES ('/branch', 'note', 'gone', 'aside', 'viewer', 'why')"
    , "INSERT INTO work (repo, target_kind, target_id, kind, author, body) VALUES ('/branch', 'note', 'kept', 'aside', 'viewer', 'still open')"
    , "INSERT INTO note_post (repo, key, seq, author, body, group_id, retired_at, file, line, line_no, ref) VALUES ('/branch', 'l2', 0, 'reviewer', 'ask', 'lg', '2026-01-06 00:00:00', 'b.hs', '+two', 2, 'feature')"
    , "INSERT INTO note_post (repo, key, seq, author, body, group_id, retired_at, file, line, line_no, ref) VALUES ('/branch', 'l1', 0, 'reviewer', 'ask', 'lg', '2026-01-06 00:00:00', 'b.hs', '+one', 1, 'feature')"
    , "INSERT INTO note_post (repo, key, seq, author, body, group_id, retired_at, file, line, line_no, ref) VALUES ('/branch', 'l1', 1, 'claude', 'answer', 'lg', '2026-01-06 00:00:00', 'b.hs', '+one', 1, 'feature')"
    ]
  SQLite.close old
  opened <- Exception.try (Store.openStore >>= \conn -> SQLite.close conn)
  reopened <- Exception.try (Store.openStore >>= \conn -> SQLite.close conn)
  conn <- Store.openStore
  open <- openNotesRead conn repo
  retired <- exportedIn conn repo NoteExport.RetiredOnly
  loadedPosts <- Store.loadThreads conn repo
  strays <- SQLite.query conn "SELECT COUNT(*) FROM work_target WHERE repo = ? AND local_id = 'gone'" (SQLite.Only repo) :: IO [SQLite.Only Int]
  archivedRows' <- archiveRows conn repo
  SQLite.close conn
  Env.setEnv "XDG_STATE_HOME" dir
  let
    succeeded outcome = either (\e -> const False (e :: Exception.SomeException)) (const True) outcome
    summaryOf name = fmap (\n -> (NoteArchive.noteState n, map NoteArchive.lineNumber (NoteArchive.noteLines n), map NoteArchive.postAuthor (NoteArchive.notePosts n), map NoteArchive.postBody (NoteArchive.notePosts n), NoteArchive.noteFile n, NoteArchive.noteRef n)) (Map.lookup name retired)
  pure
    [ Check "branch store: opens" (succeeded opened && succeeded reopened)
    , Check "branch store: the retired note is not open" (Map.keys open == ["kept"])
    , Check "branch store: the retired note moved to the archive with its time" (summaryOf "gone" == Just (NoteArchive.Retired "2026-01-05T00:00:00Z", [Just 3], ["viewer"], ["why"], Just "a.hs", Just "feature"))
    , Check "branch store: retired legacy posts moved to the archive" (summaryOf "lg" == Just (NoteArchive.Retired "2026-01-06T00:00:00Z", [Just 1, Just 2], ["viewer", "agent:claude"], ["ask", "answer"], Just "b.hs", Just "feature"))
    , Check "branch store: nothing retired is left in the live tables" (map SQLite.fromOnly strays == [0] && Map.null loadedPosts)
    , Check "branch store: reopening archives nothing twice" (archivedRows' == 2)
    ]

exampleRetireWaitsForAConcurrentWriter :: SQLite.Connection -> IO [Check]
exampleRetireWaitsForAConcurrentWriter conn = do
  let
    repo = "/remax-test/concurrent"
  recordLines conn repo
  Monad.void (WorkStore.addAside conn (scopeOf repo) (noteOnFile repo "busy" filePath [0]) Conv.Viewer (Conv.Markdown "first"))
  other <- Store.openStore
  SQLite.execute_ other "BEGIN IMMEDIATE"
  SQLite.execute other "INSERT INTO work (repo, target_kind, target_id, kind, author, body) VALUES (?, 'note', 'busy', 'aside', 'agent:claude', 'second')" (SQLite.Only repo)
  finished <- Concurrent.newEmptyMVar
  _ <- Concurrent.forkIO (Exception.try (WorkStore.retireNotesOn conn archiving WorkStore.LinesAccepted repo [Store.LineKey (lineKey 0)]) >>= Concurrent.putMVar finished)
  Concurrent.threadDelay 300000
  SQLite.execute_ other "COMMIT"
  outcome <- Concurrent.takeMVar finished
  SQLite.close other
  archived <- archivedOf conn repo
  pure
    [ Check "concurrent writer: retiring waits for it" (either (\e -> const False (e :: Exception.SomeException)) (const True) outcome)
    , Check "concurrent writer: the archived note has the reply written meanwhile" (map (map NoteArchive.postBody . NoteArchive.notePosts) archived == [["first", "second"]])
    ]

exampleOldStoreMigrates :: IO [Check]
exampleOldStoreMigrates = do
  stateHome <- Env.lookupEnv "XDG_STATE_HOME"
  dir <- maybe (pure "") pure stateHome
  let
    oldDir = dir </> "old"
  Directory.createDirectoryIfMissing True (oldDir </> "remax")
  Env.setEnv "XDG_STATE_HOME" oldDir
  old <- SQLite.open (oldDir </> "remax" </> "review.db")
  mapM_
    (SQLite.execute_ old)
    [ "CREATE TABLE note_post (repo TEXT NOT NULL, key TEXT NOT NULL, seq INTEGER NOT NULL, author TEXT NOT NULL, body TEXT NOT NULL, created_at TEXT NOT NULL DEFAULT (datetime('now')), group_id TEXT, PRIMARY KEY (repo, key, seq))"
    , "CREATE TABLE work_target (repo TEXT NOT NULL, local_id TEXT PRIMARY KEY, kind TEXT NOT NULL CHECK (kind IN ('new','note')), anchor TEXT NOT NULL CHECK (anchor IN ('lines','pull')), path TEXT NOT NULL, side TEXT NOT NULL, commit_sha TEXT, verdict TEXT, created_at TEXT NOT NULL DEFAULT (datetime('now')))"
    , "CREATE TABLE work_target_line (repo TEXT NOT NULL, local_id TEXT NOT NULL, seq INTEGER NOT NULL, key TEXT NOT NULL, line_no INTEGER NOT NULL, line TEXT NOT NULL, PRIMARY KEY (local_id, seq))"
    , "CREATE TABLE work (id INTEGER PRIMARY KEY AUTOINCREMENT, repo TEXT NOT NULL, pull INTEGER, target_kind TEXT NOT NULL CHECK (target_kind IN ('thread','comment','new','note')), target_id TEXT NOT NULL, kind TEXT NOT NULL CHECK (kind IN ('proposed','pending','aside','edit')), author TEXT NOT NULL, body TEXT NOT NULL, comment_id INTEGER, created_at TEXT NOT NULL DEFAULT (datetime('now')))"
    , "INSERT INTO work_target (repo, local_id, kind, anchor, path, side, verdict) VALUES ('/old', 'n1', 'note', 'lines', 'a.hs', 'head', 'flagged')"
    , "INSERT INTO work_target_line (repo, local_id, seq, key, line_no, line) VALUES ('/old', 'n1', 0, 'k', 3, '+x')"
    , "INSERT INTO work (repo, target_kind, target_id, kind, author, body) VALUES ('/old', 'note', 'n1', 'aside', 'viewer', 'old note')"
    ]
  SQLite.close old
  first <- Store.openStore
  SQLite.close first
  conn <- Store.openStore
  open <- openNotesRead conn "/old"
  everything <- exportedIn conn "/old" NoteExport.AllNotes
  WorkStore.retireNotesOn conn archiving WorkStore.LinesAccepted "/old" [Store.LineKey "k"]
  after <- openNotesRead conn "/old"
  retired <- exportedIn conn "/old" NoteExport.RetiredOnly
  SQLite.close conn
  Env.setEnv "XDG_STATE_HOME" dir
  pure
    [ Check "migration: old note still open" (open == Map.fromList [("n1", ["old note"])])
    , Check "migration: old note exported open" (fmap (map NoteArchive.postBody . NoteArchive.notePosts) (Map.lookup "n1" everything) == Just ["old note"])
    , Check "migration: old note retires" (Map.null after && Map.keys retired == ["n1"])
    ]

main :: IO ()
main = do
  pid <- Posix.getProcessID
  tmp <- Directory.getTemporaryDirectory
  let
    root = tmp </> ("remax-notes-test-" <> show pid)
    state = root </> "state"
  Exception.bracket_
    (Directory.createDirectoryIfMissing True (state </> "remax") >> Env.setEnv "XDG_STATE_HOME" state)
    (Env.lookupEnv "REMAX_TEST_KEEP" >>= maybe (Directory.removeDirectoryRecursive root) (const (pure ())))
    $ do
      conn <- Store.openStore
      counter <- IORef.newIORef (0 :: Int)
      results <-
        mapM
          ( \retention ->
              QC.quickCheckWithResult
                QC.stdArgs {QC.maxSuccess = 300}
                (propRetiredNotesStayOutOfReadsAndInTheExport conn retention counter)
          )
          [NoteArchive.ArchiveNotes, NoteArchive.DeleteNotes]
      examples <-
        concat
          <$> sequence
            [ exampleContextSurvivesUnmarking conn
            , exampleForgetKeepsContext conn
            , exampleRetirementLeavesATombstone conn NoteArchive.ArchiveNotes "archive"
            , exampleRetirementLeavesATombstone conn NoteArchive.DeleteNotes "delete"
            , exampleArchivingOffDeletes conn
            , exampleLegacyNotesAreRetiredNotDeleted conn
            , exampleRepliedLegacyGroupIsOneNote conn
            , exampleLegacyPartialUnmarkKeepsTheNoteOnItsOtherLines conn
            , exampleRepliedLegacyGroupIsArchivedOnce conn
            , exampleLegacyGroupIsArchivedOnceAcrossRetirements conn
            , exampleExportSeesEachNoteOnceWhileNotesRetire conn
            , exampleResolveAllRetiresAnsweredNotes conn NoteArchive.ArchiveNotes "archive"
            , exampleResolveAllRetiresAnsweredNotes conn NoteArchive.DeleteNotes "delete"
            , exampleUnnoteResolvesAndLeavesNoLegacyPosts conn
            , exampleLegacyAuthorsAreNormalised conn
            , exampleScopedForgetKeepsDraftsOutsideThePath conn
            , exampleRetireWaitsForAConcurrentWriter conn
            , exampleScopedForgetRetiresOnlyNotesUnderThePath conn
            , exampleExportOrdersByStartAndPostsLast conn
            , exampleOldStoreMigrates
            , exampleBranchStoreOpens
            , exampleReadOnlyOpenTakesNoWriteLock
            ]
      SQLite.close conn
      let
        failures = [name | Check name ok <- examples, not ok]
      Monad.forM_ failures (putStrLn . ("FAILED: " <>))
      Monad.unless (all QC.isSuccess results && null failures) Exit.exitFailure
      putStrLn (show (length examples) ++ " example checks passed")
