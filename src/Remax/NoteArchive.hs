{-# LANGUAGE OverloadedStrings #-}

module Remax.NoteArchive
  ( Retirement (..)
  , ExportedNote (..)
  , ExportedLine (..)
  , ExportedPost (..)
  , Reach (..)
  , Retention (..)
  , RetiredReason (..)
  , reasonText
  , reasonFromText
  , createSchema
  , retireOnKeys
  , retireIds
  , retireUnder
  , liveNotes
  , archivedNotes
  , absorbRetiredRows
  ) where

import Control.Monad qualified as Monad
import Data.Int qualified as DataInt
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Database.SQLite.Simple qualified as SQLite
import Remax.Author qualified as Author

data Retirement = Open | Retired T.Text
  deriving (Eq, Show)

data ExportedLine = ExportedLine
  { lineNumber :: Maybe Int
  , lineText :: T.Text
  }
  deriving (Eq, Show)

data ExportedPost = ExportedPost
  { postKind :: T.Text
  , postAuthor :: T.Text
  , postBody :: T.Text
  , postAt :: T.Text
  }
  deriving (Eq, Show)

data ExportedNote = ExportedNote
  { noteId :: T.Text
  , noteRepo :: T.Text
  , noteState :: Retirement
  , noteFile :: Maybe T.Text
  , noteSide :: Maybe T.Text
  , noteVerdict :: Maybe T.Text
  , noteRef :: Maybe T.Text
  , noteCommit :: Maybe T.Text
  , noteStarted :: T.Text
  , noteLines :: [ExportedLine]
  , notePosts :: [ExportedPost]
  }
  deriving (Eq, Show)

data RetiredReason = LinesAccepted | NoteResolved
  deriving (Eq, Show)

reasonText :: RetiredReason -> T.Text
reasonText reason =
  case reason of
    LinesAccepted -> "accepted"
    NoteResolved -> "resolved"

reasonFromText :: T.Text -> RetiredReason
reasonFromText name = if name == "accepted" then LinesAccepted else NoteResolved

data Reach = LegacyPostsOnly | EveryNote RetiredReason
  deriving (Eq, Show)

data Retention = DeleteNotes | ArchiveNotes
  deriving (Eq, Show)

createSchema :: SQLite.Connection -> IO ()
createSchema conn =
  mapM_
    (SQLite.execute_ conn)
    [ "CREATE TABLE IF NOT EXISTS note_archive (\
      \  archived_id INTEGER PRIMARY KEY AUTOINCREMENT,\
      \  repo TEXT NOT NULL,\
      \  note_id TEXT NOT NULL,\
      \  file TEXT,\
      \  side TEXT,\
      \  verdict TEXT,\
      \  ref TEXT,\
      \  commit_sha TEXT,\
      \  created_at TEXT NOT NULL,\
      \  retired_at TEXT NOT NULL DEFAULT (datetime('now')))"
    , "CREATE TABLE IF NOT EXISTS note_archive_line (\
      \  archived_id INTEGER NOT NULL REFERENCES note_archive(archived_id),\
      \  seq INTEGER NOT NULL,\
      \  line_no INTEGER,\
      \  line TEXT NOT NULL,\
      \  PRIMARY KEY (archived_id, seq))"
    , "CREATE TABLE IF NOT EXISTS note_archive_post (\
      \  archived_id INTEGER NOT NULL REFERENCES note_archive(archived_id),\
      \  seq INTEGER NOT NULL,\
      \  kind TEXT NOT NULL,\
      \  author TEXT NOT NULL,\
      \  body TEXT NOT NULL,\
      \  created_at TEXT NOT NULL,\
      \  PRIMARY KEY (archived_id, seq))"
    ]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs =
  case splitAt n xs of
    ([], _) -> []
    (chunk, rest) -> chunk : chunksOf n rest

holesFor :: [a] -> T.Text
holesFor xs = T.intercalate ", " (map (const "?") xs)

data TargetRow = TargetRow
  { trRepo :: T.Text
  , trLocalId :: T.Text
  , trPath :: T.Text
  , trSide :: T.Text
  , trVerdict :: Maybe T.Text
  , trCommit :: Maybe T.Text
  , trCreatedAt :: T.Text
  }

instance SQLite.FromRow TargetRow where
  fromRow = TargetRow <$> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field

data LineRow = LineRow
  { lrRepo :: T.Text
  , lrLocalId :: T.Text
  , lrKey :: T.Text
  , lrLineNo :: Int
  , lrText :: T.Text
  }

instance SQLite.FromRow LineRow where
  fromRow = LineRow <$> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field

data WorkRow = WorkRow
  { wrRepo :: T.Text
  , wrTargetId :: T.Text
  , wrKind :: T.Text
  , wrAuthor :: T.Text
  , wrBody :: T.Text
  , wrCreatedAt :: T.Text
  }

instance SQLite.FromRow WorkRow where
  fromRow = WorkRow <$> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field

data LegacyRow = LegacyRow
  { lgRepo :: T.Text
  , lgGroup :: T.Text
  , lgKey :: T.Text
  , lgAuthor :: T.Text
  , lgBody :: T.Text
  , lgCreatedAt :: T.Text
  }

instance SQLite.FromRow LegacyRow where
  fromRow = LegacyRow <$> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field

data Place = Place
  { plKey :: T.Text
  , plFile :: T.Text
  , plLine :: T.Text
  , plLineNo :: Maybe Int
  , plRef :: Maybe T.Text
  , plCommit :: Maybe T.Text
  }

instance SQLite.FromRow Place where
  fromRow = Place <$> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field

data DraftTarget = DraftTarget
  { dtPath :: T.Text
  , dtSide :: T.Text
  , dtVerdict :: Maybe T.Text
  , dtCommit :: Maybe T.Text
  , dtCreatedAt :: T.Text
  }

data DraftLine = DraftLine
  { dlKey :: T.Text
  , dlLineNo :: Int
  , dlText :: T.Text
  }

data DraftLegacyPost = DraftLegacyPost
  { dgKey :: T.Text
  , dgPost :: ExportedPost
  }

data Draft = Draft
  { draftTarget :: Maybe DraftTarget
  , draftLines :: [DraftLine]
  , draftWork :: [ExportedPost]
  , draftLegacy :: [DraftLegacyPost]
  }

instance Semigroup Draft where
  a <> b =
    Draft
      { draftTarget = Maybe.listToMaybe (Maybe.catMaybes [draftTarget a, draftTarget b])
      , draftLines = draftLines a ++ draftLines b
      , draftWork = draftWork a ++ draftWork b
      , draftLegacy = draftLegacy a ++ draftLegacy b
      }

instance Monoid Draft where
  mempty = Draft {draftTarget = Nothing, draftLines = [], draftWork = [], draftLegacy = []}

data NoteFilter
  = Everything (Maybe T.Text)
  | WholeNotes T.Text [T.Text]

data LegacyScope = AllLegacyRows | LegacyRowsOn (Set.Set T.Text)

data Source = TargetSource | LineSource | WorkSource | LegacySource

data Restriction = Restriction
  { rsConditions :: [T.Text]
  , rsParams :: [SQLite.SQLData]
  }

restrictionOn :: NoteFilter -> Source -> Maybe Restriction
restrictionOn noteFilter source =
  let
    idColumn = case source of
      TargetSource -> "local_id"
      LineSource -> "local_id"
      WorkSource -> "target_id"
      LegacySource -> "COALESCE(group_id, key)"
    within repo column values =
      Just
        Restriction
          { rsConditions = ["repo = ?", column <> " IN (" <> holesFor values <> ")"]
          , rsParams = SQLite.toRow (SQLite.Only repo) <> SQLite.toRow values
          }
  in
    case (noteFilter, source) of
      (Everything Nothing, _) -> Just Restriction {rsConditions = [], rsParams = []}
      (Everything (Just repo), _) -> Just Restriction {rsConditions = ["repo = ?"], rsParams = SQLite.toRow (SQLite.Only repo)}
      (WholeNotes repo ids, _) -> within repo idColumn ids

restrictedQuery :: SQLite.FromRow r => SQLite.Connection -> T.Text -> [T.Text] -> T.Text -> Maybe Restriction -> IO [r]
restrictedQuery conn select fixed order restriction =
  case restriction of
    Nothing -> pure []
    Just r ->
      let
        conditions = fixed ++ rsConditions r
        clause = if null conditions then "" else " WHERE " <> T.intercalate " AND " conditions
      in
        SQLite.query conn (SQLite.Query (select <> clause <> order)) (rsParams r)

liveNotes :: SQLite.Connection -> Maybe T.Text -> IO [ExportedNote]
liveNotes conn inRepo = loadNotes conn (Everything inRepo) AllLegacyRows

liveNotesNamed :: SQLite.Connection -> LegacyScope -> T.Text -> [T.Text] -> IO [ExportedNote]
liveNotesNamed conn scope repo names =
  fmap concat (mapM (\ids -> loadNotes conn (WholeNotes repo ids) scope) (chunksOf 400 names))

loadNotes :: SQLite.Connection -> NoteFilter -> LegacyScope -> IO [ExportedNote]
loadNotes conn noteFilter scope = do
  let
    inScope key = case scope of
      AllLegacyRows -> True
      LegacyRowsOn keys -> Set.member key keys
    restricted select fixed order source = restrictedQuery conn select fixed order (restrictionOn noteFilter source)
    placesOf ks =
      fmap
        (Map.fromList . map (\place -> (plKey place, place)))
        (SQLite.query conn (SQLite.Query ("SELECT key, file, line, line_no, ref, commit_sha FROM line_verdict WHERE key IN (" <> holesFor ks <> ")")) ks)
  targets <-
    restricted "SELECT repo, local_id, path, side, verdict, commit_sha, created_at FROM work_target" ["kind = 'note'"] "" TargetSource
  lineRows <-
    restricted "SELECT repo, local_id, key, line_no, line FROM work_target_line" [] " ORDER BY local_id, seq" LineSource
  workRows <-
    restricted "SELECT repo, target_id, kind, author, body, created_at FROM work" ["target_kind = 'note'"] " ORDER BY id" WorkSource
  legacyRows <-
    fmap
      (filter (inScope . lgKey))
      (restricted "SELECT repo, COALESCE(group_id, key), key, author, body, created_at FROM note_post" [] " ORDER BY seq, key" LegacySource)
  let
    targetIds = Set.fromList (map (\t -> (trRepo t, trLocalId t)) targets)
    targetDraft t =
      ( (trRepo t, trLocalId t)
      , mempty {draftTarget = Just DraftTarget {dtPath = trPath t, dtSide = trSide t, dtVerdict = trVerdict t, dtCommit = trCommit t, dtCreatedAt = trCreatedAt t}}
      )
    lineDraft l = ((lrRepo l, lrLocalId l), mempty {draftLines = [DraftLine {dlKey = lrKey l, dlLineNo = lrLineNo l, dlText = lrText l}]})
    workDraft w = ((wrRepo w, wrTargetId w), mempty {draftWork = [ExportedPost (wrKind w) (wrAuthor w) (wrBody w) (wrCreatedAt w)]})
    legacyDraft l =
      ( (lgRepo l, lgGroup l)
      , mempty {draftLegacy = [DraftLegacyPost {dgKey = lgKey l, dgPost = ExportedPost "aside" (Author.authorText (Author.legacyAuthor (lgAuthor l))) (lgBody l) (lgCreatedAt l)}]}
      )
    drafts =
      Map.fromListWith
        (flip (<>))
        ( map targetDraft targets
            ++ map lineDraft (filter (\l -> Set.member (lrRepo l, lrLocalId l) targetIds) lineRows)
            ++ map workDraft workRows
            ++ map legacyDraft legacyRows
        )
    wantedKeys = List.nub (concatMap keysOf (Map.elems drafts))
  places <- fmap Map.unions (mapM placesOf (chunksOf 400 wantedKeys))
  pure (Maybe.mapMaybe (\((repo, nid), draft) -> assemble places repo nid draft) (Map.toList drafts))

keysOf :: Draft -> [T.Text]
keysOf draft = map dlKey (draftLines draft) ++ map dgKey (draftLegacy draft)

assemble :: Map.Map T.Text Place -> T.Text -> T.Text -> Draft -> Maybe ExportedNote
assemble places repo nid draft =
  let
    placeOf key = Map.lookup key places
    lineNoOf key = placeOf key >>= plLineNo
    legacyKeys =
      List.sortOn
        (\k -> (Maybe.isNothing (lineNoOf k), Maybe.fromMaybe 0 (lineNoOf k), k))
        (List.nub (map dgKey (draftLegacy draft)))
    legacyLines =
      List.sortOn
        lineNumber
        (Maybe.mapMaybe (\k -> fmap (\p -> ExportedLine (plLineNo p) (plLine p)) (placeOf k)) legacyKeys)
    targetLines = map (\l -> ExportedLine (Just (dlLineNo l)) (dlText l)) (draftLines draft)
    contextKeys = if null targetLines then legacyKeys else map dlKey (draftLines draft)
    context = Maybe.mapMaybe placeOf contextKeys
    firstKey = Maybe.listToMaybe legacyKeys
    legacyPosts = map dgPost (filter (\p -> Just (dgKey p) == firstKey) (draftLegacy draft))
    posts = List.sortOn postAt (legacyPosts ++ draftWork draft)
    firstJust f = Maybe.listToMaybe (Maybe.mapMaybe f context)
    started = case draftTarget draft of
      Just target -> dtCreatedAt target
      Nothing -> maybe "" postAt (Maybe.listToMaybe posts)
  in
    if Maybe.isNothing (draftTarget draft) && null (draftLegacy draft)
      then Nothing
      else
        Just
          ExportedNote
            { noteId = nid
            , noteRepo = repo
            , noteState = Open
            , noteFile = case draftTarget draft of
                Just target -> Just (dtPath target)
                Nothing -> firstJust (Just . plFile)
            , noteSide = fmap dtSide (draftTarget draft)
            , noteVerdict = draftTarget draft >>= dtVerdict
            , noteRef = firstJust plRef
            , noteCommit = case draftTarget draft >>= dtCommit of
                Just sha -> Just sha
                Nothing -> firstJust plCommit
            , noteStarted = started
            , noteLines = if null targetLines then legacyLines else targetLines
            , notePosts = posts
            }

data ArchivedHead = ArchivedHead
  { ahId :: DataInt.Int64
  , ahRepo :: T.Text
  , ahNoteId :: T.Text
  , ahFile :: Maybe T.Text
  , ahSide :: Maybe T.Text
  , ahVerdict :: Maybe T.Text
  , ahRef :: Maybe T.Text
  , ahCommit :: Maybe T.Text
  , ahStarted :: T.Text
  , ahRetired :: T.Text
  }

instance SQLite.FromRow ArchivedHead where
  fromRow =
    ArchivedHead
      <$> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field

data ArchivedLine = ArchivedLine DataInt.Int64 ExportedLine

instance SQLite.FromRow ArchivedLine where
  fromRow = do
    archived <- SQLite.field
    number <- SQLite.field
    text <- SQLite.field
    pure (ArchivedLine archived (ExportedLine number text))

data ArchivedPost = ArchivedPost DataInt.Int64 ExportedPost

instance SQLite.FromRow ArchivedPost where
  fromRow = do
    archived <- SQLite.field
    kind <- SQLite.field
    author <- SQLite.field
    body <- SQLite.field
    at <- SQLite.field
    pure (ArchivedPost archived (ExportedPost kind author body at))

archivedNotes :: SQLite.Connection -> IO [ExportedNote]
archivedNotes conn = do
  heads <-
    SQLite.query_
      conn
      "SELECT archived_id, repo, note_id, file, side, verdict, ref, commit_sha, created_at, retired_at FROM note_archive ORDER BY archived_id"
  lineRows <-
    SQLite.query_ conn "SELECT archived_id, line_no, line FROM note_archive_line ORDER BY archived_id, seq"
  postRows <-
    SQLite.query_ conn "SELECT archived_id, kind, author, body, created_at FROM note_archive_post ORDER BY archived_id, seq"
  let
    linesOf = Map.fromListWith (flip (++)) (map (\(ArchivedLine a l) -> (a, [l])) lineRows)
    postsOf = Map.fromListWith (flip (++)) (map (\(ArchivedPost a p) -> (a, [p])) postRows)
    note h =
      ExportedNote
        { noteId = ahNoteId h
        , noteRepo = ahRepo h
        , noteState = Retired (ahRetired h)
        , noteFile = ahFile h
        , noteSide = ahSide h
        , noteVerdict = ahVerdict h
        , noteRef = ahRef h
        , noteCommit = ahCommit h
        , noteStarted = ahStarted h
        , noteLines = Map.findWithDefault [] (ahId h) linesOf
        , notePosts = Map.findWithDefault [] (ahId h) postsOf
        }
  pure (map note heads)

retireOnKeys :: SQLite.Connection -> Retention -> Reach -> T.Text -> [T.Text] -> IO ()
retireOnKeys conn retention reach repo keys = do
  let
    keyChunks = chunksOf 400 keys
  groups <- fmap Set.unions (mapM (legacyGroupsOnKeys conn repo) keyChunks)
  targets <- case reach of
    LegacyPostsOnly -> pure Set.empty
    EveryNote _ -> fmap Set.unions (mapM (targetIdsOnKeys conn repo) keyChunks)
  Monad.when (retention == ArchiveNotes) $ do
    whole <- liveNotesNamed conn AllLegacyRows repo (Set.toList targets)
    partial <- liveNotesNamed conn (LegacyRowsOn (Set.fromList keys)) repo (Set.toList (Set.difference groups targets))
    Monad.forM_ (whole ++ partial) (archiveNote conn Nothing)
  Monad.forM_ keyChunks $ \ks -> do
    SQLite.execute
      conn
      (SQLite.Query ("DELETE FROM note_post WHERE repo = ? AND key IN (" <> holesFor ks <> ")"))
      (SQLite.toRow (SQLite.Only repo) <> SQLite.toRow ks)
    case reach of
      LegacyPostsOnly -> pure ()
      EveryNote reason -> retireTargetsOnKeys conn reason repo ks

legacyGroupsOnKeys :: SQLite.Connection -> T.Text -> [T.Text] -> IO (Set.Set T.Text)
legacyGroupsOnKeys conn repo ks =
  fmap
    (Set.fromList . map SQLite.fromOnly)
    ( SQLite.query
        conn
        (SQLite.Query ("SELECT DISTINCT COALESCE(group_id, key) FROM note_post WHERE repo = ? AND key IN (" <> holesFor ks <> ")"))
        (SQLite.toRow (SQLite.Only repo) <> SQLite.toRow ks)
    )

targetIdsOnKeys :: SQLite.Connection -> T.Text -> [T.Text] -> IO (Set.Set T.Text)
targetIdsOnKeys conn repo ks =
  fmap
    (Set.fromList . map SQLite.fromOnly)
    ( SQLite.query
        conn
        (SQLite.Query ("SELECT DISTINCT local_id FROM work_target_line WHERE repo = ? AND key IN (" <> holesFor ks <> ")"))
        (SQLite.toRow (SQLite.Only repo) <> SQLite.toRow ks)
    )

retireTargetsOnKeys :: SQLite.Connection -> RetiredReason -> T.Text -> [T.Text] -> IO ()
retireTargetsOnKeys conn reason repo ks = do
  let
    params = SQLite.toRow (SQLite.Only repo) <> SQLite.toRow ks
    onKeys = "SELECT local_id FROM work_target_line WHERE repo = ? AND key IN (" <> holesFor ks <> ")"
    targetParams = SQLite.toRow (SQLite.Only repo) <> params
  SQLite.execute
    conn
    (SQLite.Query ("INSERT OR REPLACE INTO retired_note (repo, local_id, reason) SELECT repo, local_id, ? FROM work_target WHERE repo = ? AND kind = 'note' AND local_id IN (" <> onKeys <> ")"))
    (SQLite.toRow (SQLite.Only (reasonText reason)) <> targetParams)
  SQLite.execute conn (SQLite.Query ("DELETE FROM work WHERE repo = ? AND target_kind = 'note' AND target_id IN (" <> onKeys <> ")")) targetParams
  SQLite.execute conn (SQLite.Query ("DELETE FROM work_target WHERE repo = ? AND kind = 'note' AND local_id IN (" <> onKeys <> ")")) targetParams
  SQLite.execute
    conn
    "DELETE FROM work_target_line WHERE repo = ? AND local_id NOT IN (SELECT local_id FROM work_target WHERE repo = ?)"
    (repo, repo)

retireIds :: SQLite.Connection -> Retention -> T.Text -> [T.Text] -> IO ()
retireIds conn retention repo ids = do
  Monad.when (retention == ArchiveNotes) $ do
    notes <- liveNotesNamed conn AllLegacyRows repo ids
    Monad.forM_ notes (archiveNote conn Nothing)
  Monad.forM_ ids $ \nid -> do
    SQLite.execute conn "INSERT OR REPLACE INTO retired_note (repo, local_id, reason) VALUES (?, ?, 'resolved')" (repo, nid)
    deleteNoteRows conn repo nid

retireUnder :: SQLite.Connection -> Retention -> T.Text -> T.Text -> IO ()
retireUnder conn retention repo scope =
  case (retention, T.null scope) of
    (DeleteNotes, True) -> do
      SQLite.execute conn "DELETE FROM work WHERE repo = ? AND target_kind = 'note'" (SQLite.Only repo)
      SQLite.execute conn "DELETE FROM work_target_line WHERE repo = ? AND local_id IN (SELECT local_id FROM work_target WHERE repo = ? AND kind = 'note')" (repo, repo)
      SQLite.execute conn "DELETE FROM work_target WHERE repo = ? AND kind = 'note'" (SQLite.Only repo)
    _ -> do
      let
        under note = T.null scope || maybe False (\f -> f == scope || (scope <> "/") `T.isPrefixOf` f) (noteFile note)
      notes <- liveNotes conn (Just repo)
      let
        wanted = filter under notes
      Monad.when (retention == ArchiveNotes) $
        Monad.forM_ wanted (archiveNote conn Nothing)
      Monad.forM_ wanted (deleteNoteRows conn repo . noteId)

deleteNoteRows :: SQLite.Connection -> T.Text -> T.Text -> IO ()
deleteNoteRows conn repo nid = do
  let
    here = (repo, nid)
  SQLite.execute conn "DELETE FROM work WHERE repo = ? AND target_kind = 'note' AND target_id = ?" here
  SQLite.execute conn "DELETE FROM work_target_line WHERE repo = ? AND local_id = ?" here
  SQLite.execute conn "DELETE FROM work_target WHERE repo = ? AND local_id = ?" here
  SQLite.execute conn "DELETE FROM note_post WHERE repo = ? AND COALESCE(group_id, key) = ?" here

archiveNote :: SQLite.Connection -> Maybe T.Text -> ExportedNote -> IO ()
archiveNote conn retiredAt n = do
  existing <-
    SQLite.query
      conn
      "SELECT archived_id FROM note_archive WHERE repo = ? AND note_id = ? ORDER BY archived_id LIMIT 1"
      (noteRepo n, noteId n)
  case existing of
    [SQLite.Only archived] -> do
      SQLite.execute conn "UPDATE note_archive SET retired_at = COALESCE(?, datetime('now')) WHERE archived_id = ?" (retiredAt, archived :: DataInt.Int64)
      oldLines <- SQLite.query conn "SELECT archived_id, line_no, line FROM note_archive_line WHERE archived_id = ? ORDER BY seq" (SQLite.Only archived)
      oldPosts <- SQLite.query conn "SELECT archived_id, kind, author, body, created_at FROM note_archive_post WHERE archived_id = ? ORDER BY seq" (SQLite.Only archived)
      let
        keptLines = map (\(ArchivedLine _ l) -> l) oldLines
        keptPosts = map (\(ArchivedPost _ p) -> p) oldPosts
      SQLite.execute conn "DELETE FROM note_archive_line WHERE archived_id = ?" (SQLite.Only archived)
      SQLite.execute conn "DELETE FROM note_archive_post WHERE archived_id = ?" (SQLite.Only archived)
      storeArchiveParts
        conn
        archived
        (List.sortOn lineNumber (keptLines ++ filter (`notElem` keptLines) (noteLines n)))
        (List.sortOn postAt (keptPosts ++ filter (`notElem` keptPosts) (notePosts n)))
    _ -> do
      SQLite.execute
        conn
        "INSERT INTO note_archive (repo, note_id, file, side, verdict, ref, commit_sha, created_at, retired_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, COALESCE(?, datetime('now')))"
        (noteRepo n, noteId n, noteFile n, noteSide n, noteVerdict n, noteRef n, noteCommit n, noteStarted n, retiredAt)
      archived <- SQLite.lastInsertRowId conn
      storeArchiveParts conn archived (noteLines n) (notePosts n)

storeArchiveParts :: SQLite.Connection -> DataInt.Int64 -> [ExportedLine] -> [ExportedPost] -> IO ()
storeArchiveParts conn archived noteLinesOf notePostsOf = do
  SQLite.executeMany
    conn
    "INSERT INTO note_archive_line (archived_id, seq, line_no, line) VALUES (?, ?, ?, ?)"
    (zipWith (\i l -> (archived, i :: Int, lineNumber l, lineText l)) [0 ..] noteLinesOf)
  SQLite.executeMany
    conn
    "INSERT INTO note_archive_post (archived_id, seq, kind, author, body, created_at) VALUES (?, ?, ?, ?, ?, ?)"
    (zipWith (\i p -> (archived, i :: Int, postKind p, postAuthor p, postBody p, postAt p)) [0 ..] notePostsOf)

columnsOf :: SQLite.Connection -> T.Text -> IO [T.Text]
columnsOf conn table =
  fmap (map SQLite.fromOnly) (SQLite.query conn "SELECT name FROM pragma_table_info(?)" (SQLite.Only table))

data NoteName = NoteName
  { nnRepo :: T.Text
  , nnId :: T.Text
  }

instance SQLite.FromRow NoteName where
  fromRow = NoteName <$> SQLite.field <*> SQLite.field

data RetiredPost = RetiredPost
  { rpRepo :: T.Text
  , rpGroup :: T.Text
  , rpKey :: T.Text
  , rpAuthor :: T.Text
  , rpBody :: T.Text
  , rpCreatedAt :: T.Text
  , rpFile :: Maybe T.Text
  , rpLine :: Maybe T.Text
  , rpLineNo :: Maybe Int
  , rpRef :: Maybe T.Text
  , rpCommit :: Maybe T.Text
  , rpRetiredAt :: T.Text
  }

instance SQLite.FromRow RetiredPost where
  fromRow =
    RetiredPost
      <$> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field

absorbRetiredRows :: SQLite.Connection -> IO ()
absorbRetiredRows conn = do
  targetCols <- columnsOf conn "work_target"
  postCols <- columnsOf conn "note_post"
  let
    retiredTargets = "retired_at" `elem` targetCols
    retiredPosts = "retired_at" `elem` postCols
    owes present query =
      if present
        then fmap (any SQLite.fromOnly) (SQLite.query_ conn query)
        else pure False
  owedTargets <- owes retiredTargets "SELECT EXISTS (SELECT 1 FROM work_target WHERE kind = 'note' AND retired_at IS NOT NULL)"
  owedPosts <- owes retiredPosts "SELECT EXISTS (SELECT 1 FROM note_post WHERE retired_at IS NOT NULL)"
  Monad.when (owedTargets || owedPosts) $
    SQLite.withImmediateTransaction conn $ do
      Monad.when retiredTargets (absorbRetiredTargets conn targetCols)
      Monad.when retiredPosts (absorbRetiredPosts conn postCols)

columnOrNull :: [T.Text] -> T.Text -> T.Text
columnOrNull cols name = if name `elem` cols then name else "NULL"

absorbRetiredTargets :: SQLite.Connection -> [T.Text] -> IO ()
absorbRetiredTargets conn cols = do
  retired <-
    SQLite.query_ conn "SELECT repo, local_id FROM work_target WHERE kind = 'note' AND retired_at IS NOT NULL ORDER BY rowid"
  Monad.forM_ retired $ \name -> do
    let
      here = (nnRepo name, nnId name)
    SQLite.execute
      conn
      ( SQLite.Query
          ( "INSERT INTO note_archive (repo, note_id, file, side, verdict, ref, commit_sha, created_at, retired_at)\
            \ SELECT repo, local_id, path, side, verdict, "
              <> columnOrNull cols "ref"
              <> ", commit_sha, created_at, retired_at FROM work_target WHERE repo = ? AND local_id = ?"
          )
      )
      here
    archived <- SQLite.lastInsertRowId conn
    SQLite.execute
      conn
      "INSERT INTO note_archive_line (archived_id, seq, line_no, line)\
      \ SELECT ?, seq, line_no, line FROM work_target_line WHERE repo = ? AND local_id = ?"
      (SQLite.Only archived SQLite.:. here)
    SQLite.execute
      conn
      "INSERT INTO note_archive_post (archived_id, seq, kind, author, body, created_at)\
      \ SELECT ?, ROW_NUMBER() OVER (ORDER BY id) - 1, kind, author, body, created_at\
      \ FROM work WHERE repo = ? AND target_kind = 'note' AND target_id = ?"
      (SQLite.Only archived SQLite.:. here)
    SQLite.execute conn "DELETE FROM work WHERE repo = ? AND target_kind = 'note' AND target_id = ?" here
    SQLite.execute conn "DELETE FROM work_target_line WHERE repo = ? AND local_id = ?" here
    SQLite.execute conn "DELETE FROM work_target WHERE repo = ? AND local_id = ?" here

absorbRetiredPosts :: SQLite.Connection -> [T.Text] -> IO ()
absorbRetiredPosts conn cols = do
  rows <-
    SQLite.query_
      conn
      ( SQLite.Query
          ( "SELECT repo, COALESCE(group_id, key), key, author, body, created_at, "
              <> T.intercalate ", " (map (columnOrNull cols) ["file", "line", "line_no", "ref", "commit_sha"])
              <> ", retired_at FROM note_post WHERE retired_at IS NOT NULL ORDER BY repo, COALESCE(group_id, key), seq, key"
          )
      )
  let
    groups = Map.fromListWith (flip (++)) (map (\r -> ((rpRepo r, rpGroup r), [r])) rows)
  Monad.forM_ (Map.toList groups) $ \((repo, gid), posts) ->
    archiveNote conn (Just (maximum (map rpRetiredAt posts))) (retiredGroupNote repo gid posts)
  SQLite.execute_ conn "DELETE FROM note_post WHERE retired_at IS NOT NULL"

retiredGroupNote :: T.Text -> T.Text -> [RetiredPost] -> ExportedNote
retiredGroupNote repo gid posts =
  let
    keys =
      List.sortOn
        (\r -> (Maybe.isNothing (rpLineNo r), Maybe.fromMaybe 0 (rpLineNo r), rpKey r))
        (Map.elems (Map.fromListWith (\_ earlier -> earlier) (map (\r -> (rpKey r, r)) posts)))
    firstKey = fmap rpKey (Maybe.listToMaybe keys)
    firstJust f = Maybe.listToMaybe (Maybe.mapMaybe f keys)
    post r = ExportedPost "aside" (Author.authorText (Author.legacyAuthor (rpAuthor r))) (rpBody r) (rpCreatedAt r)
  in
    ExportedNote
      { noteId = gid
      , noteRepo = repo
      , noteState = Open
      , noteFile = firstJust rpFile
      , noteSide = Nothing
      , noteVerdict = Nothing
      , noteRef = firstJust rpRef
      , noteCommit = firstJust rpCommit
      , noteStarted = minimum (map rpCreatedAt posts)
      , noteLines = map (\r -> ExportedLine (rpLineNo r) (Maybe.fromMaybe "" (rpLine r))) keys
      , notePosts = map post (filter (\r -> Just (rpKey r) == firstKey) posts)
      }
