{-# LANGUAGE OverloadedStrings #-}

-- Where verdicts are remembered: the XDG state directory (usually
-- ~/.local/state/remax/review.db).
-- One row per changed line, keyed by content (see Remax.Key), with the
-- verdict it was given, the pass that gave it, and the line's position
-- at the time (advisory — content keys are the identity; the number is
-- a courtesy for humans and agents reading the punch list). Notes are a
-- conversation: ordered posts with an author, kept per line key in
-- note_post, every key of a noted group carrying the same thread under
-- the same group name (see Remax.Key.noteGroupId). Passes record when each
-- review ran, so "what did I review yesterday" stays answerable.
module Remax.Store
  ( Verdict (..)
  , verdictText
  , parseVerdict
  , openStore
  , loadVerdicts
  , beginPass
  , finishPass
  , recordVerdicts
  , Post (..)
  , loadThreads
  , loadNoteGroups
  , saveReviewerPost
  , appendAgentPost
  , clearThread
  , resolveAnswered
  , setComposing
  , composingNow
  , storeDirectory
  , Presence (..)
  , markPresence
  , clearWaiting
  , presenceNow
  , deleteKeys
  , GroupId (..)
  , LineKey (..)
  , PunchRow (..)
  , punchRows
  , repoRows
  , deleteFiles
  ) where

import Control.Monad (unless, void, when)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Database.SQLite.Simple
import System.Directory
  ( XdgDirectory (XdgState)
  , createDirectoryIfMissing
  , getModificationTime
  , getXdgDirectory
  , listDirectory
  , removeFile
  )
import System.FilePath ((</>))
import System.IO.Error (catchIOError)
import System.Posix.Process (getProcessID)

import Remax.Key (noteGroupId)

data Verdict = Reviewed | Flagged | Questioned
  deriving (Eq, Ord, Show)

-- One entry in a note conversation. Threads are ordered lists of posts;
-- the author is "reviewer" for the human in the session, or the name an
-- agent gave on the command line.
data Post = Post
  { postAuthor :: Text
  , postBody :: Text
  }
  deriving (Eq, Ord, Show)

verdictText :: Verdict -> Text
verdictText Reviewed = "reviewed"
verdictText Flagged = "flagged"
verdictText Questioned = "questioned"

parseVerdict :: Text -> Maybe Verdict
parseVerdict "reviewed" = Just Reviewed
parseVerdict "flagged" = Just Flagged
parseVerdict "questioned" = Just Questioned
parseVerdict _ = Nothing

storeDirectory :: IO FilePath
storeDirectory = getXdgDirectory XdgState "remax"

openStore :: IO Connection
openStore = do
  dir <- storeDirectory
  createDirectoryIfMissing True dir
  conn <- open (dir </> "review.db")
  -- a persistent session and an agent running `remax list` overlap;
  -- short writes should wait for each other rather than error
  void (query_ conn "PRAGMA busy_timeout = 3000" :: IO [Only Int])
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS pass (\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \  repo TEXT NOT NULL,\
    \  scope TEXT NOT NULL,\
    \  started_at TEXT NOT NULL DEFAULT (datetime('now')),\
    \  finished_at TEXT)"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS line_verdict (\
    \  key TEXT PRIMARY KEY,\
    \  repo TEXT NOT NULL,\
    \  file TEXT NOT NULL,\
    \  line TEXT NOT NULL,\
    \  verdict TEXT NOT NULL\
    \    CHECK (verdict IN ('reviewed','flagged','questioned')),\
    \  pass_id INTEGER REFERENCES pass(id),\
    \  updated_at TEXT NOT NULL DEFAULT (datetime('now')),\
    \  line_no INTEGER,\
    \  note TEXT,\
    \  commit_sha TEXT,\
    \  commit_subject TEXT,\
    \  reply TEXT)"
  execute_
    conn
    "CREATE INDEX IF NOT EXISTS line_verdict_repo ON line_verdict(repo)"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS note_post (\
    \  repo TEXT NOT NULL,\
    \  key TEXT NOT NULL,\
    \  seq INTEGER NOT NULL,\
    \  author TEXT NOT NULL,\
    \  body TEXT NOT NULL,\
    \  created_at TEXT NOT NULL DEFAULT (datetime('now')),\
    \  group_id TEXT,\
    \  PRIMARY KEY (repo, key, seq))"
  -- A flag whose note is still being typed is not news yet: remax marks
  -- the repository while a note popup is open, and an agent waiting on
  -- the reviewer holds off until it clears. Otherwise the verdict — which
  -- is written the instant the key is pressed, so the tray and the meters
  -- answer at once — reaches the agent seconds before the sentence saying
  -- what is wrong with it.
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS note_composing (\
    \  repo TEXT PRIMARY KEY,\
    \  since TEXT NOT NULL DEFAULT (datetime('now')))"
  -- several instances (sessions, agent CLI calls) share this file; WAL
  -- lets readers proceed under a writer instead of stacking on the lock
  void (query_ conn "PRAGMA journal_mode = WAL" :: IO [Only Text])
  migrate conn
  pure conn

-- Databases created before a column existed pick it up here.
migrate :: Connection -> IO ()
migrate conn = do
  cols <-
    query_ conn "PRAGMA table_info(line_verdict)" ::
      IO [(Int, Text, Text, Int, Maybe Text, Int)]
  let
    names = map (\(_, name, _, _, _, _) -> name) cols
  unless ("line_no" `elem` names) $
    execute_ conn "ALTER TABLE line_verdict ADD COLUMN line_no INTEGER"
  unless ("note" `elem` names) $
    execute_ conn "ALTER TABLE line_verdict ADD COLUMN note TEXT"
  unless ("commit_sha" `elem` names) $
    execute_ conn "ALTER TABLE line_verdict ADD COLUMN commit_sha TEXT"
  unless ("commit_subject" `elem` names) $
    execute_ conn "ALTER TABLE line_verdict ADD COLUMN commit_subject TEXT"
  unless ("reply" `elem` names) $
    execute_ conn "ALTER TABLE line_verdict ADD COLUMN reply TEXT"
  postCols <-
    query_ conn "PRAGMA table_info(note_post)" ::
      IO [(Int, Text, Text, Int, Maybe Text, Int)]
  let
    postNames = map (\(_, name, _, _, _, _) -> name) postCols
  -- one-time carry-over from the old note/reply column pair into the
  -- conversation table: note becomes the opening reviewer post, reply
  -- the agent's answer after it
  [Only posts] <- query_ conn "SELECT COUNT(*) FROM note_post" :: IO [Only Int]
  [Only oldNotes] <-
    query_
      conn
      "SELECT COUNT(*) FROM line_verdict\
      \ WHERE note IS NOT NULL OR reply IS NOT NULL" ::
      IO [Only Int]
  when (posts == 0 && oldNotes > 0) $
    withTransaction conn $ do
      execute_
        conn
        "INSERT INTO note_post (repo, key, seq, author, body, created_at)\
        \ SELECT repo, key, 0, 'reviewer', note, updated_at\
        \ FROM line_verdict WHERE note IS NOT NULL"
      execute_
        conn
        "INSERT INTO note_post (repo, key, seq, author, body, created_at)\
        \ SELECT repo, key, 1, 'agent', reply, updated_at\
        \ FROM line_verdict WHERE reply IS NOT NULL"
      execute_ conn "UPDATE line_verdict SET note = NULL, reply = NULL"
  -- last, so that anything the carry-over above just inserted is named
  -- too rather than left without a group
  unless ("group_id" `elem` postNames) $ do
    execute_ conn "ALTER TABLE note_post ADD COLUMN group_id TEXT"
    nameExistingGroups conn

loadVerdicts :: Connection -> Text -> IO (Map Text Verdict)
loadVerdicts conn repo = do
  rows <-
    query conn "SELECT key, verdict FROM line_verdict WHERE repo = ?" (Only repo)
  let
    parseRow (k, vt) = fmap (\v -> (k, v)) (parseVerdict vt)
  pure . Map.fromList . mapMaybe parseRow $ rows

beginPass :: Connection -> Text -> Text -> IO Int64
beginPass conn repo scope = do
  execute conn "INSERT INTO pass (repo, scope) VALUES (?, ?)" (repo, scope)
  lastInsertRowId conn

finishPass :: Connection -> Int64 -> IO ()
finishPass conn passId =
  execute
    conn
    "UPDATE pass SET finished_at = datetime('now') WHERE id = ?"
    (Only passId)

-- Re-verdicting a line updates its row in place, so updated_at reads as
-- the last time it was looked at; a note already on the line survives.
-- The commit context (sha, subject) is stamped when the review knows
-- which commit it is looking at — a rebase stop — and is NULL otherwise.
recordVerdicts ::
  Connection ->
  Text ->
  Int64 ->
  Verdict ->
  Maybe (Text, Text) ->
  [(Text, Text, Text, Int)] ->
  IO ()
recordVerdicts conn repo passId v mCommit rows =
  let
    (sha, subject) = case mCommit of
      Just (s, subj) -> (Just s, Just subj)
      Nothing -> (Nothing, Nothing)
    toParams (key, file, line, lineNo) =
      (key, repo, file, line, lineNo, verdictText v, passId, sha, subject)
  in
    withTransaction conn $ do
      executeMany
        conn
        "INSERT INTO line_verdict\
        \ (key, repo, file, line, line_no, verdict, pass_id,\
        \  commit_sha, commit_subject, updated_at)\
        \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, datetime('now'))\
        \ ON CONFLICT(key) DO UPDATE SET\
        \   repo = excluded.repo, file = excluded.file,\
        \   line = excluded.line, line_no = excluded.line_no,\
        \   verdict = excluded.verdict, pass_id = excluded.pass_id,\
        \   commit_sha = excluded.commit_sha,\
        \   commit_subject = excluded.commit_subject,\
        \   updated_at = excluded.updated_at"
        (map toParams rows)
      -- accepting closes the conversation; other verdicts keep it open
      when (v == Reviewed) $
        mapM_
          ( \ks ->
              let
                holes = mconcat (replicate (length ks - 1) ", ?")
              in
                execute
                  conn
                  (Query ("DELETE FROM note_post WHERE repo = ? AND key IN (?" <> holes <> ")"))
                  (toRow (Only repo) <> toRow ks)
          )
          (chunks 400 (map (\(key, _, _, _) -> key) rows))

-- Every conversation in the repo, by line key, posts in order. Keys of
-- a noted group carry identical threads, so display code can collapse
-- them by equality — the same trick the single note column used.
-- Where an agent waiting on the reviewer says so, and keeps saying so.
--
-- A directory of files rather than a table, and a directory nothing
-- watches: the store's own is watched so that a reply lands in the
-- session as it is written, and a heartbeat kept there would wake every
-- open session twice a second for news it already has.
waitingDirectory :: IO FilePath
waitingDirectory = do
  dir <- fmap (</> "waiting") storeDirectory
  createDirectoryIfMissing True dir
  pure dir

-- What an agent is doing about a review.
--
-- Waiting is blocked on the reviewer with nothing to do. Working is
-- attending the review but busy — reading the feedback, changing the
-- file, writing a reply. They are told apart because they read entirely
-- differently from the other chair: one says your turn, the other says
-- something is happening.
data Presence = Waiting | Working
  deriving (Eq, Show)

presenceWord :: Presence -> Text
presenceWord p = case p of
  Waiting -> "waiting"
  Working -> "working"

-- Announce what this process is doing about these repositories. Called
-- again on a beat: the file's timestamp is the heartbeat, so a listener
-- can tell somebody still here from somebody gone without being told.
markPresence :: Presence -> [Text] -> IO ()
markPresence what repos = do
  dir <- waitingDirectory
  pid <- getProcessID
  ignoringIOErrors
    (TIO.writeFile (dir </> show pid) (T.unlines (presenceWord what : repos)))

clearWaiting :: IO ()
clearWaiting = do
  dir <- waitingDirectory
  pid <- getProcessID
  ignoringIOErrors (removeFile (dir </> show pid))

-- Which repositories somebody is waiting on, and which somebody is
-- working on, right now.
--
-- A heartbeat older than a few seconds is somebody who has gone away —
-- killed, disconnected, or a session that ended — which is why this is
-- read from timestamps rather than from whoever remembered to tidy up.
presenceNow :: IO ([Text], [Text])
presenceNow = do
  dir <- waitingDirectory
  names <- catchIOError (listDirectory dir) (const (pure []))
  now <- getCurrentTime
  found <- mapM (freshRepos dir now) names
  let
    ofKind k = concatMap snd (filter ((== k) . fst) found)
  pure (ofKind Waiting, ofKind Working)

waitingStaleAfter :: NominalDiffTime
waitingStaleAfter = 4

freshRepos :: FilePath -> UTCTime -> FilePath -> IO (Presence, [Text])
freshRepos dir now name =
  let
    path = dir </> name
    nothing = (Working, [])
  in
    flip catchIOError (const (pure nothing)) $ do
      touched <- getModificationTime path
      if diffUTCTime now touched > waitingStaleAfter
        then pure nothing
        else do
          body <- TIO.readFile path
          pure $ case filter (not . T.null) (T.lines body) of
            (w : repos) ->
              (if w == presenceWord Waiting then Waiting else Working, repos)
            [] -> nothing

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors act = catchIOError act (const (pure ()))

-- Say whether this repository has a note being written into it.
setComposing :: Connection -> Text -> Bool -> IO ()
setComposing conn repo composing =
  if composing
    then
      execute
        conn
        "INSERT INTO note_composing (repo) VALUES (?)\
        \ ON CONFLICT(repo) DO UPDATE SET since = datetime('now')"
        (Only repo)
    else execute conn "DELETE FROM note_composing WHERE repo = ?" (Only repo)

-- Whether any of these repositories is mid-note.
--
-- Bounded by age as well as by the marker: a session killed with its
-- popup open leaves the row behind, and an agent that waited on it would
-- wait for as long as nobody noticed. Ten minutes is far longer than
-- writing a note and far shorter than caring.
composingNow :: Connection -> [Text] -> IO Bool
composingNow conn repos = do
  rows <-
    mapM
      ( \repo ->
          query
            conn
            "SELECT 1 FROM note_composing\
            \ WHERE repo = ? AND since > datetime('now', '-10 minutes')"
            (Only repo) ::
            IO [Only Int]
      )
      repos
  pure (any (not . null) rows)

-- Name the groups that were recorded before groups had names.
--
-- Done the way the code used to do it, which is the point: keys of one
-- file whose conversation read the same were treated as one group,
-- because comparing the words was the only handle there was. Those
-- notes keep behaving exactly as they did; only what happens from here
-- is different. Run once, when the column appears.
nameExistingGroups :: Connection -> IO ()
nameExistingGroups conn = withTransaction conn $ do
  rows <-
    query_
      conn
      "SELECT repo, key, author, body FROM note_post ORDER BY repo, key, seq" ::
      IO [(Text, Text, Text, Text)]
  places <-
    query_ conn "SELECT repo, key, file FROM line_verdict" ::
      IO [(Text, Text, Text)]
  let
    fileOf =
      Map.fromList (map (\(repo, key, file) -> ((repo, key), file)) places)
    -- each key's conversation, in order, as something comparable
    saidBy =
      Map.fromListWith
        (flip (++))
        ( map
            (\(repo, key, author, body) -> ((repo, key), [(author, body)]))
            rows
        )
    -- what used to count as one group: same repository, same file, same
    -- words. A note on a key no verdict remains for groups alone.
    sameGroup ((repo, key), said) =
      ((repo, Map.findWithDefault key (repo, key) fileOf, said), [key])
    grouped = Map.fromListWith (++) (map sameGroup (Map.toList saidBy))
    name ((repo, _, _), keys) =
      map (\key -> (noteGroupId repo keys, repo, key)) keys
  mapM_
    ( \(gid, repo, key) ->
        execute
          conn
          "UPDATE note_post SET group_id = ? WHERE repo = ? AND key = ?"
          (gid, repo, key)
    )
    (concatMap name (Map.toList grouped))

loadThreads :: Connection -> Text -> IO (Map Text [Post])
loadThreads conn repo = do
  rows <-
    query
      conn
      "SELECT key, author, body FROM note_post\
      \ WHERE repo = ? ORDER BY key, seq"
      (Only repo)
  let
    add (key, author, body) =
      Map.insertWith (flip (++)) key [Post author body]
  pure (foldr add Map.empty (reverse rows))

-- Which named group each key's conversation belongs to. The session
-- needs this to tell one note from another note worded the same.
loadNoteGroups :: Connection -> Text -> IO (Map Text Text)
loadNoteGroups conn repo = do
  rows <-
    query
      conn
      "SELECT DISTINCT key, group_id FROM note_post\
      \ WHERE repo = ? AND group_id IS NOT NULL"
      (Only repo) ::
      IO [(Text, Text)]
  pure (Map.fromList rows)

nextSeq :: Connection -> Text -> Text -> IO Int
nextSeq conn repo key = do
  rows <-
    query
      conn
      "SELECT COALESCE(MAX(seq), -1) FROM note_post WHERE repo = ? AND key = ?"
      (repo, key) ::
      IO [Only Int]
  pure $ case rows of
    (Only n : _) -> n + 1
    [] -> 0

trailingPost :: Connection -> Text -> Text -> IO (Maybe (Int, Text))
trailingPost conn repo key = do
  rows <-
    query
      conn
      "SELECT seq, author FROM note_post\
      \ WHERE repo = ? AND key = ? ORDER BY seq DESC LIMIT 1"
      (repo, key)
  pure $ case rows of
    (r : _) -> Just r
    [] -> Nothing

-- The reviewer's save from the note popup: while their own post is the
-- newest in the thread it stays editable in place; once someone else
-- has answered, saving starts the next post instead. Only lines that
-- already carry verdicts take a thread. Returns how many lines.
saveReviewerPost :: Connection -> Text -> Text -> [Text] -> IO Int
saveReviewerPost conn repo body keys =
  withTransaction conn $ do
    marked <- markedKeys conn repo keys
    -- a group that already has a name keeps it, so editing a note does
    -- not re-form the group under a second one
    held <- heldGroupId conn repo marked
    let
      gid = case held of
        Just g -> g
        Nothing -> noteGroupId repo marked
    mapM_
      ( \key -> do
          trailing <- trailingPost conn repo key
          case trailing of
            Just (seq_, "reviewer") ->
              execute
                conn
                "UPDATE note_post SET body = ?, created_at = datetime('now')\
                \ WHERE repo = ? AND key = ? AND seq = ?"
                (body, repo, key, seq_)
            _ -> do
              seq_ <- nextSeq conn repo key
              execute
                conn
                "INSERT INTO note_post (repo, key, seq, author, body, group_id)\
                \ VALUES (?, ?, ?, 'reviewer', ?, ?)"
                (repo, key, seq_, body, gid)
      )
      marked
    -- every post on these keys answers to the one name, including any
    -- that predate it and any key joining the group now
    mapM_
      ( \key ->
          execute
            conn
            "UPDATE note_post SET group_id = ? WHERE repo = ? AND key = ?"
            (gid, repo, key)
      )
      marked
    pure (length marked)

-- The name these keys' conversation already goes by, if any of them
-- carries one.
heldGroupId :: Connection -> Text -> [Text] -> IO (Maybe Text)
heldGroupId conn repo keys =
  let
    batch ks = do
      let
        holes = mconcat (replicate (length ks - 1) ", ?")
      rows <-
        query
          conn
          ( Query
              ( "SELECT group_id FROM note_post WHERE repo = ?\
                \ AND group_id IS NOT NULL AND key IN (?"
                  <> holes
                  <> ") LIMIT 1"
              )
          )
          (toRow (Only repo) <> toRow ks) ::
          IO [Only Text]
      pure (map (\(Only g) -> g) rows)
  in
    if null keys
      then pure Nothing
      else do
        found <- mapM batch (chunks 400 keys)
        pure (listToMaybe (concat found))

-- Every key the named group covers.
groupMembers :: Connection -> Text -> Text -> IO [Text]
groupMembers conn repo gid = do
  rows <-
    query
      conn
      "SELECT DISTINCT key FROM note_post WHERE repo = ? AND group_id = ?"
      (repo, gid) ::
      IO [Only Text]
  pure (map (\(Only k) -> k) rows)

-- An agent's post, addressed by a group's name or by any line key of a
-- noted group; it lands on every key of that group, so the session shows
-- one conversation under the lines the note was written across. A key
-- without a thread takes the post alone. Returns how many lines it
-- landed on.
--
-- The group is found by name. It used to be found by comparing what the
-- notes said, which made two notes worded alike into one group and sent
-- a reply to hunks nobody had asked about; see noteGroupId.
appendAgentPost :: Connection -> Text -> Text -> Text -> Text -> IO Int
appendAgentPost conn repo author target body = do
  addressed <- listedConversation conn repo target
  case addressed of
    Nothing -> pure 0
    Just (gid, groupKeys) -> do
      withTransaction conn $
        mapM_
          ( \k -> do
              seq_ <- nextSeq conn repo k
              execute
                conn
                "INSERT INTO note_post (repo, key, seq, author, body, group_id)\
                \ VALUES (?, ?, ?, ?, ?, ?)"
                (repo, k, seq_, author, body, gid)
          )
          groupKeys
      pure (length groupKeys)

listedConversation :: Connection -> Text -> Text -> IO (Maybe (Text, [Text]))
listedConversation conn repo target = do
  named <- groupMembers conn repo target
  found <-
    if null named
      then do
        byKey <- keyConversation conn repo target
        case byKey of
          Just conversation -> pure (Just conversation)
          Nothing -> bareLineConversation conn repo target
      else pure (Just (target, named))
  case found of
    Nothing -> pure Nothing
    Just (_, keys) -> do
      listed <- listedKeys conn repo keys
      pure (if null listed then Nothing else found)

bareLineConversation :: Connection -> Text -> Text -> IO (Maybe (Text, [Text]))
bareLineConversation conn repo gid = do
  rows <- repoRows conn repo
  pure $
    case filter (\(key, _) -> noteGroupId repo [key] == gid) rows of
      ((key, _) : _) -> Just (gid, [key])
      [] -> Nothing

keyConversation :: Connection -> Text -> Text -> IO (Maybe (Text, [Text]))
keyConversation conn repo key = do
  present <-
    query
      conn
      "SELECT COUNT(*) FROM line_verdict WHERE repo = ? AND key = ?"
      (repo, key) ::
      IO [Only Int]
  case present of
    (Only 0 : _) -> pure Nothing
    _ -> do
      held <- heldGroupId conn repo [key]
      fmap Just $ case held of
        -- a line nobody has noted takes the post by itself, under a name
        -- of its own, so an answer to it can be found again later
        Nothing -> pure (noteGroupId repo [key], [key])
        Just g -> fmap ((,) g) (groupMembers conn repo g)

-- Drop the whole conversation from some lines. Returns how many lines
-- actually had one.
clearThread :: Connection -> Text -> [Text] -> IO Int
clearThread conn repo keys =
  let
    batch ks = do
      let
        holes = mconcat (replicate (length ks - 1) ", ?")
      cleared <-
        query
          conn
          (Query ("SELECT COUNT(DISTINCT key) FROM note_post WHERE repo = ? AND key IN (?" <> holes <> ")"))
          (toRow (Only repo) <> toRow ks) ::
          IO [Only Int]
      execute
        conn
        (Query ("DELETE FROM note_post WHERE repo = ? AND key IN (?" <> holes <> ")"))
        (toRow (Only repo) <> toRow ks)
      pure $ case cleared of
        (Only n : _) -> n
        [] -> 0
  in
    withTransaction conn $ do
      counts <- mapM batch (chunks 400 keys)
      pure (sum counts)

-- Close every answered conversation at once: a thread whose newest post
-- came from someone other than the reviewer clears entirely. Returns
-- how many lines.
resolveAnswered :: Connection -> Text -> IO Int
resolveAnswered conn repo = do
  threads <- loadThreads conn repo
  let
    answered = answeredKeys threads
  case answered of
    [] -> pure 0
    keys -> clearThread conn repo keys

answeredKeys :: Map Text [Post] -> [Text]
answeredKeys threads =
  let
    endsElsewhere posts = case reverse posts of
      (p : _) -> postAuthor p /= "reviewer"
      [] -> False
  in
    Map.keys (Map.filter endsElsewhere threads)

-- Threads attach only to lines that already carry verdicts; noting an
-- unmarked line would record a conversation nothing displays.
markedKeys :: Connection -> Text -> [Text] -> IO [Text]
markedKeys conn repo keys =
  let
    batch ks = do
      let
        holes = mconcat (replicate (length ks - 1) ", ?")
      rows <-
        query
          conn
          (Query ("SELECT key FROM line_verdict WHERE repo = ? AND key IN (?" <> holes <> ")"))
          (toRow (Only repo) <> toRow ks) ::
          IO [Only Text]
      pure (map (\(Only k) -> k) rows)
  in
    fmap concat (mapM batch (chunks 400 keys))

listedKeys :: Connection -> Text -> [Text] -> IO [Text]
listedKeys conn repo keys =
  let
    batch ks = do
      let
        holes = mconcat (replicate (length ks - 1) ", ?")
      rows <-
        query
          conn
          ( Query
              ( "SELECT key FROM line_verdict WHERE repo = ?\
                \ AND verdict <> 'reviewed' AND key IN (?"
                  <> holes
                  <> ")"
              )
          )
          (toRow (Only repo) <> toRow ks) ::
          IO [Only Text]
      pure (map (\(Only k) -> k) rows)
  in
    fmap concat (mapM batch (chunks 400 keys))

newtype GroupId = GroupId {groupIdToText :: Text}
  deriving (Eq, Ord, Show)

newtype LineKey = LineKey {lineKeyToText :: Text}
  deriving (Eq, Ord, Show)

data PunchRow = PunchRow
  { punchKey :: LineKey
  , punchGroup :: Maybe GroupId
  , punchFile :: Text
  , punchVerdict :: Verdict
  , punchLine :: Text
  , punchLineNo :: Maybe Int
  , punchThread :: [Post]
  , punchCommitSha :: Maybe Text
  , punchCommitSubject :: Maybe Text
  , punchAt :: Text
  }

-- Every flagged or questioned line for the repo, grouped by file.
punchRows :: Connection -> Text -> IO [PunchRow]
punchRows conn repo = do
  rows <-
    query
      conn
      "SELECT key, file, verdict, line, line_no,\
      \ commit_sha, commit_subject, updated_at FROM line_verdict\
      \ WHERE repo = ? AND verdict <> 'reviewed'\
      \ ORDER BY file, line_no, updated_at DESC"
      (Only repo)
  threads <- loadThreads conn repo
  groups <- loadNoteGroups conn repo
  let
    toPunch (key, file, verdictName, line, lineNo, sha, subject, at) =
      fmap
        ( \verdict ->
            PunchRow
              { punchKey = LineKey key
              , punchGroup = fmap GroupId (Map.lookup key groups)
              , punchFile = file
              , punchVerdict = verdict
              , punchLine = line
              , punchLineNo = lineNo
              , punchThread = Map.findWithDefault [] key threads
              , punchCommitSha = sha
              , punchCommitSubject = subject
              , punchAt = at
              }
        )
        (parseVerdict verdictName)
  pure (mapMaybe toPunch rows)

repoRows :: Connection -> Text -> IO [(Text, Text)]
repoRows conn repo =
  query conn "SELECT key, file FROM line_verdict WHERE repo = ?" (Only repo)

-- Forget the verdicts for particular line keys (unmarking a hunk or a
-- selection of lines).
deleteKeys :: Connection -> Text -> [Text] -> IO ()
deleteKeys conn repo keys =
  let
    batch ks =
      let
        holes = mconcat (replicate (length ks - 1) ", ?")
      in
        do
          execute
            conn
            (Query ("DELETE FROM line_verdict WHERE repo = ? AND key IN (?" <> holes <> ")"))
            (toRow (Only repo) <> toRow ks)
          execute
            conn
            (Query ("DELETE FROM note_post WHERE repo = ? AND key IN (?" <> holes <> ")"))
            (toRow (Only repo) <> toRow ks)
  in
    withTransaction conn $
      mapM_ batch (chunks 400 keys)

-- Batched so a repository with thousands of recorded lines stays inside
-- sqlite's limit on parameters per statement.
deleteFiles :: Connection -> Text -> [Text] -> IO ()
deleteFiles conn repo files =
  let
    batch fs =
      let
        holes = mconcat (replicate (length fs - 1) ", ?") -- first hole below
      in
        do
          execute
            conn
            ( Query
                ( "DELETE FROM note_post WHERE repo = ? AND key IN\
                  \ (SELECT key FROM line_verdict WHERE repo = ? AND file IN (?"
                    <> holes
                    <> "))"
                )
            )
            (toRow (repo, repo) <> toRow fs)
          execute
            conn
            (Query ("DELETE FROM line_verdict WHERE repo = ? AND file IN (?" <> holes <> ")"))
            (toRow (Only repo) <> toRow fs)
  in
    withTransaction conn $
      mapM_ batch (chunks 400 files)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)
