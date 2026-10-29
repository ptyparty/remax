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
  , VerdictOrigin (..)
  , noOrigin
  , Post (..)
  , loadThreads
  , loadNoteGroups
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
  , PullNumber (..)
  , RemoteThreadId (..)
  , CommentId (..)
  , ReviewId (..)
  , Resolution (..)
  , recordView
  , sessionView
  , cachedRepoName
  , saveRepoName
  , TourStep (..)
  , clearTour
  , appendTourStep
  , loadTour
  , tourSubjects
  , TourRequest (..)
  , askTour
  , tourRequests
  , clearTourRequest
  , rememberWorktree
  , forgetWorktree
  , pullWorktree
  , originalOf
  , recordMoveBetween
  , ReviewMove (..)
  , reviewMoveFor
  ) where

import Control.Monad qualified as Monad
import Data.Int qualified as DataInt
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock qualified as Clock
import Database.SQLite.Simple qualified as SQLite
import System.Directory qualified as Directory
import System.FilePath ((</>))
import System.IO.Error qualified as IOError
import System.Posix.Process qualified as Posix

import Remax.Key qualified as Key

data Verdict = Reviewed | Flagged | Questioned
  deriving (Eq, Ord, Show)

-- One entry in a note conversation. Threads are ordered lists of posts;
-- the author is "reviewer" for the human in the session, or the name an
-- agent gave on the command line.
data Post = Post
  { postAuthor :: T.Text
  , postBody :: T.Text
  }
  deriving (Eq, Ord, Show)

isReviewerPost :: Post -> Bool
isReviewerPost post = postAuthor post == "reviewer"

verdictText :: Verdict -> T.Text
verdictText Reviewed = "reviewed"
verdictText Flagged = "flagged"
verdictText Questioned = "questioned"

parseVerdict :: T.Text -> Maybe Verdict
parseVerdict "reviewed" = Just Reviewed
parseVerdict "flagged" = Just Flagged
parseVerdict "questioned" = Just Questioned
parseVerdict _ = Nothing

storeDirectory :: IO FilePath
storeDirectory = Directory.getXdgDirectory Directory.XdgState "remax"

openStore :: IO SQLite.Connection
openStore = do
  dir <- storeDirectory
  Directory.createDirectoryIfMissing True dir
  conn <- SQLite.open (dir </> "review.db")
  -- a persistent session and an agent running `remax list` overlap;
  -- short writes should wait for each other rather than error
  Monad.void (SQLite.query_ conn "PRAGMA busy_timeout = 3000" :: IO [SQLite.Only Int])
  SQLite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS pass (\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \  repo TEXT NOT NULL,\
    \  scope TEXT NOT NULL,\
    \  started_at TEXT NOT NULL DEFAULT (datetime('now')),\
    \  finished_at TEXT)"
  SQLite.execute_
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
    \  reply TEXT,\
    \  ref TEXT)"
  SQLite.execute_
    conn
    "CREATE INDEX IF NOT EXISTS line_verdict_repo ON line_verdict(repo)"
  SQLite.execute_
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
  SQLite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS note_composing (\
    \  repo TEXT PRIMARY KEY,\
    \  since TEXT NOT NULL DEFAULT (datetime('now')))"
  SQLite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS session_view (\
    \  repo TEXT PRIMARY KEY,\
    \  mode TEXT NOT NULL,\
    \  since TEXT NOT NULL DEFAULT (datetime('now')))"
  SQLite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS tour_step (\
    \  repo TEXT NOT NULL,\
    \  subject TEXT NOT NULL,\
    \  seq INTEGER NOT NULL,\
    \  author TEXT NOT NULL,\
    \  note TEXT NOT NULL,\
    \  created_at TEXT NOT NULL DEFAULT (datetime('now')),\
    \  title TEXT,\
    \  PRIMARY KEY (repo, subject, seq))"
  SQLite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS tour_request (\
    \  repo TEXT NOT NULL,\
    \  subject TEXT NOT NULL,\
    \  mode TEXT NOT NULL,\
    \  prompt TEXT NOT NULL,\
    \  asked_at TEXT NOT NULL DEFAULT (datetime('now')),\
    \  PRIMARY KEY (repo, subject))"
  SQLite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS pull_worktree (\
    \  repo TEXT NOT NULL,\
    \  pull INTEGER NOT NULL,\
    \  path TEXT NOT NULL,\
    \  PRIMARY KEY (repo, pull))"
  SQLite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS review_move (\
    \  repo TEXT PRIMARY KEY,\
    \  pull INTEGER NOT NULL,\
    \  to_path TEXT NOT NULL,\
    \  moved_at TEXT NOT NULL DEFAULT (datetime('now')))"
  SQLite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS tour_hunk (\
    \  repo TEXT NOT NULL,\
    \  subject TEXT NOT NULL,\
    \  seq INTEGER NOT NULL,\
    \  key TEXT NOT NULL,\
    \  PRIMARY KEY (repo, subject, seq, key))"
  SQLite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS github_repo (\
    \  origin_url TEXT PRIMARY KEY,\
    \  name_with_owner TEXT NOT NULL,\
    \  resolved_at TEXT NOT NULL DEFAULT (datetime('now')))"
  SQLite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS work (\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \  repo TEXT NOT NULL,\
    \  pull INTEGER,\
    \  target_kind TEXT NOT NULL CHECK (target_kind IN ('thread','comment','new','note')),\
    \  target_id TEXT NOT NULL,\
    \  kind TEXT NOT NULL CHECK (kind IN ('proposed','pending','aside','edit')),\
    \  author TEXT NOT NULL,\
    \  body TEXT NOT NULL,\
    \  comment_id INTEGER,\
    \  created_at TEXT NOT NULL DEFAULT (datetime('now')))"
  SQLite.execute_
    conn
    "CREATE INDEX IF NOT EXISTS work_repo ON work(repo, target_kind, target_id)"
  SQLite.execute_
    conn
    "UPDATE work SET pull =\
    \  (SELECT w.pull FROM work w WHERE w.repo = work.repo AND w.target_kind = work.target_kind\
    \   AND w.target_id = work.target_id AND w.pull IS NOT NULL ORDER BY w.id LIMIT 1)\
    \ WHERE target_kind IN ('thread', 'comment', 'new') AND pull IS NOT\
    \  (SELECT w.pull FROM work w WHERE w.repo = work.repo AND w.target_kind = work.target_kind\
    \   AND w.target_id = work.target_id AND w.pull IS NOT NULL ORDER BY w.id LIMIT 1)"
  SQLite.execute_
    conn
    "UPDATE work SET pull = NULL WHERE target_kind = 'note' AND pull IS NOT NULL"
  SQLite.execute_
    conn
    "UPDATE work SET kind = 'proposed' WHERE kind = 'pending' AND author LIKE 'agent:%' AND target_kind = 'new'"
  SQLite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS work_target (\
    \  repo TEXT NOT NULL,\
    \  local_id TEXT PRIMARY KEY,\
    \  kind TEXT NOT NULL CHECK (kind IN ('new','note')),\
    \  anchor TEXT NOT NULL CHECK (anchor IN ('lines','pull')),\
    \  path TEXT NOT NULL,\
    \  side TEXT NOT NULL,\
    \  commit_sha TEXT,\
    \  verdict TEXT,\
    \  created_at TEXT NOT NULL DEFAULT (datetime('now')))"
  SQLite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS work_target_line (\
    \  repo TEXT NOT NULL,\
    \  local_id TEXT NOT NULL,\
    \  seq INTEGER NOT NULL,\
    \  key TEXT NOT NULL,\
    \  line_no INTEGER NOT NULL,\
    \  line TEXT NOT NULL,\
    \  PRIMARY KEY (local_id, seq))"
  SQLite.execute_
    conn
    "CREATE INDEX IF NOT EXISTS work_target_line_key ON work_target_line(repo, key)"
  SQLite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS work_submitted (\
    \  repo TEXT NOT NULL,\
    \  local_id TEXT PRIMARY KEY,\
    \  thread_id TEXT NOT NULL)"
  -- several instances (sessions, agent CLI calls) share this file; WAL
  -- lets readers proceed under a writer instead of stacking on the lock
  Monad.void (SQLite.query_ conn "PRAGMA journal_mode = WAL" :: IO [SQLite.Only T.Text])
  migrate conn
  pure conn

-- Databases created before a column existed pick it up here.
migrate :: SQLite.Connection -> IO ()
migrate conn = do
  cols <-
    SQLite.query_ conn "PRAGMA table_info(line_verdict)" ::
      IO [(Int, T.Text, T.Text, Int, Maybe T.Text, Int)]
  let
    names = map (\(_, name, _, _, _, _) -> name) cols
  Monad.unless ("line_no" `elem` names) $
    SQLite.execute_ conn "ALTER TABLE line_verdict ADD COLUMN line_no INTEGER"
  Monad.unless ("note" `elem` names) $
    SQLite.execute_ conn "ALTER TABLE line_verdict ADD COLUMN note TEXT"
  Monad.unless ("commit_sha" `elem` names) $
    SQLite.execute_ conn "ALTER TABLE line_verdict ADD COLUMN commit_sha TEXT"
  Monad.unless ("commit_subject" `elem` names) $
    SQLite.execute_ conn "ALTER TABLE line_verdict ADD COLUMN commit_subject TEXT"
  Monad.unless ("reply" `elem` names) $
    SQLite.execute_ conn "ALTER TABLE line_verdict ADD COLUMN reply TEXT"
  Monad.unless ("ref" `elem` names) $
    SQLite.execute_ conn "ALTER TABLE line_verdict ADD COLUMN ref TEXT"
  stepCols <-
    SQLite.query_ conn "PRAGMA table_info(tour_step)" ::
      IO [(Int, T.Text, T.Text, Int, Maybe T.Text, Int)]
  Monad.unless ("title" `elem` map (\(_, name, _, _, _, _) -> name) stepCols) $
    SQLite.execute_ conn "ALTER TABLE tour_step ADD COLUMN title TEXT"
  postCols <-
    SQLite.query_ conn "PRAGMA table_info(note_post)" ::
      IO [(Int, T.Text, T.Text, Int, Maybe T.Text, Int)]
  let
    postNames = map (\(_, name, _, _, _, _) -> name) postCols
  -- one-time carry-over from the old note/reply column pair into the
  -- conversation table: note becomes the opening reviewer post, reply
  -- the agent's answer after it
  [SQLite.Only posts] <- SQLite.query_ conn "SELECT COUNT(*) FROM note_post" :: IO [SQLite.Only Int]
  [SQLite.Only oldNotes] <-
    SQLite.query_
      conn
      "SELECT COUNT(*) FROM line_verdict\
      \ WHERE note IS NOT NULL OR reply IS NOT NULL" ::
      IO [SQLite.Only Int]
  Monad.when (posts == 0 && oldNotes > 0) $
    SQLite.withTransaction conn $ do
      SQLite.execute_
        conn
        "INSERT INTO note_post (repo, key, seq, author, body, created_at)\
        \ SELECT repo, key, 0, 'reviewer', note, updated_at\
        \ FROM line_verdict WHERE note IS NOT NULL"
      SQLite.execute_
        conn
        "INSERT INTO note_post (repo, key, seq, author, body, created_at)\
        \ SELECT repo, key, 1, 'agent', reply, updated_at\
        \ FROM line_verdict WHERE reply IS NOT NULL"
      SQLite.execute_ conn "UPDATE line_verdict SET note = NULL, reply = NULL"
  -- last, so that anything the carry-over above just inserted is named
  -- too rather than left without a group
  Monad.unless ("group_id" `elem` postNames) $ do
    SQLite.execute_ conn "ALTER TABLE note_post ADD COLUMN group_id TEXT"
    nameExistingGroups conn

recordView :: SQLite.Connection -> T.Text -> T.Text -> IO ()
recordView conn repo mode =
  SQLite.execute
    conn
    "INSERT INTO session_view (repo, mode) VALUES (?, ?)\
    \ ON CONFLICT(repo) DO UPDATE SET mode = excluded.mode, since = datetime('now')"
    (repo, mode)

sessionView :: SQLite.Connection -> T.Text -> IO (Maybe T.Text)
sessionView conn repo = do
  rows <-
    SQLite.query conn "SELECT mode FROM session_view WHERE repo = ?" (SQLite.Only repo) ::
      IO [SQLite.Only T.Text]
  pure (Maybe.listToMaybe (map SQLite.fromOnly rows))

cachedRepoName :: SQLite.Connection -> T.Text -> IO (Maybe T.Text)
cachedRepoName conn url = do
  rows <-
    SQLite.query conn "SELECT name_with_owner FROM github_repo WHERE origin_url = ?" (SQLite.Only url) ::
      IO [SQLite.Only T.Text]
  pure (Maybe.listToMaybe (map SQLite.fromOnly rows))

saveRepoName :: SQLite.Connection -> T.Text -> T.Text -> IO ()
saveRepoName conn url name =
  SQLite.execute
    conn
    "INSERT INTO github_repo (origin_url, name_with_owner) VALUES (?, ?)\
    \ ON CONFLICT(origin_url) DO UPDATE SET name_with_owner = excluded.name_with_owner, resolved_at = datetime('now')"
    (url, name)

data TourStep = TourStep
  { tourSeq :: Int
  , tourAuthor :: T.Text
  , tourTitle :: Maybe T.Text
  , tourNote :: T.Text
  , tourKeys :: [T.Text]
  }
  deriving (Eq, Show)

tourSubjects :: SQLite.Connection -> T.Text -> IO [T.Text]
tourSubjects conn repo = do
  rows <-
    SQLite.query
      conn
      "SELECT subject FROM tour_step WHERE repo = ?\
      \ UNION SELECT subject FROM tour_request WHERE repo = ?"
      (repo, repo) ::
      IO [SQLite.Only T.Text]
  pure (map SQLite.fromOnly rows)

clearTour :: SQLite.Connection -> T.Text -> T.Text -> IO ()
clearTour conn repo subject =
  SQLite.withTransaction conn $ do
    SQLite.execute conn "DELETE FROM tour_hunk WHERE repo = ? AND subject = ?" (repo, subject)
    SQLite.execute conn "DELETE FROM tour_step WHERE repo = ? AND subject = ?" (repo, subject)

appendTourStep :: SQLite.Connection -> T.Text -> T.Text -> T.Text -> Maybe T.Text -> T.Text -> [T.Text] -> IO Int
appendTourStep conn repo subject author title note keys =
  SQLite.withTransaction conn $ do
    rows <-
      SQLite.query
        conn
        "SELECT COALESCE(MAX(seq), 0) FROM tour_step WHERE repo = ? AND subject = ?"
        (repo, subject) ::
        IO [SQLite.Only Int]
    let
      seq_ = maybe 1 ((+ 1) . SQLite.fromOnly) (Maybe.listToMaybe rows)
    SQLite.execute
      conn
      "INSERT INTO tour_step (repo, subject, seq, author, title, note) VALUES (?, ?, ?, ?, ?, ?)"
      (repo, subject, seq_, author, title, note)
    SQLite.executeMany
      conn
      "INSERT OR IGNORE INTO tour_hunk (repo, subject, seq, key) VALUES (?, ?, ?, ?)"
      (map (\k -> (repo, subject, seq_, k)) keys)
    pure seq_

loadTour :: SQLite.Connection -> T.Text -> T.Text -> IO [TourStep]
loadTour conn repo subject = do
  steps <-
    SQLite.query
      conn
      "SELECT seq, author, title, note FROM tour_step WHERE repo = ? AND subject = ? ORDER BY seq"
      (repo, subject) ::
      IO [(Int, T.Text, Maybe T.Text, T.Text)]
  keys <-
    SQLite.query
      conn
      "SELECT seq, key FROM tour_hunk WHERE repo = ? AND subject = ?"
      (repo, subject) ::
      IO [(Int, T.Text)]
  let
    bySeq = Map.fromListWith (++) (map (\(s, k) -> (s, [k])) keys)
    toStep (s, a, t, n) =
      TourStep {tourSeq = s, tourAuthor = a, tourTitle = t, tourNote = n, tourKeys = Map.findWithDefault [] s bySeq}
  pure (map toStep steps)

data TourRequest = TourRequest
  { requestSubject :: T.Text
  , requestMode :: T.Text
  , requestPrompt :: T.Text
  , requestAskedAt :: T.Text
  }
  deriving (Eq, Show)

askTour :: SQLite.Connection -> T.Text -> T.Text -> T.Text -> T.Text -> IO ()
askTour conn repo subject mode prompt =
  SQLite.execute
    conn
    "INSERT INTO tour_request (repo, subject, mode, prompt) VALUES (?, ?, ?, ?)\
    \ ON CONFLICT(repo, subject) DO UPDATE SET\
    \   mode = excluded.mode, prompt = excluded.prompt, asked_at = datetime('now')"
    (repo, subject, mode, prompt)

tourRequests :: SQLite.Connection -> T.Text -> IO [TourRequest]
tourRequests conn repo = do
  rows <-
    SQLite.query
      conn
      "SELECT subject, mode, prompt, asked_at FROM tour_request WHERE repo = ? ORDER BY asked_at"
      (SQLite.Only repo)
  pure (map (\(s, m, p, at) -> TourRequest {requestSubject = s, requestMode = m, requestPrompt = p, requestAskedAt = at}) rows)

clearTourRequest :: SQLite.Connection -> T.Text -> T.Text -> IO ()
clearTourRequest conn repo subject =
  SQLite.execute conn "DELETE FROM tour_request WHERE repo = ? AND subject = ?" (repo, subject)

rememberWorktree :: SQLite.Connection -> T.Text -> Int -> T.Text -> IO ()
rememberWorktree conn repo pull path =
  SQLite.execute
    conn
    "INSERT INTO pull_worktree (repo, pull, path) VALUES (?, ?, ?)\
    \ ON CONFLICT(repo, pull) DO UPDATE SET path = excluded.path"
    (repo, pull, path)

forgetWorktree :: SQLite.Connection -> T.Text -> Int -> IO ()
forgetWorktree conn repo pull =
  SQLite.execute conn "DELETE FROM pull_worktree WHERE repo = ? AND pull = ?" (repo, pull)

pullWorktree :: SQLite.Connection -> T.Text -> Int -> IO (Maybe T.Text)
pullWorktree conn repo pull = do
  rows <-
    SQLite.query conn "SELECT path FROM pull_worktree WHERE repo = ? AND pull = ?" (repo, pull) ::
      IO [SQLite.Only T.Text]
  pure (Maybe.listToMaybe (map SQLite.fromOnly rows))

worktreePull :: SQLite.Connection -> T.Text -> IO (Maybe (T.Text, Int))
worktreePull conn path = do
  rows <-
    SQLite.query conn "SELECT repo, pull FROM pull_worktree WHERE path = ?" (SQLite.Only path) ::
      IO [(T.Text, Int)]
  pure (Maybe.listToMaybe rows)

originalOf :: SQLite.Connection -> T.Text -> IO T.Text
originalOf conn path = fmap (maybe path fst) (worktreePull conn path)

recordMoveBetween :: SQLite.Connection -> T.Text -> T.Text -> IO ()
recordMoveBetween conn from to = do
  fromGroup <- worktreePull conn from
  toGroup <- worktreePull conn to
  let
    original = maybe from fst fromGroup
    sameGroup = from /= to && maybe to fst toGroup == original
    pull = fmap snd (case toGroup of Just _ -> toGroup; Nothing -> fromGroup)
  case (sameGroup, pull) of
    (True, Just n) ->
      SQLite.execute
        conn
        "INSERT INTO review_move (repo, pull, to_path) VALUES (?, ?, ?)\
        \ ON CONFLICT(repo) DO UPDATE SET pull = excluded.pull, to_path = excluded.to_path, moved_at = datetime('now')"
        (original, n, to)
    _ -> pure ()

data ReviewMove = ReviewMove
  { moveRepo :: T.Text
  , movePull :: Int
  , moveTo :: T.Text
  , moveAt :: T.Text
  }
  deriving (Eq, Ord, Show)

reviewMoveFor :: SQLite.Connection -> T.Text -> IO (Maybe ReviewMove)
reviewMoveFor conn path = do
  original <- originalOf conn path
  rows <-
    SQLite.query conn "SELECT repo, pull, to_path, moved_at FROM review_move WHERE repo = ?" (SQLite.Only original) ::
      IO [(T.Text, Int, T.Text, T.Text)]
  pure (fmap (\(r, n, to, at) -> ReviewMove {moveRepo = r, movePull = n, moveTo = to, moveAt = at}) (Maybe.listToMaybe rows))

loadVerdicts :: SQLite.Connection -> T.Text -> IO (Map.Map T.Text Verdict)
loadVerdicts conn repo = do
  rows <-
    SQLite.query conn "SELECT key, verdict FROM line_verdict WHERE repo = ?" (SQLite.Only repo)
  let
    parseRow (k, vt) = fmap (\v -> (k, v)) (parseVerdict vt)
  pure . Map.fromList . Maybe.mapMaybe parseRow $ rows

beginPass :: SQLite.Connection -> T.Text -> T.Text -> IO DataInt.Int64
beginPass conn repo scope = do
  SQLite.execute conn "INSERT INTO pass (repo, scope) VALUES (?, ?)" (repo, scope)
  SQLite.lastInsertRowId conn

finishPass :: SQLite.Connection -> DataInt.Int64 -> IO ()
finishPass conn passId =
  SQLite.execute
    conn
    "UPDATE pass SET finished_at = datetime('now') WHERE id = ?"
    (SQLite.Only passId)

-- Re-verdicting a line updates its row in place, so updated_at reads as
-- the last time it was looked at; a note already on the line survives.
-- The commit context (sha, subject) is stamped when the review knows
-- which commit it is looking at — a rebase stop, or the tip of a ref
-- under review, which also records the ref — and is NULL otherwise.
data VerdictOrigin = VerdictOrigin
  { originCommit :: Maybe (T.Text, T.Text)
  , originRef :: Maybe T.Text
  }

noOrigin :: VerdictOrigin
noOrigin = VerdictOrigin {originCommit = Nothing, originRef = Nothing}

recordVerdicts ::
  SQLite.Connection ->
  T.Text ->
  DataInt.Int64 ->
  Verdict ->
  VerdictOrigin ->
  [(T.Text, T.Text, T.Text, Int)] ->
  IO ()
recordVerdicts conn repo passId v origin rows =
  let
    (sha, subject) = case originCommit origin of
      Just (s, subj) -> (Just s, Just subj)
      Nothing -> (Nothing, Nothing)
    toParams (key, file, line, lineNo) =
      (key, repo, file, line, lineNo, verdictText v, passId, sha, subject, originRef origin)
  in
    SQLite.withTransaction conn $ do
      SQLite.executeMany
        conn
        "INSERT INTO line_verdict\
        \ (key, repo, file, line, line_no, verdict, pass_id,\
        \  commit_sha, commit_subject, ref, updated_at)\
        \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, datetime('now'))\
        \ ON CONFLICT(key) DO UPDATE SET\
        \   repo = excluded.repo, file = excluded.file,\
        \   line = excluded.line, line_no = excluded.line_no,\
        \   verdict = excluded.verdict, pass_id = excluded.pass_id,\
        \   commit_sha = excluded.commit_sha,\
        \   commit_subject = excluded.commit_subject,\
        \   ref = excluded.ref,\
        \   updated_at = excluded.updated_at"
        (map toParams rows)
      -- accepting closes the conversation; other verdicts keep it open
      Monad.when (v == Reviewed) $
        mapM_
          ( \ks ->
              let
                holes = mconcat (replicate (length ks - 1) ", ?")
              in
                SQLite.execute
                  conn
                  (SQLite.Query ("DELETE FROM note_post WHERE repo = ? AND key IN (?" <> holes <> ")"))
                  (SQLite.toRow (SQLite.Only repo) <> SQLite.toRow ks)
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
  Directory.createDirectoryIfMissing True dir
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

presenceWord :: Presence -> T.Text
presenceWord p = case p of
  Waiting -> "waiting"
  Working -> "working"

-- Announce what this process is doing about these repositories. Called
-- again on a beat: the file's timestamp is the heartbeat, so a listener
-- can tell somebody still here from somebody gone without being told.
markPresence :: Presence -> [T.Text] -> IO ()
markPresence what repos = do
  dir <- waitingDirectory
  pid <- Posix.getProcessID
  ignoringIOErrors
    (TIO.writeFile (dir </> show pid) (T.unlines (presenceWord what : repos)))

clearWaiting :: IO ()
clearWaiting = do
  dir <- waitingDirectory
  pid <- Posix.getProcessID
  ignoringIOErrors (Directory.removeFile (dir </> show pid))

-- Which repositories somebody is waiting on, and which somebody is
-- working on, right now.
--
-- A heartbeat older than a few seconds is somebody who has gone away —
-- killed, disconnected, or a session that ended — which is why this is
-- read from timestamps rather than from whoever remembered to tidy up.
presenceNow :: IO ([T.Text], [T.Text])
presenceNow = do
  dir <- waitingDirectory
  names <- IOError.catchIOError (Directory.listDirectory dir) (const (pure []))
  now <- Clock.getCurrentTime
  found <- mapM (freshRepos dir now) names
  let
    ofKind k = concatMap snd (filter ((== k) . fst) found)
  pure (ofKind Waiting, ofKind Working)

waitingStaleAfter :: Clock.NominalDiffTime
waitingStaleAfter = 4

freshRepos :: FilePath -> Clock.UTCTime -> FilePath -> IO (Presence, [T.Text])
freshRepos dir now name =
  let
    path = dir </> name
    nothing = (Working, [])
  in
    flip IOError.catchIOError (const (pure nothing)) $ do
      touched <- Directory.getModificationTime path
      if Clock.diffUTCTime now touched > waitingStaleAfter
        then pure nothing
        else do
          body <- TIO.readFile path
          pure $ case filter (not . T.null) (T.lines body) of
            (w : repos) ->
              (if w == presenceWord Waiting then Waiting else Working, repos)
            [] -> nothing

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors act = IOError.catchIOError act (const (pure ()))

-- Say whether this repository has a note being written into it.
setComposing :: SQLite.Connection -> T.Text -> Bool -> IO ()
setComposing conn repo composing =
  if composing
    then
      SQLite.execute
        conn
        "INSERT INTO note_composing (repo) VALUES (?)\
        \ ON CONFLICT(repo) DO UPDATE SET since = datetime('now')"
        (SQLite.Only repo)
    else SQLite.execute conn "DELETE FROM note_composing WHERE repo = ?" (SQLite.Only repo)

-- Whether any of these repositories is mid-note.
--
-- Bounded by age as well as by the marker: a session killed with its
-- popup open leaves the row behind, and an agent that waited on it would
-- wait for as long as nobody noticed. Ten minutes is far longer than
-- writing a note and far shorter than caring.
composingNow :: SQLite.Connection -> [T.Text] -> IO Bool
composingNow conn repos = do
  rows <-
    mapM
      ( \repo ->
          SQLite.query
            conn
            "SELECT 1 FROM note_composing\
            \ WHERE repo = ? AND since > datetime('now', '-10 minutes')"
            (SQLite.Only repo) ::
            IO [SQLite.Only Int]
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
nameExistingGroups :: SQLite.Connection -> IO ()
nameExistingGroups conn = SQLite.withTransaction conn $ do
  rows <-
    SQLite.query_
      conn
      "SELECT repo, key, author, body FROM note_post ORDER BY repo, key, seq" ::
      IO [(T.Text, T.Text, T.Text, T.Text)]
  places <-
    SQLite.query_ conn "SELECT repo, key, file FROM line_verdict" ::
      IO [(T.Text, T.Text, T.Text)]
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
      map (\key -> (Key.noteGroupId repo keys, repo, key)) keys
  mapM_
    ( \(gid, repo, key) ->
        SQLite.execute
          conn
          "UPDATE note_post SET group_id = ? WHERE repo = ? AND key = ?"
          (gid, repo, key)
    )
    (concatMap name (Map.toList grouped))

loadThreads :: SQLite.Connection -> T.Text -> IO (Map.Map T.Text [Post])
loadThreads conn repo = do
  rows <-
    SQLite.query
      conn
      "SELECT key, author, body FROM note_post\
      \ WHERE repo = ? ORDER BY key, seq"
      (SQLite.Only repo)
  let
    add (key, author, body) =
      Map.insertWith (flip (++)) key [Post author body]
  pure (foldr add Map.empty (reverse rows))

-- Which named group each key's conversation belongs to. The session
-- needs this to tell one note from another note worded the same.
loadNoteGroups :: SQLite.Connection -> T.Text -> IO (Map.Map T.Text T.Text)
loadNoteGroups conn repo = do
  rows <-
    SQLite.query
      conn
      "SELECT DISTINCT key, group_id FROM note_post\
      \ WHERE repo = ? AND group_id IS NOT NULL"
      (SQLite.Only repo) ::
      IO [(T.Text, T.Text)]
  pure (Map.fromList rows)

-- Drop the whole conversation from some lines. Returns how many lines
-- actually had one.
clearThread :: SQLite.Connection -> T.Text -> [T.Text] -> IO Int
clearThread conn repo keys =
  let
    batch ks = do
      let
        holes = mconcat (replicate (length ks - 1) ", ?")
      cleared <-
        SQLite.query
          conn
          (SQLite.Query ("SELECT COUNT(DISTINCT key) FROM note_post WHERE repo = ? AND key IN (?" <> holes <> ")"))
          (SQLite.toRow (SQLite.Only repo) <> SQLite.toRow ks) ::
          IO [SQLite.Only Int]
      SQLite.execute
        conn
        (SQLite.Query ("DELETE FROM note_post WHERE repo = ? AND key IN (?" <> holes <> ")"))
        (SQLite.toRow (SQLite.Only repo) <> SQLite.toRow ks)
      pure $ case cleared of
        (SQLite.Only n : _) -> n
        [] -> 0
  in
    SQLite.withTransaction conn $ do
      counts <- mapM batch (chunks 400 keys)
      pure (sum counts)

-- Close every answered conversation at once: a thread whose newest post
-- came from someone other than the reviewer clears entirely. Returns
-- how many lines.
resolveAnswered :: SQLite.Connection -> T.Text -> IO Int
resolveAnswered conn repo = do
  threads <- loadThreads conn repo
  let
    answered = answeredKeys threads
  case answered of
    [] -> pure 0
    keys -> clearThread conn repo keys

answeredKeys :: Map.Map T.Text [Post] -> [T.Text]
answeredKeys threads =
  Map.keys (Map.filter (maybe False (not . isReviewerPost) . Maybe.listToMaybe . reverse) threads)

newtype GroupId = GroupId {groupIdToText :: T.Text}
  deriving (Eq, Ord, Show)

newtype LineKey = LineKey {lineKeyToText :: T.Text}
  deriving (Eq, Ord, Show)

data PunchRow = PunchRow
  { punchKey :: LineKey
  , punchFile :: T.Text
  , punchVerdict :: Verdict
  , punchLine :: T.Text
  , punchLineNo :: Maybe Int
  , punchThread :: [Post]
  , punchCommitSha :: Maybe T.Text
  , punchCommitSubject :: Maybe T.Text
  , punchRef :: Maybe T.Text
  , punchAt :: T.Text
  }

-- Every flagged or questioned line for the repo, grouped by file.
punchRows :: SQLite.Connection -> T.Text -> IO [PunchRow]
punchRows conn repo = do
  rows <-
    SQLite.query
      conn
      "SELECT key, file, verdict, line, line_no,\
      \ commit_sha, commit_subject, ref, updated_at FROM line_verdict\
      \ WHERE repo = ? AND verdict <> 'reviewed'\
      \ ORDER BY file, line_no, updated_at DESC"
      (SQLite.Only repo)
  threads <- loadThreads conn repo
  let
    toPunch (key, file, verdictName, line, lineNo, sha, subject, ref, at) =
      fmap
        ( \verdict ->
            PunchRow
              { punchKey = LineKey key
              , punchFile = file
              , punchVerdict = verdict
              , punchLine = line
              , punchLineNo = lineNo
              , punchThread = Map.findWithDefault [] key threads
              , punchCommitSha = sha
              , punchCommitSubject = subject
              , punchRef = ref
              , punchAt = at
              }
        )
        (parseVerdict verdictName)
  pure (Maybe.mapMaybe toPunch rows)

repoRows :: SQLite.Connection -> T.Text -> IO [(T.Text, T.Text)]
repoRows conn repo =
  SQLite.query conn "SELECT key, file FROM line_verdict WHERE repo = ?" (SQLite.Only repo)

-- Forget the verdicts for particular line keys (unmarking a hunk or a
-- selection of lines).
deleteKeys :: SQLite.Connection -> T.Text -> [T.Text] -> IO ()
deleteKeys conn repo keys =
  let
    batch ks =
      let
        holes = mconcat (replicate (length ks - 1) ", ?")
      in
        do
          SQLite.execute
            conn
            (SQLite.Query ("DELETE FROM line_verdict WHERE repo = ? AND key IN (?" <> holes <> ")"))
            (SQLite.toRow (SQLite.Only repo) <> SQLite.toRow ks)
          SQLite.execute
            conn
            (SQLite.Query ("DELETE FROM note_post WHERE repo = ? AND key IN (?" <> holes <> ")"))
            (SQLite.toRow (SQLite.Only repo) <> SQLite.toRow ks)
  in
    SQLite.withTransaction conn $
      mapM_ batch (chunks 400 keys)

-- Batched so a repository with thousands of recorded lines stays inside
-- sqlite's limit on parameters per statement.
deleteFiles :: SQLite.Connection -> T.Text -> [T.Text] -> IO ()
deleteFiles conn repo files =
  let
    batch fs =
      let
        holes = mconcat (replicate (length fs - 1) ", ?") -- first hole below
      in
        do
          SQLite.execute
            conn
            ( SQLite.Query
                ( "DELETE FROM note_post WHERE repo = ? AND key IN\
                  \ (SELECT key FROM line_verdict WHERE repo = ? AND file IN (?"
                    <> holes
                    <> "))"
                )
            )
            (SQLite.toRow (repo, repo) <> SQLite.toRow fs)
          SQLite.execute
            conn
            (SQLite.Query ("DELETE FROM line_verdict WHERE repo = ? AND file IN (?" <> holes <> ")"))
            (SQLite.toRow (SQLite.Only repo) <> SQLite.toRow fs)
  in
    SQLite.withTransaction conn $
      mapM_ batch (chunks 400 files)

newtype PullNumber = PullNumber {pullNumberToInt :: Int}
  deriving (Eq, Ord, Show)

newtype RemoteThreadId = RemoteThreadId {remoteThreadIdToText :: T.Text}
  deriving (Eq, Ord, Show)

newtype CommentId = CommentId {commentIdToInt64 :: DataInt.Int64}
  deriving (Eq, Ord, Show)

newtype ReviewId = ReviewId {reviewIdToInt64 :: DataInt.Int64}
  deriving (Eq, Ord, Show)

data Resolution = Unresolved | Resolved
  deriving (Eq, Show)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)
