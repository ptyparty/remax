{-# LANGUAGE OverloadedStrings #-}

-- Where verdicts are remembered: the XDG state directory (usually
-- ~/.local/state/remax/review.db).
-- One row per changed line, keyed by content (see Remax.Key), with the
-- verdict it was given, the pass that gave it, and the line's position
-- at the time (advisory — content keys are the identity; the number is
-- a courtesy for humans and agents reading the punch list). Notes are a
-- conversation: ordered posts with an author, kept per line key in
-- note_post (every key of a noted group carries the same thread, the
-- same way the old single note column did). Passes record when each
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
  , saveReviewerPost
  , appendAgentPost
  , clearThread
  , resolveAnswered
  , storeDirectory
  , deleteKeys
  , PunchRow (..)
  , punchRows
  , repoRows
  , deleteFiles
  ) where

import Control.Monad (unless, void, when)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Database.SQLite.Simple
import System.Directory (XdgDirectory (XdgState), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))

data Verdict = Reviewed | Flagged | Questioned
  deriving (Eq, Ord, Show)

-- One entry in a note conversation. Threads are ordered lists of posts;
-- the author is "reviewer" for the human in the session, or the name an
-- agent gave on the command line.
data Post = Post
  { postAuthor :: Text
  , postBody :: Text
  }
  deriving (Eq, Show)

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
    \  PRIMARY KEY (repo, key, seq))"
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
                "INSERT INTO note_post (repo, key, seq, author, body)\
                \ VALUES (?, ?, ?, 'reviewer', ?)"
                (repo, key, seq_, body)
      )
      marked
    pure (length marked)

-- An agent's post, addressed by any line key of a noted group; it lands
-- on every key carrying the same thread in the same file, so the
-- session shows one conversation under the group. A key without a
-- thread takes the post alone. Returns how many lines it landed on.
appendAgentPost :: Connection -> Text -> Text -> Text -> Text -> IO Int
appendAgentPost conn repo author key body = do
  present <-
    query
      conn
      "SELECT COUNT(*) FROM line_verdict WHERE repo = ? AND key = ?"
      (repo, key) ::
      IO [Only Int]
  case present of
    (Only 0 : _) -> pure 0
    _ -> do
      threads <- loadThreads conn repo
      let
        groupKeys = case Map.lookup key threads of
          Nothing -> [key]
          Just thread ->
            Map.keys (Map.filter (== thread) threads)
      withTransaction conn $
        mapM_
          ( \k -> do
              seq_ <- nextSeq conn repo k
              execute
                conn
                "INSERT INTO note_post (repo, key, seq, author, body)\
                \ VALUES (?, ?, ?, ?, ?)"
                (repo, k, seq_, author, body)
          )
          groupKeys
      pure (length groupKeys)

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

data PunchRow = PunchRow
  { punchKey :: Text
  , punchFile :: Text
  , punchVerdict :: Text
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
  let
    toPunch (key, file, verdict, line, lineNo, sha, subject, at) =
      PunchRow
        { punchKey = key
        , punchFile = file
        , punchVerdict = verdict
        , punchLine = line
        , punchLineNo = lineNo
        , punchThread = Map.findWithDefault [] key threads
        , punchCommitSha = sha
        , punchCommitSubject = subject
        , punchAt = at
        }
  pure (map toPunch rows)

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
