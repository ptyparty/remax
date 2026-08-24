{-# LANGUAGE OverloadedStrings #-}

-- Where verdicts are remembered: ~/.local/state/remax/review.db.
-- One row per changed line, keyed by content (see Remax.Key), with the
-- verdict it was given, the pass that gave it, an optional reviewer note,
-- and the line's position at the time (advisory — content keys are the
-- identity; the number is a courtesy for humans and agents reading the
-- punch list). Passes record when each review ran, so "what did I review
-- yesterday" stays answerable.
module Remax.Store
  ( Verdict (..)
  , verdictText
  , parseVerdict
  , openStore
  , loadVerdicts
  , beginPass
  , finishPass
  , recordVerdicts
  , recordNotes
  , clearNotes
  , loadNotes
  , loadReplies
  , replyToNote
  , storeDirectory
  , deleteKeys
  , PunchRow (..)
  , punchRows
  , repoRows
  , deleteFiles
  ) where

import Control.Monad (unless, void)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Database.SQLite.Simple
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))

data Verdict = Reviewed | Flagged | Questioned
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
storeDirectory = do
  home <- getHomeDirectory
  pure (home </> ".local" </> "state" </> "remax")

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
  migrate conn
  pure conn

-- Databases created before a column existed pick it up here.
migrate :: Connection -> IO ()
migrate conn = do
  cols <-
    query_ conn "PRAGMA table_info(line_verdict)"
      :: IO [(Int, Text, Text, Int, Maybe Text, Int)]
  let names = map (\(_, name, _, _, _, _) -> name) cols
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

loadVerdicts :: Connection -> Text -> IO (Map Text Verdict)
loadVerdicts conn repo = do
  rows <-
    query conn "SELECT key, verdict FROM line_verdict WHERE repo = ?" (Only repo)
  let parseRow (k, vt) = fmap (\v -> (k, v)) (parseVerdict vt)
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
recordVerdicts
  :: Connection
  -> Text
  -> Int64
  -> Verdict
  -> Maybe (Text, Text)
  -> [(Text, Text, Text, Int)]
  -> IO ()
recordVerdicts conn repo passId v mCommit rows =
  let (sha, subject) = case mCommit of
        Just (s, subj) -> (Just s, Just subj)
        Nothing -> (Nothing, Nothing)
      toParams (key, file, line, lineNo) =
        (key, repo, file, line, lineNo, verdictText v, passId, sha, subject)
  in withTransaction conn $
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

-- Attach a reviewer note to lines that already carry verdicts. Returns
-- how many rows the note landed on. Writing a note clears any standing
-- agent reply — the question changed, so the old answer is stale.
recordNotes :: Connection -> Text -> Text -> [Text] -> IO Int
recordNotes conn repo note keys =
  let batch ks = do
        let holes = mconcat (replicate (length ks - 1) ", ?")
        execute
          conn
          (Query ("UPDATE line_verdict SET note = ?, reply = NULL WHERE repo = ? AND key IN (?" <> holes <> ")"))
          (toRow (note, repo) <> toRow ks)
        changes conn
  in withTransaction conn $ do
       counts <- mapM batch (chunks 400 keys)
       pure (sum counts)

-- Take the notes back off some lines. Returns how many rows changed.
clearNotes :: Connection -> Text -> [Text] -> IO Int
clearNotes conn repo keys =
  let batch ks = do
        let holes = mconcat (replicate (length ks - 1) ", ?")
        execute
          conn
          (Query ("UPDATE line_verdict SET note = NULL, reply = NULL WHERE repo = ? AND note IS NOT NULL AND key IN (?" <> holes <> ")"))
          (toRow (Only repo) <> toRow ks)
        changes conn
  in withTransaction conn $ do
       counts <- mapM batch (chunks 400 keys)
       pure (sum counts)

-- Every note in the repo, by line key — the session's in-memory view.
loadNotes :: Connection -> Text -> IO (Map Text Text)
loadNotes conn repo = do
  rows <-
    query
      conn
      "SELECT key, note FROM line_verdict WHERE repo = ? AND note IS NOT NULL"
      (Only repo)
  pure (Map.fromList rows)

-- Every agent reply in the repo, by line key.
loadReplies :: Connection -> Text -> IO (Map Text Text)
loadReplies conn repo = do
  rows <-
    query
      conn
      "SELECT key, reply FROM line_verdict WHERE repo = ? AND reply IS NOT NULL"
      (Only repo)
  pure (Map.fromList rows)

-- An agent's answer to a note, addressed by any line key of the noted
-- group; the reply lands on every line sharing that note, so the
-- session shows one reply under the group. A key without a note takes
-- the reply alone. Returns how many rows it landed on.
replyToNote :: Connection -> Text -> Text -> Text -> IO Int
replyToNote conn repo key text = do
  rows <-
    query
      conn
      "SELECT file, note FROM line_verdict WHERE repo = ? AND key = ?"
      (repo, key)
      :: IO [(Text, Maybe Text)]
  case rows of
    [] -> pure 0
    ((file, mNote) : _) -> do
      case mNote of
        Just note ->
          execute
            conn
            "UPDATE line_verdict SET reply = ? WHERE repo = ? AND file = ? AND note = ?"
            (text, repo, file, note)
        Nothing ->
          execute
            conn
            "UPDATE line_verdict SET reply = ? WHERE repo = ? AND key = ?"
            (text, repo, key)
      changes conn

data PunchRow = PunchRow
  { punchKey :: Text
  , punchFile :: Text
  , punchVerdict :: Text
  , punchLine :: Text
  , punchLineNo :: Maybe Int
  , punchNote :: Maybe Text
  , punchReply :: Maybe Text
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
      "SELECT key, file, verdict, line, line_no, note, reply,\
      \ commit_sha, commit_subject, updated_at FROM line_verdict\
      \ WHERE repo = ? AND verdict <> 'reviewed'\
      \ ORDER BY file, line_no, updated_at DESC"
      (Only repo)
  let toPunch ((key, file, verdict, line, lineNo, note) :. (reply, sha, subject, at)) =
        PunchRow
          { punchKey = key
          , punchFile = file
          , punchVerdict = verdict
          , punchLine = line
          , punchLineNo = lineNo
          , punchNote = note
          , punchReply = reply
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
  let batch ks =
        let holes = mconcat (replicate (length ks - 1) ", ?")
        in execute
             conn
             (Query ("DELETE FROM line_verdict WHERE repo = ? AND key IN (?" <> holes <> ")"))
             (toRow (Only repo) <> toRow ks)
  in withTransaction conn $
       mapM_ batch (chunks 400 keys)

-- Batched so a repository with thousands of recorded lines stays inside
-- sqlite's limit on parameters per statement.
deleteFiles :: Connection -> Text -> [Text] -> IO ()
deleteFiles conn repo files =
  let batch fs =
        let holes = mconcat (replicate (length fs - 1) ", ?") -- first hole below
        in execute
             conn
             (Query ("DELETE FROM line_verdict WHERE repo = ? AND file IN (?" <> holes <> ")"))
             (toRow (Only repo) <> toRow fs)
  in withTransaction conn $
       mapM_ batch (chunks 400 files)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)
