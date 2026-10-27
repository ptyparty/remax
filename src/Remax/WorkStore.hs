{-# LANGUAGE OverloadedStrings #-}

module Remax.WorkStore
  ( Scope (..)
  , loadWork
  , addPending
  , addAside
  , setProposal
  , addEdit
  , editWork
  , discardWork
  , takeWork
  , recordSubmitted
  , growNote
  , retireNotesOn
  , forgetWork
  , newLocalId
  ) where

import Control.Monad qualified as Monad
import Data.Digest.Pure.SHA qualified as SHA
import Data.Int qualified as DataInt
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Time.Clock qualified as Clock
import Database.SQLite.Simple qualified as SQLite
import Remax.Conversation qualified as Conv
import Remax.Store qualified as Store

-- | Which repository's work, and which pull request's when one is open.
data Scope = Scope
  { scopeRepo :: T.Text
  , scopePull :: Maybe Int
  }

loadWork :: SQLite.Connection -> Scope -> IO Conv.OwnWork
loadWork conn scope = do
  verdicts <- Store.loadVerdicts conn (scopeRepo scope)
  targets <- loadTargets conn scope
  legacy <- loadLegacyNotes conn (scopeRepo scope)
  let
    legacyTargets = Map.fromList (Maybe.mapMaybe (\t -> fmap (\lid -> (lid, t)) (Conv.targetLocalId t)) (Map.keys legacy))
  items <- loadItems conn scope (Map.union targets legacyTargets)
  submitted <- loadSubmitted conn (scopeRepo scope)
  pure
    Conv.OwnWork
      { Conv.workItems = Map.unionWith (++) legacy items
      , Conv.workVerdicts = Map.fromList (map (\(k, v) -> (Store.LineKey k, v)) (Map.toList verdicts))
      , Conv.workSubmitted = submitted
      }

loadTargets :: SQLite.Connection -> Scope -> IO (Map.Map Conv.LocalId Conv.Target)
loadTargets conn scope = do
  rows <-
    SQLite.query
      conn
      "SELECT local_id, kind, anchor, path, side, commit_sha, verdict FROM work_target WHERE repo = ?"
      (SQLite.Only (scopeRepo scope)) ::
      IO [(T.Text, T.Text, T.Text, T.Text, T.Text, Maybe T.Text, Maybe T.Text)]
  lineRows <-
    SQLite.query
      conn
      "SELECT local_id, key, line_no, line FROM work_target_line WHERE repo = ? ORDER BY local_id, seq"
      (SQLite.Only (scopeRepo scope)) ::
      IO [(T.Text, T.Text, Int, T.Text)]
  let
    linesOf = Map.fromListWith (flip (++)) (map (\(lid, k, n, l) -> (lid, [Conv.SpanLine (Store.LineKey k) n l])) lineRows)
    target (lid, kind, anchor, path, side, sha, verdict) = do
      spanLines <- NEL.nonEmpty (Map.findWithDefault [] lid linesOf)
      let
        lineSpan =
          Conv.LineSpan
            { Conv.spanPath = path
            , Conv.spanSide = Conv.parseSide side
            , Conv.spanLines = spanLines
            , Conv.spanAt = fmap Conv.CommitSha sha
            }
      case (kind, anchor) of
        ("note", _) -> Just (Conv.LocalId lid, Conv.OnLinesOnly (Conv.LocalId lid) lineSpan (Maybe.fromMaybe Store.Questioned (verdict >>= Store.parseVerdict)))
        (_, "lines") -> Just (Conv.LocalId lid, Conv.NewThread (Conv.LocalId lid) (Conv.OnLines lineSpan))
        _ -> Nothing
    pullTarget (lid, kind, anchor, _, _, _, _) =
      if kind == "new" && anchor == "pull"
        then Just (Conv.LocalId lid, Conv.NewThread (Conv.LocalId lid) Conv.OnPull)
        else Nothing
  pure (Map.fromList (Maybe.mapMaybe target rows ++ Maybe.mapMaybe pullTarget rows))

loadItems :: SQLite.Connection -> Scope -> Map.Map Conv.LocalId Conv.Target -> IO (Map.Map Conv.Target [(Conv.WorkId, Conv.Work)])
loadItems conn scope targets = do
  rows <-
    SQLite.query
      conn
      "SELECT id, target_kind, target_id, kind, author, body, comment_id, created_at FROM work\
      \ WHERE repo = ? AND (pull IS ? OR pull IS NULL) ORDER BY id"
      (scopeRepo scope, scopePull scope) ::
      IO [(Int, T.Text, T.Text, T.Text, T.Text, T.Text, Maybe DataInt.Int64, T.Text)]
  let
    item (wid, tkind, tid, kind, author, body, cid, at) = do
      target <- case tkind of
        "thread" -> Just (Conv.OnThread (Store.RemoteThreadId tid))
        "comment" -> fmap (Conv.OnComment . Store.CommentId) (readInt64 tid)
        _ -> Map.lookup (Conv.LocalId tid) targets
      work <- case kind of
        "proposed" -> case Conv.parseAuthor author of
          Conv.Agent name -> Just (Conv.Proposed name (Conv.Markdown body))
          _ -> Nothing
        "pending" -> Just (Conv.Pending (Conv.parseAuthor author) (Conv.Markdown body))
        "aside" -> Just (Conv.Aside (Conv.parseAuthor author) (Conv.Markdown body) (isoStamp at))
        "edit" -> fmap (\c -> Conv.Edit (Store.CommentId c) (Conv.Markdown body)) cid
        _ -> Nothing
      Just (target, [(Conv.StoredWork wid, work)])
  pure (Map.fromListWith (flip (++)) (Maybe.mapMaybe item rows))

{- | SQLite's datetime('now') reads "2026-10-27 15:04:05"; ages are
taken from the ISO 8601 form GitHub uses.
-}
isoStamp :: T.Text -> T.Text
isoStamp t =
  if T.length t == 19 && T.index t 10 == ' '
    then T.replace " " "T" t <> "Z"
    else t

readInt64 :: T.Text -> Maybe DataInt.Int64
readInt64 t =
  case reads (T.unpack t) of
    [(n, "")] -> Just n
    _ -> Nothing

{- | Notes written by a remax from before this store existed, read in
place: each named group is one note on its keys' lines.
-}
loadLegacyNotes :: SQLite.Connection -> T.Text -> IO (Map.Map Conv.Target [(Conv.WorkId, Conv.Work)])
loadLegacyNotes conn repo = do
  posts <-
    SQLite.query
      conn
      "SELECT group_id, key, seq, author, body, created_at FROM note_post\
      \ WHERE repo = ? AND group_id IS NOT NULL ORDER BY group_id, seq, key"
      (SQLite.Only repo) ::
      IO [(T.Text, T.Text, Int, T.Text, T.Text, T.Text)]
  places <-
    SQLite.query
      conn
      "SELECT key, file, line, line_no, verdict FROM line_verdict WHERE repo = ?"
      (SQLite.Only repo) ::
      IO [(T.Text, T.Text, T.Text, Maybe Int, T.Text)]
  let
    placeOf = Map.fromList (map (\(k, f, l, n, v) -> (k, (f, l, n, v))) places)
    byGroup = Map.fromListWith (flip (++)) (map (\(g, k, s, a, b, at) -> (g, [(k, s, a, b, at)])) posts)
    note (gid, rows) = do
      let
        keys = List.nub (map (\(k, _, _, _, _) -> k) rows)
        placed = Maybe.mapMaybe (\k -> fmap (\p -> (k, p)) (Map.lookup k placeOf)) keys
      (_, (path, _, _, _)) <- Maybe.listToMaybe placed
      spanLines <-
        NEL.nonEmpty
          ( List.sortOn
              Conv.slLineNo
              (map (\(k, (_, l, n, _)) -> Conv.SpanLine (Store.LineKey k) (Maybe.fromMaybe 0 n) l) placed)
          )
      let
        verdicts = Maybe.mapMaybe (\(_, (_, _, _, v)) -> Store.parseVerdict v) placed
        verdict = if Store.Flagged `elem` verdicts then Store.Flagged else Store.Questioned
        side = if all (\l -> T.isPrefixOf "-" (Conv.slText l)) spanLines then Conv.BaseSide else Conv.HeadSide
        lineSpan = Conv.LineSpan {Conv.spanPath = path, Conv.spanSide = side, Conv.spanLines = spanLines, Conv.spanAt = Nothing}
        firstKey = NEL.head spanLines
        onFirst (k, _, _, _, _) = Store.LineKey k == Conv.slKey firstKey
        post (k, s, a, b, at) =
          (Conv.LegacyPost k s, Conv.Aside (if a == "reviewer" then Conv.Viewer else Conv.Agent (Conv.AgentName a)) (Conv.Markdown b) (isoStamp at))
      Just (Conv.OnLinesOnly (Conv.LocalId gid) lineSpan verdict, map post (filter onFirst rows))
  pure (Map.fromList (Maybe.mapMaybe note (Map.toList byGroup)))

loadSubmitted :: SQLite.Connection -> T.Text -> IO (Map.Map Conv.LocalId Store.RemoteThreadId)
loadSubmitted conn repo = do
  rows <-
    SQLite.query conn "SELECT local_id, thread_id FROM work_submitted WHERE repo = ?" (SQLite.Only repo) ::
      IO [(T.Text, T.Text)]
  pure (Map.fromList (map (\(l, t) -> (Conv.LocalId l, Store.RemoteThreadId t)) rows))

targetColumns :: Conv.Target -> (T.Text, T.Text)
targetColumns target =
  case target of
    Conv.OnThread (Store.RemoteThreadId t) -> ("thread", t)
    Conv.OnComment (Store.CommentId c) -> ("comment", T.pack (show c))
    Conv.NewThread (Conv.LocalId l) _ -> ("new", l)
    Conv.OnLinesOnly (Conv.LocalId l) _ _ -> ("note", l)

ensureTarget :: SQLite.Connection -> Scope -> Conv.Target -> IO ()
ensureTarget conn scope target =
  let
    repo = scopeRepo scope
    insertSpan lid kind verdict lineSpan = do
      SQLite.execute
        conn
        "INSERT OR IGNORE INTO work_target (repo, local_id, kind, anchor, path, side, commit_sha, verdict)\
        \ VALUES (?, ?, ?, 'lines', ?, ?, ?, ?)"
        ( repo
        , Conv.localIdToText lid
        , kind :: T.Text
        , Conv.spanPath lineSpan
        , Conv.sideText (Conv.spanSide lineSpan)
        , fmap Conv.commitShaToText (Conv.spanAt lineSpan)
        , fmap Store.verdictText verdict
        )
      SQLite.executeMany
        conn
        "INSERT OR IGNORE INTO work_target_line (repo, local_id, seq, key, line_no, line) VALUES (?, ?, ?, ?, ?, ?)"
        ( zipWith
            (\i l -> (repo, Conv.localIdToText lid, i :: Int, Store.lineKeyToText (Conv.slKey l), Conv.slLineNo l, Conv.slText l))
            [0 ..]
            (NEL.toList (Conv.spanLines lineSpan))
        )
  in
    case target of
      Conv.NewThread lid (Conv.OnLines lineSpan) -> insertSpan lid "new" Nothing lineSpan
      Conv.NewThread lid Conv.OnPull ->
        SQLite.execute
          conn
          "INSERT OR IGNORE INTO work_target (repo, local_id, kind, anchor, path, side) VALUES (?, ?, 'new', 'pull', '', 'head')"
          (repo, Conv.localIdToText lid)
      Conv.OnLinesOnly lid lineSpan verdict -> insertSpan lid "note" (Just verdict) lineSpan
      _ -> pure ()

insertWork :: SQLite.Connection -> Scope -> Conv.Target -> T.Text -> Conv.Author -> Conv.Markdown -> Maybe Store.CommentId -> IO Conv.WorkId
insertWork conn scope target kind author body cid =
  SQLite.withTransaction conn $ do
    ensureTarget conn scope target
    let
      (tkind, tid) = targetColumns target
    SQLite.execute
      conn
      "INSERT INTO work (repo, pull, target_kind, target_id, kind, author, body, comment_id)\
      \ VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
      (scopeRepo scope, scopePull scope, tkind, tid, kind, Conv.authorText author, Conv.markdownText body, fmap Store.commentIdToInt64 cid)
    fmap (Conv.StoredWork . fromIntegral) (SQLite.lastInsertRowId conn)

addPending :: SQLite.Connection -> Scope -> Conv.Target -> Conv.Author -> Conv.Markdown -> IO Conv.WorkId
addPending conn scope target author body = insertWork conn scope target "pending" author body Nothing

addAside :: SQLite.Connection -> Scope -> Conv.Target -> Conv.Author -> Conv.Markdown -> IO Conv.WorkId
addAside conn scope target author body = insertWork conn scope target "aside" author body Nothing

-- | At most one proposal per conversation: the next replaces it.
setProposal :: SQLite.Connection -> Scope -> Conv.Target -> Conv.AgentName -> Conv.Markdown -> IO Conv.WorkId
setProposal conn scope target name body = do
  let
    (tkind, tid) = targetColumns target
  SQLite.execute
    conn
    "DELETE FROM work WHERE repo = ? AND target_kind = ? AND target_id = ? AND kind = 'proposed'"
    (scopeRepo scope, tkind, tid)
  insertWork conn scope target "proposed" (Conv.Agent name) body Nothing

addEdit :: SQLite.Connection -> Scope -> Conv.Target -> Store.CommentId -> Conv.Markdown -> IO Conv.WorkId
addEdit conn scope target cid body = insertWork conn scope target "edit" Conv.Viewer body (Just cid)

editWork :: SQLite.Connection -> Conv.WorkId -> Conv.Markdown -> IO ()
editWork conn wid body =
  case wid of
    Conv.StoredWork i -> SQLite.execute conn "UPDATE work SET body = ? WHERE id = ?" (Conv.markdownText body, i)
    Conv.LegacyPost _ _ -> pure ()

discardWork :: SQLite.Connection -> Conv.WorkId -> IO ()
discardWork conn wid =
  case wid of
    Conv.StoredWork i -> SQLite.execute conn "DELETE FROM work WHERE id = ?" (SQLite.Only i)
    Conv.LegacyPost _ _ -> pure ()

takeWork :: SQLite.Connection -> Conv.WorkId -> IO ()
takeWork conn wid =
  case wid of
    Conv.StoredWork i ->
      SQLite.execute
        conn
        "UPDATE work SET kind = 'pending', author = 'viewer' WHERE id = ? AND kind = 'proposed'"
        (SQLite.Only i)
    Conv.LegacyPost _ _ -> pure ()

recordSubmitted :: SQLite.Connection -> T.Text -> Conv.LocalId -> Store.RemoteThreadId -> IO ()
recordSubmitted conn repo lid tid =
  SQLite.execute
    conn
    "INSERT INTO work_submitted (repo, local_id, thread_id) VALUES (?, ?, ?)\
    \ ON CONFLICT(local_id) DO UPDATE SET thread_id = excluded.thread_id"
    (repo, Conv.localIdToText lid, Store.remoteThreadIdToText tid)

{- | A joined note grows to cover the union of its lines and the new
selection, in file order.
-}
growNote :: SQLite.Connection -> T.Text -> Conv.LocalId -> Conv.LineSpan -> IO ()
growNote conn repo lid lineSpan =
  SQLite.withTransaction conn $ do
    SQLite.execute
      conn
      "DELETE FROM work_target_line WHERE repo = ? AND local_id = ?"
      (repo, Conv.localIdToText lid)
    SQLite.executeMany
      conn
      "INSERT INTO work_target_line (repo, local_id, seq, key, line_no, line) VALUES (?, ?, ?, ?, ?, ?)"
      ( zipWith
          (\i l -> (repo, Conv.localIdToText lid, i :: Int, Store.lineKeyToText (Conv.slKey l), Conv.slLineNo l, Conv.slText l))
          [0 ..]
          (NEL.toList (Conv.spanLines lineSpan))
      )

-- | Accepting lines retires the notes written on them, old and new alike.
retireNotesOn :: SQLite.Connection -> T.Text -> [Store.LineKey] -> IO ()
retireNotesOn conn repo keys =
  Monad.unless (null keys) $
    SQLite.withTransaction conn $
      mapM_
        ( \batch -> do
            let
              holes = mconcat (replicate (length batch - 1) ", ?")
              params = SQLite.toRow (SQLite.Only repo) <> SQLite.toRow (map Store.lineKeyToText batch)
            SQLite.execute
              conn
              ( SQLite.Query
                  ( "DELETE FROM work WHERE repo = ? AND target_kind = 'note' AND target_id IN\
                    \ (SELECT local_id FROM work_target_line WHERE repo = ? AND key IN (?"
                      <> holes
                      <> "))"
                  )
              )
              (SQLite.toRow (SQLite.Only repo) <> params)
            SQLite.execute
              conn
              ( SQLite.Query
                  ( "DELETE FROM work_target WHERE repo = ? AND kind = 'note' AND local_id IN\
                    \ (SELECT local_id FROM work_target_line WHERE repo = ? AND key IN (?"
                      <> holes
                      <> "))"
                  )
              )
              (SQLite.toRow (SQLite.Only repo) <> params)
            SQLite.execute
              conn
              "DELETE FROM work_target_line WHERE repo = ? AND local_id NOT IN (SELECT local_id FROM work_target WHERE repo = ?)"
              (repo, repo)
        )
        (chunksOf 400 keys)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs =
  case splitAt n xs of
    ([], _) -> []
    (chunk, rest) -> chunk : chunksOf n rest

forgetWork :: SQLite.Connection -> T.Text -> IO ()
forgetWork conn repo =
  SQLite.withTransaction conn $ do
    SQLite.execute conn "DELETE FROM work WHERE repo = ?" (SQLite.Only repo)
    SQLite.execute conn "DELETE FROM work_target_line WHERE repo = ?" (SQLite.Only repo)
    SQLite.execute conn "DELETE FROM work_target WHERE repo = ?" (SQLite.Only repo)
    SQLite.execute conn "DELETE FROM work_submitted WHERE repo = ?" (SQLite.Only repo)

{- | A name for a new thread or note: the lines it covers and the moment
it was written, so two notes on one selection stay apart.
-}
newLocalId :: T.Text -> [Store.LineKey] -> IO Conv.LocalId
newLocalId repo keys = do
  now <- Clock.getCurrentTime
  let
    seed = T.intercalate "\NUL" ("remax/work/1" : repo : T.pack (show now) : map Store.lineKeyToText keys)
  pure (Conv.LocalId (T.take 16 (T.pack (SHA.showDigest (SHA.sha256 (TLE.encodeUtf8 (TL.fromStrict seed)))))))
