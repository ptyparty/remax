{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Conversation
  ( resolveAllAnswered
  , refreshNoteGroups
  , resolveNotesInMemory
  , closeConversation
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Remax.Store qualified as Store
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Verdict qualified as Verdict

-- Attach a reviewer note to the selection's lines (or the current hunk),
-- so a ! or ? carries its reason into `remax list`.
-- :resolve all — sweep every answered note in the repository: anything
-- an agent has replied to is considered handled and both sides clear.
resolveAllAnswered :: Brick.EventM Types.Name Types.St ()
resolveAllAnswered = do
  st <- State.get
  n <- MonadIO.liftIO (Store.resolveAnswered (Types.stConn st) (Types.repoText st))
  threads <- MonadIO.liftIO (Store.loadThreads (Types.stConn st) (Types.repoText st))
  groups <- MonadIO.liftIO (Store.loadNoteGroups (Types.stConn st) (Types.repoText st))
  State.modify (\s -> s {Types.stThreads = threads, Types.stNoteGroups = groups})
  Types.setMsg $
    if n == 0
      then "no answered notes to resolve"
      else "resolved " <> Types.tshow n <> " answered line(s)"

-- Acceptance closes the note conversation; the store upsert already
-- cleared the rows, this keeps the inline display in step.
-- Re-read which named group each noted key belongs to. The names are
-- assigned by the store when a note is saved, so the session cannot
-- work them out for itself; it asks again after any write that could
-- have formed or dissolved a group.
refreshNoteGroups :: Brick.EventM Types.Name Types.St ()
refreshNoteGroups = do
  st <- State.get
  groups <- MonadIO.liftIO (Store.loadNoteGroups (Types.stConn st) (Types.repoText st))
  State.modify (\s -> s {Types.stNoteGroups = groups})

resolveNotesInMemory :: [T.Text] -> Brick.EventM Types.Name Types.St ()
resolveNotesInMemory keys = do
  State.modify $ \s ->
    s {Types.stThreads = foldr Map.delete (Types.stThreads s) keys}
  refreshNoteGroups

-- Every key carrying the same conversation as one of these.
--
-- A note is one conversation however many lines it was written across,
-- and those lines need not all be part of the change: a note made over a
-- selection that reaches past it is recorded against the unchanged lines
-- too, under the key a remark uses. Closing it from the hunk's own keys
-- leaves that half alive, keyed to a line no hunk owns, where nothing
-- will ever resolve it — and it reappears beside whichever hunk that
-- line happens to sit next to.
-- Deliberately the rows beside the accepted ones, not every key whose
-- posts happen to match. Two hunks flagged with the same sentence carry
-- equal conversations while being nothing to do with each other, and
-- retiring one would take the other with it — which is a worse bug than
-- the one this is here to fix.
conversationKeys :: Types.St -> Types.FileView -> [T.Text] -> [T.Text]
conversationKeys st fv keys =
  let
    rows = Types.fvRows fv
    threadAt i = do
      r <- Types.safeIdx rows i
      Maybe.listToMaybe (Maybe.mapMaybe (`Map.lookup` Types.stThreads st) (Verdict.rowFeedbackKeys r))
    keysAt i = maybe [] Verdict.rowFeedbackKeys (Types.safeIdx rows i)
    seeds =
      [ i
      | (i, r) <- zip [0 ..] rows
      , any (`elem` keys) (Verdict.rowFeedbackKeys r)
      ]
    -- the run of neighbours carrying the same conversation, which is the
    -- block the note is drawn under
    grow step i posts =
      let
        j = i + step
      in
        if threadAt j == Just posts then j : grow step j posts else []
    block i = case threadAt i of
      Nothing -> []
      Just posts -> (i : grow (-1) i posts) ++ grow 1 i posts
  in
    Set.toList (Set.fromList (concatMap keysAt (concatMap block seeds)))

-- Retire the conversation the accepted lines belonged to, wherever the
-- rest of it lives.
--
-- One keystroke made the mark and one retires it: the lines outside the
-- hunk lose their verdict as well as their share of the conversation.
-- Closing the note but leaving them flagged would leave a mark standing
-- with nothing left to say why, and a punch list asking for work that
-- has just been done.
--
-- Cleared rather than marked reviewed, because reviewed says a change
-- has been read and a line nobody changed has none.
closeConversation :: [T.Text] -> Brick.EventM Types.Name Types.St ()
closeConversation keys = do
  st <- State.get
  let
    wider = case Types.currentFileMay st of
      Nothing -> keys
      Just fv -> conversationKeys st fv keys
    beyond = filter (`notElem` keys) wider
  Monad.unless (null beyond) $ do
    Monad.void (MonadIO.liftIO (Store.clearThread (Types.stConn st) (Types.repoText st) beyond))
    MonadIO.liftIO (Store.deleteKeys (Types.stConn st) (Types.repoText st) beyond)
    State.modify $ \s ->
      s {Types.stVerdicts = foldr Map.delete (Types.stVerdicts s) beyond}
  resolveNotesInMemory wider
