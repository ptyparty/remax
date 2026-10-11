{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Conversation
  ( resolveAllAnswered
  , refreshNoteGroups
  , resolveNotesInMemory
  , closeConversation
  ) where

import Brick
  ( EventM
  )
import Control.Monad
  ( unless
  , void
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
  ( get
  , modify
  )
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( listToMaybe
  , mapMaybe
  )
import qualified Data.Set as Set
import Data.Text (Text)
import Remax.Store
import Remax.UI.Core.Types
import Remax.UI.Core.Verdict

-- Attach a reviewer note to the selection's lines (or the current hunk),
-- so a ! or ? carries its reason into `remax list`.
-- :resolve all — sweep every answered note in the repository: anything
-- an agent has replied to is considered handled and both sides clear.
resolveAllAnswered :: EventM Name St ()
resolveAllAnswered = do
  st <- get
  n <- liftIO (resolveAnswered (stConn st) (repoText st))
  threads <- liftIO (loadThreads (stConn st) (repoText st))
  groups <- liftIO (loadNoteGroups (stConn st) (repoText st))
  modify (\s -> s {stThreads = threads, stNoteGroups = groups})
  setMsg $
    if n == 0
      then "no answered notes to resolve"
      else "resolved " <> tshow n <> " answered line(s)"

-- Acceptance closes the note conversation; the store upsert already
-- cleared the rows, this keeps the inline display in step.
-- Re-read which named group each noted key belongs to. The names are
-- assigned by the store when a note is saved, so the session cannot
-- work them out for itself; it asks again after any write that could
-- have formed or dissolved a group.
refreshNoteGroups :: EventM Name St ()
refreshNoteGroups = do
  st <- get
  groups <- liftIO (loadNoteGroups (stConn st) (repoText st))
  modify (\s -> s {stNoteGroups = groups})

resolveNotesInMemory :: [Text] -> EventM Name St ()
resolveNotesInMemory keys = do
  modify $ \s ->
    s {stThreads = foldr Map.delete (stThreads s) keys}
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
conversationKeys :: St -> FileView -> [Text] -> [Text]
conversationKeys st fv keys =
  let
    rows = fvRows fv
    threadAt i = do
      r <- safeIdx rows i
      listToMaybe (mapMaybe (`Map.lookup` stThreads st) (rowFeedbackKeys r))
    keysAt i = maybe [] rowFeedbackKeys (safeIdx rows i)
    seeds =
      [ i
      | (i, r) <- zip [0 ..] rows
      , any (`elem` keys) (rowFeedbackKeys r)
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
closeConversation :: [Text] -> EventM Name St ()
closeConversation keys = do
  st <- get
  let
    wider = case currentFileMay st of
      Nothing -> keys
      Just fv -> conversationKeys st fv keys
    beyond = filter (`notElem` keys) wider
  unless (null beyond) $ do
    void (liftIO (clearThread (stConn st) (repoText st) beyond))
    liftIO (deleteKeys (stConn st) (repoText st) beyond)
    modify $ \s ->
      s {stVerdicts = foldr Map.delete (stVerdicts s) beyond}
  resolveNotesInMemory wider
