{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Verdicts
  ( togglePendingOnly
  , cycleRunFilter
  , setRunFilter
  , showCommitPopup
  , revertTarget
  , acceptSelection
  , applyVerdict
  , runDeferredAdvance
  , clearVerdictBySelection
  , cancelMark
  , applyToDisk
  , acceptPath
  ) where

import Brick
  ( EventM
  )
import Control.Applicative ((<|>))
import Control.Exception
  ( IOException
  , try
  )
import Control.Monad
  ( forM_
  , unless
  , void
  , when
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
  ( get
  , modify
  )
import Data.List
  ( isPrefixOf
  )
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( mapMaybe
  )
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Remax.Config
import Remax.DiffModel
import Remax.Git
  ( RebaseStatus (..)
  , commitMessage
  , headCommit
  )
import Remax.Modal
import Remax.Store
import Remax.UI.Action.Conversation
import Remax.UI.Core.Types
import Remax.UI.Core.Verdict
import Remax.UI.Core.View
import Remax.UI.Model.Load
import Remax.UI.Model.Nav
import qualified Remax.Zipper as Z
import System.FilePath
  ( dropTrailingPathSeparator
  , makeRelative
  , normalise
  , (</>)
  )

togglePendingOnly :: EventM Name St ()
togglePendingOnly = do
  modify $ \s ->
    let
      now = not (stOnlyPending s)
    in
      s
        { stOnlyPending = now
        , stMsg =
            if now
              then "pending hunks only"
              else "showing all hunks"
        }
  scrollTrayToCurrent

-- Space k cycles all → code → comments; :filter names one outright.
-- Independent of the pending filter, so "the code hunks nobody has
-- looked at yet" is one combination of the two.
cycleRunFilter :: EventM Name St ()
cycleRunFilter = do
  st <- get
  setRunFilter (nextRunFilter (stRunFilter st))

setRunFilter :: RunFilter -> EventM Name St ()
setRunFilter chosen = do
  modify (\s -> s {stRunFilter = chosen, stMsg = filterMsg chosen})
  -- the entry list reshapes under the viewport, so the current hunk has
  -- to be found again
  scrollTrayToCurrent

filterMsg :: RunFilter -> Text
filterMsg chosen =
  case chosen of
    FilterAll -> "walking every hunk"
    FilterCode -> "walking code hunks only"
    FilterComments -> "walking comment hunks only"

-- The full message of the commit under review: the stopped commit's
-- own sha during a rebase (HEAD differs at a conflict stop), HEAD
-- otherwise.
showCommitPopup :: EventM Name St ()
showCommitPopup = do
  st <- get
  let
    ref = case stRebase st of
      Just rb
        | not (T.null (rebaseSha rb)) -> rebaseSha rb
      _ -> "HEAD"
  got <- liftIO (commitMessage (stRoot st) ref)
  case got of
    Left e -> setMsg e
    Right body -> modify (\s -> s {stInfoPopup = Just ("commit", body)})

-- :revert / Space r — put the base text back for the hunk under the
-- cursor, or for every hunk a selection touches. The revert is an
-- ordinary buffer edit, so u undoes it and the diff re-derives at once.
revertTarget :: EventM Name St ()
revertTarget = do
  st <- get
  case currentFileMay st of
    Nothing -> setMsg "no file on screen"
    Just fv
      | fvStale fv -> setMsg "hunks are stale — press Esc first"
      | Set.null (revertTargets st fv) -> setMsg "nothing to revert here"
      | otherwise -> do
          rebuildWith (stCurF st) (revertedBuffer fv (revertTargets st fv))
          applyToDisk (stCurF st)
          setMsg "\10226 reverted to base"

-- Which hunks the revert reaches: the ones a selection touches, or the
-- one under the cursor.
revertTargets :: St -> FileView -> Set.Set Int
revertTargets st fv =
  let
    ed = fvEd fv
    lineSpan = selectionRows ed
    charSpan = fmap (\((lr, _), (hr, _)) -> (lr, hr)) (charSelection ed)
    -- by where a row answers, not by where it owns a line: a
    -- removal-only hunk has no line of its own, so matching on
    -- rNewRow alone made it unselectable and left X-then-revert
    -- reporting nothing to do
    touchedBySpan (a, b) =
      let
        pick entry@(row, _) =
          case rRunIx row of
            Just ix | rowAt entry >= a && rowAt entry <= b -> Just ix
            _ -> Nothing
      in
        Set.fromList (mapMaybe pick (rowRestPoints fv))
  in
    case lineSpan <|> charSpan of
      Just spanRows -> touchedBySpan spanRows
      Nothing -> maybe Set.empty Set.singleton (hunkUnderCursor st)

-- The buffer with the targeted hunks put back to their base text, the
-- cursor parked on the first line that changed.
revertedBuffer :: FileView -> Set.Set Int -> EdState
revertedBuffer fv targetIxs =
  let
    ed = fvEd fv
    inTarget row = maybe False (`Set.member` targetIxs) (rRunIx row)
    emit row =
      if inTarget row
        then case rMark row of
          '-' -> [rText row]
          '+' -> []
          _ -> maybe [] (const [rText row]) (rNewRow row)
        else maybe [] (const [rText row]) (rNewRow row)
    newLines = concatMap emit (fvRows fv)
    keptLines = if null newLines then [""] else newLines
    plusRows = concatMap (\row -> if inTarget row then maybe [] pure (rNewRow row) else []) (fvRows fv)
    parkRow =
      case plusRows of
        (r0 : _) -> min r0 (length keptLines - 1)
        [] -> min (fst (cursorRC ed)) (length keptLines - 1)
  in
    ed
      { edZipper = Z.moveCursor (max 0 parkRow, 0) (Z.textZipper keptLines Nothing)
      , edUndo = take 100 (edZipper ed : edUndo ed)
      , edRedo = []
      , edTouched = True
      , edSelAnchor = Nothing
      , edMode = Normal
      }

-- Accepting demands a visible object: Enter only acts on an active
-- selection, so what gets marked reviewed is always what was just seen
-- highlighted on screen. X puts the whole hunk one keystroke away.
acceptSelection :: EventM Name St ()
acceptSelection = do
  st <- get
  case activeSelectionRows (fvEd (currentFile st)) of
    Just _ -> void (applyVerdictLines Reviewed)
    Nothing ->
      modify
        (\s -> s {stMsg = "select what you're accepting — X selects the hunk"})

-- Selection-first, like every other verb: an active selection scopes the
-- verdict to those lines; no selection means the whole current hunk.
-- Reports whether anything was actually marked.
applyVerdict :: Verdict -> EventM Name St Bool
applyVerdict v = do
  st <- get
  case activeSelectionRows (fvEd (currentFile st)) of
    Just _ -> applyVerdictLines v
    Nothing -> applyVerdictHunk v

-- The advance a mark would have made, released when the note popup
-- closes.
runDeferredAdvance :: EventM Name St ()
runDeferredAdvance = do
  st <- get
  when (stDeferAdvance st) $ do
    modify (\s -> s {stDeferAdvance = False})
    advanceToPending

clearVerdictBySelection :: EventM Name St ()
clearVerdictBySelection = do
  st <- get
  case activeSelectionRows (fvEd (currentFile st)) of
    Just _ -> clearVerdict VerdictLines
    Nothing -> clearVerdict VerdictHunk

-- The commit a verdict describes, when the session knows it: at a rebase
-- stop, HEAD is the commit under review. Ambient reviews record no
-- commit — agents recover one from the line text with git blame.
verdictCommitContext :: EventM Name St (Maybe (Text, Text))
verdictCommitContext = do
  st <- get
  case stRebase st of
    Nothing -> pure Nothing
    Just rb
      | rebaseConflicted rb -> pure Nothing
      | otherwise -> liftIO (headCommit (stRoot st))

-- Mark the whole current hunk and move on to the next pending one.
applyVerdictHunk :: Verdict -> EventM Name St Bool
applyVerdictHunk v = do
  st <- get
  let
    fv = currentFile st
    rs = fvRuns fv
    path = T.pack (fvPath fv)
    -- with no selection, the verb needs the cursor to actually be in
    -- the hunk it would mark — acting on one out of view is how
    -- mistakes get recorded
    target = do
      here <- hunkUnderCursor st
      if here == clampH rs (stCurH st)
        then safeIdx rs (clampH rs (stCurH st))
        else Nothing
  case target of
    Nothing -> do
      setMsg "cursor isn't in the current hunk — Space h makes this one current"
      pure False
    Just r -> do
      let
        storeRow (k, l, n) = (k, path, l, n)
        rows = map storeRow (runChanges fv (clampH rs (stCurH st)))
      mCommit <- verdictCommitContext
      liftIO (recordVerdicts (stConn st) (repoText st) (stPassId st) v mCommit rows)
      when (v == Reviewed) (closeConversation (runKeys r))
      modify $ \s ->
        s
          { stVerdicts =
              foldr (\k m -> Map.insert k v m) (stVerdicts s) (runKeys r)
          , stLastMarked = runKeys r
          , stLastRows = rows
          , stCounts = Map.insertWith (+) v 1 (stCounts s)
          , stPunch =
              if v == Reviewed
                then stPunch s
                else (fvPath fv, runAnchor r + 1, v) : stPunch s
          }
      setEd (stCurF st) (collapseSelection (fvEd fv))
      deferred <- get
      unless (stDeferAdvance deferred) advanceToPending
      pure True

-- Mark just the selected lines (or the cursor's line); the hunk stays
-- pending until every one of its lines is settled, and the cursor stays
-- where it is.
applyVerdictLines :: Verdict -> EventM Name St Bool
applyVerdictLines v = do
  st <- get
  let
    fv = currentFile st
    (lo, hi) = verdictLineSpan (fvEd fv)
    -- accepting says a change has been read, and a line nobody changed
    -- has none; ! and ? are remarks and reach any line
    picks = selectedChanges (v /= Reviewed) fv lo hi
  if null picks
    then do
      setMsg "no changed lines here"
      pure False
    else do
      recordLineVerdicts v fv lo picks
      advanceAfterLines v lo hi
      pure True

recordLineVerdicts :: Verdict -> FileView -> Int -> [(Text, Text, Int)] -> EventM Name St ()
recordLineVerdicts v fv lo picks = do
  st <- get
  let
    path = T.pack (fvPath fv)
    storeRow (k, l, n) = (k, path, l, n)
    keys = map (\(k, _, _) -> k) picks
  mCommit <- verdictCommitContext
  liftIO (recordVerdicts (stConn st) (repoText st) (stPassId st) v mCommit (map storeRow picks))
  when (v == Reviewed) (closeConversation keys)
  modify $ \s ->
    s
      { stVerdicts = foldr (\(k, _, _) m -> Map.insert k v m) (stVerdicts s) picks
      , stLastMarked = keys
      , stLastRows = map storeRow picks
      , stPunch =
          if v == Reviewed
            then stPunch s
            else (fvPath fv, lo + 1, v) : stPunch s
      , stMsg = tshow (length picks) <> " line(s) marked"
      }

-- auto-advance: a mark that finishes the hunk moves on to the next
-- pending hunk, a partial mark steps to the line after the
-- selection so line-by-line review flows
advanceAfterLines :: Verdict -> Int -> Int -> EventM Name St ()
advanceAfterLines v lo hi = do
  settled <- get
  let
    fvSettled = currentFile settled
    rs = fvRuns fvSettled
    curIx = clampH rs (stCurH settled)
    -- A verdict says something only about the hunks its lines belong
    -- to. Marking a line somewhere else leaves the current hunk
    -- exactly as unreviewed as it was, so asking whether that hunk is
    -- finished is asking about the wrong thing: it would move the
    -- review on off the back of a verdict that never touched it.
    onCurrent = Set.member curIx (selectedRuns fvSettled lo hi)
    hunkDone =
      case safeIdx rs curIx of
        Just r -> not (runUnvisited (stVerdicts settled) r)
        Nothing -> True
  -- including the advance a flag would otherwise have deferred until
  -- its note was written
  unless onCurrent (modify (\s -> s {stDeferAdvance = False}))
  let
    deferring = stDeferAdvance settled && onCurrent
  if onCurrent && hunkDone
    then do
      modify (\s -> s {stCounts = Map.insertWith (+) v 1 (stCounts s)})
      setEd (stCurF settled) (collapseSelection (fvEd fvSettled))
      unless deferring advanceToPending
    else unless deferring $ do
      let
        ed = fvEd fvSettled
        nextRow = min (hi + 1) (bufferLineCount ed - 1)
        stepped = (collapseSelection ed) {edZipper = Z.moveCursor (nextRow, 0) (edZipper ed)}
      setEd (stCurF settled) stepped

-- Take the verdicts back off the hunk or the selected lines, so an
-- erroneous mark can be undone.
clearVerdict :: VerdictScope -> EventM Name St ()
clearVerdict scope = do
  st <- get
  let
    fv = currentFile st
    rs = fvRuns fv
    keys = case scope of
      VerdictLines ->
        let
          (lo, hi) = verdictLineSpan (fvEd fv)
        in
          -- clearing reaches remarks too: a remark you have thought
          -- better of should come off the same way a verdict does
          map (\(k, _, _) -> k) (selectedChanges True fv lo hi)
      VerdictHunk ->
        case safeIdx rs (clampH rs (stCurH st)) of
          Just r -> runKeys r
          Nothing -> []
    marked = filter (`Map.member` stVerdicts st) keys
  if null marked
    then modify (\s -> s {stMsg = "nothing marked here"})
    else do
      liftIO (deleteKeys (stConn st) (repoText st) marked)
      modify $ \s ->
        s
          { stVerdicts = foldr Map.delete (stVerdicts s) marked
          , stMsg = "unmarked " <> tshow (length marked) <> " line(s)"
          }
      setEd (stCurF st) (collapseSelection (fvEd fv))

-- Put back what a mark changed, in the store and on screen.
--
-- Not simply an unmark: a line that already carried a verdict had it
-- overwritten the instant the key was pressed, so cancelling has to
-- restore that rather than leave the line bare.
cancelMark :: MarkUndo -> EventM Name St ()
cancelMark undoing = do
  st <- get
  let
    before = muBefore undoing
    fresh = [k | (k, Nothing) <- Map.toList before]
    replaced = Map.fromListWith (++) [(v, [k]) | (k, Just v) <- Map.toList before]
    rowsFor ks = filter (\(k, _, _, _) -> k `elem` ks) (muRows undoing)
  unless (null fresh) $
    liftIO (deleteKeys (stConn st) (repoText st) fresh)
  mCommit <- verdictCommitContext
  forM_ (Map.toList replaced) $ \(v, ks) ->
    liftIO
      ( recordVerdicts
          (stConn st)
          (repoText st)
          (stPassId st)
          v
          mCommit
          (rowsFor ks)
      )
  modify $ \s ->
    s
      { stVerdicts =
          Map.foldrWithKey
            (\k mv m -> maybe (Map.delete k m) (\v -> Map.insert k v m) mv)
            (stVerdicts s)
            before
      , -- the tallies the summary prints, as they stood before the mark
        stCounts = muCounts undoing
      , stPunch = muPunch undoing
      , stLastMarked = []
      , stLastRows = []
      }

advanceToPending :: EventM Name St ()
advanceToPending = do
  st <- get
  let
    here =
      ( stCurF st
      , clampH (fvRuns (currentFile st)) (stCurH st)
      )
    -- scan starts just after the current hunk and wraps all the way;
    -- hunks nothing has been said about come first, and the flagged/
    -- questioned ones only get circled back to once those run out
    wrapped =
      let
        (upToHere, afterHere) = break (> here) (filteredRunPairs st)
      in
        afterHere ++ upToHere
    unvisited = filter (pairUnvisited st) wrapped
    pending = filter (pairPending st) wrapped
  case unvisited ++ pending of
    ((f, h) : _) -> parkAnchor f h
    [] ->
      modify
        ( \s ->
            s
              { stMsg =
                  "All hunks have verdicts — "
                    <> quitKeys (cfgKeys (stConfig s))
                    <> " to quit."
              }
        )

-- Write the buffer through to the working file. remax writes eagerly —
-- a normal-mode edit and Esc from insert both land here — which a
-- deleted file cannot tolerate: its buffer is empty, so any write at all
-- would recreate on disk the very file whose removal is under review,
-- and call that an edit. Nothing but content deliberately put back
-- (:revert restoring the base, or typing into the buffer) may resurrect
-- it, and that write is what stops the file being deleted.
applyToDisk :: Int -> EventM Name St ()
applyToDisk i = do
  st <- get
  let
    fv = stFiles st !! i
    outLines = bufferLines (fvEd fv)
    wouldRecreate = fvDeleted fv && all T.null outLines
  when (edTouched (fvEd fv) && not wouldRecreate) $ do
    got <-
      liftIO
        ( try (TIO.writeFile (stRoot st </> fvPath fv) (T.unlines outLines)) ::
            IO (Either IOException ())
        )
    modify $ \s ->
      s
        { stMsg = case got of
            Right () -> "\9998 wrote " <> T.pack (fvPath fv)
            Left _ -> "! could not write " <> T.pack (fvPath fv)
        , stWritten = case got of
            Right () -> Set.insert (fvPath fv) (stWritten s)
            Left _ -> stWritten s
        }
    case got of
      Right () -> modify (updateFile i (\v -> v {fvDeleted = False}))
      Left _ -> pure ()

-- Accept everything under a directory (or one file) as reviewed — the
-- bulk verb for generated code nobody reads line by line. Matching is
-- by path component, so `gen` covers gen/a.txt but not generated.hs.
acceptPath :: Text -> EventM Name St ()
acceptPath rawPath = do
  st <- get
  let
    prefix =
      dropTrailingPathSeparator
        (normalise (makeRelative (stRoot st) (T.unpack rawPath)))
    matches fv =
      fvPath fv == prefix || (prefix ++ "/") `isPrefixOf` fvPath fv
    targets = filter matches (stFiles st)
    fileRows fv =
      let
        hunks = [0 .. length (fvRuns fv) - 1]
        perRun h =
          map
            (\(k, l, n) -> (k, T.pack (fvPath fv), l, n))
            (runChanges fv h)
      in
        concatMap perRun hunks
    rows = concatMap fileRows targets
    keys = map (\(k, _, _, _) -> k) rows
    hunkCount = sum (map (length . fvRuns) targets)
  if null targets
    then setMsg ("nothing in the change set under " <> T.pack prefix)
    else do
      mCommit <- verdictCommitContext
      liftIO
        ( recordVerdicts
            (stConn st)
            (repoText st)
            (stPassId st)
            Reviewed
            mCommit
            rows
        )
      resolveNotesInMemory keys
      modify $ \s ->
        s
          { stVerdicts =
              foldr (\k m -> Map.insert k Reviewed m) (stVerdicts s) keys
          , stCounts = Map.insertWith (+) Reviewed hunkCount (stCounts s)
          }
      advanceToPending
      setMsg
        ( "accepted "
            <> tshow hunkCount
            <> " hunk(s) in "
            <> tshow (length targets)
            <> " file(s) under "
            <> T.pack prefix
        )

-- Files beyond the change set ---------------------------------------------------
