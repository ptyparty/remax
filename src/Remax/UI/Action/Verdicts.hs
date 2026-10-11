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

import Brick qualified as Brick
import Control.Applicative ((<|>))
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Remax.Config qualified as Config
import Remax.DiffModel qualified as DiffModel
import Remax.Git qualified as Git
import Remax.Modal qualified as Modal
import Remax.Store qualified as Store
import Remax.UI.Action.Conversation qualified as Conversation
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Verdict qualified as Verdict
import Remax.UI.Core.View qualified as View
import Remax.UI.Model.Load qualified as Load
import Remax.UI.Model.Nav qualified as Nav
import Remax.Zipper qualified as Z
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

togglePendingOnly :: Brick.EventM Types.Name Types.St ()
togglePendingOnly = do
  State.modify $ \s ->
    let
      now = not (Types.stOnlyPending s)
    in
      s
        { Types.stOnlyPending = now
        , Types.stMsg =
            if now
              then "pending hunks only"
              else "showing all hunks"
        }
  Nav.scrollTrayToCurrent

-- Space k cycles all → code → comments; :filter names one outright.
-- Independent of the pending filter, so "the code hunks nobody has
-- looked at yet" is one combination of the two.
cycleRunFilter :: Brick.EventM Types.Name Types.St ()
cycleRunFilter = do
  st <- State.get
  setRunFilter (Types.nextRunFilter (Types.stRunFilter st))

setRunFilter :: Types.RunFilter -> Brick.EventM Types.Name Types.St ()
setRunFilter chosen = do
  State.modify (\s -> s {Types.stRunFilter = chosen, Types.stMsg = filterMsg chosen})
  -- the entry list reshapes under the viewport, so the current hunk has
  -- to be found again
  Nav.scrollTrayToCurrent

filterMsg :: Types.RunFilter -> T.Text
filterMsg chosen =
  case chosen of
    Types.FilterAll -> "walking every hunk"
    Types.FilterCode -> "walking code hunks only"
    Types.FilterComments -> "walking comment hunks only"

-- The full message of the commit under review: the stopped commit's
-- own sha during a rebase (HEAD differs at a conflict stop), HEAD
-- otherwise.
showCommitPopup :: Brick.EventM Types.Name Types.St ()
showCommitPopup = do
  st <- State.get
  let
    ref = case Types.stRebase st of
      Just rb
        | not (T.null (Git.rebaseSha rb)) -> Git.rebaseSha rb
      _ -> "HEAD"
  got <- MonadIO.liftIO (Git.commitMessage (Types.stRoot st) ref)
  case got of
    Left e -> Types.setMsg e
    Right body -> State.modify (\s -> s {Types.stInfoPopup = Just ("commit", body)})

-- :revert / Space r — put the base text back for the hunk under the
-- cursor, or for every hunk a selection touches. The revert is an
-- ordinary buffer edit, so u undoes it and the diff re-derives at once.
revertTarget :: Brick.EventM Types.Name Types.St ()
revertTarget = do
  st <- State.get
  case Types.currentFileMay st of
    Nothing -> Types.setMsg "no file on screen"
    Just fv
      | Types.fvStale fv -> Types.setMsg "hunks are stale — press Esc first"
      | Set.null (revertTargets st fv) -> Types.setMsg "nothing to revert here"
      | otherwise -> do
          Load.rebuildWith (Types.stCurF st) (revertedBuffer fv (revertTargets st fv))
          applyToDisk (Types.stCurF st)
          Types.setMsg "\10226 reverted to base"

-- Which hunks the revert reaches: the ones a selection touches, or the
-- one under the cursor.
revertTargets :: Types.St -> Types.FileView -> Set.Set Int
revertTargets st fv =
  let
    ed = Types.fvEd fv
    lineSpan = Modal.selectionRows ed
    charSpan = fmap (\((lr, _), (hr, _)) -> (lr, hr)) (Modal.charSelection ed)
    -- by where a row answers, not by where it owns a line: a
    -- removal-only hunk has no line of its own, so matching on
    -- rNewRow alone made it unselectable and left X-then-revert
    -- reporting nothing to do
    touchedBySpan (a, b) =
      let
        pick entry@(row, _) =
          case DiffModel.rRunIx row of
            Just ix | View.rowAt entry >= a && View.rowAt entry <= b -> Just ix
            _ -> Nothing
      in
        Set.fromList (Maybe.mapMaybe pick (View.rowRestPoints fv))
  in
    case lineSpan <|> charSpan of
      Just spanRows -> touchedBySpan spanRows
      Nothing -> maybe Set.empty Set.singleton (Nav.hunkUnderCursor st)

-- The buffer with the targeted hunks put back to their base text, the
-- cursor parked on the first line that changed.
revertedBuffer :: Types.FileView -> Set.Set Int -> Modal.EdState
revertedBuffer fv targetIxs =
  let
    ed = Types.fvEd fv
    inTarget row = maybe False (`Set.member` targetIxs) (DiffModel.rRunIx row)
    emit row =
      if inTarget row
        then case DiffModel.rMark row of
          '-' -> [DiffModel.rText row]
          '+' -> []
          _ -> maybe [] (const [DiffModel.rText row]) (DiffModel.rNewRow row)
        else maybe [] (const [DiffModel.rText row]) (DiffModel.rNewRow row)
    newLines = concatMap emit (Types.fvRows fv)
    keptLines = if null newLines then [""] else newLines
    plusRows = concatMap (\row -> if inTarget row then maybe [] pure (DiffModel.rNewRow row) else []) (Types.fvRows fv)
    parkRow =
      case plusRows of
        (r0 : _) -> min r0 (length keptLines - 1)
        [] -> min (fst (Modal.cursorRC ed)) (length keptLines - 1)
  in
    ed
      { Modal.edZipper = Z.moveCursor (max 0 parkRow, 0) (Z.textZipper keptLines Nothing)
      , Modal.edUndo = take 100 (Modal.edZipper ed : Modal.edUndo ed)
      , Modal.edRedo = []
      , Modal.edTouched = True
      , Modal.edSelAnchor = Nothing
      , Modal.edMode = Modal.Normal
      }

-- Accepting demands a visible object: Enter only acts on an active
-- selection, so what gets marked reviewed is always what was just seen
-- highlighted on screen. X puts the whole hunk one keystroke away.
acceptSelection :: Brick.EventM Types.Name Types.St ()
acceptSelection = do
  st <- State.get
  case View.activeSelectionRows (Types.fvEd (Types.currentFile st)) of
    Just _ -> Monad.void (applyVerdictLines Store.Reviewed)
    Nothing ->
      State.modify
        (\s -> s {Types.stMsg = "select what you're accepting — X selects the hunk"})

-- Selection-first, like every other verb: an active selection scopes the
-- verdict to those lines; no selection means the whole current hunk.
-- Reports whether anything was actually marked.
applyVerdict :: Store.Verdict -> Brick.EventM Types.Name Types.St Bool
applyVerdict v = do
  st <- State.get
  case View.activeSelectionRows (Types.fvEd (Types.currentFile st)) of
    Just _ -> applyVerdictLines v
    Nothing -> applyVerdictHunk v

-- The advance a mark would have made, released when the note popup
-- closes.
runDeferredAdvance :: Brick.EventM Types.Name Types.St ()
runDeferredAdvance = do
  st <- State.get
  Monad.when (Types.stDeferAdvance st) $ do
    State.modify (\s -> s {Types.stDeferAdvance = False})
    advanceToPending

clearVerdictBySelection :: Brick.EventM Types.Name Types.St ()
clearVerdictBySelection = do
  st <- State.get
  case View.activeSelectionRows (Types.fvEd (Types.currentFile st)) of
    Just _ -> clearVerdict Types.VerdictLines
    Nothing -> clearVerdict Types.VerdictHunk

-- The commit a verdict describes, when the session knows it: at a rebase
-- stop, HEAD is the commit under review. Ambient reviews record no
-- commit — agents recover one from the line text with git blame.
verdictCommitContext :: Brick.EventM Types.Name Types.St (Maybe (T.Text, T.Text))
verdictCommitContext = do
  st <- State.get
  case Types.stRebase st of
    Nothing -> pure Nothing
    Just rb
      | Git.rebaseConflicted rb -> pure Nothing
      | otherwise -> MonadIO.liftIO (Git.headCommit (Types.stRoot st))

-- Mark the whole current hunk and move on to the next pending one.
applyVerdictHunk :: Store.Verdict -> Brick.EventM Types.Name Types.St Bool
applyVerdictHunk v = do
  st <- State.get
  let
    fv = Types.currentFile st
    rs = Types.fvRuns fv
    path = T.pack (Types.fvPath fv)
    -- with no selection, the verb needs the cursor to actually be in
    -- the hunk it would mark — acting on one out of view is how
    -- mistakes get recorded
    target = do
      here <- Nav.hunkUnderCursor st
      if here == Types.clampH rs (Types.stCurH st)
        then Types.safeIdx rs (Types.clampH rs (Types.stCurH st))
        else Nothing
  case target of
    Nothing -> do
      Types.setMsg "cursor isn't in the current hunk — Space h makes this one current"
      pure False
    Just r -> do
      let
        storeRow (k, l, n) = (k, path, l, n)
        rows = map storeRow (Nav.runChanges fv (Types.clampH rs (Types.stCurH st)))
      mCommit <- verdictCommitContext
      MonadIO.liftIO (Store.recordVerdicts (Types.stConn st) (Types.repoText st) (Types.stPassId st) v mCommit rows)
      Monad.when (v == Store.Reviewed) (Conversation.closeConversation (DiffModel.runKeys r))
      State.modify $ \s ->
        s
          { Types.stVerdicts =
              foldr (\k m -> Map.insert k v m) (Types.stVerdicts s) (DiffModel.runKeys r)
          , Types.stLastMarked = DiffModel.runKeys r
          , Types.stLastRows = rows
          , Types.stCounts = Map.insertWith (+) v 1 (Types.stCounts s)
          , Types.stPunch =
              if v == Store.Reviewed
                then Types.stPunch s
                else (Types.fvPath fv, DiffModel.runAnchor r + 1, v) : Types.stPunch s
          }
      Nav.setEd (Types.stCurF st) (View.collapseSelection (Types.fvEd fv))
      deferred <- State.get
      Monad.unless (Types.stDeferAdvance deferred) advanceToPending
      pure True

-- Mark just the selected lines (or the cursor's line); the hunk stays
-- pending until every one of its lines is settled, and the cursor stays
-- where it is.
applyVerdictLines :: Store.Verdict -> Brick.EventM Types.Name Types.St Bool
applyVerdictLines v = do
  st <- State.get
  let
    fv = Types.currentFile st
    (lo, hi) = View.verdictLineSpan (Types.fvEd fv)
    -- accepting says a change has been read, and a line nobody changed
    -- has none; ! and ? are remarks and reach any line
    picks = Nav.selectedChanges (v /= Store.Reviewed) fv lo hi
  if null picks
    then do
      Types.setMsg "no changed lines here"
      pure False
    else do
      recordLineVerdicts v fv lo picks
      advanceAfterLines v lo hi
      pure True

recordLineVerdicts :: Store.Verdict -> Types.FileView -> Int -> [(T.Text, T.Text, Int)] -> Brick.EventM Types.Name Types.St ()
recordLineVerdicts v fv lo picks = do
  st <- State.get
  let
    path = T.pack (Types.fvPath fv)
    storeRow (k, l, n) = (k, path, l, n)
    keys = map (\(k, _, _) -> k) picks
  mCommit <- verdictCommitContext
  MonadIO.liftIO (Store.recordVerdicts (Types.stConn st) (Types.repoText st) (Types.stPassId st) v mCommit (map storeRow picks))
  Monad.when (v == Store.Reviewed) (Conversation.closeConversation keys)
  State.modify $ \s ->
    s
      { Types.stVerdicts = foldr (\(k, _, _) m -> Map.insert k v m) (Types.stVerdicts s) picks
      , Types.stLastMarked = keys
      , Types.stLastRows = map storeRow picks
      , Types.stPunch =
          if v == Store.Reviewed
            then Types.stPunch s
            else (Types.fvPath fv, lo + 1, v) : Types.stPunch s
      , Types.stMsg = Types.tshow (length picks) <> " line(s) marked"
      }

-- auto-advance: a mark that finishes the hunk moves on to the next
-- pending hunk, a partial mark steps to the line after the
-- selection so line-by-line review flows
advanceAfterLines :: Store.Verdict -> Int -> Int -> Brick.EventM Types.Name Types.St ()
advanceAfterLines v lo hi = do
  settled <- State.get
  let
    fvSettled = Types.currentFile settled
    rs = Types.fvRuns fvSettled
    curIx = Types.clampH rs (Types.stCurH settled)
    -- A verdict says something only about the hunks its lines belong
    -- to. Marking a line somewhere else leaves the current hunk
    -- exactly as unreviewed as it was, so asking whether that hunk is
    -- finished is asking about the wrong thing: it would move the
    -- review on off the back of a verdict that never touched it.
    onCurrent = Set.member curIx (Nav.selectedRuns fvSettled lo hi)
    hunkDone =
      case Types.safeIdx rs curIx of
        Just r -> not (Verdict.runUnvisited (Types.stVerdicts settled) r)
        Nothing -> True
  -- including the advance a flag would otherwise have deferred until
  -- its note was written
  Monad.unless onCurrent (State.modify (\s -> s {Types.stDeferAdvance = False}))
  let
    deferring = Types.stDeferAdvance settled && onCurrent
  if onCurrent && hunkDone
    then do
      State.modify (\s -> s {Types.stCounts = Map.insertWith (+) v 1 (Types.stCounts s)})
      Nav.setEd (Types.stCurF settled) (View.collapseSelection (Types.fvEd fvSettled))
      Monad.unless deferring advanceToPending
    else Monad.unless deferring $ do
      let
        ed = Types.fvEd fvSettled
        nextRow = min (hi + 1) (Modal.bufferLineCount ed - 1)
        stepped = (View.collapseSelection ed) {Modal.edZipper = Z.moveCursor (nextRow, 0) (Modal.edZipper ed)}
      Nav.setEd (Types.stCurF settled) stepped

-- Take the verdicts back off the hunk or the selected lines, so an
-- erroneous mark can be undone.
clearVerdict :: Types.VerdictScope -> Brick.EventM Types.Name Types.St ()
clearVerdict scope = do
  st <- State.get
  let
    fv = Types.currentFile st
    rs = Types.fvRuns fv
    keys = case scope of
      Types.VerdictLines ->
        let
          (lo, hi) = View.verdictLineSpan (Types.fvEd fv)
        in
          -- clearing reaches remarks too: a remark you have thought
          -- better of should come off the same way a verdict does
          map (\(k, _, _) -> k) (Nav.selectedChanges True fv lo hi)
      Types.VerdictHunk ->
        case Types.safeIdx rs (Types.clampH rs (Types.stCurH st)) of
          Just r -> DiffModel.runKeys r
          Nothing -> []
    marked = filter (`Map.member` Types.stVerdicts st) keys
  if null marked
    then State.modify (\s -> s {Types.stMsg = "nothing marked here"})
    else do
      MonadIO.liftIO (Store.deleteKeys (Types.stConn st) (Types.repoText st) marked)
      State.modify $ \s ->
        s
          { Types.stVerdicts = foldr Map.delete (Types.stVerdicts s) marked
          , Types.stMsg = "unmarked " <> Types.tshow (length marked) <> " line(s)"
          }
      Nav.setEd (Types.stCurF st) (View.collapseSelection (Types.fvEd fv))

-- Put back what a mark changed, in the store and on screen.
--
-- Not simply an unmark: a line that already carried a verdict had it
-- overwritten the instant the key was pressed, so cancelling has to
-- restore that rather than leave the line bare.
cancelMark :: Types.MarkUndo -> Brick.EventM Types.Name Types.St ()
cancelMark undoing = do
  st <- State.get
  let
    before = Types.muBefore undoing
    fresh = [k | (k, Nothing) <- Map.toList before]
    replaced = Map.fromListWith (++) [(v, [k]) | (k, Just v) <- Map.toList before]
    rowsFor ks = filter (\(k, _, _, _) -> k `elem` ks) (Types.muRows undoing)
  Monad.unless (null fresh) $
    MonadIO.liftIO (Store.deleteKeys (Types.stConn st) (Types.repoText st) fresh)
  mCommit <- verdictCommitContext
  Monad.forM_ (Map.toList replaced) $ \(v, ks) ->
    MonadIO.liftIO
      ( Store.recordVerdicts
          (Types.stConn st)
          (Types.repoText st)
          (Types.stPassId st)
          v
          mCommit
          (rowsFor ks)
      )
  State.modify $ \s ->
    s
      { Types.stVerdicts =
          Map.foldrWithKey
            (\k mv m -> maybe (Map.delete k m) (\v -> Map.insert k v m) mv)
            (Types.stVerdicts s)
            before
      , -- the tallies the summary prints, as they stood before the mark
        Types.stCounts = Types.muCounts undoing
      , Types.stPunch = Types.muPunch undoing
      , Types.stLastMarked = []
      , Types.stLastRows = []
      }

advanceToPending :: Brick.EventM Types.Name Types.St ()
advanceToPending = do
  st <- State.get
  let
    here =
      ( Types.stCurF st
      , Types.clampH (Types.fvRuns (Types.currentFile st)) (Types.stCurH st)
      )
    -- scan starts just after the current hunk and wraps all the way;
    -- hunks nothing has been said about come first, and the flagged/
    -- questioned ones only get circled back to once those run out
    wrapped =
      let
        (upToHere, afterHere) = break (> here) (Verdict.filteredRunPairs st)
      in
        afterHere ++ upToHere
    unvisited = filter (Verdict.pairUnvisited st) wrapped
    pending = filter (Verdict.pairPending st) wrapped
  case unvisited ++ pending of
    ((f, h) : _) -> Nav.parkAnchor f h
    [] ->
      State.modify
        ( \s ->
            s
              { Types.stMsg =
                  "All hunks have verdicts — "
                    <> Types.quitKeys (Config.cfgKeys (Types.stConfig s))
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
applyToDisk :: Int -> Brick.EventM Types.Name Types.St ()
applyToDisk i = do
  st <- State.get
  let
    fv = Types.stFiles st !! i
    outLines = Modal.bufferLines (Types.fvEd fv)
    wouldRecreate = Types.fvDeleted fv && all T.null outLines
  Monad.when (Modal.edTouched (Types.fvEd fv) && not wouldRecreate) $ do
    got <-
      MonadIO.liftIO
        ( Exception.try (TIO.writeFile (Types.stRoot st </> Types.fvPath fv) (T.unlines outLines)) ::
            IO (Either Exception.IOException ())
        )
    State.modify $ \s ->
      s
        { Types.stMsg = case got of
            Right () -> "\9998 wrote " <> T.pack (Types.fvPath fv)
            Left _ -> "! could not write " <> T.pack (Types.fvPath fv)
        , Types.stWritten = case got of
            Right () -> Set.insert (Types.fvPath fv) (Types.stWritten s)
            Left _ -> Types.stWritten s
        }
    case got of
      Right () -> State.modify (Verdict.updateFile i (\v -> v {Types.fvDeleted = False}))
      Left _ -> pure ()

-- Accept everything under a directory (or one file) as reviewed — the
-- bulk verb for generated code nobody reads line by line. Matching is
-- by path component, so `gen` covers gen/a.txt but not generated.hs.
acceptPath :: T.Text -> Brick.EventM Types.Name Types.St ()
acceptPath rawPath = do
  st <- State.get
  let
    prefix =
      FilePath.dropTrailingPathSeparator
        (FilePath.normalise (FilePath.makeRelative (Types.stRoot st) (T.unpack rawPath)))
    matches fv =
      Types.fvPath fv == prefix || (prefix ++ "/") `List.isPrefixOf` Types.fvPath fv
    targets = filter matches (Types.stFiles st)
    fileRows fv =
      let
        hunks = [0 .. length (Types.fvRuns fv) - 1]
        perRun h =
          map
            (\(k, l, n) -> (k, T.pack (Types.fvPath fv), l, n))
            (Nav.runChanges fv h)
      in
        concatMap perRun hunks
    rows = concatMap fileRows targets
    keys = map (\(k, _, _, _) -> k) rows
    hunkCount = sum (map (length . Types.fvRuns) targets)
  if null targets
    then Types.setMsg ("nothing in the change set under " <> T.pack prefix)
    else do
      mCommit <- verdictCommitContext
      MonadIO.liftIO
        ( Store.recordVerdicts
            (Types.stConn st)
            (Types.repoText st)
            (Types.stPassId st)
            Store.Reviewed
            mCommit
            rows
        )
      Conversation.resolveNotesInMemory keys
      State.modify $ \s ->
        s
          { Types.stVerdicts =
              foldr (\k m -> Map.insert k Store.Reviewed m) (Types.stVerdicts s) keys
          , Types.stCounts = Map.insertWith (+) Store.Reviewed hunkCount (Types.stCounts s)
          }
      advanceToPending
      Types.setMsg
        ( "accepted "
            <> Types.tshow hunkCount
            <> " hunk(s) in "
            <> Types.tshow (length targets)
            <> " file(s) under "
            <> T.pack prefix
        )

-- Files beyond the change set ---------------------------------------------------
