{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Rebase
  ( rebaseStopMsg
  , startRebase
  , continueRebase
  , abortRebase
  , amendHead
  , applyAmendMessage
  , fileCountLabel
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.Set qualified as Set
import Data.Text qualified as T
import Remax.Git qualified as Git
import Remax.Modal qualified as Modal
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Model.Load qualified as Load
import Remax.Vi qualified as Vi
import System.FilePath qualified as FilePath

rebaseStopMsg :: Git.RebaseStatus -> T.Text
rebaseStopMsg rb =
  let
    at = Types.tshow (Git.rebaseStep rb) <> "/" <> Types.tshow (Git.rebaseTotal rb)
  in
    if Git.rebaseConflicted rb
      then "conflict at commit " <> at <> " — resolve, then :continue"
      else "commit " <> at <> " — review, edit, :continue (:abort! backs out)"

-- Start the surgical commit-by-commit pass: `git rebase --exec false`
-- pauses the sequencer after every commit (the failing exec is the stop
-- mechanism), and rebase awareness takes over from there.
startRebase :: Maybe T.Text -> Brick.EventM Types.Name Types.St ()
startRebase mRef = do
  st <- State.get
  case Types.stRebase st of
    Just _ -> Types.setMsg "a rebase is already in progress — :continue or :abort!"
    Nothing -> do
      dirty <- MonadIO.liftIO (Git.trackedChanges (Types.stRoot st))
      if dirty
        then Types.setMsg "tracked changes in the tree — commit or stash before :rebase"
        else do
          target <- case mRef of
            Just ref -> do
              ok <- MonadIO.liftIO (Git.refExists (Types.stRoot st) ref)
              pure (if ok then Right ref else Left ("no such ref: " <> ref))
            Nothing -> do
              got <- MonadIO.liftIO (Git.branchRoot (Types.stRoot st))
              pure (fmap (T.take 10) got)
          case target of
            Left e -> Types.setMsg e
            Right ref -> do
              res <- MonadIO.liftIO (Git.runGit (Types.stRoot st) ["rebase", "--exec", "false", T.unpack ref])
              rb <- MonadIO.liftIO (Git.rebaseStatus (Types.stRoot st))
              State.modify (\s -> s {Types.stRebase = rb, Types.stWritten = Set.empty})
              case rb of
                Just info -> do
                  Load.forceGitRefresh
                  Types.setMsg (rebaseStopMsg info)
                Nothing ->
                  Types.setMsg $ case res of
                    Left e -> e
                    Right () -> "nothing to rebase — already at " <> ref

-- Fold this stop's edits into the stopped commit and advance. A conflict
-- stop has no commit yet, so --continue makes it; an exec stop already
-- committed, so the edits are amended in first.
continueRebase :: Brick.EventM Types.Name Types.St ()
continueRebase = do
  st <- State.get
  case Types.stRebase st of
    Nothing -> Types.setMsg "no rebase in progress"
    Just rb -> do
      let
        root = Types.stRoot st
      -- a rebase stop starts from a clean tree, so everything dirty now
      -- is this stop's fix — including edits made through $EDITOR or by
      -- an agent, which never pass through remax's own write path
      folded <- MonadIO.liftIO $ do
        addRes <- Git.runGit root ["add", "-A"]
        case addRes of
          Left e -> pure (Left e)
          Right () -> do
            staged <- Git.hasStagedChanges root
            if not (Git.rebaseConflicted rb) && staged
              then Git.runGit root ["commit", "--amend", "--no-edit"]
              else pure (Right ())
      case folded of
        Left e -> Types.setMsg e
        Right () -> do
          res <- MonadIO.liftIO (Git.runGit root ["rebase", "--continue"])
          rbNext <- MonadIO.liftIO (Git.rebaseStatus root)
          State.modify (\s -> s {Types.stRebase = rbNext, Types.stWritten = Set.empty})
          Load.forceGitRefresh
          case rbNext of
            Nothing -> Types.setMsg "rebase complete — base restored"
            Just next ->
              -- --continue itself exits nonzero when the next exec-false
              -- stop is reached; a stop that actually advanced is fine
              if Git.rebaseStep next == Git.rebaseStep rb && not (Git.rebaseConflicted next)
                then
                  Types.setMsg
                    ( case res of
                        Left e -> e
                        Right () -> rebaseStopMsg next
                    )
                else Types.setMsg (rebaseStopMsg next)

abortRebase :: Brick.EventM Types.Name Types.St ()
abortRebase = do
  st <- State.get
  case Types.stRebase st of
    Nothing -> Types.setMsg "no rebase in progress"
    Just _ -> do
      res <- MonadIO.liftIO (Git.runGit (Types.stRoot st) ["rebase", "--abort"])
      rbNext <- MonadIO.liftIO (Git.rebaseStatus (Types.stRoot st))
      State.modify (\s -> s {Types.stRebase = rbNext, Types.stWritten = Set.empty})
      Load.forceGitRefresh
      Types.setMsg $ case res of
        Left e -> e
        Right () -> "rebase aborted"

-- The standalone fold-fixes-into-HEAD flow (pairs with :base HEAD^).
-- :amend — the commit message of the commit under review (HEAD; during
-- a rebase that is the stopped commit) opens for editing, and saving
-- amends it, folding in any files written this session outside a
-- rebase (a rebase stop leaves the folding to :continue, so the amend
-- there is message-only).
amendHead :: Brick.EventM Types.Name Types.St ()
amendHead = do
  st <- State.get
  got <- MonadIO.liftIO (Git.commitBody (Types.stRoot st) "HEAD")
  -- what folds in with the message: everything the session is showing as
  -- changed, asked of git rather than remembered, so an agent's edits and
  -- \$EDITOR's edits count the same as the ones typed in here. At a rebase
  -- stop nothing folds in -- :continue owns that.
  folding <- case Types.stRebase st of
    Just _ -> pure []
    Nothing -> MonadIO.liftIO (Git.changedFiles (Types.effectiveMode st) (Types.stRoot st))
  case got of
    Left e -> Types.setMsg e
    Right msg -> do
      let
        ls = if T.null (T.strip msg) then [""] else T.lines msg
        ed = Modal.newEdState 0 ls
      State.modify
        ( \s ->
            s
              { Types.stNotePopup = Just (Types.NotePopup ed (Types.AmendMessage folding))
              , Types.stVi = Vi.viIdle
              }
        )

applyAmendMessage :: [FilePath] -> T.Text -> Brick.EventM Types.Name Types.St ()
applyAmendMessage folding text = do
  st <- State.get
  if T.null text
    then Types.setMsg "empty commit message — amend cancelled"
    else do
      res <- MonadIO.liftIO $ do
        addRes <-
          if null folding
            then pure (Right ())
            else Git.runGit (Types.stRoot st) (["add", "--"] ++ folding)
        case addRes of
          Left e -> pure (Left e)
          Right () ->
            Git.runGit (Types.stRoot st) ["commit", "--amend", "-m", T.unpack text]
      case res of
        Left e -> Types.setMsg e
        Right () -> do
          Monad.unless (null folding) (State.modify (\s -> s {Types.stWritten = Set.empty}))
          Load.forceGitRefresh
          Types.setMsg $
            if null folding
              then "amended the commit message"
              else
                "amended HEAD: message + "
                  <> fileCountLabel (length folding)
                  <> " ("
                  <> foldedNames 3 folding
                  <> ")"

fileCountLabel :: Int -> T.Text
fileCountLabel n = Types.tshow n <> (if n == 1 then " file" else " files")

-- Names for the status line: a few, then a count for the rest, so the
-- message says what it staged without running off the bar.
foldedNames :: Int -> [FilePath] -> T.Text
foldedNames k paths =
  let
    shown = map (T.pack . FilePath.takeFileName) (take k paths)
    more = length paths - length shown
  in
    T.intercalate ", " shown
      <> (if more > 0 then " +" <> Types.tshow more <> " more" else "")
