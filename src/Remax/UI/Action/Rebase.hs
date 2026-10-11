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

import Brick
  ( EventM
  )
import Control.Monad
  ( unless
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
  ( get
  , modify
  )
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Remax.Git
  ( RebaseStatus (..)
  , branchRoot
  , changedFiles
  , commitBody
  , hasStagedChanges
  , rebaseStatus
  , refExists
  , runGit
  , trackedChanges
  )
import Remax.Modal
import Remax.UI.Core.Types
import Remax.UI.Model.Load
import Remax.Vi
  ( viIdle
  )
import System.FilePath
  ( takeFileName
  )

rebaseStopMsg :: RebaseStatus -> Text
rebaseStopMsg rb =
  let
    at = tshow (rebaseStep rb) <> "/" <> tshow (rebaseTotal rb)
  in
    if rebaseConflicted rb
      then "conflict at commit " <> at <> " — resolve, then :continue"
      else "commit " <> at <> " — review, edit, :continue (:abort! backs out)"

-- Start the surgical commit-by-commit pass: `git rebase --exec false`
-- pauses the sequencer after every commit (the failing exec is the stop
-- mechanism), and rebase awareness takes over from there.
startRebase :: Maybe Text -> EventM Name St ()
startRebase mRef = do
  st <- get
  case stRebase st of
    Just _ -> setMsg "a rebase is already in progress — :continue or :abort!"
    Nothing -> do
      dirty <- liftIO (trackedChanges (stRoot st))
      if dirty
        then setMsg "tracked changes in the tree — commit or stash before :rebase"
        else do
          target <- case mRef of
            Just ref -> do
              ok <- liftIO (refExists (stRoot st) ref)
              pure (if ok then Right ref else Left ("no such ref: " <> ref))
            Nothing -> do
              got <- liftIO (branchRoot (stRoot st))
              pure (fmap (T.take 10) got)
          case target of
            Left e -> setMsg e
            Right ref -> do
              res <- liftIO (runGit (stRoot st) ["rebase", "--exec", "false", T.unpack ref])
              rb <- liftIO (rebaseStatus (stRoot st))
              modify (\s -> s {stRebase = rb, stWritten = Set.empty})
              case rb of
                Just info -> do
                  forceGitRefresh
                  setMsg (rebaseStopMsg info)
                Nothing ->
                  setMsg $ case res of
                    Left e -> e
                    Right () -> "nothing to rebase — already at " <> ref

-- Fold this stop's edits into the stopped commit and advance. A conflict
-- stop has no commit yet, so --continue makes it; an exec stop already
-- committed, so the edits are amended in first.
continueRebase :: EventM Name St ()
continueRebase = do
  st <- get
  case stRebase st of
    Nothing -> setMsg "no rebase in progress"
    Just rb -> do
      let
        root = stRoot st
      -- a rebase stop starts from a clean tree, so everything dirty now
      -- is this stop's fix — including edits made through $EDITOR or by
      -- an agent, which never pass through remax's own write path
      folded <- liftIO $ do
        addRes <- runGit root ["add", "-A"]
        case addRes of
          Left e -> pure (Left e)
          Right () -> do
            staged <- hasStagedChanges root
            if not (rebaseConflicted rb) && staged
              then runGit root ["commit", "--amend", "--no-edit"]
              else pure (Right ())
      case folded of
        Left e -> setMsg e
        Right () -> do
          res <- liftIO (runGit root ["rebase", "--continue"])
          rbNext <- liftIO (rebaseStatus root)
          modify (\s -> s {stRebase = rbNext, stWritten = Set.empty})
          forceGitRefresh
          case rbNext of
            Nothing -> setMsg "rebase complete — base restored"
            Just next ->
              -- --continue itself exits nonzero when the next exec-false
              -- stop is reached; a stop that actually advanced is fine
              if rebaseStep next == rebaseStep rb && not (rebaseConflicted next)
                then
                  setMsg
                    ( case res of
                        Left e -> e
                        Right () -> rebaseStopMsg next
                    )
                else setMsg (rebaseStopMsg next)

abortRebase :: EventM Name St ()
abortRebase = do
  st <- get
  case stRebase st of
    Nothing -> setMsg "no rebase in progress"
    Just _ -> do
      res <- liftIO (runGit (stRoot st) ["rebase", "--abort"])
      rbNext <- liftIO (rebaseStatus (stRoot st))
      modify (\s -> s {stRebase = rbNext, stWritten = Set.empty})
      forceGitRefresh
      setMsg $ case res of
        Left e -> e
        Right () -> "rebase aborted"

-- The standalone fold-fixes-into-HEAD flow (pairs with :base HEAD^).
-- :amend — the commit message of the commit under review (HEAD; during
-- a rebase that is the stopped commit) opens for editing, and saving
-- amends it, folding in any files written this session outside a
-- rebase (a rebase stop leaves the folding to :continue, so the amend
-- there is message-only).
amendHead :: EventM Name St ()
amendHead = do
  st <- get
  got <- liftIO (commitBody (stRoot st) "HEAD")
  -- what folds in with the message: everything the session is showing as
  -- changed, asked of git rather than remembered, so an agent's edits and
  -- \$EDITOR's edits count the same as the ones typed in here. At a rebase
  -- stop nothing folds in -- :continue owns that.
  folding <- case stRebase st of
    Just _ -> pure []
    Nothing -> liftIO (changedFiles (effectiveMode st) (stRoot st))
  case got of
    Left e -> setMsg e
    Right msg -> do
      let
        ls = if T.null (T.strip msg) then [""] else T.lines msg
        ed = newEdState 0 ls
      modify
        ( \s ->
            s
              { stNotePopup = Just (NotePopup ed (AmendMessage folding))
              , stVi = viIdle
              }
        )

applyAmendMessage :: [FilePath] -> Text -> EventM Name St ()
applyAmendMessage folding text = do
  st <- get
  if T.null text
    then setMsg "empty commit message — amend cancelled"
    else do
      res <- liftIO $ do
        addRes <-
          if null folding
            then pure (Right ())
            else runGit (stRoot st) (["add", "--"] ++ folding)
        case addRes of
          Left e -> pure (Left e)
          Right () ->
            runGit (stRoot st) ["commit", "--amend", "-m", T.unpack text]
      case res of
        Left e -> setMsg e
        Right () -> do
          unless (null folding) (modify (\s -> s {stWritten = Set.empty}))
          forceGitRefresh
          setMsg $
            if null folding
              then "amended the commit message"
              else
                "amended HEAD: message + "
                  <> fileCountLabel (length folding)
                  <> " ("
                  <> foldedNames 3 folding
                  <> ")"

fileCountLabel :: Int -> Text
fileCountLabel n = tshow n <> (if n == 1 then " file" else " files")

-- Names for the status line: a few, then a count for the rest, so the
-- message says what it staged without running off the bar.
foldedNames :: Int -> [FilePath] -> Text
foldedNames k paths =
  let
    shown = map (T.pack . takeFileName) (take k paths)
    more = length paths - length shown
  in
    T.intercalate ", " shown
      <> (if more > 0 then " +" <> tshow more <> " more" else "")
