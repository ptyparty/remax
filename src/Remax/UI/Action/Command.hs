{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Command
  ( commandTable
  , cmdCandidates
  , openCmdPrompt
  , promptComplete
  , lineAddress
  , runCommand
  ) where

import Brick qualified as Brick
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Remax.Config qualified as Config
import Remax.Git qualified as Git
import Remax.GitHub qualified as GitHub
import Remax.UI.Action.Conversation qualified as Conversation
import Remax.UI.Action.Editor qualified as Editor
import Remax.UI.Action.Notes qualified as NoteAction
import Remax.UI.Action.Open qualified as Open
import Remax.UI.Action.Picker qualified as PickerAction
import Remax.UI.Action.Pull qualified as PullAction
import Remax.UI.Action.Rebase qualified as Rebase
import Remax.UI.Action.Repo qualified as Repo
import Remax.UI.Action.Settings qualified as Settings
import Remax.UI.Action.Tree qualified as TreeAction
import Remax.UI.Action.Verdicts qualified as VerdictAction
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Model.Nav qualified as Nav
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

-- What each command is called, what it takes, and what to tell a human
-- browsing the menu.
data ArgKind
  = NoArg
  | ArgRef
  | ArgMode
  | ArgPath
  | ArgDir
  | ArgText
  | ArgSet
  | ArgPull

commandTable :: [(T.Text, ArgKind, T.Text)]
commandTable =
  [ ("base", ArgRef, "diff against REF; bare :base picks from history")
  , ("branch", NoArg, "whole-branch view (merge-base base)")
  , ("mode", ArgMode, modeChoices)
  , ("rebase", ArgRef, "commit-by-commit review pass")
  , ("continue", NoArg, "fold edits into the stopped commit, advance")
  , ("abort", NoArg, "abort the rebase (asks first)")
  , ("abort!", NoArg, "abort the rebase, no questions")
  , ("amend", NoArg, "edit the commit message; saving amends (+ session edits)")
  , ("accept", ArgDir, "mark everything under a directory reviewed")
  , ("note", ArgText, "note popup (:note TEXT for a one-liner)")
  , ("unnote", NoArg, "remove the note from the target lines")
  , ("resolve", ArgText, "resolve the note here; 'all' sweeps answered ones")
  , ("revert", NoArg, "revert hunk under cursor (or selection) to base")
  , ("open", ArgPath, "open a file in the session")
  , ("close", NoArg, "drop the current file")
  , ("files", NoArg, "fuzzy file finder")
  , ("tree", NoArg, "project tree with per-file diff stats")
  , ("repo", NoArg, "switch to another repository here (picker)")
  , ("help", NoArg, "the keys, grouped (also Space ?)")
  , ("commit", NoArg, "full message of the commit under review / HEAD")
  , ("grep", ArgText, "project-wide search (picker)")
  , ("edit", ArgPath, "$EDITOR at the cursor (or on PATH)")
  , ("set", ArgSet, "change a setting (live + persisted)")
  , ("checkout", NoArg, "switch branches (picker)")
  , ("pr", ArgPull, "open pull request N read-only; bare :pr or :pr list picks one; :pr commits walks its commits; :pr edit checks it out, :pr review returns")
  , ("q", NoArg, "quit")
  ]

modeCommands :: [(T.Text, Brick.EventM Types.Name Types.St ())]
modeCommands =
  [ ("head", Settings.setMode (Git.VsRef "HEAD"))
  , ("unstaged", Settings.setMode Git.VsIndex)
  , ("untracked", Settings.setMode Git.Untracked)
  , ("tree", Settings.returnToTree)
  ]

modeNames :: [T.Text]
modeNames = map fst modeCommands

modeChoices :: T.Text
modeChoices = T.intercalate " | " modeNames

argKindOf :: T.Text -> Maybe ArgKind
argKindOf name =
  case filter (\(n, _, _) -> n == name) commandTable of
    ((_, kind, _) : _) -> Just kind
    [] -> Nothing

-- Candidates for the word being typed: command names before the first
-- space, that command's argument domain after it.
cmdCandidates :: Types.St -> T.Text -> [T.Text]
cmdCandidates st input =
  let
    (done, word) = T.breakOnEnd " " input
    prefixed = filter (word `T.isPrefixOf`)
  in
    case T.words done of
      [] -> prefixed (map (\(n, _, _) -> n) commandTable)
      ["set"] -> prefixed Settings.settingNames
      ["set", "keys"] -> prefixed Config.keyModeNames
      ["set", "note-on-flag"] -> prefixed ["0", "1"]
      ["resolve"] -> prefixed ["all"]
      ["pr"] -> prefixed ["list", "commits", "edit", "review"]
      [cmd] -> case argKindOf cmd of
        Just ArgRef -> prefixed ("branch" : Types.stCmdRefs st)
        Just ArgMode -> prefixed modeNames
        Just ArgPath -> prefixed (Types.stCmdFiles st)
        Just ArgDir -> prefixed (Types.stCmdDirs st ++ Types.stCmdFiles st)
        _ -> []
      _ -> []

openCmdPrompt :: Brick.EventM Types.Name Types.St ()
openCmdPrompt = do
  st <- State.get
  files <- MonadIO.liftIO (Git.listFiles Git.VisibleFiles (Types.stRoot st))
  refs <- MonadIO.liftIO (Git.listRefs (Types.stRoot st))
  let
    dirs =
      Set.toList
        (Set.fromList (concatMap (parentDirs . FilePath.takeDirectory) files))
    parentDirs d =
      if d == "." || d == "/" || null d
        then []
        else d : parentDirs (FilePath.takeDirectory d)
  State.modify $ \s ->
    s
      { Types.stPrompt = Just (Types.CmdPrompt "")
      , Types.stCmdFiles = map T.pack files
      , Types.stCmdDirs = map T.pack dirs
      , Types.stCmdRefs = refs
      , Types.stPromptCycle = Nothing
      }

-- Complete the current word to a candidate; repeated Tab cycles. The
-- originally typed text stays the filter, so cycling never narrows.
promptComplete :: Int -> T.Text -> Brick.EventM Types.Name Types.St ()
promptComplete step input = do
  st <- State.get
  let
    (base, prevIx) = case Types.stPromptCycle st of
      Just (b, i) -> (b, i + step)
      Nothing -> (input, if step > 0 then 0 else -1)
    cands = cmdCandidates st base
  case cands of
    [] -> pure ()
    _ -> do
      let
        n = length cands
        ix = ((prevIx `mod` n) + n) `mod` n
        cand = cands !! ix
        (done, _) = T.breakOnEnd " " base
        suffix =
          case (T.null done, argKindOf cand) of
            (True, Just NoArg) -> ""
            (True, Just _) -> " "
            _ -> ""
        completed = done <> cand <> suffix
        -- accepting a command that takes arguments moves to a fresh
        -- stage: the next Tab completes the argument, not the verb
        nextCycle =
          if T.null suffix then Just (base, ix) else Nothing
      State.modify $ \s ->
        s
          { Types.stPrompt = Just (Types.CmdPrompt completed)
          , Types.stPromptCycle = nextCycle
          }

-- A bare number on the command line is a line address, ex-style.
-- Read as an Integer and saturated, so an absurdly wide number still
-- means "the end of the file" rather than wrapping around to the top.
lineAddress :: T.Text -> Maybe Int
lineAddress t = case TR.decimal t of
  Right (n, rest)
    | T.null rest ->
        Just (fromInteger (min n (fromIntegral (maxBound :: Int))))
  _ -> Nothing

runCommand :: T.Text -> Brick.EventM Types.Name Types.St ()
runCommand input =
  case T.words input of
    [] -> pure ()
    ["q"] -> Brick.halt
    ["base"] -> PickerAction.openBasePicker
    ["checkout"] -> PickerAction.openBranchPicker
    ["checkout", branch] -> PickerAction.applyCheckout branch
    ["pr"] -> barePullCommand
    ["pr", "list"] -> PickerAction.openPullPicker
    ["pr", "commits"] -> PullAction.startCommitWalk
    ["pr", "edit"] -> PullAction.editPull
    ["pr", "review"] -> PullAction.reviewPull
    ["pr", n] | Just k <- GitHub.pullNumber n -> PullAction.openPull k
    ["base", "branch"] -> Settings.setBaseBranch
    ["branch"] -> Settings.setBaseBranch
    ["base", ref] -> Settings.setBaseRef ref
    ["mode", m] | Just switch <- lookup m modeCommands -> switch
    ["ref"] -> PickerAction.openRefPicker
    ["ref", ref] -> Settings.setRef ref
    ["mode", other] ->
      Types.setMsg ("unknown mode " <> other <> " — " <> modeChoices)
    ["rebase"] -> Rebase.startRebase Nothing
    ["rebase", ref] -> Rebase.startRebase (Just ref)
    ["continue"] -> Rebase.continueRebase
    ["abort"] -> Types.setMsg "abort the rebase and lose its progress? :abort! confirms"
    ["abort!"] -> Rebase.abortRebase
    ["amend"] -> Rebase.amendHead
    ["note"] -> NoteAction.openNotePopup Types.NoteAsked
    ("note" : rest) -> NoteAction.noteCmd (T.unwords rest)
    ["unnote"] -> NoteAction.unnoteCmd
    ["revert"] -> VerdictAction.revertTarget
    ["walk"] -> VerdictAction.cycleWalkFilter
    ["tour"] -> VerdictAction.resumeTour
    ("tour" : "ask" : rest) -> VerdictAction.askTour (T.unwords rest)
    ["tour", n] | Just k <- lineAddress n -> VerdictAction.gotoTourStep k
    ["walk", which] -> setWalkNamed which
    ["resolve"] -> NoteAction.unnoteCmd
    ["resolve", "all"] -> Conversation.resolveAllAnswered
    ["accept", path] -> VerdictAction.acceptPath path
    ["set"] -> Settings.showSettings
    ["set", key] -> Settings.showSetting key
    ["set", key, val] -> Settings.applySet key val
    ["open", path] -> Open.openFilePath path
    ["close"] -> Open.closeCurrent
    ["files"] -> TreeAction.openFileFinder
    ["tree"] -> TreeAction.openTree
    ["help"] -> Types.showHelp
    ["repo"] -> PickerAction.openRepoPicker
    ["repo", name] -> switchRepoNamed name
    ["commit"] -> PickerAction.openCommitPicker
    ["commit", ref] -> PickerAction.reviewCommit ref
    ["message"] -> VerdictAction.showCommitPopup
    ("grep" : rest) -> PickerAction.openGrepPicker (T.unwords rest)
    ["edit"] -> Editor.editCurrent
    ["edit", path] -> editPath path
    [tok] | Just n <- lineAddress tok -> Nav.gotoLine n
    _ -> Types.setMsg ("unknown command: " <> input)

barePullCommand :: Brick.EventM Types.Name Types.St ()
barePullCommand = do
  st <- State.get
  case Git.subjectOf (Types.stMode st) of
    Git.Pull pr _ -> Types.setMsg ("PR #" <> Types.tshow (Git.prNumber pr) <> " has no PR view yet \8212 :pr list picks a pull request")
    _ -> PickerAction.openPullPicker

setWalkNamed :: T.Text -> Brick.EventM Types.Name Types.St ()
setWalkNamed which =
  case filter ((== which) . Types.walkLabel) Types.walkCycle of
    (chosen : _) -> VerdictAction.setWalkFilter chosen
    [] -> Types.setMsg ("walk = " <> T.intercalate " | " (map Types.walkLabel Types.walkCycle))

switchRepoNamed :: T.Text -> Brick.EventM Types.Name Types.St ()
switchRepoNamed name = do
  Repo.rescanRepos
  st <- State.get
  case filter ((== T.unpack name) . FilePath.takeFileName) (Types.stRoots st) of
    (root : _) -> Repo.switchRepo root
    [] -> Types.setMsg ("no repository called " <> name <> " here")

editPath :: T.Text -> Brick.EventM Types.Name Types.St ()
editPath path = Types.whenWritable $ \grant -> do
  st <- State.get
  Editor.launchEditor grant (Types.stRoot st </> T.unpack path) 1
