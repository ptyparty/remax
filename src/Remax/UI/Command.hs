{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Command
  ( commandTable
  , cmdCandidates
  , openCmdPrompt
  , promptComplete
  , lineAddress
  , runCommand
  ) where

import Brick
  ( EventM
  , halt
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
  ( get
  , modify
  )
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Remax.Config
import Remax.Git
  ( FileScope (..)
  , ReviewMode (..)
  , listFiles
  , listRefs
  )
import Remax.UI.Conversation
import Remax.UI.Editor
import Remax.UI.Nav
import Remax.UI.Notes
import Remax.UI.Open
import Remax.UI.Picker
import Remax.UI.Rebase
import Remax.UI.Repo
import Remax.UI.Settings
import Remax.UI.Tree
import Remax.UI.Types
import Remax.UI.Verdicts
import System.FilePath
  ( takeDirectory
  , takeFileName
  , (</>)
  )

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
  | ArgFilter

commandTable :: [(Text, ArgKind, Text)]
commandTable =
  [ ("base", ArgRef, "diff against REF; bare :base picks from history")
  , ("branch", NoArg, "whole-branch view (merge-base base)")
  , ("mode", ArgMode, "head | unstaged | untracked")
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
  , ("filter", ArgFilter, "walk all | code | comments (also Space k)")
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
  , ("q", NoArg, "quit")
  ]

argKindOf :: Text -> Maybe ArgKind
argKindOf name =
  case filter (\(n, _, _) -> n == name) commandTable of
    ((_, kind, _) : _) -> Just kind
    [] -> Nothing

-- Candidates for the word being typed: command names before the first
-- space, that command's argument domain after it.
cmdCandidates :: St -> Text -> [Text]
cmdCandidates st input =
  let
    (done, word) = T.breakOnEnd " " input
    prefixed = filter (word `T.isPrefixOf`)
  in
    case T.words done of
      [] -> prefixed (map (\(n, _, _) -> n) commandTable)
      ["set"] -> prefixed settingNames
      ["set", "keys"] -> prefixed keyModeNames
      ["set", "note-on-flag"] -> prefixed ["0", "1"]
      ["resolve"] -> prefixed ["all"]
      [cmd] -> case argKindOf cmd of
        Just ArgRef -> prefixed ("branch" : stCmdRefs st)
        Just ArgMode -> prefixed ["head", "unstaged", "untracked"]
        Just ArgPath -> prefixed (stCmdFiles st)
        Just ArgDir -> prefixed (stCmdDirs st ++ stCmdFiles st)
        Just ArgFilter -> prefixed (map runFilterLabel filterCycle)
        _ -> []
      _ -> []

openCmdPrompt :: EventM Name St ()
openCmdPrompt = do
  st <- get
  files <- liftIO (listFiles VisibleFiles (stRoot st))
  refs <- liftIO (listRefs (stRoot st))
  let
    dirs =
      Set.toList
        (Set.fromList (concatMap (parentDirs . takeDirectory) files))
    parentDirs d =
      if d == "." || d == "/" || null d
        then []
        else d : parentDirs (takeDirectory d)
  modify $ \s ->
    s
      { stPrompt = Just (CmdPrompt "")
      , stCmdFiles = map T.pack files
      , stCmdDirs = map T.pack dirs
      , stCmdRefs = refs
      , stPromptCycle = Nothing
      }

-- Complete the current word to a candidate; repeated Tab cycles. The
-- originally typed text stays the filter, so cycling never narrows.
promptComplete :: Int -> Text -> EventM Name St ()
promptComplete step input = do
  st <- get
  let
    (base, prevIx) = case stPromptCycle st of
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
      modify $ \s ->
        s
          { stPrompt = Just (CmdPrompt completed)
          , stPromptCycle = nextCycle
          }

-- A bare number on the command line is a line address, ex-style.
-- Read as an Integer and saturated, so an absurdly wide number still
-- means "the end of the file" rather than wrapping around to the top.
lineAddress :: Text -> Maybe Int
lineAddress t = case TR.decimal t of
  Right (n, rest)
    | T.null rest ->
        Just (fromInteger (min n (fromIntegral (maxBound :: Int))))
  _ -> Nothing

runCommand :: Text -> EventM Name St ()
runCommand input =
  case T.words input of
    [] -> pure ()
    ["q"] -> halt
    ["base"] -> openBasePicker
    ["checkout"] -> openBranchPicker
    ["checkout", branch] -> applyCheckout branch
    ["base", "branch"] -> setBaseBranch
    ["branch"] -> setBaseBranch
    ["base", ref] -> setBase (VsRef ref)
    ["mode", "head"] -> setBase (VsRef "HEAD")
    ["mode", "unstaged"] -> setBase VsIndex
    ["mode", "untracked"] -> setBase Untracked
    ["mode", other] ->
      setMsg ("unknown mode " <> other <> " — head | unstaged | untracked")
    ["rebase"] -> startRebase Nothing
    ["rebase", ref] -> startRebase (Just ref)
    ["continue"] -> continueRebase
    ["abort"] -> setMsg "abort the rebase and lose its progress? :abort! confirms"
    ["abort!"] -> abortRebase
    ["amend"] -> amendHead
    ["note"] -> openNotePopup NoteAsked
    ("note" : rest) -> noteCmd (T.unwords rest)
    ["unnote"] -> unnoteCmd
    ["revert"] -> revertTarget
    ["filter"] -> cycleRunFilter
    ["filter", which] -> setFilterNamed which
    ["resolve"] -> unnoteCmd
    ["resolve", "all"] -> resolveAllAnswered
    ["accept", path] -> acceptPath path
    ["set"] -> showSettings
    ["set", key] -> showSetting key
    ["set", key, val] -> applySet key val
    ["open", path] -> openFilePath path
    ["close"] -> closeCurrent
    ["files"] -> openFileFinder
    ["tree"] -> openTree
    ["help"] -> showHelp
    ["repo"] -> openRepoPicker
    ["repo", name] -> switchRepoNamed name
    ["commit"] -> showCommitPopup
    ("grep" : rest) -> openGrepPicker (T.unwords rest)
    ["edit"] -> editCurrent
    ["edit", path] -> editPath path
    [tok] | Just n <- lineAddress tok -> gotoLine n
    _ -> setMsg ("unknown command: " <> input)

setFilterNamed :: Text -> EventM Name St ()
setFilterNamed which =
  case filter ((== which) . runFilterLabel) filterCycle of
    (chosen : _) -> setRunFilter chosen
    [] -> setMsg ("filter = " <> T.intercalate " | " (map runFilterLabel filterCycle))

switchRepoNamed :: Text -> EventM Name St ()
switchRepoNamed name = do
  rescanRepos
  st <- get
  case filter ((== T.unpack name) . takeFileName) (stRoots st) of
    (root : _) -> switchRepo root
    [] -> setMsg ("no repository called " <> name <> " here")

editPath :: Text -> EventM Name St ()
editPath path = do
  st <- get
  launchEditor (stRoot st </> T.unpack path) 1
