{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Settings
  ( settingNames
  , showSettings
  , showSetting
  , applySet
  , toggleIgnoreWs
  , setMode
  , returnToTree
  , setBaseRef
  , setRef
  , setBaseBranch
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Remax.Config qualified as Config
import Remax.DiffModel qualified as DiffModel
import Remax.Git qualified as Git
import Remax.Store qualified as Store
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Model.Load qualified as Load
import Remax.Vi qualified as Vi

-- Settings management: :set with no arguments lists everything, one
-- argument shows a value, two applies it — live and persisted to the
-- config file.
settingNames :: [T.Text]
settingNames =
  [ "keys"
  , "scrolloff"
  , "note-wrap"
  , "note-on-flag"
  , "ignore-whitespace"
  , "word-diff"
  , "untracked"
  , "split-threshold"
  , "piece-min"
  , "piece-max"
  ]

settingValue :: Config.Config -> T.Text -> T.Text
settingValue cfg key =
  case key of
    "keys" -> Config.keyModeToText (Config.cfgKeys cfg)
    "scrolloff" -> Types.tshow (Config.cfgScrolloff cfg)
    "note-wrap" -> Types.tshow (Config.cfgNoteWrap cfg)
    "note-on-flag" -> if Config.cfgNoteOnFlag cfg then "1" else "0"
    "ignore-whitespace" -> if Config.cfgIgnoreWs cfg then "1" else "0"
    "word-diff" -> if Config.cfgWordDiff cfg then "1" else "0"
    "untracked" -> if Config.cfgUntracked cfg then "1" else "0"
    "split-threshold" -> Types.tshow (DiffModel.splitThreshold (Config.cfgSplit cfg))
    "piece-min" -> Types.tshow (DiffModel.pieceMin (Config.cfgSplit cfg))
    "piece-max" -> Types.tshow (DiffModel.pieceMax (Config.cfgSplit cfg))
    _ -> "?"

showSettings :: Brick.EventM Types.Name Types.St ()
showSettings = do
  st <- State.get
  let
    one k = k <> "=" <> settingValue (Types.stConfig st) k
  Types.setMsg (T.intercalate "  " (map one settingNames))

showSetting :: T.Text -> Brick.EventM Types.Name Types.St ()
showSetting key = do
  st <- State.get
  if key `elem` settingNames
    then Types.setMsg (key <> " = " <> settingValue (Types.stConfig st) key)
    else Types.setMsg ("unknown setting: " <> key)

applySet :: T.Text -> T.Text -> Brick.EventM Types.Name Types.St ()
applySet key val = do
  st <- State.get
  let
    cfg = Types.stConfig st
    asInt = case TR.signed TR.decimal val of
      Right (n, rest) | T.null rest -> Just (n :: Int)
      _ -> Nothing
    finish :: Config.Config -> Brick.EventM Types.Name Types.St () -> Brick.EventM Types.Name Types.St ()
    finish newCfg after = do
      State.modify (\s -> s {Types.stConfig = newCfg})
      MonadIO.liftIO (Config.persistSetting key val)
      after
      Types.setMsg (":set " <> key <> " = " <> val)
    withInt build after = case asInt of
      Just n -> finish (build n) after
      Nothing -> Types.setMsg (key <> " wants a number")
    splitWith f = f (Config.cfgSplit cfg)
  case key of
    "keys"
      | val `elem` Config.keyModeNames || val == "vim" ->
          -- switching family mid-command would strand a half-typed one
          finish cfg {Config.cfgKeys = Config.keyModeFromText val} (State.modify (\s -> s {Types.stVi = Vi.viIdle}))
      | otherwise -> Types.setMsg ("keys = " <> T.intercalate " | " Config.keyModeNames)
    "scrolloff" -> withInt (\n -> cfg {Config.cfgScrolloff = max 0 n}) (pure ())
    "note-wrap" -> withInt (\n -> cfg {Config.cfgNoteWrap = max 20 n}) (pure ())
    "note-on-flag" ->
      withInt (\n -> cfg {Config.cfgNoteOnFlag = n /= 0}) (pure ())
    -- both change what the diff says, so every file is decorated again
    "ignore-whitespace" ->
      withInt (\n -> cfg {Config.cfgIgnoreWs = n /= 0}) Load.rebuildAllFiles
    "word-diff" ->
      withInt (\n -> cfg {Config.cfgWordDiff = n /= 0}) (pure ())
    -- this one changes which files are in the review at all, so the
    -- change set is asked for again rather than merely redecorated
    "untracked" ->
      withInt (\n -> cfg {Config.cfgUntracked = n /= 0}) Load.refreshGit
    "split-threshold" ->
      withInt
        (\n -> cfg {Config.cfgSplit = splitWith (\sc -> sc {DiffModel.splitThreshold = max 1 n})})
        Load.rebuildAllFiles
    "piece-min" ->
      withInt
        (\n -> cfg {Config.cfgSplit = splitWith (\sc -> sc {DiffModel.pieceMin = max 1 n})})
        Load.rebuildAllFiles
    "piece-max" ->
      withInt
        (\n -> cfg {Config.cfgSplit = splitWith (\sc -> sc {DiffModel.pieceMax = max 2 n})})
        Load.rebuildAllFiles
    _ -> Types.setMsg ("unknown setting: " <> key)

toggleIgnoreWs :: Brick.EventM Types.Name Types.St ()
toggleIgnoreWs = do
  st <- State.get
  applySet "ignore-whitespace" (if Config.cfgIgnoreWs (Types.stConfig st) then "0" else "1")

-- Switch what the session reviews against. Each switch closes the pass
-- and opens a new one, so pass history records what was reviewed vs what.
setMode :: Git.ReviewMode -> Brick.EventM Types.Name Types.St ()
setMode mode = do
  st <- State.get
  ok <- case mode of
    Git.VsRef ref -> MonadIO.liftIO (Git.refExists (Types.stRoot st) ref)
    _ -> pure True
  case (ok, mode) of
    (False, Git.VsRef ref) -> Types.setMsg ("no such ref: " <> ref)
    _ -> installMode st mode

installMode :: Types.St -> Git.ReviewMode -> Brick.EventM Types.Name Types.St ()
installMode st mode = do
  let
    subject = Git.subjectOf mode
    changedSubject = subject /= Git.subjectOf (Types.stMode st)
    treeMode = case subject of
      Git.WorkingTree -> mode
      Git.Ref _ -> Types.stTreeMode st
  MonadIO.liftIO (Store.finishPass (Types.stConn st) (Types.stPassId st))
  newPass <- MonadIO.liftIO (Store.beginPass (Types.stConn st) (Types.repoText st) (Git.modeLabel mode))
  State.modify $ \s ->
    s
      { Types.stMode = mode
      , Types.stTreeMode = treeMode
      , Types.stPassId = newPass
      , Types.stFiles = if changedSubject then [] else Types.stFiles s
      , Types.stOpened = if changedSubject then Set.empty else Types.stOpened s
      }
  Load.forceGitRefresh
  refreshed <- State.get
  Monad.unless
    (null (Types.stFiles refreshed))
    (Types.setMsg ("base: " <> Git.modeLabel mode))

returnToTree :: Brick.EventM Types.Name Types.St ()
returnToTree = do
  st <- State.get
  setMode (Types.stTreeMode st)

setBaseRef :: T.Text -> Brick.EventM Types.Name Types.St ()
setBaseRef base = do
  st <- State.get
  ok <- MonadIO.liftIO (Git.refExists (Types.stRoot st) base)
  if not ok
    then Types.setMsg ("no such ref: " <> base)
    else setMode $ case Types.stMode st of
      Git.AtRef rr -> Git.AtRef rr {Git.rrBase = base}
      _ -> Git.VsRef base

subjectTip :: Types.St -> T.Text
subjectTip st =
  case Git.subjectOf (Types.stMode st) of
    Git.Ref tip -> tip
    Git.WorkingTree -> "HEAD"

-- The whole-branch view: base = where this branch forked from the
-- mainline. Purely a diff base — no rebase involved.
setBaseBranch :: Brick.EventM Types.Name Types.St ()
setBaseBranch = do
  st <- State.get
  got <- MonadIO.liftIO (Git.branchRootOf (Types.stRoot st) (subjectTip st))
  case got of
    Left e -> Types.setMsg e
    Right sha -> setBaseRef (T.take 10 sha)

setRef :: T.Text -> Brick.EventM Types.Name Types.St ()
setRef tip = do
  st <- State.get
  ok <- MonadIO.liftIO (Git.refExists (Types.stRoot st) tip)
  if not ok
    then Types.setMsg ("no such ref: " <> tip)
    else do
      got <- MonadIO.liftIO (Git.branchRootOf (Types.stRoot st) tip)
      case got of
        Left e -> Types.setMsg e
        Right sha -> setMode (Git.AtRef (Git.RefReview {Git.rrTip = tip, Git.rrBase = T.take 10 sha}))
