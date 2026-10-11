{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Settings
  ( settingNames
  , showSettings
  , showSetting
  , applySet
  , setBase
  , setBaseBranch
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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Remax.Config
import Remax.DiffModel
import Remax.Git
  ( ReviewMode (..)
  , branchRoot
  , modeLabel
  , refExists
  )
import Remax.Store
import Remax.UI.Load
import Remax.UI.Types
import Remax.Vi
  ( viIdle
  )

-- Settings management: :set with no arguments lists everything, one
-- argument shows a value, two applies it — live and persisted to the
-- config file.
settingNames :: [Text]
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

settingValue :: Config -> Text -> Text
settingValue cfg key =
  case key of
    "keys" -> keyModeToText (cfgKeys cfg)
    "scrolloff" -> tshow (cfgScrolloff cfg)
    "note-wrap" -> tshow (cfgNoteWrap cfg)
    "note-on-flag" -> if cfgNoteOnFlag cfg then "1" else "0"
    "ignore-whitespace" -> if cfgIgnoreWs cfg then "1" else "0"
    "word-diff" -> if cfgWordDiff cfg then "1" else "0"
    "untracked" -> if cfgUntracked cfg then "1" else "0"
    "split-threshold" -> tshow (splitThreshold (cfgSplit cfg))
    "piece-min" -> tshow (pieceMin (cfgSplit cfg))
    "piece-max" -> tshow (pieceMax (cfgSplit cfg))
    _ -> "?"

showSettings :: EventM Name St ()
showSettings = do
  st <- get
  let
    one k = k <> "=" <> settingValue (stConfig st) k
  setMsg (T.intercalate "  " (map one settingNames))

showSetting :: Text -> EventM Name St ()
showSetting key = do
  st <- get
  if key `elem` settingNames
    then setMsg (key <> " = " <> settingValue (stConfig st) key)
    else setMsg ("unknown setting: " <> key)

applySet :: Text -> Text -> EventM Name St ()
applySet key val = do
  st <- get
  let
    cfg = stConfig st
    asInt = case TR.signed TR.decimal val of
      Right (n, rest) | T.null rest -> Just (n :: Int)
      _ -> Nothing
    finish :: Config -> EventM Name St () -> EventM Name St ()
    finish newCfg after = do
      modify (\s -> s {stConfig = newCfg})
      liftIO (persistSetting key val)
      after
      setMsg (":set " <> key <> " = " <> val)
    withInt build after = case asInt of
      Just n -> finish (build n) after
      Nothing -> setMsg (key <> " wants a number")
    splitWith f = f (cfgSplit cfg)
  case key of
    "keys"
      | val `elem` keyModeNames || val == "vim" ->
          -- switching family mid-command would strand a half-typed one
          finish cfg {cfgKeys = keyModeFromText val} (modify (\s -> s {stVi = viIdle}))
      | otherwise -> setMsg ("keys = " <> T.intercalate " | " keyModeNames)
    "scrolloff" -> withInt (\n -> cfg {cfgScrolloff = max 0 n}) (pure ())
    "note-wrap" -> withInt (\n -> cfg {cfgNoteWrap = max 20 n}) (pure ())
    "note-on-flag" ->
      withInt (\n -> cfg {cfgNoteOnFlag = n /= 0}) (pure ())
    -- both change what the diff says, so every file is decorated again
    "ignore-whitespace" ->
      withInt (\n -> cfg {cfgIgnoreWs = n /= 0}) rebuildAllFiles
    "word-diff" ->
      withInt (\n -> cfg {cfgWordDiff = n /= 0}) (pure ())
    -- this one changes which files are in the review at all, so the
    -- change set is asked for again rather than merely redecorated
    "untracked" ->
      withInt (\n -> cfg {cfgUntracked = n /= 0}) refreshGit
    "split-threshold" ->
      withInt
        (\n -> cfg {cfgSplit = splitWith (\sc -> sc {splitThreshold = max 1 n})})
        rebuildAllFiles
    "piece-min" ->
      withInt
        (\n -> cfg {cfgSplit = splitWith (\sc -> sc {pieceMin = max 1 n})})
        rebuildAllFiles
    "piece-max" ->
      withInt
        (\n -> cfg {cfgSplit = splitWith (\sc -> sc {pieceMax = max 2 n})})
        rebuildAllFiles
    _ -> setMsg ("unknown setting: " <> key)

-- Switch what the session reviews against. Each switch closes the pass
-- and opens a new one, so pass history records what was reviewed vs what.
setBase :: ReviewMode -> EventM Name St ()
setBase mode = do
  st <- get
  ok <- case mode of
    VsRef ref -> liftIO (refExists (stRoot st) ref)
    _ -> pure True
  case (ok, mode) of
    (False, VsRef ref) -> setMsg ("no such ref: " <> ref)
    _ -> do
      liftIO (finishPass (stConn st) (stPassId st))
      newPass <- liftIO (beginPass (stConn st) (repoText st) (modeLabel mode))
      modify (\s -> s {stMode = mode, stPassId = newPass})
      forceGitRefresh
      refreshed <- get
      unless
        (null (stFiles refreshed))
        (setMsg ("base: " <> modeLabel mode))

-- The whole-branch view: base = where this branch forked from the
-- mainline. Purely a diff base — no rebase involved.
setBaseBranch :: EventM Name St ()
setBaseBranch = do
  st <- get
  got <- liftIO (branchRoot (stRoot st))
  case got of
    Left e -> setMsg e
    Right sha -> setBase (VsRef (T.take 10 sha))
