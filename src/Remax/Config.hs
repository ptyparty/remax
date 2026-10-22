{-# LANGUAGE OverloadedStrings #-}

-- User-tunable knobs, read from the XDG config directory (usually
-- ~/.config/remax/config.toml). The format
-- is a flat `key = number` list ('#' comments allowed); unknown keys and
-- unparsable lines are skipped, and a missing file means the defaults —
-- the same tolerance the theme loader practices.
module Remax.Config
  ( Config (..)
  , KeyMode (..)
  , keyModeFromText
  , keyModeToText
  , keyModeNames
  , defaultConfig
  , loadConfig
  , persistSetting
  , loadIgnores
  , ignoredPath
  ) where

import Control.Exception qualified as Exception
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read qualified as TR
import System.Directory qualified as Directory
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

import Data.List qualified as List

import Remax.DiffModel qualified as DiffModel

-- Which keybinding family drives the buffer: helix-style modal,
-- vi-style modal, or modeless emacs chords.
data KeyMode = KeysHelix | KeysVi | KeysEmacs
  deriving (Eq)

-- The one place the keymap names are spelled, so the config file, `:set
-- keys`, and its completion cannot drift apart. Anything unrecognized
-- reads as the default, matching how the rest of the file is parsed.
keyModeFromText :: T.Text -> KeyMode
keyModeFromText raw =
  case T.strip raw of
    "emacs" -> KeysEmacs
    "vi" -> KeysVi
    "vim" -> KeysVi
    _ -> KeysHelix

keyModeToText :: KeyMode -> T.Text
keyModeToText mode =
  case mode of
    KeysHelix -> "helix"
    KeysVi -> "vi"
    KeysEmacs -> "emacs"

keyModeNames :: [T.Text]
keyModeNames = ["helix", "vi", "emacs"]

data Config = Config
  { cfgKeys :: KeyMode
  , cfgScrolloff :: Int
  , cfgNoteWrap :: Int
  , cfgNoteOnFlag :: Bool -- ! and ? open the note popup immediately
  , cfgIgnoreWs :: Bool -- compare lines with their whitespace removed
  , cfgWordDiff :: Bool -- mark the words that differ within a changed line (on)
  , cfgUntracked :: Bool -- show files git does not track yet (on)
  , cfgSettle :: Bool -- accepted lines glow briefly as their wash clears (on)
  , cfgSplit :: DiffModel.SplitConfig
  }

defaultConfig :: Config
defaultConfig =
  Config
    { cfgKeys = KeysHelix
    , cfgScrolloff = 5
    , cfgNoteWrap = 58
    , cfgNoteOnFlag = True
    , cfgIgnoreWs = False
    , cfgWordDiff = True
    , cfgUntracked = True
    , cfgSettle = True
    , cfgSplit = DiffModel.defaultSplitConfig
    }

configPath :: IO FilePath
configPath = do
  dir <- Directory.getXdgDirectory Directory.XdgConfig "remax"
  pure (dir </> "config.toml")

loadConfig :: IO Config
loadConfig = do
  path <- configPath
  got <- Exception.try (TIO.readFile path) :: IO (Either Exception.IOException T.Text)
  pure $ case got of
    Left _ -> defaultConfig
    Right body -> fromEntries (parseEntries body)

-- Raw key/value pairs; values may be numbers or bare words.
parseEntries :: T.Text -> [(T.Text, T.Text)]
parseEntries body =
  let
    entry line = do
      let
        t = T.strip line
      _ <- if T.null t || "#" `T.isPrefixOf` t then Nothing else Just ()
      let
        (lhs, rhs) = T.breakOn "=" t
      _ <- if T.null rhs then Nothing else Just ()
      pure (T.strip lhs, T.strip (T.drop 1 rhs))
  in
    Maybe.mapMaybe entry (T.lines body)

-- Persist one setting: replace its line in the config file (create the
-- file if needed), keeping everything else — comments included — as-is.
persistSetting :: T.Text -> T.Text -> IO ()
persistSetting key value = do
  path <- configPath
  got <- Exception.try (TIO.readFile path) :: IO (Either Exception.IOException T.Text)
  let
    body = either (const "") id got
    ls = T.lines body
    isKeyLine l = case parseEntries l of
      ((k, _) : _) -> k == key
      [] -> False
    newLine = key <> " = " <> value
    replaced =
      if any isKeyLine ls
        then map (\l -> if isKeyLine l then newLine else l) ls
        else ls ++ [newLine]
  Directory.createDirectoryIfMissing True (FilePath.takeDirectory path)
  TIO.writeFile path (T.unlines replaced)

-- Per-repository exclusions: .remaxignore at the repo root, one pattern
-- per line ('#' comments). Two forms: a path prefix, matched by
-- component (`gen` covers gen/a.txt, not generated.hs), and `*suffix`
-- for basename endings (`*.min.js`). Ignored files never enter the
-- change set; :open still works on them.
loadIgnores :: FilePath -> IO [T.Text]
loadIgnores root = do
  got <-
    Exception.try (TIO.readFile (root </> ".remaxignore")) ::
      IO (Either Exception.IOException T.Text)
  pure $ case got of
    Left _ -> []
    Right body ->
      let
        keep line =
          let
            t = T.strip line
          in
            if T.null t || "#" `T.isPrefixOf` t then Nothing else Just t
      in
        Maybe.mapMaybe keep (T.lines body)

ignoredPath :: [T.Text] -> FilePath -> Bool
ignoredPath pats path =
  let
    matchOne pat =
      case T.stripPrefix "*" pat of
        Just suffix -> suffix `T.isSuffixOf` T.pack path
        Nothing ->
          let
            p = FilePath.dropTrailingPathSeparator (T.unpack pat)
          in
            path == p || (p ++ "/") `List.isPrefixOf` path
  in
    any matchOne pats

fromEntries :: [(T.Text, T.Text)] -> Config
fromEntries entries =
  let
    getInt key fallback =
      case lookup key entries of
        Nothing -> fallback
        Just raw -> case TR.signed TR.decimal raw of
          Right (v, rest) | T.null (T.strip rest) -> v
          _ -> fallback
    base = defaultConfig
    baseSplit = cfgSplit base
  in
    Config
      { cfgKeys = keyModeFromText (maybe "" id (lookup "keys" entries))
      , cfgScrolloff = max 0 (getInt "scrolloff" (cfgScrolloff base))
      , cfgNoteWrap = max 20 (getInt "note-wrap" (cfgNoteWrap base))
      , cfgNoteOnFlag = getInt "note-on-flag" (1 :: Int) /= 0
      , cfgIgnoreWs = getInt "ignore-whitespace" (0 :: Int) /= 0
      , cfgWordDiff = getInt "word-diff" (1 :: Int) /= 0
      , cfgUntracked = getInt "untracked" (1 :: Int) /= 0
      , cfgSettle = getInt "settle" (1 :: Int) /= 0
      , cfgSplit =
          DiffModel.SplitConfig
            { DiffModel.splitThreshold =
                max 1 (getInt "split-threshold" (DiffModel.splitThreshold baseSplit))
            , DiffModel.pieceMin = max 1 (getInt "piece-min" (DiffModel.pieceMin baseSplit))
            , DiffModel.pieceMax = max 2 (getInt "piece-max" (DiffModel.pieceMax baseSplit))
            }
      }
