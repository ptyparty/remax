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

import Control.Exception (IOException, try)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import System.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath (takeDirectory, (</>))

import Data.List (isPrefixOf)
import System.FilePath (dropTrailingPathSeparator)

import Remax.DiffModel (SplitConfig (..), defaultSplitConfig)

-- Which keybinding family drives the buffer: helix-style modal,
-- vi-style modal, or modeless emacs chords.
data KeyMode = KeysHelix | KeysVi | KeysEmacs
  deriving (Eq)

-- The one place the keymap names are spelled, so the config file, `:set
-- keys`, and its completion cannot drift apart. Anything unrecognized
-- reads as the default, matching how the rest of the file is parsed.
keyModeFromText :: Text -> KeyMode
keyModeFromText raw =
  case T.strip raw of
    "emacs" -> KeysEmacs
    "vi" -> KeysVi
    "vim" -> KeysVi
    _ -> KeysHelix

keyModeToText :: KeyMode -> Text
keyModeToText mode =
  case mode of
    KeysHelix -> "helix"
    KeysVi -> "vi"
    KeysEmacs -> "emacs"

keyModeNames :: [Text]
keyModeNames = ["helix", "vi", "emacs"]

data Config = Config
  { cfgKeys :: KeyMode
  , cfgScrolloff :: Int
  , cfgNoteWrap :: Int
  , cfgNoteOnFlag :: Bool -- ! and ? open the note popup immediately
  , cfgSplit :: SplitConfig
  }

defaultConfig :: Config
defaultConfig =
  Config
    { cfgKeys = KeysHelix
    , cfgScrolloff = 5
    , cfgNoteWrap = 58
    , cfgNoteOnFlag = True
    , cfgSplit = defaultSplitConfig
    }

configPath :: IO FilePath
configPath = do
  dir <- getXdgDirectory XdgConfig "remax"
  pure (dir </> "config.toml")

loadConfig :: IO Config
loadConfig = do
  path <- configPath
  got <- try (TIO.readFile path) :: IO (Either IOException Text)
  pure $ case got of
    Left _ -> defaultConfig
    Right body -> fromEntries (parseEntries body)

-- Raw key/value pairs; values may be numbers or bare words.
parseEntries :: Text -> [(Text, Text)]
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
    mapMaybe entry (T.lines body)

-- Persist one setting: replace its line in the config file (create the
-- file if needed), keeping everything else — comments included — as-is.
persistSetting :: Text -> Text -> IO ()
persistSetting key value = do
  path <- configPath
  got <- try (TIO.readFile path) :: IO (Either IOException Text)
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
  createDirectoryIfMissing True (takeDirectory path)
  TIO.writeFile path (T.unlines replaced)

-- Per-repository exclusions: .remaxignore at the repo root, one pattern
-- per line ('#' comments). Two forms: a path prefix, matched by
-- component (`gen` covers gen/a.txt, not generated.hs), and `*suffix`
-- for basename endings (`*.min.js`). Ignored files never enter the
-- change set; :open still works on them.
loadIgnores :: FilePath -> IO [Text]
loadIgnores root = do
  got <-
    try (TIO.readFile (root </> ".remaxignore")) ::
      IO (Either IOException Text)
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
        mapMaybe keep (T.lines body)

ignoredPath :: [Text] -> FilePath -> Bool
ignoredPath pats path =
  let
    matchOne pat =
      case T.stripPrefix "*" pat of
        Just suffix -> suffix `T.isSuffixOf` T.pack path
        Nothing ->
          let
            p = dropTrailingPathSeparator (T.unpack pat)
          in
            path == p || (p ++ "/") `isPrefixOf` path
  in
    any matchOne pats

fromEntries :: [(Text, Text)] -> Config
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
      , cfgSplit =
          SplitConfig
            { splitThreshold =
                max 1 (getInt "split-threshold" (splitThreshold baseSplit))
            , pieceMin = max 1 (getInt "piece-min" (pieceMin baseSplit))
            , pieceMax = max 2 (getInt "piece-max" (pieceMax baseSplit))
            }
      }
