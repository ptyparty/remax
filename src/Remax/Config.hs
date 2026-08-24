{-# LANGUAGE OverloadedStrings #-}

-- User-tunable knobs, read from ~/.config/remax/config.toml. The format
-- is a flat `key = number` list ('#' comments allowed); unknown keys and
-- unparsable lines are skipped, and a missing file means the defaults —
-- the same tolerance the theme loader practices.
module Remax.Config
  ( Config (..)
  , defaultConfig
  , loadConfig
  , loadIgnores
  , ignoredPath
  ) where

import Control.Exception (IOException, try)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Data.List (isPrefixOf)
import System.FilePath (dropTrailingPathSeparator)

import Remax.DiffModel (SplitConfig (..), defaultSplitConfig)

data Config = Config
  { cfgScrolloff :: Int
  , cfgNoteWrap :: Int
  , cfgNoteOnFlag :: Bool -- ! and ? open the note popup immediately
  , cfgSplit :: SplitConfig
  }

defaultConfig :: Config
defaultConfig =
  Config
    { cfgScrolloff = 5
    , cfgNoteWrap = 58
    , cfgNoteOnFlag = True
    , cfgSplit = defaultSplitConfig
    }

loadConfig :: IO Config
loadConfig = do
  home <- getHomeDirectory
  got <-
    try (TIO.readFile (home </> ".config" </> "remax" </> "config.toml"))
      :: IO (Either IOException Text)
  pure $ case got of
    Left _ -> defaultConfig
    Right body -> fromEntries (parseEntries body)

parseEntries :: Text -> [(Text, Int)]
parseEntries body =
  let entry line = do
        let t = T.strip line
        _ <- if T.null t || "#" `T.isPrefixOf` t then Nothing else Just ()
        let (lhs, rhs) = T.breakOn "=" t
        _ <- if T.null rhs then Nothing else Just ()
        n <- case TR.signed TR.decimal (T.strip (T.drop 1 rhs)) of
          Right (v, rest) | T.null (T.strip rest) -> Just v
          _ -> Nothing
        pure (T.strip lhs, n)
  in mapMaybe entry (T.lines body)

-- Per-repository exclusions: .remaxignore at the repo root, one pattern
-- per line ('#' comments). Two forms: a path prefix, matched by
-- component (`gen` covers gen/a.txt, not generated.hs), and `*suffix`
-- for basename endings (`*.min.js`). Ignored files never enter the
-- change set; :open still works on them.
loadIgnores :: FilePath -> IO [Text]
loadIgnores root = do
  got <-
    try (TIO.readFile (root </> ".remaxignore"))
      :: IO (Either IOException Text)
  pure $ case got of
    Left _ -> []
    Right body ->
      let keep line =
            let t = T.strip line
            in if T.null t || "#" `T.isPrefixOf` t then Nothing else Just t
      in mapMaybe keep (T.lines body)

ignoredPath :: [Text] -> FilePath -> Bool
ignoredPath pats path =
  let matchOne pat =
        case T.stripPrefix "*" pat of
          Just suffix -> suffix `T.isSuffixOf` T.pack path
          Nothing ->
            let p = dropTrailingPathSeparator (T.unpack pat)
            in path == p || (p ++ "/") `isPrefixOf` path
  in any matchOne pats

fromEntries :: [(Text, Int)] -> Config
fromEntries entries =
  let get key fallback = fromMaybe fallback (lookup key entries)
      base = defaultConfig
      baseSplit = cfgSplit base
  in Config
       { cfgScrolloff = max 0 (get "scrolloff" (cfgScrolloff base))
       , cfgNoteWrap = max 20 (get "note-wrap" (cfgNoteWrap base))
       , cfgNoteOnFlag = get "note-on-flag" 1 /= 0
       , cfgSplit =
           SplitConfig
             { splitThreshold =
                 max 1 (get "split-threshold" (splitThreshold baseSplit))
             , pieceMin = max 1 (get "piece-min" (pieceMin baseSplit))
             , pieceMax = max 2 (get "piece-max" (pieceMax baseSplit))
             }
       }
