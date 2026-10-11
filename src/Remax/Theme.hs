{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Load the user's helix theme so the review colors match the editor they
-- live in. Themes are simple TOML: scope keys mapping to a color name or
-- an inline { fg, bg, modifiers } table, a [palette] of named colors, and
-- an optional `inherits` chain. Only that subset is parsed; anything
-- unrecognized is skipped, and any failure falls back to Nothing so the
-- caller can use its built-in palette.
module Remax.Theme
  ( HelixTheme
  , loadHelixTheme
  , scopeAttr
  , scopeStyleAttr
  , hasScope
  ) where

import Control.Exception qualified as Exception
import Data.Char qualified as Char
import Data.FileEmbed qualified as FileEmbed
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Graphics.Vty qualified as V
import Numeric qualified as Numeric
import System.Directory qualified as Directory
import System.Environment qualified as Env
import System.FilePath ((</>))

data ScopeStyle = ScopeStyle
  { ssFg :: Maybe T.Text
  , ssBg :: Maybe T.Text
  , ssMods :: [T.Text]
  }

data HelixTheme = HelixTheme
  { htScopes :: Map.Map T.Text ScopeStyle
  , htPalette :: Map.Map T.Text T.Text
  }

-- ~/.config/helix/config.toml names the theme; the theme file is looked
-- up in the user's theme directory first, then in any helix runtime on
-- the machine, and finally in the themes vendored into the binary — so
-- an inherits chain resolves even with no helix runtime installed
-- (helix AppImages mount theirs only while helix runs).
loadHelixTheme :: IO (Maybe HelixTheme)
loadHelixTheme = do
  home <- Directory.getHomeDirectory
  name <- themeName home
  case name of
    Nothing -> pure Nothing
    Just n -> loadChain home n 0

themeName :: FilePath -> IO (Maybe T.Text)
themeName home = do
  got <- readMaybeFile (home </> ".config" </> "helix" </> "config.toml")
  pure $ do
    body <- got
    lookup "theme" (topLevelStrings body)

loadChain :: FilePath -> T.Text -> Int -> IO (Maybe HelixTheme)
loadChain home name depth
  | depth > 5 = pure Nothing
  | otherwise = do
      got <- themeBody home name
      case got of
        Nothing -> pure Nothing
        Just body -> do
          let
            (scopes, palette, inherits) = parseTheme body
            self = HelixTheme scopes palette
          case inherits of
            Nothing -> pure (Just self)
            Just parent -> do
              up <- loadChain home parent (depth + 1)
              pure . Just $ case up of
                Nothing -> self
                Just p ->
                  HelixTheme
                    (Map.union scopes (htScopes p))
                    (Map.union palette (htPalette p))

-- The theme's text, from the first place that has it: a theme file on
-- disk, or the vendored copy embedded in the binary.
themeBody :: FilePath -> T.Text -> IO (Maybe T.Text)
themeBody home name = do
  found <- findThemeFile home name
  case found of
    Just path -> readMaybeFile path
    Nothing -> pure (lookup name vendoredThemes)

vendoredThemes :: [(T.Text, T.Text)]
vendoredThemes =
  [ ("jellybeans", TE.decodeUtf8 $(FileEmbed.embedFile "vendor/themes/jellybeans.toml"))
  ]

findThemeFile :: FilePath -> T.Text -> IO (Maybe FilePath)
findThemeFile home name = do
  runtimeEnv <- Env.lookupEnv "HELIX_RUNTIME"
  let
    file = T.unpack name ++ ".toml"
    envCandidates = case runtimeEnv of
      Just rt -> [rt </> "themes" </> file]
      Nothing -> []
    candidates =
      [home </> ".config" </> "helix" </> "themes" </> file]
        ++ envCandidates
        ++ [ home </> ".config" </> "helix" </> "runtime" </> "themes" </> file
           , "/usr/lib/helix/runtime/themes" </> file
           , "/usr/share/helix/runtime/themes" </> file
           ]
    go [] = pure Nothing
    go (p : ps) = do
      ok <- Directory.doesFileExist p
      if ok then pure (Just p) else go ps
  go candidates

readMaybeFile :: FilePath -> IO (Maybe T.Text)
readMaybeFile p = do
  got <- Exception.try (TIO.readFile p) :: IO (Either Exception.IOException T.Text)
  pure (either (const Nothing) Just got)

-- Parsing --------------------------------------------------------------

-- (scopes, palette, inherits)
parseTheme :: T.Text -> (Map.Map T.Text ScopeStyle, Map.Map T.Text T.Text, Maybe T.Text)
parseTheme body =
  let
    go [] _ scopes pal inh = (scopes, pal, inh)
    go (l : ls) inPalette scopes pal inh =
      let
        t = T.strip l
      in
        if T.null t || "#" `T.isPrefixOf` t
          then go ls inPalette scopes pal inh
          else
            if "[" `T.isPrefixOf` t
              then go ls (T.strip t == "[palette]") scopes pal inh
              else case parseKeyValue t of
                Nothing -> go ls inPalette scopes pal inh
                Just (key, rest)
                  | inPalette ->
                      case quotedString rest of
                        Just v ->
                          go ls inPalette scopes (Map.insert key v pal) inh
                        Nothing -> go ls inPalette scopes pal inh
                  | key == "inherits" ->
                      go ls inPalette scopes pal (quotedString rest)
                  | otherwise ->
                      case parseStyle rest of
                        Just s ->
                          go ls inPalette (Map.insert key s scopes) pal inh
                        Nothing -> go ls inPalette scopes pal inh
  in
    go (T.lines body) False Map.empty Map.empty Nothing

-- `"a.b" = rest` or `bare = rest`
parseKeyValue :: T.Text -> Maybe (T.Text, T.Text)
parseKeyValue t = do
  let
    (lhs, rhs) = T.breakOn "=" t
  _ <- if T.null rhs then Nothing else Just ()
  let
    key = T.strip lhs
    cleaned = T.dropAround (== '"') key
  if T.null cleaned then Nothing else Just (cleaned, T.strip (T.drop 1 rhs))

-- A bare color string, or an inline table with fg/bg/modifiers. The
-- nested underline table holds no fg/bg keys, so scanning for those
-- substrings inside the braces is safe for real theme files.
parseStyle :: T.Text -> Maybe ScopeStyle
parseStyle rest
  | "\"" `T.isPrefixOf` rest =
      fmap (\v -> ScopeStyle (Just v) Nothing []) (quotedString rest)
  | "{" `T.isPrefixOf` rest =
      Just
        ScopeStyle
          { ssFg = fieldString "fg" rest
          , ssBg = fieldString "bg" rest
          , ssMods = modifiers rest
          }
  | otherwise = Nothing

quotedString :: T.Text -> Maybe T.Text
quotedString t =
  case T.splitOn "\"" t of
    (_ : v : _) -> Just v
    _ -> Nothing

fieldString :: T.Text -> T.Text -> Maybe T.Text
fieldString name t =
  case T.breakOn (name <> " ") (T.replace (name <> "=") (name <> " =") t) of
    (_, rest)
      | T.null rest -> Nothing
      | otherwise -> quotedString rest

modifiers :: T.Text -> [T.Text]
modifiers t =
  case T.breakOn "modifiers" t of
    (_, rest)
      | T.null rest -> []
      | otherwise ->
          let
            inside =
              T.takeWhile (/= ']') (T.drop 1 (T.dropWhile (/= '[') rest))
            usable m =
              not (T.null (T.strip m)) && T.strip m /= ","
          in
            filter usable (T.splitOn "\"" inside)

-- Only for config.toml's top-level `theme = "name"`.
topLevelStrings :: T.Text -> [(T.Text, T.Text)]
topLevelStrings body =
  let
    beforeSection =
      takeWhile (not . ("[" `T.isPrefixOf`) . T.strip) (T.lines body)
    entry l = do
      (k, rest) <- parseKeyValue (T.strip l)
      v <- quotedString rest
      pure (k, v)
  in
    Maybe.mapMaybe entry beforeSection

-- Color resolution -------------------------------------------------------

resolveColor :: HelixTheme -> T.Text -> Maybe V.Color
resolveColor th raw =
  hexOrNamed (Maybe.fromMaybe raw (Map.lookup raw (htPalette th)))

hexOrNamed :: T.Text -> Maybe V.Color
hexOrNamed t
  | "#" `T.isPrefixOf` t
  , T.length t == 7
  , T.all Char.isHexDigit (T.drop 1 t) =
      let
        hex a b = case Numeric.readHex [a, b] of
          [(n, _)] -> n :: Int
          _ -> 0
      in
        case T.unpack (T.drop 1 t) of
          [r1, r2, g1, g2, b1, b2] ->
            -- linearColor keeps true 24-bit values; rgbColor quantizes
            -- to the 256-color cube at construction time
            Just (V.linearColor (hex r1 r2) (hex g1 g2) (hex b1 b2))
          _ -> Nothing
  | otherwise = lookup t namedColors

namedColors :: [(T.Text, V.Color)]
namedColors =
  [ ("black", V.black)
  , ("red", V.red)
  , ("green", V.green)
  , ("yellow", V.yellow)
  , ("blue", V.blue)
  , ("magenta", V.magenta)
  , ("cyan", V.cyan)
  , ("white", V.white)
  , ("gray", V.brightBlack)
  , ("grey", V.brightBlack)
  , ("light-red", V.brightRed)
  , ("light-green", V.brightGreen)
  , ("light-yellow", V.brightYellow)
  , ("light-blue", V.brightBlue)
  , ("light-magenta", V.brightMagenta)
  , ("light-cyan", V.brightCyan)
  , ("light-gray", V.brightWhite)
  , ("light-grey", V.brightWhite)
  ]

-- helix resolves "a.b.c" by dropping dotted segments until something
-- matches; so does this.
lookupScope :: HelixTheme -> T.Text -> Maybe ScopeStyle
lookupScope th =
  let
    go name =
      case Map.lookup name (htScopes th) of
        Just s -> Just s
        Nothing ->
          let
            parts = T.splitOn "." name
          in
            if length parts <= 1
              then Nothing
              else go (T.intercalate "." (init parts))
  in
    go

styleToAttr :: HelixTheme -> ScopeStyle -> V.Attr -> V.Attr
styleToAttr th s base =
  let
    withFg a = case ssFg s >>= resolveColor th of
      Just c -> V.withForeColor a c
      Nothing -> a
    withBg a = case ssBg s >>= resolveColor th of
      Just c -> V.withBackColor a c
      Nothing -> a
    applyMod a m = case m of
      "bold" -> V.withStyle a V.bold
      "italic" -> V.withStyle a V.italic
      "dim" -> V.withStyle a V.dim
      "underlined" -> V.withStyle a V.underline
      "crossed_out" -> V.withStyle a V.strikethrough
      "reversed" -> V.withStyle a V.reverseVideo
      _ -> a
  in
    foldl applyMod (withBg (withFg base)) (ssMods s)

-- Whether the theme resolves this scope (directly or via dotted fallback).
hasScope :: HelixTheme -> T.Text -> Bool
hasScope th name =
  case lookupScope th name of
    Just _ -> True
    Nothing -> False

-- The attr for a scope, or the fallback when the theme has nothing for it.
scopeAttr :: Maybe HelixTheme -> T.Text -> V.Attr -> V.Attr
scopeAttr mth name fallback =
  case mth of
    Nothing -> fallback
    Just th ->
      case lookupScope th name of
        Just s -> styleToAttr th s V.defAttr
        Nothing -> fallback

-- Like scopeAttr but building on a base attr (used for the default).
scopeStyleAttr :: Maybe HelixTheme -> T.Text -> V.Attr -> V.Attr
scopeStyleAttr mth name base =
  case mth of
    Nothing -> base
    Just th ->
      case lookupScope th name of
        Just s -> styleToAttr th s base
        Nothing -> base
