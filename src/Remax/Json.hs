{-# LANGUAGE OverloadedStrings #-}

module Remax.Json
  ( Json (..)
  , encode
  , decode
  , field
  ) where

import Data.Char qualified as Char
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Numeric qualified as Numeric

data Json
  = JNull
  | JBool Bool
  | JNumber T.Text
  | JString T.Text
  | JArray [Json]
  | JObject [(T.Text, Json)]
  deriving (Eq, Show)

encode :: Json -> T.Text
encode json =
  case json of
    JNull -> "null"
    JBool b -> if b then "true" else "false"
    JNumber n -> n
    JString s -> encodeString s
    JArray xs -> "[" <> T.intercalate "," (map encode xs) <> "]"
    JObject kvs -> "{" <> T.intercalate "," (map (\(k, v) -> encodeString k <> ":" <> encode v) kvs) <> "}"

encodeString :: T.Text -> T.Text
encodeString s =
  let
    escape c =
      case c of
        '"' -> "\\\""
        '\\' -> "\\\\"
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        _
          | Char.ord c < 0x20 -> "\\u" <> T.justifyRight 4 '0' (T.pack (Numeric.showHex (Char.ord c) ""))
          | otherwise -> T.singleton c
  in
    "\"" <> T.concatMap escape s <> "\""

field :: T.Text -> Json -> Maybe Json
field key json =
  case json of
    JObject kvs -> lookup key kvs
    _ -> Nothing

decode :: T.Text -> Maybe Json
decode t =
  case value (T.stripStart t) of
    Just (json, rest) | T.null (T.strip rest) -> Just json
    _ -> Nothing

value :: T.Text -> Maybe (Json, T.Text)
value t =
  case T.uncons t of
    Just ('{', rest) -> object (T.stripStart rest) []
    Just ('[', rest) -> array (T.stripStart rest) []
    Just ('"', rest) -> fmap (\(s, more) -> (JString s, more)) (string rest "")
    Just _
      | Just rest <- T.stripPrefix "null" t -> Just (JNull, rest)
      | Just rest <- T.stripPrefix "true" t -> Just (JBool True, rest)
      | Just rest <- T.stripPrefix "false" t -> Just (JBool False, rest)
      | otherwise ->
          let
            (n, rest) = T.span (`elem` ("+-.eE0123456789" :: String)) t
          in
            if T.null n then Nothing else Just (JNumber n, rest)
    Nothing -> Nothing

object :: T.Text -> [(T.Text, Json)] -> Maybe (Json, T.Text)
object t acc =
  case T.uncons t of
    Just ('}', rest) | null acc -> Just (JObject [], rest)
    Just ('"', rest) -> do
      (key, afterKey) <- string rest ""
      afterColon <- T.stripPrefix ":" (T.stripStart afterKey)
      (v, afterValue) <- value (T.stripStart afterColon)
      let
        kvs = (key, v) : acc
      case T.uncons (T.stripStart afterValue) of
        Just (',', more) -> object (T.stripStart more) kvs
        Just ('}', more) -> Just (JObject (reverse kvs), more)
        _ -> Nothing
    _ -> Nothing

array :: T.Text -> [Json] -> Maybe (Json, T.Text)
array t acc =
  case T.uncons t of
    Just (']', rest) | null acc -> Just (JArray [], rest)
    _ -> do
      (v, afterValue) <- value t
      case T.uncons (T.stripStart afterValue) of
        Just (',', more) -> array (T.stripStart more) (v : acc)
        Just (']', more) -> Just (JArray (reverse (v : acc)), more)
        _ -> Nothing

string :: T.Text -> String -> Maybe (T.Text, T.Text)
string t acc =
  case T.uncons t of
    Just ('"', rest) -> Just (T.pack (reverse acc), rest)
    Just ('\\', rest) -> do
      (c, more) <- T.uncons rest
      case c of
        'u' -> do
          let
            (hex, after) = T.splitAt 4 more
          code <- Maybe.listToMaybe (Maybe.mapMaybe (\(n, leftover) -> if null leftover then Just n else Nothing) (Numeric.readHex (T.unpack hex)))
          string after (Char.chr code : acc)
        _ -> do
          unescaped <- lookup c [('"', '"'), ('\\', '\\'), ('/', '/'), ('n', '\n'), ('r', '\r'), ('t', '\t'), ('b', '\b'), ('f', '\f')]
          string more (unescaped : acc)
    Just (c, rest) -> string rest (c : acc)
    Nothing -> Nothing
