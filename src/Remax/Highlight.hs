{-# LANGUAGE OverloadedStrings #-}

-- Token segments carry their brick attribute name directly: tree-sitter
-- captures map to "ts.<scope>" names (resolved against the helix theme),
-- and text with no grammar renders plain. No lexer fallback — every
-- dependency of the highlighting path is permissively licensed.
module Remax.Highlight
  ( Seg
  , plainLines
  , plainLine
  , scopeSeg
  , tsScopeAttr
  ) where

import Brick (AttrName, attrName)
import Data.Text (Text)
import qualified Data.Text as T

type Seg = (AttrName, Text)

plainAttr :: AttrName
plainAttr = attrName "plain"

plainLines :: [Text] -> [[Seg]]
plainLines = map plainLine

plainLine :: Text -> [Seg]
plainLine l = [(plainAttr, l)]

-- A tree-sitter capture scope ("function.method") to a segment; the empty
-- scope is an unstyled stretch.
scopeSeg :: Text -> AttrName
scopeSeg "" = plainAttr
scopeSeg scope = tsScopeAttr scope

-- "ts" <> the dotted scope path, so brick's attribute hierarchy resolves
-- "ts.function.method" down to "ts.function" when the theme lacks the
-- deeper entry.
tsScopeAttr :: Text -> AttrName
tsScopeAttr scope =
  foldl (<>) (attrName "ts") (map (attrName . T.unpack) (T.splitOn "." scope))
