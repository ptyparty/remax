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

import Brick qualified as Brick
import Data.Text qualified as T

type Seg = (Brick.AttrName, T.Text)

plainAttr :: Brick.AttrName
plainAttr = Brick.attrName "plain"

plainLines :: [T.Text] -> [[Seg]]
plainLines = map plainLine

plainLine :: T.Text -> [Seg]
plainLine l = [(plainAttr, l)]

-- A tree-sitter capture scope ("function.method") to a segment; the empty
-- scope is an unstyled stretch.
scopeSeg :: T.Text -> Brick.AttrName
scopeSeg "" = plainAttr
scopeSeg scope = tsScopeAttr scope

-- "ts" <> the dotted scope path, so brick's attribute hierarchy resolves
-- "ts.function.method" down to "ts.function" when the theme lacks the
-- deeper entry.
tsScopeAttr :: T.Text -> Brick.AttrName
tsScopeAttr scope =
  foldl (<>) (Brick.attrName "ts") (map (Brick.attrName . T.unpack) (T.splitOn "." scope))
