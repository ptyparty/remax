{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Help
  ( helpLayer
  , spaceMenu
  , pendingMenuLayer
  ) where

import Brick
  ( Padding (Max)
  , Widget
  , hBox
  , hLimit
  , padRight
  , txt
  , vBox
  , withAttr
  )
import Brick.Types
  ( Size (Greedy)
  , Widget (Widget)
  , availWidth
  , getContext
  , render
  )
import Data.Text (Text)
import qualified Data.Text as T
import Remax.Config
import Remax.Modal
import Remax.UI.Action.Menus
import Remax.UI.Core.Attrs
import Remax.UI.Core.Types
import Remax.UI.Core.Widgets

-- Two columns, split where the row counts come out even, so the sheet is
-- one glance rather than a scroll.
helpLayer :: KeyMode -> Widget Name
helpLayer mode =
  Widget Greedy Greedy $ do
    ctx <- getContext
    let
      groups = helpGroups mode
      colWidth = max 36 (min 50 (div (availWidth ctx - 24) 2))
      keyWidth = 13
      entryRow (key, desc) =
        hBox
          [ hLimit keyWidth (padRight Max (withAttr noteInlineAttr (txt (" " <> key))))
          , withAttr statusAttr (txt desc)
          ]
      groupRows (name, entries) =
        (withAttr floatTitleAttr (txt (" " <> name)) : map entryRow entries)
          ++ [txt " "]
      height g = length (snd g) + 2
      half = div (sum (map height groups)) 2
      -- take a group into the left column while doing so lands closer to
      -- the halfway mark than stopping would; a plain "stop once over
      -- half" split leaves the columns lopsided
      pack used pending =
        case pending of
          [] -> ([], [])
          (g : rest)
            | used == 0
                || abs (used + height g - half) <= abs (half - used) ->
                let
                  (a, b) = pack (used + height g) rest
                in
                  (g : a, b)
            | otherwise -> ([], g : rest)
      (leftGroups, rightGroups) = pack 0 groups
      column shown = vBox (concatMap groupRows shown)
    render $
      floatFrame "keys" "any key closes" $
        hBox
          [ hLimit colWidth (padRight Max (column leftGroups))
          , hLimit colWidth (padRight Max (column rightGroups))
          ]

spaceMenu :: Widget Name
spaceMenu =
  let
    item (key, label, _) =
      hBox
        [ withAttr noteInlineAttr (txt (" " <> key))
        , withAttr statusAttr (txt ("  " <> label))
        ]
  in
    cornerFrame "space" "any other key cancels" . hLimit 26 . padRight Max $
      vBox (map item spaceMenuEntries)

-- What a half-typed prefix is waiting for.
--
-- The m menu reads its own dispatch table, so it cannot describe itself
-- wrongly. The g menu cannot: half its keys are the modal layer's
-- (handleG) and half are review-level interceptions in handleHelixKey,
-- so this list is written out and has to be kept beside them.
--
-- f, t and r are left out on purpose. They wait for any character at
-- all, and "type a character" is not worth a box in the corner.
pendingMenu :: Text -> Maybe (Text, [(Text, Text)])
pendingMenu label =
  let
    pairs =
      [ ("( ) [ ] { } < >", "brackets")
      , ("\" ' `", "quotes")
      ]
    objects = pairs ++ [("w W", "word under the cursor")]
    fromMatch = map (\(k, what, _) -> (T.singleton k, what)) matchMenu
  in
    case label of
      "g" ->
        Just
          ( "goto"
          ,
            [ ("g", "first line")
            , ("e", "last line")
            , ("h", "line start")
            , ("l", "line end")
            , ("s", "first non-blank")
            , ("a", "alternate file")
            , ("n", "next file")
            , ("p", "previous file")
            , ("r", "another repository")
            ]
          )
      "m" -> Just ("match", fromMatch)
      "ms" -> Just ("surround with", pairs)
      "md" -> Just ("delete surround", pairs)
      "mr" -> Just ("replace surround", pairs)
      "mi" -> Just ("select inside", objects)
      "ma" -> Just ("select around", objects)
      _ -> Nothing

pendingMenuLayer :: St -> Maybe (Widget Name)
pendingMenuLayer st = do
  fv <- currentFileMay st
  label <- pendingLabel (fvEd fv)
  (title, entries) <- pendingMenu label
  let
    -- the key column is padded to the widest of them, so the labels line
    -- up even where a key is a row of brackets rather than one letter
    keyW = maximum (map (T.length . fst) entries)
    item (key, what) =
      hBox
        [ withAttr noteInlineAttr (txt (" " <> T.justifyLeft keyW ' ' key))
        , withAttr statusAttr (txt ("  " <> what))
        ]
    width = 5 + keyW + maximum (map (T.length . snd) entries)
  pure
    ( cornerFrame title "any other key cancels" . hLimit width . padRight Max $
        vBox (map item entries)
    )
