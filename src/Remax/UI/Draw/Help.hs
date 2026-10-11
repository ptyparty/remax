{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Help
  ( helpLayer
  , spaceMenu
  , pendingMenuLayer
  ) where

import Brick qualified as Brick
import Brick.Types qualified as BrickTypes
import Data.Text qualified as T
import Remax.Config qualified as Config
import Remax.Modal qualified as Modal
import Remax.UI.Action.Menus qualified as Menus
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Widgets qualified as Widgets

-- Two columns, split where the row counts come out even, so the sheet is
-- one glance rather than a scroll.
helpLayer :: Config.KeyMode -> BrickTypes.Widget Types.Name
helpLayer mode =
  BrickTypes.Widget BrickTypes.Greedy BrickTypes.Greedy $ do
    ctx <- BrickTypes.getContext
    let
      groups = Menus.helpGroups mode
      colWidth = max 36 (min 50 (div (BrickTypes.availWidth ctx - 24) 2))
      keyWidth = 13
      entryRow (key, desc) =
        Brick.hBox
          [ Brick.hLimit keyWidth (Brick.padRight Brick.Max (Brick.withAttr Attrs.noteInlineAttr (Brick.txt (" " <> key))))
          , Brick.withAttr Attrs.statusAttr (Brick.txt desc)
          ]
      groupRows (name, entries) =
        (Brick.withAttr Attrs.floatTitleAttr (Brick.txt (" " <> name)) : map entryRow entries)
          ++ [Brick.txt " "]
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
      column shown = Brick.vBox (concatMap groupRows shown)
    BrickTypes.render $
      Widgets.floatFrame "keys" "any key closes" $
        Brick.hBox
          [ Brick.hLimit colWidth (Brick.padRight Brick.Max (column leftGroups))
          , Brick.hLimit colWidth (Brick.padRight Brick.Max (column rightGroups))
          ]

spaceMenu :: BrickTypes.Widget Types.Name
spaceMenu =
  let
    item (key, label, _) =
      Brick.hBox
        [ Brick.withAttr Attrs.noteInlineAttr (Brick.txt (" " <> key))
        , Brick.withAttr Attrs.statusAttr (Brick.txt ("  " <> label))
        ]
  in
    Widgets.cornerFrame "space" "any other key cancels" . Brick.hLimit 26 . Brick.padRight Brick.Max $
      Brick.vBox (map item Menus.spaceMenuEntries)

-- What a half-typed prefix is waiting for.
--
-- The m menu reads its own dispatch table, so it cannot describe itself
-- wrongly. The g menu cannot: half its keys are the modal layer's
-- (handleG) and half are review-level interceptions in handleHelixKey,
-- so this list is written out and has to be kept beside them.
--
-- f, t and r are left out on purpose. They wait for any character at
-- all, and "type a character" is not worth a box in the corner.
pendingMenu :: T.Text -> Maybe (T.Text, [(T.Text, T.Text)])
pendingMenu label =
  let
    pairs =
      [ ("( ) [ ] { } < >", "brackets")
      , ("\" ' `", "quotes")
      ]
    objects = pairs ++ [("w W", "word under the cursor")]
    fromMatch = map (\(k, what, _) -> (T.singleton k, what)) Modal.matchMenu
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

pendingMenuLayer :: Types.St -> Maybe (BrickTypes.Widget Types.Name)
pendingMenuLayer st = do
  fv <- Types.currentFileMay st
  label <- Modal.pendingLabel (Types.fvEd fv)
  (title, entries) <- pendingMenu label
  let
    -- the key column is padded to the widest of them, so the labels line
    -- up even where a key is a row of brackets rather than one letter
    keyW = maximum (map (T.length . fst) entries)
    item (key, what) =
      Brick.hBox
        [ Brick.withAttr Attrs.noteInlineAttr (Brick.txt (" " <> T.justifyLeft keyW ' ' key))
        , Brick.withAttr Attrs.statusAttr (Brick.txt ("  " <> what))
        ]
    width = 5 + keyW + maximum (map (T.length . snd) entries)
  pure
    ( Widgets.cornerFrame title "any other key cancels" . Brick.hLimit width . Brick.padRight Brick.Max $
        Brick.vBox (map item entries)
    )
