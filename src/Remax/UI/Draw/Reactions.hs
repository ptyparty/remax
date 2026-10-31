{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Reactions
  ( reactionLayer
  ) where

import Brick qualified as Brick
import Brick.Types qualified as BrickTypes
import Data.List qualified as List
import Remax.Conversation qualified as Conv
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Widgets qualified as Widgets

reactionLayer :: Types.ReactionPicker -> BrickTypes.Widget Types.Name
reactionLayer picker =
  let
    counts = Types.rkReactions picker
    countOf r = List.find ((== r) . Conv.rcReaction) counts
    cell (n, r) =
      let
        found = countOf r
        mine = maybe False Conv.rcMine found
        total = maybe "" (\c -> " " <> Types.tshow (Conv.rcCount c)) found
        label = Types.tshow (n :: Int) <> " " <> Conv.reactionGlyph r <> " " <> Conv.reactionText r <> total <> (if mine then " \183 you" else "")
      in
        Brick.withAttr (if mine then Attrs.questionMarkAttr else Attrs.floatAttr) (Brick.hLimit 18 (Brick.padRight Brick.Max (Brick.txt label)))
    cells = map cell (zip [1 ..] Conv.allReactions)
    rows = [take 4 cells, drop 4 cells]
    preview = Brick.withAttr Attrs.hintAttr (Brick.txt (Widgets.clipT 70 (Types.rkPreview picker)))
  in
    Widgets.floatFrame
      ("react \183 " <> Types.rkOwner picker)
      "1-8 posts at once \183 one of yours again removes it \183 Esc closes"
      (Brick.vBox (preview : Brick.txt " " : map Brick.hBox rows))
