{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Cite
  ( citeLayer
  ) where

import Brick qualified as Brick
import Brick.Types qualified as BrickTypes
import Data.Text qualified as T
import Remax.Git qualified as Git
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Widgets qualified as Widgets

citeLayer :: Types.CitePicker -> BrickTypes.Widget Types.Name
citeLayer picker =
  let
    commits = Types.cpCommits picker
    ageWidth = maximum (0 : map (T.length . Git.ccAge) commits)
    pushed c = case Git.ccPushed c of
      Git.Unpushed -> "unpushed  "
      Git.OnPullHead -> "on the PR "
      Git.NoPull -> ""
    row (i, c) =
      let
        line = Git.ccSha c <> "  " <> pushed c <> T.justifyLeft ageWidth ' ' (Git.ccAge c) <> "  " <> Git.ccSubject c
        attr
          | i == Types.cpAt picker = Attrs.selAttr
          | Git.ccPushed c == Git.Unpushed = Attrs.questionMarkAttr
          | otherwise = Attrs.floatAttr
      in
        Brick.withAttr attr (Brick.txt (Widgets.padT (Widgets.clipT 80 line)))
    shown = take 15 (drop (max 0 (Types.cpAt picker - 14)) (zip [0 ..] commits))
  in
    Widgets.floatFrame "cite a commit" "\8593\8595 moves \183 \9166 inserts the sha \183 Esc closes" (Brick.hLimit 82 (Brick.vBox (map row shown)))
