{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Picker
  ( pickerLayer
  ) where

import Brick ((<+>))
import Brick qualified as Brick
import Brick.Types qualified as BrickTypes
import Data.Text qualified as T
import Remax.UI.Action.Fuzzy qualified as Fuzzy
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Widgets qualified as Widgets

pickerLayer :: Types.Picker -> BrickTypes.Widget Types.Name
pickerLayer pk =
  BrickTypes.Widget BrickTypes.Greedy BrickTypes.Greedy $ do
    ctx <- BrickTypes.getContext
    let
      boxWidth = min 150 (max 50 (BrickTypes.availWidth ctx - 16))
      visibleCount = max 5 (BrickTypes.availHeight ctx - 14)
      start = max 0 (Types.pkSel pk - (visibleCount - 1))
      shown = take visibleCount (drop start (zip [0 ..] (Types.pkMatches pk)))
      inputRow =
        Brick.hBox
          [ Brick.withAttr Attrs.statusAttr (Brick.txt (" " <> Types.pkInput pk <> "\9612"))
          , Brick.padLeft Brick.Max (Brick.withAttr Attrs.hintAttr (Brick.txt (pickerCounter pk <> " ")))
          ]
      emptyRow = Brick.withAttr Attrs.hintAttr (Brick.txt "  nothing matches")
      matchRows =
        if null shown
          then emptyRow : replicate (visibleCount - 1) (Brick.txt " ")
          else map (pickerMatchRow pk boxWidth) shown ++ replicate (visibleCount - length shown) (Brick.txt " ")
    BrickTypes.render $
      Widgets.floatFrame (pickerTitle (Types.pkMode pk)) (pickerHint (Types.pkMode pk)) $
        Brick.hLimit boxWidth (Brick.vBox (inputRow : Brick.txt " " : matchRows))

pickerMatchRow :: Types.Picker -> Int -> (Int, T.Text) -> BrickTypes.Widget Types.Name
pickerMatchRow pk boxWidth (i, display) =
  let
    shownText = T.take (boxWidth - 4) display
    content = case Types.pkMode pk of
      Types.PickGrep -> Fuzzy.highlightInfix (Types.pkInput pk) shownText
      _ -> Fuzzy.highlightSubseq (Types.pkInput pk) shownText
    line = Brick.padRight Brick.Max (Brick.txt " " <+> content)
  in
    if i == Types.pkSel pk
      then Brick.withDefAttr Attrs.floatSelAttr (Brick.withAttr Attrs.floatSelBarAttr (Brick.txt "\9614") <+> line)
      else Brick.txt " " <+> line

pickerCounter :: Types.Picker -> T.Text
pickerCounter pk =
  case Types.pkMode pk of
    Types.PickGrep -> Types.tshow (length (Types.pkMatches pk)) <> " hits"
    _ -> Types.tshow (length (Types.pkMatches pk)) <> "/" <> Types.tshow (length (Types.pkAll pk))

pickerTitle :: Types.PickerMode -> T.Text
pickerTitle mode =
  case mode of
    Types.PickFiles -> "files"
    Types.PickBase -> "base"
    Types.PickBranch -> "checkout"
    Types.PickRef -> "ref"
    Types.PickCommit -> "commit"
    Types.PickParent _ -> "parent"
    Types.PickRepo -> "repo"
    Types.PickGrep -> "grep"
    Types.PickPull -> "open pull requests"

pickerHint :: Types.PickerMode -> T.Text
pickerHint mode =
  case mode of
    Types.PickBase -> "\8593\8595 move · \9166 review against this · esc closes"
    Types.PickBranch -> "\8593\8595 move · \9166 checks out · esc closes"
    Types.PickRef -> "\8593\8595 move · \9166 review this ref without checking it out · esc closes"
    Types.PickCommit -> "\8593\8595 move · \9166 opens a ref's history, or reviews a commit against its parent · esc closes"
    Types.PickParent _ -> "\8593\8595 move · \9166 review against this parent · esc closes"
    Types.PickRepo -> "\8593\8595 move · \9166 switch to this repository · esc closes"
    Types.PickPull -> "\9166 open in review mode · Esc close · :pr N skips the picker"
    _ -> "\8593\8595 move · \9166 open · esc closes"
