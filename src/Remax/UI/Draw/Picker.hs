{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Picker
  ( pickerLayer
  ) where

import Brick
  ( Padding (Max)
  , Widget
  , hBox
  , hLimit
  , padLeft
  , padRight
  , txt
  , vBox
  , withAttr
  , withDefAttr
  , (<+>)
  )
import Brick.Types
  ( Size (Greedy)
  , Widget (Widget)
  , availHeight
  , availWidth
  , getContext
  , render
  )
import Data.Text (Text)
import qualified Data.Text as T
import Remax.UI.Attrs
import Remax.UI.Fuzzy
import Remax.UI.Types
import Remax.UI.Widgets

pickerLayer :: Picker -> Widget Name
pickerLayer pk =
  Widget Greedy Greedy $ do
    ctx <- getContext
    let
      boxWidth = min 150 (max 50 (availWidth ctx - 16))
      visibleCount = max 5 (availHeight ctx - 14)
      start = max 0 (pkSel pk - (visibleCount - 1))
      shown = take visibleCount (drop start (zip [0 ..] (pkMatches pk)))
      inputRow =
        hBox
          [ withAttr statusAttr (txt (" " <> pkInput pk <> "\9612"))
          , padLeft Max (withAttr hintAttr (txt (pickerCounter pk <> " ")))
          ]
      emptyRow = withAttr hintAttr (txt "  nothing matches")
      matchRows =
        if null shown
          then emptyRow : replicate (visibleCount - 1) (txt " ")
          else map (pickerMatchRow pk boxWidth) shown ++ replicate (visibleCount - length shown) (txt " ")
    render $
      floatFrame (pickerTitle (pkMode pk)) (pickerHint (pkMode pk)) $
        hLimit boxWidth (vBox (inputRow : txt " " : matchRows))

pickerMatchRow :: Picker -> Int -> (Int, Text) -> Widget Name
pickerMatchRow pk boxWidth (i, display) =
  let
    shownText = T.take (boxWidth - 4) display
    content = case pkMode pk of
      PickGrep -> highlightInfix (pkInput pk) shownText
      _ -> highlightSubseq (pkInput pk) shownText
    line = padRight Max (txt " " <+> content)
  in
    if i == pkSel pk
      then withDefAttr floatSelAttr (withAttr floatSelBarAttr (txt "\9614") <+> line)
      else txt " " <+> line

pickerCounter :: Picker -> Text
pickerCounter pk =
  case pkMode pk of
    PickGrep -> tshow (length (pkMatches pk)) <> " hits"
    _ -> tshow (length (pkMatches pk)) <> "/" <> tshow (length (pkAll pk))

pickerTitle :: PickerMode -> Text
pickerTitle mode =
  case mode of
    PickFiles -> "files"
    PickBase -> "base"
    PickBranch -> "checkout"
    PickRepo -> "repo"
    PickGrep -> "grep"

pickerHint :: PickerMode -> Text
pickerHint mode =
  case mode of
    PickBase -> "\8593\8595 move · \9166 review against this · esc closes"
    PickBranch -> "\8593\8595 move · \9166 checks out · esc closes"
    PickRepo -> "\8593\8595 move · \9166 switch to this repository · esc closes"
    _ -> "\8593\8595 move · \9166 open · esc closes"
