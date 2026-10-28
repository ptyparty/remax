{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Submit
  ( submitLayer
  ) where

import Brick qualified as Brick
import Brick.Types qualified as BrickTypes
import Data.Text qualified as T
import Remax.Config qualified as Config
import Remax.Git qualified as Git
import Remax.GitHub qualified as GitHub
import Remax.Modal qualified as Modal
import Remax.Submit qualified as Submit
import Remax.UI.Action.Submit qualified as SubmitAction
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Widgets qualified as Widgets

submitLayer :: Types.St -> Types.SubmitDialog -> BrickTypes.Widget Types.Name
submitLayer st dialog =
  let
    number = case Git.subjectOf (Types.stMode st) of
      Git.Pull pr _ -> "#" <> Types.tshow (Git.prNumber pr)
      _ -> ""
    title = "submit review \183 " <> number <> (if Types.sdSending dialog then " \183 sending\8230" else "")
    width = 94
    items = Types.sdItems dialog
    itemRows = zipWith (itemRow st dialog (width - 4)) [0 ..] items
    going =
      Brick.withAttr Attrs.hintAttr (Brick.txt "going out \183 ")
        Brick.<+> Brick.withAttr Attrs.noteInlineAttr (Brick.txt (Types.tshow (length items)))
        Brick.<+> Brick.withAttr Attrs.hintAttr (Brick.txt ("   " <> Submit.countsText (map Submit.placedItem items)))
    failure = case Types.sdFailure dialog of
      Just why -> [Brick.withAttr Attrs.conflictAttr (Brick.txt (Widgets.clipT (width - 4) ("\9888 " <> why))), Brick.txt " "]
      Nothing -> []
    body =
      Brick.vBox
        ( [ eventRow dialog
          , Brick.txt " "
          , Brick.withAttr Attrs.hintAttr (Brick.txt "summary")
          , summaryRows dialog (width - 4)
          , Brick.txt " "
          ]
            ++ failure
            ++ [going]
            ++ (if null items then [Brick.withAttr Attrs.hintAttr (Brick.txt "  nothing pending")] else itemRows)
        )
  in
    Widgets.floatFrame title (hint (Config.cfgKeys (Types.stConfig st)) dialog) (Brick.hLimit width (Brick.padRight Brick.Max (Brick.padLeftRight 1 body)))

hint :: Config.KeyMode -> Types.SubmitDialog -> T.Text
hint keys dialog =
  case Types.sdFocus dialog of
    Types.SummaryFocus ->
      case Modal.edMode (Types.sdSummary dialog) of
        Modal.Insert -> "Esc \9166 posts \183 " <> Types.cancelKeyText keys <> " closes"
        Modal.Normal -> "\9166 posts \183 h/l picks the event \183 Tab goes to the list \183 i edits \183 Esc closes"
    Types.ListFocus -> "\8593\8595 moves \183 e edits \183 dd drops \183 \9166 jumps to its line \183 Tab or Esc goes to the summary"

eventRow :: Types.SubmitDialog -> BrickTypes.Widget Types.Name
eventRow dialog =
  let
    radio event label =
      let
        on = Types.sdEvent dialog == Just event
        dimmed = Types.sdCommentOnly dialog && event /= GitHub.EventComment
      in
        Brick.withAttr
          (if on then Attrs.noteInlineAttr else if dimmed then Attrs.hairAttr else Attrs.msgAttr)
          (Brick.txt ((if on then "(\8226) " else "( ) ") <> label <> "   "))
  in
    Brick.hBox
      [ Brick.withAttr Attrs.hintAttr (Brick.txt "event    ")
      , radio GitHub.EventComment "Comment"
      , radio GitHub.EventApprove "Approve"
      , radio GitHub.EventRequestChanges "Request changes"
      ]

summaryRows :: Types.SubmitDialog -> Int -> BrickTypes.Widget Types.Name
summaryRows dialog width =
  let
    ed = Types.sdSummary dialog
    ls = Modal.bufferLines ed
    (cr, cc) = Modal.cursorRC ed
    shown = take 6 (zip [0 ..] ls)
    row (i, l) = Brick.txt (Widgets.padT (T.take width l) <> (if i == (0 :: Int) && null (drop 1 ls) && T.null l then "" else ""))
    focused = Types.sdFocus dialog == Types.SummaryFocus
    widget = Brick.vBox (map row shown)
  in
    if focused
      then Brick.showCursor Types.TheCursor (Brick.Location (min width cc, min 5 cr)) widget
      else widget

itemRow :: Types.St -> Types.SubmitDialog -> Int -> Int -> Submit.Placed -> BrickTypes.Widget Types.Name
itemRow st dialog width i placed =
  let
    (glyph, place, what, first) = SubmitAction.itemLabel st placed
    selected = Types.sdFocus dialog == Types.ListFocus && Types.sdSel dialog == i
    refused = case Submit.placedAs placed of
      Submit.Unplaceable why -> " \183 " <> why
      _ -> ""
    line =
      glyph
        <> " "
        <> T.justifyLeft 14 ' ' (T.take 14 place)
        <> " "
        <> T.justifyLeft 16 ' ' (T.take 16 what)
        <> " "
        <> Widgets.clipT (max 8 (width - 36)) (first <> refused)
  in
    Brick.withAttr (if selected then Attrs.floatSelAttr else Attrs.msgAttr) (Brick.txt (Widgets.padT line))
