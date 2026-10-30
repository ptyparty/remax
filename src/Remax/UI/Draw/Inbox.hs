{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Inbox
  ( inboxLayer
  , inboxReply
  ) where

import Brick qualified as Brick
import Brick.Types qualified as BrickTypes
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.Config qualified as Config
import Remax.Conversation qualified as Conv
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Inbox qualified as Inbox
import Remax.UI.Core.PullView qualified as PullView
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.View qualified as View
import Remax.UI.Core.Widgets qualified as Widgets
import Remax.UI.Draw.Notes qualified as DrawNotes
import Remax.UI.Draw.PullView qualified as DrawPullView

inboxLayer :: Types.St -> Types.Inbox -> BrickTypes.Widget Types.Name
inboxLayer st inbox =
  BrickTypes.Widget BrickTypes.Greedy BrickTypes.Greedy $ do
    ctx <- BrickTypes.getContext
    let
      convs = Inbox.entries st (Types.ibFilter inbox)
      chosen = Inbox.selected inbox convs
      geometry = View.inboxGeometry (BrickTypes.availWidth ctx)
      boxWidth = View.igBox geometry
      bodyRows = max 6 (BrickTypes.availHeight ctx - 10)
      listWidth = View.igList geometry
      previewWidth = View.igPreview geometry
      title = "feedback \183 " <> Types.tshow (length convs)
      body =
        Brick.hBox
          [ Brick.hLimit listWidth (Brick.vLimit bodyRows (listColumn st inbox listWidth bodyRows convs chosen))
          , Brick.withAttr Attrs.hintAttr (Brick.hLimit 1 (Brick.vLimit bodyRows (Brick.fill '\9474')))
          , Brick.txt " "
          , Brick.hLimit previewWidth (Brick.vLimit bodyRows (rightColumn st inbox previewWidth bodyRows chosen))
          ]
      shownHint = maybe (hint (Config.cfgKeys (Types.stConfig st)) inbox) (DrawNotes.notePopupHint st) (inboxReply st)
    BrickTypes.render (Widgets.floatFrame title shownHint (Brick.hLimit boxWidth body))

inboxReply :: Types.St -> Maybe Types.NotePopup
inboxReply st = do
  np <- Types.stNotePopup st
  case View.popupPlacement st np of
    View.InInbox -> Just np
    View.Floating -> Nothing

rightColumn :: Types.St -> Types.Inbox -> Int -> Int -> Maybe Conv.Conversation -> BrickTypes.Widget Types.Name
rightColumn st inbox width rows chosen =
  case inboxReply st of
    Just np ->
      Brick.padBottom Brick.Max $
        Brick.vBox
          [ Brick.withAttr Attrs.noteInlineAttr (Brick.txt (Widgets.clipT width (DrawNotes.popupTitle np)))
          , Brick.txt " "
          , Brick.padRight Brick.Max (DrawNotes.notePopupPane st np width (rows - 2))
          ]
    Nothing -> previewColumn st inbox width chosen

listColumn :: Types.St -> Types.Inbox -> Int -> Int -> [Conv.Conversation] -> Maybe Conv.Conversation -> BrickTypes.Widget Types.Name
listColumn st inbox width rows convs chosen =
  let
    filtering = Types.ibFocus inbox == Types.InboxFiltering
    filterRow =
      if filtering || not (T.null (Types.ibFilter inbox))
        then [Brick.withAttr (if filtering then Attrs.noteInlineAttr else Attrs.hintAttr) (Brick.txt (Widgets.padT ("/ " <> Types.ibFilter inbox)))]
        else []
    room = rows - length filterRow
    at = Maybe.fromMaybe 0 (chosen >>= \c -> List.findIndex ((== Conv.convId c) . Conv.convId) convs)
    start = if at < room then 0 else at - room + 1
    isChosen conv = fmap Conv.convId chosen == Just (Conv.convId conv)
    shown = take room (drop start convs)
    entryRows = case shown of
      [] -> [Brick.withAttr Attrs.hintAttr (Brick.txt "nothing matches")]
      _ -> map (\conv -> entryRow st width (isChosen conv) conv) shown
  in
    Brick.padRight Brick.Max (Brick.vBox (filterRow ++ entryRows))

entryRow :: Types.St -> Int -> Bool -> Conv.Conversation -> BrickTypes.Widget Types.Name
entryRow st width isChosen conv =
  let
    s = Inbox.summary st conv
    line =
      Inbox.suGlyph s
        <> " "
        <> Inbox.suPlace s
        <> " \183 "
        <> Inbox.suAuthor s
        <> " \183 "
        <> Inbox.suAge s
        <> " \183 "
        <> Inbox.suText s
    attr
      | isChosen = Attrs.floatSelAttr
      | otherwise = case Inbox.suTone s of
          Inbox.Awaiting -> Attrs.noteInlineAttr
          Inbox.Settled -> Attrs.hintAttr
          Inbox.Ordinary -> Attrs.msgAttr
  in
    Brick.withAttr attr (Brick.txt (Widgets.padT (Widgets.clipT width line)))

previewColumn :: Types.St -> Types.Inbox -> Int -> Maybe Conv.Conversation -> BrickTypes.Widget Types.Name
previewColumn st inbox width chosen =
  case chosen of
    Nothing -> Brick.txt " "
    Just conv ->
      let
        rows = drop (Types.ibScroll inbox) (PullView.conversationRows st (width - 4) conv)
      in
        Brick.padBottom Brick.Max (Brick.vBox (map (DrawPullView.rowWidget False) rows))

hint :: Config.KeyMode -> Types.Inbox -> T.Text
hint keys inbox =
  case (Types.ibFocus inbox, keys) of
    (Types.InboxFiltering, _) -> "type to filter \183 \9166 keeps it \183 Esc clears it"
    (Types.InboxList, Config.KeysEmacs) -> "C-n/C-p moves \183 \9166 goes to it \183 r replies \183 / filters \183 C-d/C-u scrolls \183 C-c C-k closes"
    (Types.InboxList, _) -> "j/k moves \183 \9166 goes to it \183 r replies \183 / filters \183 C-d/C-u scrolls \183 Esc closes"
