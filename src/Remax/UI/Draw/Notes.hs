{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Notes
  ( notePopupLayer
  ) where

import Brick ((<+>))
import Brick qualified as Brick
import Brick.Types qualified as BrickTypes
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.Conversation qualified as Conv
import Remax.Modal qualified as Modal
import Remax.SoftWrap qualified as SoftWrap
import Remax.Store qualified as Store
import Remax.UI.Action.Notes qualified as NoteAction
import Remax.UI.Action.Rebase qualified as Rebase
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Markdown qualified as Markdown
import Remax.UI.Core.Threads qualified as Threads
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.View qualified as View
import Remax.UI.Core.Widgets qualified as Widgets

notePopupLayer :: Types.St -> Types.NotePopup -> BrickTypes.Widget Types.Name
notePopupLayer st np =
  let
    ed = Types.npEd np
    thread = popupThread st np
    title = popupTitle np thread <> " \183 " <> modeTag (Modal.edMode ed)
    targetRows = popupTargetRows st np
  in
    BrickTypes.Widget BrickTypes.Greedy BrickTypes.Greedy $ do
      ctx <- BrickTypes.getContext
      let
        boxWidth = NoteAction.popupBoxWidth (BrickTypes.availWidth ctx)
        bodyRows = max 6 (BrickTypes.availHeight ctx - 14)
        -- the editor keeps its room first; the transcript takes what is
        -- left, and gives up its oldest posts to fit rather than
        -- squeezing the box you are typing in
        rows = SoftWrap.softRows (NoteAction.popupRowWidth boxWidth) (Modal.bufferLines ed)
        editorRows = max 3 (min 10 (length rows + 1))
        budget = bodyRows - editorRows - length targetRows - 1
        transcript = case Types.npPurpose np of
          Types.WorkFor wp -> workRows st (boxWidth - 2) budget wp
          _ -> transcriptRows (boxWidth - 2) budget (NoteAction.transcriptPosts thread)
      BrickTypes.render $
        Widgets.floatFrame title (popupHint np thread) $
          Brick.hLimit boxWidth $
            Brick.padRight Brick.Max $
              Brick.vLimit bodyRows $
                Brick.vBox (targetRows ++ transcript ++ [noteBodyWidget ed rows])

modeTag :: Modal.Mode -> T.Text
modeTag mode =
  case mode of
    Modal.Insert -> "ins"
    Modal.Normal -> "nor"

popupThread :: Types.St -> Types.NotePopup -> [Store.Post]
popupThread st np =
  case Types.npPurpose np of
    Types.AmendMessage _ -> []
    Types.WorkFor _ -> []
    Types.NoteFor keys _ ->
      case Maybe.mapMaybe (`Map.lookup` Types.stThreads st) keys of
        (t : _) -> t
        [] -> []

popupTitle :: Types.NotePopup -> [Store.Post] -> T.Text
popupTitle np thread =
  case Types.npPurpose np of
    Types.AmendMessage _ -> "amend"
    Types.WorkFor wp -> workTitle wp
    Types.NoteFor _ _ -> case NoteAction.noteAct thread of
      NoteAction.NoteNew -> "note"
      NoteAction.NoteEdit -> "note \183 editing yours"
      NoteAction.NoteReply who -> "note \183 reply to " <> who

popupHint :: Types.NotePopup -> [Store.Post] -> T.Text
popupHint np thread =
  case Types.npPurpose np of
    Types.AmendMessage _ -> "\9166 amend (nor) \183 C-c cancel"
    Types.WorkFor wp -> workHint wp
    -- what backing out costs is worth saying, since it differs: a note
    -- the mark opened takes the mark with it, one asked for does not
    Types.NoteFor _ (Types.NoteFromMark _) ->
      "\9166 save (nor) \183 C-c cancels the mark \183 empty note clears"
    Types.NoteFor _ Types.NoteAsked -> case NoteAction.noteAct thread of
      NoteAction.NoteReply _ ->
        "\9166 post (nor) \183 C-c discard \183 empty clears the thread"
      _ -> "\9166 save (nor) \183 C-c discard \183 empty note clears"

-- which line the conversation is about; the popup never used to say
popupTargetRows :: Types.St -> Types.NotePopup -> [BrickTypes.Widget Types.Name]
popupTargetRows st np =
  case Types.npPurpose np of
    Types.AmendMessage target -> amendTargetRows st target
    Types.WorkFor wp -> [Brick.withAttr Attrs.hintAttr (Brick.txt (Types.wpPlace wp)), Brick.txt " "]
    Types.NoteFor _ _ -> case Types.currentFileMay st of
      Nothing -> []
      Just fv ->
        let
          cur = fst (Modal.cursorRC (Types.fvEd fv))
          decl = case View.declAt (Modal.bufferLines (Types.fvEd fv)) cur of
            Just d -> " \183 " <> View.declName d
            Nothing -> ""
        in
          [ Brick.withAttr Attrs.hintAttr (Brick.txt (T.pack (Types.fvPath fv) <> ":" <> Types.tshow (cur + 1) <> decl))
          , Brick.txt " "
          ]

workTitle :: Types.WorkPopup -> T.Text
workTitle wp =
  case Types.wpAction wp of
    Types.ReplyTo _ author -> "reply \183 to " <> author
    Types.AskOn _ -> "ask \183 the agent"
    Types.CommentOn anchor -> "comment \183 " <> Threads.anchorPlace anchor
    Types.EditPending _ origin -> case origin of
      Types.ReplyOrigin author -> "reply \183 to " <> author
      Types.CommentOrigin -> "comment \183 " <> T.takeWhile (/= ' ') (Types.wpPlace wp)
      Types.AskOrigin -> "ask \183 the agent"
    Types.TakeProposal _ _ author -> "reply \183 to " <> author <> " \183 from the proposal"

workHint :: Types.WorkPopup -> T.Text
workHint wp =
  case Types.wpAction wp of
    Types.ReplyTo _ _ -> "\9166 post (nor) \183 C-g inserts a sha \183 C-c discard \183 ? asks the agent instead"
    Types.AskOn _ -> "\9166 save (nor) \183 C-c discard \183 r replies on GitHub instead"
    Types.CommentOn _ -> "\9166 save (nor) \183 C-g inserts a sha \183 C-c discard \183 a selection pre-fills a suggestion"
    Types.EditPending _ _ -> "\9166 save (nor) \183 C-g inserts a sha \183 C-c discard changes"
    Types.TakeProposal {} -> "\9166 post (nor) \183 C-g inserts a sha \183 C-c discard \183 the proposal goes once you post"

-- The thread's log above the editor: the newest two posts in full,
-- older ones collapsed to one line each, then the rule naming what
-- the text becomes.
workRows :: Types.St -> Int -> Int -> Types.WorkPopup -> [BrickTypes.Widget Types.Name]
workRows st width budget wp =
  let
    conv = Types.wpConv wp >>= \cid -> Conv.conversationFor cid (Types.stConversations st)
    posts = maybe [] (filter shownInLog . Conv.convPosts) conv
    shownInLog p = case Conv.postState p of
      Conv.Posted {} -> True
      Conv.Local _ -> True
      _ -> False
    column = max 6 (min 16 (maximum (0 : map (T.length . Threads.authorName . Conv.postAuthor) posts)))
    (older, newest) = splitAt (max 0 (length posts - 2)) posts
    collapsed p =
      Brick.withAttr
        Attrs.hintAttr
        ( Brick.txt
            ( Widgets.clipT
                width
                ( "\8942 "
                    <> Threads.authorName (Conv.postAuthor p)
                    <> " \183 "
                    <> ageOf p
                    <> " \183 "
                    <> Markdown.firstLine (Conv.postBody p)
                    <> "\8230"
                )
            )
        )
    ageOf p = case Conv.postState p of
      Conv.Posted _ at _ -> Threads.ageText (Types.stNow st) at
      Conv.Local _ -> "local"
      _ -> ""
    full p =
      let
        bodyWidth = max 12 (width - column - 3 - 9)
        rendered = Markdown.render bodyWidth (Conv.postBody p)
        rows = if null rendered then [Markdown.plainRow ""] else rendered
        authorAttr = if Conv.postAuthor p == Conv.Viewer then Attrs.noteInlineAttr else Attrs.replyInlineAttr
        row (j, r) =
          Brick.hBox
            ( [ Brick.withAttr authorAttr (Brick.txt (T.justifyRight column ' ' (if j == (0 :: Int) then Threads.authorName (Conv.postAuthor p) else "")))
              , Brick.withAttr Attrs.hintAttr (Brick.txt " \9474 ")
              ]
                ++ map (\seg -> Brick.withAttr (Markdown.segAttr seg) (Brick.txt (Markdown.segText seg))) (Markdown.rowSegments r)
                ++ (if j == 0 then [Brick.withAttr Attrs.hintAttr (Brick.txt ("  " <> ageOf p))] else [])
            )
      in
        map row (zip [0 ..] rows)
    logRows = map collapsed older ++ concatMap full newest
    rule =
      Brick.withAttr Attrs.hintAttr (Brick.txt (Widgets.clipT width ("\9472 " <> ruleText wp <> " " <> T.replicate width "\9472")))
    kept = drop (max 0 (length logRows - (budget - 2))) logRows
  in
    if null logRows
      then [rule, Brick.txt " "]
      else kept ++ [Brick.txt " ", rule, Brick.txt " "]

ruleText :: Types.WorkPopup -> T.Text
ruleText wp =
  case Types.wpAction wp of
    Types.ReplyTo _ author -> "reply to " <> author <> " \183 waits for :submit"
    Types.AskOn _ -> "ask the agent"
    Types.CommentOn _ -> "comment \183 waits for :submit"
    Types.EditPending _ origin -> case origin of
      Types.ReplyOrigin author -> "your reply to " <> author <> " \183 waits for :submit"
      Types.CommentOrigin -> "your comment \183 waits for :submit"
      Types.AskOrigin -> "ask the agent"
    Types.TakeProposal _ _ author -> "reply to " <> author <> " \183 waits for :submit"

-- an amend that quietly stages files is the surprise this is here
-- to remove: say what folds in before ⏎ does it
amendTargetRows :: Types.St -> Types.AmendTarget -> [BrickTypes.Widget Types.Name]
amendTargetRows st target =
  case target of
    Types.AmendStoppedCommit ->
      [ Brick.withAttr Attrs.hintAttr (Brick.txt "the stopped commit \183 :continue commits it with this message")
      , Brick.txt " "
      ]
    Types.AmendHead folding -> amendHeadRows st folding

amendHeadRows :: Types.St -> [FilePath] -> [BrickTypes.Widget Types.Name]
amendHeadRows st folding
  | null folding =
      [ Brick.withAttr
          Attrs.hintAttr
          ( Brick.txt
              ( case Types.stRebase st of
                  Just _ -> "message only \183 :continue folds in this stop's edits"
                  Nothing -> "message only \183 nothing else has changed"
              )
          )
      , Brick.txt " "
      ]
  | otherwise =
      Brick.withAttr Attrs.hintAttr (Brick.txt ("folding in " <> Rebase.fileCountLabel (length folding) <> ":"))
        : map (\f -> Brick.withAttr Attrs.noteInlineAttr (Brick.txt ("  " <> T.pack f))) (take 8 folding)
        ++ [ Brick.withAttr Attrs.hintAttr (Brick.txt ("  \8230 and " <> Types.tshow (length folding - 8) <> " more"))
           | length folding > 8
           ]
        ++ [Brick.txt " "]

-- selections paint here the same way they do in the main buffer:
-- whole rows for x, column spans for v
noteBodyWidget :: Modal.EdState -> [SoftWrap.SoftRow] -> BrickTypes.Widget Types.Name
noteBodyWidget ed rows =
  BrickTypes.Widget BrickTypes.Greedy BrickTypes.Greedy $ do
    ctx <- BrickTypes.getContext
    let
      (cr, cc) = Modal.cursorRC ed
      (cursorIx, cursorRow) = SoftWrap.cursorSoftRow cr cc rows
      height = max 1 (BrickTypes.availHeight ctx)
      top = max 0 (cursorIx - height + 1)
      cursorAt = Brick.Location (cc - SoftWrap.srStart cursorRow, cursorIx - top)
    BrickTypes.render $
      Brick.showCursor Types.TheCursor cursorAt $
        Brick.vBox (map (paintSoftRow ed) (take height (drop top rows)))

paintSoftRow :: Modal.EdState -> SoftWrap.SoftRow -> BrickTypes.Widget Types.Name
paintSoftRow ed row =
  let
    text = SoftWrap.srText row
    plain = Brick.txt (Widgets.padT text)
    inLineSel = case Modal.selectionRows ed of
      Just (a, b) -> SoftWrap.srLine row >= a && SoftWrap.srLine row <= b
      Nothing -> False
  in
    if inLineSel
      then Brick.withDefAttr Attrs.selAttr plain
      else case spanFor (Modal.charSelection ed) row of
        Nothing -> plain
        Just (start, mEnd) -> selectedLine text start mEnd

spanFor :: Maybe ((Int, Int), (Int, Int)) -> SoftWrap.SoftRow -> Maybe (Int, Maybe Int)
spanFor charSel row = do
  ((lr, lc), (hr, hc)) <- charSel
  let
    i = SoftWrap.srLine row
    start = if i == lr then lc - SoftWrap.srStart row else 0
    end = if i == hr then Just (hc - SoftWrap.srStart row) else Nothing
    pastRow = case SoftWrap.srEnd row of
      SoftWrap.LineContinues -> start >= T.length (SoftWrap.srText row)
      SoftWrap.LineEnds -> False
  if i < lr || i > hr || pastRow || maybe False (< 0) end
    then Nothing
    else Just (max 0 start, end)

selectedLine :: T.Text -> Int -> Maybe Int -> BrickTypes.Widget Types.Name
selectedLine l start mEnd =
  let
    (pre, rest) = T.splitAt start l
    (mid, post) = case mEnd of
      Nothing -> (rest, T.empty)
      Just e -> T.splitAt (e + 1 - start) rest
    midW = if T.null mid then Brick.txt " " else Brick.txt mid
  in
    Brick.txt pre <+> Brick.withDefAttr Attrs.selAttr midW <+> Brick.txt post

transcriptRows :: Int -> Int -> [Store.Post] -> [BrickTypes.Widget Types.Name]
transcriptRows width budget posts =
  let
    rowsFor = concatMap (NoteAction.postRows width)
    (shownRows, elided)
      | null posts = ([], 0 :: Int)
      | length (rowsFor posts) <= budget = (rowsFor posts, 0)
      | otherwise =
          let
            drops ps
              | null ps = ([], length posts)
              | length (rowsFor ps) <= budget - 1 =
                  (rowsFor ps, length posts - length ps)
              | otherwise = drops (drop 1 ps)
          in
            drops posts
    markerRows =
      [ Brick.withAttr
          Attrs.hintAttr
          (Brick.txt ("\8230 " <> Types.tshow elided <> " earlier post" <> (if elided == 1 then "" else "s")))
      | elided > 0
      ]
    ruleRows =
      [Brick.withAttr Attrs.hairAttr (Brick.vLimit 1 (Brick.fill '\9472')) | not (null shownRows) || elided > 0]
  in
    markerRows ++ shownRows ++ ruleRows

-- Switching repositories --------------------------------------------------------
