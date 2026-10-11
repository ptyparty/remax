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
import Remax.Modal qualified as Modal
import Remax.Store qualified as Store
import Remax.UI.Action.Notes qualified as NoteAction
import Remax.UI.Action.Rebase qualified as Rebase
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.View qualified as View
import Remax.UI.Core.Widgets qualified as Widgets

notePopupLayer :: Types.St -> Types.NotePopup -> BrickTypes.Widget Types.Name
notePopupLayer st np =
  let
    ed = Types.npEd np
    ls = Modal.bufferLines ed
    thread = popupThread st np
    title = popupTitle np thread <> " \183 " <> modeTag (Modal.edMode ed)
    targetRows = popupTargetRows st np
    bodyW = Brick.showCursor Types.TheCursor (Brick.Location (snd (Modal.cursorRC ed), fst (Modal.cursorRC ed))) (noteBodyWidget ed)
  in
    BrickTypes.Widget BrickTypes.Greedy BrickTypes.Greedy $ do
      ctx <- BrickTypes.getContext
      let
        boxWidth = min 90 (max 50 (BrickTypes.availWidth ctx - 20))
        bodyRows = max 6 (BrickTypes.availHeight ctx - 14)
        -- the editor keeps its room first; the transcript takes what is
        -- left, and gives up its oldest posts to fit rather than
        -- squeezing the box you are typing in
        editorRows = max 3 (min 10 (length ls + 1))
        budget = bodyRows - editorRows - length targetRows - 1
        transcript = transcriptRows (boxWidth - 2) budget (NoteAction.transcriptPosts thread)
      BrickTypes.render $
        Widgets.floatFrame title (popupHint np thread) $
          Brick.hLimit boxWidth $
            Brick.padRight Brick.Max $
              Brick.vLimit bodyRows $
                Brick.vBox (targetRows ++ transcript ++ [bodyW])

modeTag :: Modal.Mode -> T.Text
modeTag mode =
  case mode of
    Modal.Insert -> "ins"
    Modal.Normal -> "nor"

popupThread :: Types.St -> Types.NotePopup -> [Store.Post]
popupThread st np =
  case Types.npPurpose np of
    Types.AmendMessage _ -> []
    Types.NoteFor keys _ ->
      case Maybe.mapMaybe (`Map.lookup` Types.stThreads st) keys of
        (t : _) -> t
        [] -> []

popupTitle :: Types.NotePopup -> [Store.Post] -> T.Text
popupTitle np thread =
  case Types.npPurpose np of
    Types.AmendMessage _ -> "amend"
    Types.NoteFor _ _ -> case NoteAction.noteAct thread of
      NoteAction.NoteNew -> "note"
      NoteAction.NoteEdit -> "note \183 editing yours"
      NoteAction.NoteReply who -> "note \183 reply to " <> who

popupHint :: Types.NotePopup -> [Store.Post] -> T.Text
popupHint np thread =
  case Types.npPurpose np of
    Types.AmendMessage _ -> "\9166 amend (nor) \183 C-c cancel"
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
    Types.AmendMessage folding -> amendTargetRows st folding
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

-- an amend that quietly stages files is the surprise this is here
-- to remove: say what folds in before ⏎ does it
amendTargetRows :: Types.St -> [FilePath] -> [BrickTypes.Widget Types.Name]
amendTargetRows st folding
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
noteBodyWidget :: Modal.EdState -> BrickTypes.Widget Types.Name
noteBodyWidget ed =
  let
    lineSel = Modal.selectionRows ed
    charSel = Modal.charSelection ed
    inLineSel i = case lineSel of
      Just (a, b) -> i >= a && i <= b
      Nothing -> False
    bodyLine (i, l)
      | inLineSel i = Brick.withDefAttr Attrs.selAttr (Brick.txt (Widgets.padT l))
      | otherwise = case spanFor charSel i of
          Nothing -> Brick.txt (Widgets.padT l)
          Just (start, mEnd) -> selectedLine l start mEnd
  in
    Brick.vBox (map bodyLine (zip [0 ..] (Modal.bufferLines ed)))

spanFor :: Maybe ((Int, Int), (Int, Int)) -> Int -> Maybe (Int, Maybe Int)
spanFor charSel i = do
  ((lr, lc), (hr, hc)) <- charSel
  if i < lr || i > hr
    then Nothing
    else
      Just
        ( if i == lr then lc else 0
        , if i == hr then Just hc else Nothing
        )

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
