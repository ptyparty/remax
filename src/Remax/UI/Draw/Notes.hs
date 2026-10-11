{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Notes
  ( notePopupLayer
  ) where

import Brick
  ( Location (..)
  , Padding (Max)
  , Widget
  , fill
  , hLimit
  , padRight
  , showCursor
  , txt
  , vBox
  , vLimit
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
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( mapMaybe
  )
import Data.Text (Text)
import qualified Data.Text as T
import Remax.Modal
import Remax.Store (Post)
import Remax.UI.Attrs
import Remax.UI.Notes
import Remax.UI.Rebase
import Remax.UI.Types
import Remax.UI.View
import Remax.UI.Widgets

notePopupLayer :: St -> NotePopup -> Widget Name
notePopupLayer st np =
  let
    ed = npEd np
    ls = bufferLines ed
    thread = popupThread st np
    title = popupTitle np thread <> " \183 " <> modeTag (edMode ed)
    targetRows = popupTargetRows st np
    bodyW = showCursor TheCursor (Location (snd (cursorRC ed), fst (cursorRC ed))) (noteBodyWidget ed)
  in
    Widget Greedy Greedy $ do
      ctx <- getContext
      let
        boxWidth = min 90 (max 50 (availWidth ctx - 20))
        bodyRows = max 6 (availHeight ctx - 14)
        -- the editor keeps its room first; the transcript takes what is
        -- left, and gives up its oldest posts to fit rather than
        -- squeezing the box you are typing in
        editorRows = max 3 (min 10 (length ls + 1))
        budget = bodyRows - editorRows - length targetRows - 1
        transcript = transcriptRows (boxWidth - 2) budget (transcriptPosts thread)
      render $
        floatFrame title (popupHint np thread) $
          hLimit boxWidth $
            padRight Max $
              vLimit bodyRows $
                vBox (targetRows ++ transcript ++ [bodyW])

modeTag :: Mode -> Text
modeTag mode =
  case mode of
    Insert -> "ins"
    Normal -> "nor"

popupThread :: St -> NotePopup -> [Post]
popupThread st np =
  case npPurpose np of
    AmendMessage _ -> []
    NoteFor keys _ ->
      case mapMaybe (`Map.lookup` stThreads st) keys of
        (t : _) -> t
        [] -> []

popupTitle :: NotePopup -> [Post] -> Text
popupTitle np thread =
  case npPurpose np of
    AmendMessage _ -> "amend"
    NoteFor _ _ -> case noteAct thread of
      NoteNew -> "note"
      NoteEdit -> "note \183 editing yours"
      NoteReply who -> "note \183 reply to " <> who

popupHint :: NotePopup -> [Post] -> Text
popupHint np thread =
  case npPurpose np of
    AmendMessage _ -> "\9166 amend (nor) \183 C-c cancel"
    -- what backing out costs is worth saying, since it differs: a note
    -- the mark opened takes the mark with it, one asked for does not
    NoteFor _ (NoteFromMark _) ->
      "\9166 save (nor) \183 C-c cancels the mark \183 empty note clears"
    NoteFor _ NoteAsked -> case noteAct thread of
      NoteReply _ ->
        "\9166 post (nor) \183 C-c discard \183 empty clears the thread"
      _ -> "\9166 save (nor) \183 C-c discard \183 empty note clears"

-- which line the conversation is about; the popup never used to say
popupTargetRows :: St -> NotePopup -> [Widget Name]
popupTargetRows st np =
  case npPurpose np of
    AmendMessage folding -> amendTargetRows st folding
    NoteFor _ _ -> case currentFileMay st of
      Nothing -> []
      Just fv ->
        let
          cur = fst (cursorRC (fvEd fv))
          decl = case declAt (bufferLines (fvEd fv)) cur of
            Just d -> " \183 " <> declName d
            Nothing -> ""
        in
          [ withAttr hintAttr (txt (T.pack (fvPath fv) <> ":" <> tshow (cur + 1) <> decl))
          , txt " "
          ]

-- an amend that quietly stages files is the surprise this is here
-- to remove: say what folds in before ⏎ does it
amendTargetRows :: St -> [FilePath] -> [Widget Name]
amendTargetRows st folding
  | null folding =
      [ withAttr
          hintAttr
          ( txt
              ( case stRebase st of
                  Just _ -> "message only \183 :continue folds in this stop's edits"
                  Nothing -> "message only \183 nothing else has changed"
              )
          )
      , txt " "
      ]
  | otherwise =
      withAttr hintAttr (txt ("folding in " <> fileCountLabel (length folding) <> ":"))
        : map (\f -> withAttr noteInlineAttr (txt ("  " <> T.pack f))) (take 8 folding)
        ++ [ withAttr hintAttr (txt ("  \8230 and " <> tshow (length folding - 8) <> " more"))
           | length folding > 8
           ]
        ++ [txt " "]

-- selections paint here the same way they do in the main buffer:
-- whole rows for x, column spans for v
noteBodyWidget :: EdState -> Widget Name
noteBodyWidget ed =
  let
    lineSel = selectionRows ed
    charSel = charSelection ed
    inLineSel i = case lineSel of
      Just (a, b) -> i >= a && i <= b
      Nothing -> False
    bodyLine (i, l)
      | inLineSel i = withDefAttr selAttr (txt (padT l))
      | otherwise = case spanFor charSel i of
          Nothing -> txt (padT l)
          Just (start, mEnd) -> selectedLine l start mEnd
  in
    vBox (map bodyLine (zip [0 ..] (bufferLines ed)))

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

selectedLine :: Text -> Int -> Maybe Int -> Widget Name
selectedLine l start mEnd =
  let
    (pre, rest) = T.splitAt start l
    (mid, post) = case mEnd of
      Nothing -> (rest, T.empty)
      Just e -> T.splitAt (e + 1 - start) rest
    midW = if T.null mid then txt " " else txt mid
  in
    txt pre <+> withDefAttr selAttr midW <+> txt post

transcriptRows :: Int -> Int -> [Post] -> [Widget Name]
transcriptRows width budget posts =
  let
    rowsFor = concatMap (postRows width)
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
      [ withAttr
          hintAttr
          (txt ("\8230 " <> tshow elided <> " earlier post" <> (if elided == 1 then "" else "s")))
      | elided > 0
      ]
    ruleRows =
      [withAttr hairAttr (vLimit 1 (fill '\9472')) | not (null shownRows) || elided > 0]
  in
    markerRows ++ shownRows ++ ruleRows

-- Switching repositories --------------------------------------------------------
