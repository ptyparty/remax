{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Status
  ( statusBar
  ) where

import Brick ((<+>))
import Brick qualified as Brick
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.Config qualified as Config
import Remax.Conversation qualified as Conv
import Remax.Git qualified as Git
import Remax.Modal qualified as Modal
import Remax.UI.Action.Threads qualified as ThreadAction
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Ink qualified as Ink
import Remax.UI.Core.Threads qualified as Threads
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.View qualified as View
import Remax.UI.Core.Widgets qualified as Widgets
import Remax.Vi qualified as Vi

statusBar :: Types.St -> Brick.Widget Types.Name
statusBar st =
  let
    middle = case (Types.stConfirm st, Types.stPrompt st) of
      (Just confirm, _) -> Brick.withAttr Attrs.questionMarkAttr (Brick.txt (Widgets.padT (Types.confirmText confirm)))
      (_, Just (Types.SearchPrompt input)) ->
        Brick.txt (Types.searchSigil st <> input <> "\9612")
      (_, Just (Types.CmdPrompt input)) -> Brick.txt (":" <> input <> "\9612")
      (_, Just (Types.PathPrompt label input _)) -> Brick.txt (label <> input <> "\9612")
      (_, Nothing) -> Brick.withAttr Attrs.msgAttr (Brick.txt (Widgets.padT (Types.stMsg st)))
  in
    Brick.withDefAttr Attrs.statusAttr $
      modeChip st
        <+> (if Maybe.isJust (Types.stPullView st) then Brick.withAttr Attrs.hintAttr (Brick.txt " PR view ") else Brick.txt "")
        <+> pollChip st
        <+> placeSeg st
        <+> Brick.txt "  "
        <+> middle
        <+> Brick.padLeft Brick.Max (Brick.withAttr Attrs.hintAttr (Brick.txt ("  " <> statusHint st)))

chip :: Brick.AttrName -> T.Text -> Brick.Widget Types.Name
chip dotAttr label =
  Brick.hBox
    [ Brick.txt " "
    , Brick.withAttr dotAttr (Brick.txt "\9679")
    , Brick.withAttr Attrs.statusAttr (Brick.txt (" " <> label <> " "))
    ]

-- what the buffer is waiting for, most specific first: the space
-- menu, an emacs chord, a half-typed vi command, insert, then plain
-- normal — with vi's own resting label saying which keymap you are in
modeChip :: Types.St -> Brick.Widget Types.Name
modeChip st
  | Just np <- Types.stNotePopup st
  , Types.WorkFor wp <- Types.npPurpose np =
      chip
        (if Modal.edMode (Types.npEd np) == Modal.Insert then Attrs.modeDotInsAttr else Attrs.modeDotNorAttr)
        ((if Modal.edMode (Types.npEd np) == Modal.Insert then "ins " else "nor ") <> popupWord wp)
  | Types.stSpace st = chip Attrs.modeDotNorAttr "spc\8230"
  | Types.stIdleGo st = chip Attrs.modeDotNorAttr "g\8230"
  | otherwise = case Types.stEmacsPrefix st of
      Just pfx -> chip Attrs.modeDotNorAttr ("C-" <> T.singleton pfx <> "\8230")
      Nothing -> case Config.cfgKeys (Types.stConfig st) of
        Config.KeysEmacs -> chip Attrs.modeDotNorAttr "emx"
        Config.KeysVi -> viChip st
        Config.KeysHelix -> helixChip st

viChip :: Types.St -> Brick.Widget Types.Name
viChip st =
  case Types.currentFileMay st of
    Nothing -> chip Attrs.modeDotNorAttr "vi"
    Just fv -> case Modal.edMode (Types.fvEd fv) of
      Modal.Insert -> chip Attrs.modeDotInsAttr "ins"
      Modal.Normal -> case Vi.viLabel (Types.stVi st) of
        Just p -> chip Attrs.modeDotNorAttr (p <> "\8230")
        Nothing
          | Modal.edExtend (Types.fvEd fv) -> chip Attrs.modeDotNorAttr "vis"
          | otherwise -> chip Attrs.modeDotNorAttr "vi"

helixChip :: Types.St -> Brick.Widget Types.Name
helixChip st =
  case Types.currentFileMay st of
    Nothing -> chip Attrs.modeDotNorAttr "nor"
    Just fv -> case Modal.edMode (Types.fvEd fv) of
      Modal.Insert -> chip Attrs.modeDotInsAttr "ins"
      Modal.Normal -> case Modal.pendingLabel (Types.fvEd fv) of
        Just p -> chip Attrs.modeDotNorAttr (p <> "\8230")
        Nothing -> chip Attrs.modeDotNorAttr "nor"

-- the startup warning scrolls away with the next message; this keeps
-- "the watcher is a second behind" on screen for the whole session
pollChip :: Types.St -> Brick.Widget Types.Name
pollChip st =
  if Types.stWatchMode st == Types.PollingWatch
    then Brick.withAttr Attrs.watchPollAttr (Brick.txt " \9888 poll ")
    else Brick.txt ""

placeSeg :: Types.St -> Brick.Widget Types.Name
placeSeg st =
  case Maybe.catMaybes [declSeg st, meterSeg st] of
    [] -> Brick.txt ""
    segs -> Brick.hBox (Brick.txt " " : List.intersperse (Brick.withAttr Attrs.trayDeclAttr (Brick.txt " \183  ")) segs)

declSeg :: Types.St -> Maybe (Brick.Widget Types.Name)
declSeg st =
  case Types.currentFileMay st of
    Just fv ->
      -- nearest decl at or above the cursor, found by walking up
      -- from it rather than rebuilding the buffer every frame
      case List.find View.isDeclLine (Modal.bufferUpward (Types.fvEd fv)) of
        Just d -> Just (Brick.withAttr Attrs.trayDeclAttr (Brick.txt (T.take 32 (View.declName d))))
        Nothing -> Nothing
    Nothing -> Nothing

-- in vi the verbs sit under the leader, so the hint has to say so
-- Named for the keymap in use, all three of them. The verbs sit
-- under a leader in vi and under C-c in emacs, and emacs has no
-- space leader at all — so it has no space menu and no ␣? either,
-- and reaches the sheet the way it reaches every other command.
data Verbs = AllVerbs | NoFlag | Feedback

verbHint :: Verbs -> Config.KeyMode -> T.Text
verbHint verbs keys =
  let
    flag text = case verbs of
      AllVerbs -> text
      NoFlag -> ""
      Feedback -> text
    pullView text = case verbs of
      AllVerbs -> ""
      NoFlag -> text
      Feedback -> text
  in
    case keys of
      Config.KeysVi -> "\9251\9166 accept · " <> flag "\9251! flag · " <> "\9251q question · " <> pullView "\9251o PR · " <> "\9251? keys "
      Config.KeysEmacs ->
        "C-c \9166 accept · " <> flag "C-c ! flag · " <> "C-c ? question · " <> pullView "C-c o PR · " <> "M-x help "
      Config.KeysHelix -> "\9166 accept · " <> flag "! flag · " <> "? question · " <> pullView "\9251o PR · " <> "\9251 menu · \9251? keys "

pullViewHint :: Config.KeyMode -> T.Text
pullViewHint keys =
  case keys of
    Config.KeysEmacs -> "C-n C-p move \183 C-c [ ] awaiting you \183 C-c g comment \183 C-c k checks \183 esc body"
    _ -> "j/k move \183 [ ] awaiting you \183 @ comment \183 c checks \183 esc body"

popupWord :: Types.WorkPopup -> T.Text
popupWord wp =
  case Types.wpAction wp of
    Types.ReplyTo _ _ -> "reply"
    Types.AskOn Types.AskRemark _ -> "ask"
    Types.AskOn Types.FlagRemark _ -> "flag"
    Types.CommentOn _ -> "comment"
    Types.EditPending _ origin -> case origin of
      Types.ReplyOrigin _ -> "reply"
      Types.CommentOrigin -> "comment"
      Types.AskOrigin -> "ask"
    Types.TakeProposal _ (Conv.NewThread _ _) _ -> "comment"
    Types.TakeProposal {} -> "reply"
    Types.EditPosted _ _ -> "edit"

popupStatus :: Types.WorkPopup -> T.Text
popupStatus wp =
  case Types.wpAction wp of
    Types.ReplyTo _ author -> "reply to " <> author
    Types.AskOn _ _ -> "the agent about " <> T.takeWhile (/= ' ') (Types.wpPlace wp)
    Types.CommentOn anchor -> "new comment on " <> Threads.anchorPlace anchor
    Types.EditPending _ _ -> "editing your pending text"
    Types.TakeProposal _ target author -> "editing the proposal as your " <> Threads.takenAs target author
    Types.EditPosted _ _ -> "editing your comment on GitHub"

statusHint :: Types.St -> T.Text
statusHint st =
  let
    keys = Config.cfgKeys (Types.stConfig st)
    outdatedHere = filter (\c -> Map.member (Conv.convId c) (Types.stOutdated st)) (ThreadAction.cursorConversations st)
    cursorLine = maybe 0 (\fv -> fst (Modal.cursorRC (Types.fvEd fv)) + 1) (Types.currentFileMay st)
    lineHint = case ThreadAction.cursorConversations st of
      [] -> Nothing
      convs
        | not (null outdatedHere) -> Just (Threads.outdatedHint keys cursorLine (length outdatedHere))
        | any isThread convs -> Just (Threads.lineHint keys (Types.flagRule st))
        | any isNote convs -> Just (Threads.noteHint keys)
        | otherwise -> Nothing
    isThread c = case Conv.convVenue c of
      Conv.ReviewThread _ _ -> True
      _ -> False
    isNote c = case Conv.convVenue c of
      Conv.LocalOnly _ -> True
      _ -> False
  in
    case (Types.stNotePopup st, ThreadAction.bandPost st, Types.currentFileMay st) of
      _
        | Just _ <- Types.stSubmit st -> "submit \183 going out"
      (Nothing, Nothing, _)
        | Just _ <- Types.stPullView st -> pullViewHint keys
      (Just np, _, _)
        | Types.WorkFor wp <- Types.npPurpose np -> popupStatus wp
      (_, Just (conv, _, post), _) ->
        Threads.postLabel (Conv.convVenue conv) post
          <> " \183 "
          <> Threads.anchorPlace (Conv.convAnchor conv)
          <> "   "
          <> Threads.postKeys keys (Types.flagRule st) (Conv.convVenue conv) post
      (_, _, Nothing) ->
        Types.promptKeys keys <> " command · \9251? keys · " <> Types.quitKeys keys <> " quit "
      (_, _, Just fv) ->
        case lineHint of
          Just hint -> hint
          Nothing -> (if Types.fvStale fv then "hunks refresh on Esc · " else "") <> verbHint (verbsFor st) keys

verbsFor :: Types.St -> Verbs
verbsFor st =
  case Git.subjectOf (Types.effectiveMode st) of
    Git.Pull _ Git.ReadOnly -> NoFlag
    Git.Pull _ Git.Checkout -> Feedback
    _ -> AllVerbs

meterSeg :: Types.St -> Maybe (Brick.Widget Types.Name)
meterSeg st =
  let
    total = sum (map (length . Types.fvRuns) (Types.stFiles st))
    pending = sum (map Types.fvPending (Types.stFiles st))
  in
    if total == 0
      then Nothing
      else Just (Ink.meterW (Types.settleLevel st) (total - pending) total)
