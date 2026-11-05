{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Keys
  ( answerKeys
  ) where

import Brick qualified as Brick
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.Char qualified as Char
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.Config qualified as Config
import Remax.Context qualified as Context
import Remax.Modal qualified as Modal
import Remax.UI.Action.Menus qualified as Menus
import Remax.UI.Core.Types qualified as Types

answerKeys :: ([Context.KeyGroup] -> IO ()) -> Brick.EventM Types.Name Types.St ()
answerKeys respond = do
  st <- State.get
  MonadIO.liftIO (respond (keyGroups st))

keyGroups :: Types.St -> [Context.KeyGroup]
keyGroups st =
  let
    mode = Config.cfgKeys (Types.stConfig st)
  in
    case popupGroup mode st of
      Just popup -> [popup]
      Nothing -> screenGroups mode st

popupGroup :: Config.KeyMode -> Types.St -> Maybe Context.KeyGroup
popupGroup mode st
  | Types.stHelp st = Just (group "keys" [command "close the sheet" "any key" "\ESC"])
  | Maybe.isJust (Types.stInfoPopup st) = Just (group "popup" [command "close" "any key" "\ESC"])
  | Maybe.isJust (Types.stConfirm st) =
      Just (group "confirm" [command "yes, do it" "y" "y", command "leave it alone" "any other key" "\ESC"])
  | Maybe.isJust (Types.stCiting st) = Just (listGroup "cite a commit" "insert the sha")
  | Maybe.isJust (Types.stReacting st) = Just (listGroup "react" "add the reaction")
  | Just np <- Types.stNotePopup st = Just (noteGroup mode np)
  | Maybe.isJust (Types.stInbox st) = Just (listGroup "recent feedback" "go to it")
  | Maybe.isJust (Types.stTourOverview st) = Just (tourGroup mode)
  | Just dialog <- Types.stSubmit st = Just (submitGroup mode dialog)
  | Just view <- Types.stTree st = Just (treeGroup view)
  | Maybe.isJust (Types.stPicker st) = Just (listGroup "picker" "choose the row")
  | Maybe.isJust (Types.stPrompt st) =
      Just (group "prompt" [command "run it" "\9166" "\r", command "cancel" "Esc" "\ESC"])
  | otherwise = Nothing

screenGroups :: Config.KeyMode -> Types.St -> [Context.KeyGroup]
screenGroups mode st
  | Types.stSpace st = [spaceGroup Bare]
  | Maybe.isJust (Types.stEmacsPrefix st) || Types.stIdleGo st = [pendingGroup]
  | Maybe.isJust (Types.stPullView st) = pullViewGroups mode
  | null (Types.stFiles st) = [idleGroup mode]
  | otherwise =
      case Types.currentFileMay st of
        Just fv
          | Modal.hasPending (Types.fvEd fv) -> [pendingGroup]
          | mode /= Config.KeysEmacs && Modal.edMode (Types.fvEd fv) == Modal.Insert ->
              [group "insert" [command "back to normal mode" "Esc" "\ESC"]]
        _ -> bodyGroups mode

group :: T.Text -> [Context.KeyCommand] -> Context.KeyGroup
group = Context.KeyGroup

command :: T.Text -> T.Text -> T.Text -> Context.KeyCommand
command label keys send = Context.KeyCommand label keys (Just send)

listed :: T.Text -> T.Text -> Context.KeyCommand
listed label keys = Context.KeyCommand label keys Nothing

listGroup :: T.Text -> T.Text -> Context.KeyGroup
listGroup name enter =
  group name [listed "move" "\8593\8595", command enter "\9166" "\r", command "close" "Esc" "\ESC"]

pendingGroup :: Context.KeyGroup
pendingGroup = group "waiting for a key" [command "cancel" "Esc" "\ESC"]

tourGroup :: Config.KeyMode -> Context.KeyGroup
tourGroup mode =
  case mode of
    Config.KeysEmacs ->
      group
        "tour"
        [ command "next stop" "C-n" "\SO"
        , command "previous stop" "C-p" "\DLE"
        , command "go to the stop" "\9166" "\r"
        , command "close" "C-c C-k" "\ETX\v"
        ]
    _ ->
      group
        "tour"
        [ command "next stop" "j" "j"
        , command "previous stop" "k" "k"
        , command "go to the stop" "\9166" "\r"
        , command "close" "Esc" "\ESC"
        ]

noteGroup :: Config.KeyMode -> Types.NotePopup -> Context.KeyGroup
noteGroup mode np =
  let
    verb = noteVerb (Types.npPurpose np)
    discard = cancelCommand mode "discard"
  in
    group "note" $
      case Modal.edMode (Types.npEd np) of
        Modal.Insert -> [command verb "Esc \9166" "\ESC\r", discard]
        Modal.Normal -> [command verb "\9166" "\r", command "edit" "i" "i", command "discard" "Esc" "\ESC"]

noteVerb :: Types.PopupPurpose -> T.Text
noteVerb purpose =
  case purpose of
    Types.AmendMessage _ -> "amend the message"
    Types.NoteOn _ -> "save"
    Types.WorkFor wp ->
      case Types.wpAction wp of
        Types.ReplyTo _ _ -> "queue for :submit"
        Types.TakeProposal {} -> "queue for :submit"
        Types.EditPosted _ _ -> "edit on GitHub"
        _ -> "save"

cancelCommand :: Config.KeyMode -> T.Text -> Context.KeyCommand
cancelCommand mode label =
  case mode of
    Config.KeysEmacs -> command label (Types.cancelKeyText mode) "\ETX\v"
    _ -> command label (Types.cancelKeyText mode) "\ETX"

submitGroup :: Config.KeyMode -> Types.SubmitDialog -> Context.KeyGroup
submitGroup mode dialog =
  group "submit" $
    case Types.sdFocus dialog of
      Types.ListFocus ->
        [ listed "move" "\8593\8595"
        , command "edit" "e" "e"
        , command "drop" "dd" "dd"
        , command "jump to its line" "\9166" "\r"
        , command "back to the summary" "Tab" "\t"
        ]
      Types.SummaryFocus ->
        case Modal.edMode (Types.sdSummary dialog) of
          Modal.Insert -> [command "post" "Esc \9166" "\ESC\r", cancelCommand mode "close"]
          Modal.Normal ->
            [ command "post" "\9166" "\r"
            , command "go to the list" "Tab" "\t"
            , command "edit the summary" "i" "i"
            , command "close" "Esc" "\ESC"
            ]

treeGroup :: Types.TreeView -> Context.KeyGroup
treeGroup view =
  case Types.tvOp view of
    Just (Types.OpNaming _ _) -> group "file" [command "do it" "\9166" "\r", command "cancel" "Esc" "\ESC"]
    Just (Types.OpConfirming _) -> group "file" [command "delete it" "y" "y", command "cancel" "any other key" "\ESC"]
    _ ->
      group "find a file" $
        [listed "move" "\8593\8595", command "open" "\9166" "\r"]
          ++ map finderCommand Menus.finderEntries
          ++ [command "close" "Esc" "\ESC"]

finderCommand :: (T.Text, T.Text) -> Context.KeyCommand
finderCommand (keys, label) =
  Context.KeyCommand (Maybe.fromMaybe label (T.stripPrefix "in either: " label)) keys (sendFor keys)

data SpaceForm = Bare | Led

spaceGroup :: SpaceForm -> Context.KeyGroup
spaceGroup form =
  let
    lead = case form of
      Bare -> ""
      Led -> " "
    shown key = case form of
      Bare -> key
      Led -> "\9251 " <> key
    entry (key, label, _) = Context.KeyCommand label (shown key) (fmap (lead <>) (sendFor key))
  in
    group "\9251 menu" (map entry Menus.spaceMenuEntries)

spaceKeys :: [T.Text]
spaceKeys = map (\(key, _, _) -> "\9251 " <> key) Menus.spaceMenuEntries

bodyGroups :: Config.KeyMode -> [Context.KeyGroup]
bodyGroups mode =
  let
    help = map (helpGroup mode) (Menus.helpGroups mode)
  in
    case mode of
      Config.KeysEmacs -> help
      _ -> help ++ [spaceGroup Led]

helpGroup :: Config.KeyMode -> (T.Text, [(T.Text, T.Text)]) -> Context.KeyGroup
helpGroup mode (name, entries) =
  let
    kept (keys, _) =
      notElem keys (map fst Menus.finderEntries)
        && (mode == Config.KeysEmacs || notElem keys spaceKeys)
    toCommand (keys, label) = Context.KeyCommand label keys (sendFor keys)
  in
    group name (map toCommand (filter kept entries))

idleGroup :: Config.KeyMode -> Context.KeyGroup
idleGroup mode =
  case mode of
    Config.KeysEmacs ->
      group
        "remax"
        [ command "commands" "M-x" "\ESCx"
        , command "find a file" "Ctrl-p" "\DLE"
        , command "quit" "C-x C-c" "\CAN\ETX"
        ]
    _ ->
      group
        "remax"
        [ command "commands" ":" ":"
        , command "find a file" "Ctrl-p" "\DLE"
        , command "switch repository" "gr" "gr"
        , command "quit" ":q" ":q\r"
        ]

pullViewGroups :: Config.KeyMode -> [Context.KeyGroup]
pullViewGroups mode =
  case mode of
    Config.KeysEmacs ->
      [ group
          "pull request"
          [ command "next stop" "C-n" "\SO"
          , command "previous stop" "C-p" "\DLE"
          , command "open it" "\9166" "\r"
          , command "search" "C-s" "\DC3"
          , command "commands" "M-x" "\ESCx"
          , command "back to the diff" "Esc" "\ESC"
          ]
      ]
    _ ->
      [ group
          "pull request"
          [ command "next stop" "j" "j"
          , command "previous stop" "k" "k"
          , command "next thread awaiting you" "]" "]"
          , command "previous thread awaiting you" "[" "["
          , command "take the proposal, or open the lines or commit" "\9166" "\r"
          , command "reply" "r" "r"
          , command "ask the agent" "?" "?"
          , command "flag for the agent" "!" "!"
          , command "comment on the pull request" "@" "@"
          , command "edit the post" "e" "e"
          , command "react" "+" "+"
          , command "copy the post" "y" "y"
          , command "the checks" "c" "c"
          , command "fold or unfold here" "za" "za"
          , command "discard the draft" "dd" "dd"
          , command "back to the diff" "Esc" "\ESC"
          ]
      , group
          "getting around"
          [ command "find a file" "Ctrl-p" "\DLE"
          , command "back to the hunk under review" "'" "'"
          , command "next file" ")" ")"
          , command "previous file" "(" "("
          , command "search" "/" "/"
          , command "collapse / expand the tray" "Tab" "\t"
          , command "commands" ":" ":"
          ]
      , spaceGroup Led
      ]

sendFor :: T.Text -> Maybe T.Text
sendFor keys =
  case T.words keys of
    ["\9251", key] -> (" " <>) <$> chord key
    [prefix, key] | prefix `elem` ["C-c", "C-x"] -> (<>) <$> chord prefix <*> chord key
    [key] -> chord key
    _ -> Nothing

chord :: T.Text -> Maybe T.Text
chord key =
  case T.unpack key of
    "\9166" -> Just "\r"
    "\8633" -> Just "\t"
    "Tab" -> Just "\t"
    "Esc" -> Just "\ESC"
    ['C', 'o', 'n', 't', 'r', 'o', 'l', '-', c] -> control c
    ['C', 't', 'r', 'l', '-', c] -> control c
    ['C', '-', c] -> control c
    ['M', '-', c] | Char.isPrint c -> Just (T.pack ['\ESC', c])
    (':' : name)
      | not (null name) && all Char.isAlpha name -> Just (key <> "\r")
      | null name -> Just key
      | otherwise -> Nothing
    cs
      | not (null cs) && length cs <= 2 && all (\c -> Char.isPrint c && not (Char.isSpace c)) cs -> Just key
      | otherwise -> Nothing

control :: Char -> Maybe T.Text
control c
  | Char.isAsciiLower c = Just (T.singleton (Char.chr (Char.ord c - Char.ord 'a' + 1)))
  | c == '_' = Just "\US"
  | otherwise = Nothing
