{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Menus
  ( helpGroups
  , spaceMenuEntries
  ) where

import Brick qualified as Brick
import Data.Text qualified as T
import Remax.Config qualified as Config
import Remax.Store qualified as Store
import Remax.UI.Action.Editor qualified as Editor
import Remax.UI.Action.Notes qualified as NoteAction
import Remax.UI.Action.Open qualified as Open
import Remax.UI.Action.Picker qualified as PickerAction
import Remax.UI.Action.Settings qualified as Settings
import Remax.UI.Action.Threads qualified as ThreadAction
import Remax.UI.Action.Tree qualified as TreeAction
import Remax.UI.Action.Verdicts qualified as VerdictAction
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Model.Nav qualified as Nav

-- The pending-space infobox, helix style: bottom-right, listing what the
-- menu offers.
-- The cheat sheet: what a newcomer cannot otherwise find out. The :
-- commands document themselves — a bare : lists every one with its
-- description — but the keys had nowhere to be discovered, so this is
-- mostly keys, grouped by what you are trying to do rather than
-- alphabetically.
-- The keys differ by family, so the sheet does too — an emacs or vi
-- session showing helix's keys would be worse than no sheet at all.
helpGroups :: Config.KeyMode -> [(T.Text, [(T.Text, T.Text)])]
helpGroups mode =
  (reviewGroup mode : sayingGroup : editingGroups mode)
    ++ aroundGroups mode

sayingGroup :: (T.Text, [(T.Text, T.Text)])
sayingGroup =
  ( "saying something"
  ,
    [ ("@", "comment on the lines, for GitHub")
    , ("r", "reply to the thread under the cursor")
    , ("j", "enter the thread; j k move by post")
    , ("\9251 u", "drop the conversation here")
    ]
  )

aroundGroups :: Config.KeyMode -> [(T.Text, [(T.Text, T.Text)])]
aroundGroups mode =
  [
    ( "getting around"
    ,
      [ ("Ctrl-p", "find a file, flat and ranked")
      , ("\9251 '", "the last selector again, as you left it")
      , ("\9251 t", "the same dialog, as a tree")
      , ("\8633", "in either: flat \8596 tree, filter kept")
      , ("Ctrl-a", "in either: show the ignored files too")
      , ("Ctrl-x", "in either: new / rename / duplicate / delete")
      , ("gr", "switch repository, starting on the next")
      , ("gn gp", "the next / previous file, as ) and ( do")
      , ("\9251 b", "files already open in the session")
      , ("Tab", "collapse / expand the tray")
      , ("{ }", "scroll the tray")
      , -- vi wants E for the WORD motion, so the leader carries $EDITOR
        (if mode == Config.KeysVi then "\9251 e" else "E", "suspend into $EDITOR here")
      , ("\9251 a", "the alternate file")
      ]
    )
  ,
    ( "the " <> Types.promptKeys mode <> " line"
    ,
      [ (Types.promptKeys mode, "commands — a bare one lists them all")
      , (":base", "pick what to diff against")
      , (":rebase", "review commit by commit")
      , -- the command is :q whatever the keymap, but an emacs hand
        -- reaches for its own chord and should find it here
        (Types.quitKeys mode, "quit")
      ]
    )
  ]

reviewGroup :: Config.KeyMode -> (T.Text, [(T.Text, T.Text)])
reviewGroup mode =
  let
    common =
      [ ("] [", "next / previous hunk")
      , ("'", "back to the hunk under review")
      , ("\9251 h", "make this hunk the current one")
      , ("\9251 p", "show only hunks still pending")
      , ("\9251 k", "walk all / code / comment hunks")
      ]
  in
    ( "review"
    , case mode of
        Config.KeysVi ->
          [ ("\9251 \9166", "accept the selection as reviewed")
          , ("\9251 !", "flag it — come back to this")
          , ("\9251 q", "question it — explain or fix")
          , ("\9251 x", "select the whole hunk")
          ]
            ++ common
        Config.KeysEmacs ->
          [ ("C-c \9166", "accept the selection as reviewed")
          , ("C-c !", "flag it — come back to this")
          , ("C-c ?", "question it — explain or fix")
          , ("C-c x", "select the whole hunk")
          , ("C-c ] [", "next / previous hunk")
          , ("C-c p", "show only hunks still pending")
          , ("C-c k", "walk all / code / comment hunks")
          ]
        Config.KeysHelix ->
          [ ("\9166", "accept the selection as reviewed")
          , ("!", "flag it — come back to this")
          , ("?", "question it — explain or fix")
          , ("X", "select the whole hunk")
          ]
            ++ common
    )

editingGroups :: Config.KeyMode -> [(T.Text, [(T.Text, T.Text)])]
editingGroups mode =
  case mode of
    Config.KeysVi ->
      [
        ( "moving"
        ,
          [ ("h j k l", "left down up right")
          , ("w b e / W B E", "words / WORDs")
          , ("f F t T ; ,", "find / till a character, repeat")
          , ("0 ^ $", "line start / first non-blank / end")
          , ("gg G %", "file start / end / matching bracket")
          , (":42", "go to line 42")
          , (") (", "next / previous file")
          ]
        )
      ,
        ( "operators"
        ,
          [ ("d c y > <", "delete change yank indent unindent")
          , ("dw d2w dd", "operator, motion, count — or doubled")
          , ("ciw di( ya\"", "text objects: w W p ( { [ < \" ' `")
          , ("gu gU g~", "lower / upper / swap case")
          , ("x X D C s S Y", "the usual shorthands")
          ]
        )
      ,
        ( "editing"
        ,
          [ ("i a I A o O", "enter insert")
          , ("v V", "visual, visual-line")
          , ("p P J r ~", "paste / join / replace char / case")
          , ("u  Ctrl-r", "undo / redo")
          , (".", "repeat the last change")
          ]
        )
      ,
        ( "finding"
        ,
          [ ("/ ? n N", "search forward / back, repeat")
          , ("* #", "the word under the cursor, either way")
          , ("\9251 /", "search the whole project")
          ]
        )
      ]
    Config.KeysEmacs ->
      [
        ( "moving"
        ,
          [ ("C-f C-b C-n C-p", "left right down up")
          , ("C-a C-e", "line start / end")
          , ("M-f M-b", "words")
          , ("M-< M->", "file start / end")
          , (":42", "go to line 42")
          , ("C-c ) (", "next / previous file")
          ]
        )
      ,
        ( "editing"
        ,
          [ ("C-space", "set the mark; the region is the selection")
          , ("C-w M-w", "kill / copy the region")
          , ("C-k C-y", "kill to end of line / yank")
          , ("C-_", "undo")
          , ("C-x C-s", "save (edits already write through)")
          ]
        )
      ,
        ( "finding"
        ,
          [ ("C-s", "search this file")
          , ("M-x", "the command line")
          , ("\9251 /", "search the whole project")
          ]
        )
      ]
    Config.KeysHelix -> helixEditingGroups

helixEditingGroups :: [(T.Text, [(T.Text, T.Text)])]
helixEditingGroups =
  [
    ( "moving"
    ,
      [ ("h j k l", "left down up right")
      , ("w b e", "words; the crossed span is selected")
      , ("f F t T", "find / till a character")
      , ("gh gl gs", "line start / end / first non-blank")
      , ("gg ge", "file start / file end")
      , (":42", "go to line 42")
      , (") (", "next / previous file")
      , ("ga", "the alternate file")
      ]
    )
  ,
    ( "selecting"
    ,
      [ ("v", "every motion grows the selection")
      , ("x", "the line; again to extend")
      , ("%", "the whole file")
      , (";", "collapse it")
      ]
    )
  ,
    ( "editing"
    ,
      [ ("i a I A o O", "enter insert")
      , ("d c", "delete / change the selection")
      , ("u", "undo")
      , ("y p P", "yank / paste after / before")
      , ("> <", "indent / unindent")
      , ("J Q", "join lines / reflow to 80")
      , ("r ~", "replace character / toggle case")
      , (".", "repeat the last insert")
      ]
    )
  ,
    ( "finding"
    ,
      [ ("/ n N", "search this file, repeat fwd / back")
      , ("*", "search for the word under the cursor")
      , ("\9251 /", "search the whole project")
      ]
    )
  ]

-- Space, then one key. This table is the only place a space key exists:
-- the infobox is rendered from it and the keystroke is dispatched through
-- it, so a key cannot be bound without being listed. :tree arrived bound
-- but invisible precisely because those used to be two lists.
resolveOrUnnote :: Brick.EventM Types.Name Types.St ()
resolveOrUnnote = do
  handled <- ThreadAction.resolveKey
  case handled of
    ThreadAction.Handled -> pure ()
    ThreadAction.Unhandled -> NoteAction.unnoteCmd

askOrQuestion :: Brick.EventM Types.Name Types.St ()
askOrQuestion = do
  handled <- ThreadAction.askKey
  case handled of
    ThreadAction.Handled -> pure ()
    ThreadAction.Unhandled -> NoteAction.markThenNote Store.Questioned

spaceMenuEntries :: [(T.Text, T.Text, Brick.EventM Types.Name Types.St ())]
spaceMenuEntries =
  [ ("\9166", "accept as reviewed", VerdictAction.acceptSelection)
  , ("!", "flag it", NoteAction.markThenNote Store.Flagged)
  , ("q", "question it", askOrQuestion)
  , ("x", "select the hunk", Nav.selectCurrentHunk)
  , ("f", "find a file", TreeAction.openFileFinder)
  , ("b", "open files", PickerAction.openBufferPicker)
  , ("t", "project tree", TreeAction.openTree)
  , ("R", "switch repository", PickerAction.openRepoPicker)
  , ("'", "last selector", PickerAction.reopenLastSelector)
  , ("e", "$EDITOR here", Editor.editCurrent)
  , ("p", "walk filter: pending / unresolved / all", VerdictAction.cycleWalkFilter)
  , ("w", "toggle ignore whitespace", Settings.toggleIgnoreWs)
  , ("u", "resolve the thread or note here", resolveOrUnnote)
  , ("h", "make this hunk current", Nav.setCurrentHunkHere)
  , ("r", "revert hunk to base", VerdictAction.revertTarget)
  , ("a", "alternate file", Open.gotoAltFile)
  , ("/", "project search", PickerAction.openGrepPicker "")
  , ("c", "commit message", VerdictAction.showCommitPopup)
  , ("?", "keys", Types.showHelp)
  ]
