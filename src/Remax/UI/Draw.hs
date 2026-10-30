{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw
  ( draw
  ) where

import Brick qualified as Brick
import Brick.Types qualified as BrickTypes
import Data.Maybe qualified as Maybe
import Remax.Config qualified as Config
import Remax.Git qualified as Git
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.View qualified as View
import Remax.UI.Core.Widgets qualified as Widgets
import Remax.UI.Draw.Body qualified as DrawBody
import Remax.UI.Draw.Cite qualified as DrawCite
import Remax.UI.Draw.Header qualified as DrawHeader
import Remax.UI.Draw.Help qualified as DrawHelp
import Remax.UI.Draw.Inbox qualified as DrawInbox
import Remax.UI.Draw.Notes qualified as DrawNotes
import Remax.UI.Draw.Picker qualified as DrawPicker
import Remax.UI.Draw.Prompt qualified as DrawPrompt
import Remax.UI.Draw.PullView qualified as DrawPullView
import Remax.UI.Draw.Reactions qualified as DrawReactions
import Remax.UI.Draw.Status qualified as DrawStatus
import Remax.UI.Draw.Submit qualified as DrawSubmit
import Remax.UI.Draw.Tour qualified as DrawTour
import Remax.UI.Draw.Tray qualified as DrawTray
import Remax.UI.Draw.Tree qualified as DrawTree

draw :: Types.St -> [BrickTypes.Widget Types.Name]
draw st =
  let
    body = case (Types.stPullView st, Types.currentFileMay st) of
      (Just _, _) -> DrawPullView.pullViewBody st
      (Nothing, Just fv) ->
        Brick.viewport Types.BodyVP Brick.Vertical $
          BrickTypes.Widget BrickTypes.Greedy BrickTypes.Fixed $ do
            ctx <- BrickTypes.getContext
            BrickTypes.render (DrawBody.renderFile (BrickTypes.availWidth ctx) (BrickTypes.windowHeight ctx) st fv)
      (Nothing, Nothing) -> DrawBody.idleBody st
    panes =
      if Types.stTray st && not (null (Types.stFiles st))
        then [DrawTray.tray st, body]
        else [body]
    overlays =
      Maybe.catMaybes
        [ fmap DrawReactions.reactionLayer (Types.stReacting st)
        , fmap (DrawSubmit.submitLayer st) (Types.stSubmit st)
        , fmap DrawCite.citeLayer (Types.stCiting st)
        , floatingNotePopup st
        , fmap (DrawInbox.inboxLayer st) (Types.stInbox st)
        , fmap (DrawTour.tourOverviewLayer st) (Types.stTourOverview st)
        , DrawTree.fileMenuLayer =<< Types.stTree st
        , fmap (DrawTree.treeLayer st) (Types.stTree st)
        , fmap DrawPicker.pickerLayer (Types.stPicker st)
        , if Types.stSpace st then Just DrawHelp.spaceMenu else Nothing
        , DrawHelp.pendingMenuLayer st
        ]
    menuRows = case DrawPrompt.promptMenu st of
      Just menu -> [menu]
      Nothing -> []
    rebaseRows = case (Git.subjectOf (Types.stMode st), Types.rebaseShown st) of
      (Git.Pull pr editing, _) -> [DrawHeader.pullBar st pr editing]
      (_, Just rb) -> [DrawHeader.rebaseBar rb]
      (_, Nothing) -> [DrawHeader.gitBar st]
    tourRows = maybe [] (: []) (DrawHeader.tourBar st)
    helpRows =
      if Types.stHelp st then [DrawHelp.helpLayer (Config.cfgKeys (Types.stConfig st))] else []
    infoRows = case Types.stInfoPopup st of
      Just (title, infoBody) -> [Widgets.infoPopupLayer title infoBody]
      Nothing -> []
  in
    helpRows
      ++ infoRows
      ++ overlays
      ++ [ Brick.vBox
             ( [DrawHeader.headerBar st]
                 ++ rebaseRows
                 ++ tourRows
                 ++ [Widgets.hairline, Brick.hBox panes, Widgets.hairline]
                 ++ menuRows
                 ++ [DrawStatus.statusBar st]
             )
         ]

floatingNotePopup :: Types.St -> Maybe (BrickTypes.Widget Types.Name)
floatingNotePopup st = do
  np <- Types.stNotePopup st
  case View.popupPlacement st np of
    View.Floating -> Just (DrawNotes.notePopupLayer st np)
    View.InInbox -> Nothing
