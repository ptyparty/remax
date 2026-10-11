{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw
  ( draw
  ) where

import Brick
  ( ViewportType (Vertical)
  , Widget
  , hBox
  , vBox
  , viewport
  )
import Brick.Types
  ( Size (Fixed, Greedy)
  , Widget (Widget)
  , availWidth
  , getContext
  , render
  , windowHeight
  )
import Data.Maybe
  ( catMaybes
  )
import Remax.Config
import Remax.UI.Draw.Body
import Remax.UI.Draw.Header
import Remax.UI.Draw.Help
import Remax.UI.Draw.Notes
import Remax.UI.Draw.Picker
import Remax.UI.Draw.Prompt
import Remax.UI.Draw.Status
import Remax.UI.Draw.Tray
import Remax.UI.Draw.Tree
import Remax.UI.Types
import Remax.UI.Widgets

draw :: St -> [Widget Name]
draw st =
  let
    body = case currentFileMay st of
      Just fv ->
        viewport BodyVP Vertical $
          Widget Greedy Fixed $ do
            ctx <- getContext
            render (renderFile (availWidth ctx) (windowHeight ctx) st fv)
      Nothing -> idleBody st
    panes =
      if stTray st && not (null (stFiles st))
        then [tray st, body]
        else [body]
    overlays =
      catMaybes
        [ fmap (notePopupLayer st) (stNotePopup st)
        , fileMenuLayer =<< stTree st
        , fmap (treeLayer st) (stTree st)
        , fmap pickerLayer (stPicker st)
        , if stSpace st then Just spaceMenu else Nothing
        , pendingMenuLayer st
        ]
    menuRows = case promptMenu st of
      Just menu -> [menu]
      Nothing -> []
    rebaseRows = case stRebase st of
      Just rb -> [rebaseBar rb]
      Nothing -> [gitBar st]
    helpRows =
      if stHelp st then [helpLayer (cfgKeys (stConfig st))] else []
    infoRows = case stInfoPopup st of
      Just (title, infoBody) -> [infoPopupLayer title infoBody]
      Nothing -> []
  in
    helpRows
      ++ infoRows
      ++ overlays
      ++ [ vBox
             ( [headerBar st]
                 ++ rebaseRows
                 ++ [hairline, hBox panes, hairline]
                 ++ menuRows
                 ++ [statusBar st]
             )
         ]
