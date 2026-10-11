{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Prompt
  ( promptMenu
  ) where

import Brick
  ( Padding (Max)
  , Widget
  , padRight
  , txt
  , vBox
  , withAttr
  , withDefAttr
  )
import qualified Data.Text as T
import Remax.UI.Attrs
import Remax.UI.Command
import Remax.UI.Types

-- The live menu above the status line: bare ':' lists every command
-- with its description; typing narrows; the cycled candidate is lit.
promptMenu :: St -> Maybe (Widget Name)
promptMenu st =
  case stPrompt st of
    Just (CmdPrompt input) ->
      let
        base = case stPromptCycle st of
          Just (b, _) -> b
          Nothing -> input
        cands = cmdCandidates st base
        selIx = fmap snd (stPromptCycle st)
        isCmdStage = not (" " `T.isInfixOf` base)
        descOf n = case filter (\(c, _, _) -> c == n) commandTable of
          ((_, _, d) : _) -> d
          [] -> ""
        row (i, n) =
          let
            label =
              if isCmdStage
                then
                  " :"
                    <> T.justifyLeft 10 ' ' n
                    <> "  "
                    <> descOf n
                else " " <> n
            w = padRight Max (txt label)
          in
            if Just i == selIx then withDefAttr selAttr w else w
        shown = take 16 (zip [0 ..] cands)
        more = length cands - length shown
        moreRow =
          if more > 0
            then [withAttr hintAttr (txt ("   \8230 " <> tshow more <> " more"))]
            else []
      in
        case (cands, lineAddress base) of
          -- a bare number is a line address: no completions to offer,
          -- but say what it will do, like every other command does
          ([], Just n) ->
            Just
              ( txt
                  ( " :"
                      <> T.justifyLeft 10 ' ' base
                      <> "  go to line "
                      <> tshow (max 1 n)
                  )
              )
          ([], _) -> Nothing
          _ -> Just (vBox (map row shown ++ moreRow))
    _ -> Nothing
