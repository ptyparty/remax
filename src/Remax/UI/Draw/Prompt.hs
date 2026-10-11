{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Prompt
  ( promptMenu
  ) where

import Brick qualified as Brick
import Data.Text qualified as T
import Remax.UI.Action.Command qualified as Command
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Types qualified as Types

-- The live menu above the status line: bare ':' lists every command
-- with its description; typing narrows; the cycled candidate is lit.
promptMenu :: Types.St -> Maybe (Brick.Widget Types.Name)
promptMenu st =
  case Types.stPrompt st of
    Just (Types.CmdPrompt input) ->
      let
        base = case Types.stPromptCycle st of
          Just (b, _) -> b
          Nothing -> input
        cands = Command.cmdCandidates st base
        selIx = fmap snd (Types.stPromptCycle st)
        isCmdStage = not (" " `T.isInfixOf` base)
        descOf n = case filter (\(c, _, _) -> c == n) Command.commandTable of
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
            w = Brick.padRight Brick.Max (Brick.txt label)
          in
            if Just i == selIx then Brick.withDefAttr Attrs.selAttr w else w
        shown = take 16 (zip [0 ..] cands)
        more = length cands - length shown
        moreRow =
          if more > 0
            then [Brick.withAttr Attrs.hintAttr (Brick.txt ("   \8230 " <> Types.tshow more <> " more"))]
            else []
      in
        case (cands, Command.lineAddress base) of
          -- a bare number is a line address: no completions to offer,
          -- but say what it will do, like every other command does
          ([], Just n) ->
            Just
              ( Brick.txt
                  ( " :"
                      <> T.justifyLeft 10 ' ' base
                      <> "  go to line "
                      <> Types.tshow (max 1 n)
                  )
              )
          ([], _) -> Nothing
          _ -> Just (Brick.vBox (map row shown ++ moreRow))
    _ -> Nothing
