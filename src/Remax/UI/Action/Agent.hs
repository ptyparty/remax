{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Agent
  ( fromAgent
  , afterEvent
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.Modal qualified as Modal
import Remax.Remote qualified as Remote
import Remax.UI.Action.Command qualified as Command
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Model.Nav qualified as Nav
import System.FilePath qualified as FilePath

fromAgent :: Remote.Request -> (Remote.Reply -> IO ()) -> Brick.EventM Types.Name Types.St ()
fromAgent request respond = do
  st <- State.get
  let
    command = T.strip (Maybe.fromMaybe (Remote.rqCommand request) (T.stripPrefix ":" (T.strip (Remote.rqCommand request))))
    mine = sameCheckout (Types.stRoot st) `any` Remote.rqRoots request
  reply <-
    if not mine
      then pure Remote.NotHere
      else case Command.agentRule command of
        Command.ReviewerOnly -> pure (Remote.Refused (":" <> T.takeWhile (/= ' ') command <> " is the reviewer's to run"))
        Command.AgentMay
          | Nav.reviewerFree st -> fmap Remote.Ran (runForAgent command)
          | otherwise -> hold command >> pure Remote.Held
  MonadIO.liftIO (respond reply)

sameCheckout :: FilePath -> FilePath -> Bool
sameCheckout a b =
  let
    tidy = FilePath.dropTrailingPathSeparator . FilePath.normalise
  in
    tidy a == tidy b

runForAgent :: T.Text -> Brick.EventM Types.Name Types.St T.Text
runForAgent command = do
  State.modify (\s -> s {Types.stMsg = ""})
  Command.runCommand command
  Monad.when (T.takeWhile (/= ' ') command == "go") $
    State.modify (\s -> s {Types.stFollow = Types.FollowOff})
  after <- State.get
  let
    said = T.strip (Types.stMsg after)
  State.modify (\s -> s {Types.stClaudeLine = Just (Types.ClaudeRan command said (placeOf after))})
  pure said

hold :: T.Text -> Brick.EventM Types.Name Types.St ()
hold command = State.modify (\s -> s {Types.stAgentHeld = Just command, Types.stClaudeLine = Just (Types.ClaudeOffers command)})

placeOf :: Types.St -> Types.AgentPlace
placeOf st =
  let
    fv = Types.currentFileMay st
  in
    Types.AgentPlace
      { Types.apPath = fmap Types.fvPath fv
      , Types.apCursor = fmap (Modal.cursorRC . Types.fvEd) fv
      }

afterEvent :: Brick.BrickEvent Types.Name Types.REvent -> Brick.EventM Types.Name Types.St ()
afterEvent ev = do
  case ev of
    Brick.VtyEvent _ -> settleClaudeLine
    _ -> pure ()
  offerHeld

settleClaudeLine :: Brick.EventM Types.Name Types.St ()
settleClaudeLine = do
  st <- State.get
  case Types.stClaudeLine st of
    Just (Types.ClaudeRan _ _ place)
      | placeOf st /= place -> State.modify (\s -> s {Types.stClaudeLine = Nothing})
    Just (Types.ClaudeOffers _)
      | Maybe.isNothing (Types.stAgentHeld st) && Maybe.isNothing (Types.stConfirm st) ->
          State.modify (\s -> s {Types.stClaudeLine = Nothing})
    _ -> pure ()

offerHeld :: Brick.EventM Types.Name Types.St ()
offerHeld = do
  st <- State.get
  case Types.stAgentHeld st of
    Just command
      | Nav.reviewerFree st ->
          State.modify $ \s ->
            s
              { Types.stAgentHeld = Nothing
              , Types.stConfirm = Just (Types.Confirm ("Claude asks to run :" <> command <> " \183 y runs \183 any other key drops") (Monad.void (runForAgent command)))
              }
    _ -> pure ()
