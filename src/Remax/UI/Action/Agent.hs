{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Agent
  ( fromAgent
  , afterEvent
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.Modal qualified as Modal
import Remax.Remote qualified as Remote
import Remax.UI.Action.Command qualified as Command
import Remax.UI.Action.Repo qualified as Repo
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Model.Nav qualified as Nav
import System.FilePath qualified as FilePath

fromAgent :: Remote.Request -> (Remote.Reply -> IO ()) -> Brick.EventM Types.Name Types.St ()
fromAgent request respond = do
  let
    command = T.strip (Maybe.fromMaybe (Remote.rqCommand request) (T.stripPrefix ":" (T.strip (Remote.rqCommand request))))
  target <- checkoutFor (Remote.rqReach request) (Remote.rqRoots request)
  st <- State.get
  case target of
    Nothing -> MonadIO.liftIO (respond Remote.NotHere)
    Just root ->
      case Command.agentRule command of
        Command.ReviewerOnly -> MonadIO.liftIO (respond (Remote.Refused (":" <> T.takeWhile (/= ' ') command <> " is the reviewer's to run")))
        Command.AgentMay
          | Nav.reviewerFree st -> runAt root command respond
          | otherwise -> hold root command >> MonadIO.liftIO (respond Remote.Held)

checkoutFor :: Remote.Reach -> [FilePath] -> Brick.EventM Types.Name Types.St (Maybe FilePath)
checkoutFor reach roots = do
  st <- State.get
  case (sameCheckout (Types.stRoot st) `any` roots, reach, Types.stStory st) of
    (True, _, _) -> pure (Just (Types.stRoot st))
    (False, Remote.AnywhereInStory, Just _) -> do
      Repo.rescanRepos
      rescanned <- State.get
      pure (List.find (\root -> sameCheckout root `any` roots) (Types.stRoots rescanned))
    _ -> pure Nothing

runAt :: FilePath -> T.Text -> (Remote.Reply -> IO ()) -> Brick.EventM Types.Name Types.St ()
runAt root command respond = do
  st <- State.get
  let
    ran = runForAgent command >>= MonadIO.liftIO . respond . Remote.Ran
  if root == Types.stRoot st
    then ran
    else
      Repo.arriveReporting
        (MonadIO.liftIO . respond . Remote.Refused)
        Repo.Arrival {Repo.arTarget = root, Repo.arMode = Nothing, Repo.arThen = runOnceFree root command respond}

runOnceFree :: FilePath -> T.Text -> (Remote.Reply -> IO ()) -> Brick.EventM Types.Name Types.St ()
runOnceFree root command respond = do
  st <- State.get
  if Nav.reviewerFree st
    then runForAgent command >>= MonadIO.liftIO . respond . Remote.Ran
    else hold root command >> MonadIO.liftIO (respond Remote.Held)

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

hold :: FilePath -> T.Text -> Brick.EventM Types.Name Types.St ()
hold root command = do
  st <- State.get
  let
    held = Types.AgentHeld {Types.ahRoot = root, Types.ahCommand = command}
  State.modify (\s -> s {Types.stAgentHeld = Just held, Types.stClaudeLine = Just (Types.ClaudeOffers (heldText st held))})

heldText :: Types.St -> Types.AgentHeld -> T.Text
heldText st held =
  if Types.ahRoot held == Types.stRoot st
    then Types.ahCommand held
    else Types.ahCommand held <> " in " <> T.pack (FilePath.takeFileName (Types.ahRoot held))

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
    Just held
      | Nav.reviewerFree st ->
          State.modify $ \s ->
            s
              { Types.stAgentHeld = Nothing
              , Types.stConfirm = Just (Types.Confirm ("Claude asks to run :" <> heldText st held <> " \183 y runs \183 any other key drops") (runAt (Types.ahRoot held) (Types.ahCommand held) (const (pure ()))))
              }
      | otherwise -> State.modify (\s -> s {Types.stClaudeLine = Just (Types.ClaudeOffers (heldText st held))})
    _ -> pure ()
