{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Editor
  ( editCurrent
  , launchEditor
  ) where

import Brick qualified as Brick
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Control.Monad.State qualified as State
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.Modal qualified as Modal
import Remax.UI.Core.Types qualified as Types
import System.Environment qualified as Env
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import System.Process qualified as Process

editCurrent :: Brick.EventM Types.Name Types.St ()
editCurrent = do
  st <- State.get
  case Types.currentFileMay st of
    Nothing -> Types.setMsg "no file on screen — :edit PATH opens one"
    Just fv -> do
      let
        (row, _) = Modal.cursorRC (Types.fvEd fv)
      launchEditor (Types.stRoot st </> Types.fvPath fv) (row + 1)

-- Suspend the TUI, run the user's editor on the file at the given line,
-- and come back; the watcher reloads whatever the editor wrote.
launchEditor :: FilePath -> Int -> Brick.EventM Types.Name Types.St ()
launchEditor file line = Types.whenWritable $ do
  st <- State.get
  Brick.suspendAndResume $ do
    (cmd, args) <- editorInvocation file line
    got <-
      ( Exception.try $ do
          (_, _, _, ph) <- Process.createProcess (Process.proc cmd args) {Process.delegate_ctlc = True}
          Monad.void (Process.waitForProcess ph)
      ) ::
        IO (Either Exception.IOException ())
    pure $ case got of
      Left _ -> st {Types.stMsg = "could not launch " <> T.pack cmd}
      Right () -> st

{- $EDITOR, hx-aware: helix takes file:line, the classic editors take
+line, anything unrecognized just gets the file.
-}
editorInvocation :: FilePath -> Int -> IO (String, [String])
editorInvocation file line = do
  mEd <- Env.lookupEnv "EDITOR"
  let
    editorWords = words (Maybe.fromMaybe "hx" mEd)
    (cmd, extraArgs) = case editorWords of
      (c : extra) -> (c, extra)
      [] -> ("hx", [])
    name = FilePath.takeFileName cmd
    positional
      | name `elem` ["hx", "helix"] = [file ++ ":" ++ show line]
      | name `elem` ["vi", "vim", "nvim", "nano", "emacs", "kak"] =
          ["+" ++ show line, file]
      | otherwise = [file]
  pure (cmd, extraArgs ++ positional)

-- Attributes -----------------------------------------------------------------------
