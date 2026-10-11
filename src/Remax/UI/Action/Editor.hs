{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Editor
  ( editCurrent
  , launchEditor
  ) where

import Brick
  ( EventM
  , suspendAndResume
  )
import Control.Exception
  ( IOException
  , try
  )
import Control.Monad
  ( void
  )
import Control.Monad.State
  ( get
  )
import Data.Maybe
  ( fromMaybe
  )
import qualified Data.Text as T
import Remax.Modal
import Remax.UI.Core.Types
import System.Environment (lookupEnv)
import System.FilePath
  ( takeFileName
  , (</>)
  )
import System.Process
  ( CreateProcess (delegate_ctlc)
  , createProcess
  , proc
  , waitForProcess
  )

editCurrent :: EventM Name St ()
editCurrent = do
  st <- get
  case currentFileMay st of
    Nothing -> setMsg "no file on screen — :edit PATH opens one"
    Just fv -> do
      let
        (row, _) = cursorRC (fvEd fv)
      launchEditor (stRoot st </> fvPath fv) (row + 1)

-- Suspend the TUI, run the user's editor on the file at the given line,
-- and come back; the watcher reloads whatever the editor wrote.
launchEditor :: FilePath -> Int -> EventM Name St ()
launchEditor file line = do
  st <- get
  suspendAndResume $ do
    (cmd, args) <- editorInvocation file line
    got <-
      ( try $ do
          (_, _, _, ph) <- createProcess (proc cmd args) {delegate_ctlc = True}
          void (waitForProcess ph)
      ) ::
        IO (Either IOException ())
    pure $ case got of
      Left _ -> st {stMsg = "could not launch " <> T.pack cmd}
      Right () -> st

{- $EDITOR, hx-aware: helix takes file:line, the classic editors take
+line, anything unrecognized just gets the file.
-}
editorInvocation :: FilePath -> Int -> IO (String, [String])
editorInvocation file line = do
  mEd <- lookupEnv "EDITOR"
  let
    editorWords = words (fromMaybe "hx" mEd)
    (cmd, extraArgs) = case editorWords of
      (c : extra) -> (c, extra)
      [] -> ("hx", [])
    name = takeFileName cmd
    positional
      | name `elem` ["hx", "helix"] = [file ++ ":" ++ show line]
      | name `elem` ["vi", "vim", "nvim", "nano", "emacs", "kak"] =
          ["+" ++ show line, file]
      | otherwise = [file]
  pure (cmd, extraArgs ++ positional)

-- Attributes -----------------------------------------------------------------------
