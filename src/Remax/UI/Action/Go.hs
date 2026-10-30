{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Go
  ( goTo
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.Conversation qualified as Conv
import Remax.DiffModel qualified as DiffModel
import Remax.Feedback qualified as Feedback
import Remax.Git qualified as Git
import Remax.UI.Action.Inbox qualified as InboxAction
import Remax.UI.Action.Open qualified as Open
import Remax.UI.Action.PullView qualified as PullViewAction
import Remax.UI.Action.Tour qualified as TourAction
import Remax.UI.Action.Verdicts qualified as VerdictAction
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Verdict qualified as Verdict
import Remax.UI.Model.Nav qualified as Nav
import System.Directory qualified as Directory
import System.FilePath ((</>))

goTo :: [T.Text] -> Brick.EventM Types.Name Types.St ()
goTo targetWords =
  case targetWords of
    [] -> Types.setMsg "go where? a hunk id, a conversation id, PATH[:LINE], tour [N], pr or feedback"
    ["pr"] -> leavePopups >> PullViewAction.openPullView
    ["feedback"] -> leavePopups >> InboxAction.openInbox
    ["tour"] -> leavePopups >> TourAction.openTourOverview
    ["tour", n]
      | Just k <- Git.readCount n -> leavePopups >> VerdictAction.gotoTourStep k
    [word] -> goWord word
    _ -> Types.setMsg ("go takes one target, not " <> T.unwords targetWords)

data Target
  = HunkAt (Int, Int)
  | ConversationNamed Conv.Conversation
  | FileAt T.Text (Maybe Int)

goWord :: T.Text -> Brick.EventM Types.Name Types.St ()
goWord word = do
  st <- State.get
  file <- MonadIO.liftIO (fileTarget st word)
  let
    found = hunkTargets st word ++ conversationTargets st word ++ Maybe.maybeToList file
  case found of
    [] -> Types.setMsg ("nothing called " <> word <> " here: no hunk, conversation or file")
    [target] -> leavePopups >> arrive target
    several ->
      Types.setMsg (word <> " matches " <> Types.tshow (length several) <> " things (" <> T.intercalate ", " (List.nub (map kindOf several)) <> ") \8212 give more of it")

kindOf :: Target -> T.Text
kindOf target =
  case target of
    HunkAt _ -> "hunks"
    ConversationNamed _ -> "a conversation"
    FileAt _ _ -> "a file"

hunkTargets :: Types.St -> T.Text -> [Target]
hunkTargets st word =
  let
    names run = maybe False (word `T.isPrefixOf`) (Maybe.listToMaybe (DiffModel.runKeys run))
  in
    if T.length word < 4
      then []
      else map (HunkAt . fst) (filter (names . snd) (Verdict.allRunsInOrder st))

conversationTargets :: Types.St -> T.Text -> [Target]
conversationTargets st word =
  let
    convs = Types.stConversations st
  in
    case Feedback.resolveId (map (Conv.conversationIdText . Conv.convId) convs) word of
      Feedback.ResolvedId found ->
        map ConversationNamed (filter ((== found) . Conv.conversationIdText . Conv.convId) convs)
      Feedback.AmbiguousId _ -> []
      Feedback.UnknownId -> []

fileTarget :: Types.St -> T.Text -> IO (Maybe Target)
fileTarget st word = do
  let
    (path, line) = splitLine word
  exists <- Directory.doesFileExist (Types.stRoot st </> T.unpack path)
  pure (if exists then Just (FileAt path line) else Nothing)

splitLine :: T.Text -> (T.Text, Maybe Int)
splitLine word =
  let
    (before, after) = T.breakOnEnd ":" word
  in
    case (T.null before, Git.readCount after) of
      (False, Just n) -> (T.dropEnd 1 before, Just n)
      _ -> (word, Nothing)

arrive :: Target -> Brick.EventM Types.Name Types.St ()
arrive target =
  case target of
    HunkAt (f, h) -> do
      Nav.visitAnchor f h
      Nav.scrollTrayToCurrent
      st <- State.get
      Types.setMsg ("at the hunk in " <> maybe "" (T.pack . Types.fvPath) (Types.currentFileMay st))
    ConversationNamed conv -> InboxAction.jumpTo conv
    FileAt path line -> do
      Open.openFilePath path
      opened <- State.get
      Monad.when (fmap (T.pack . Types.fvPath) (Types.currentFileMay opened) == Just path) $
        Monad.forM_ line Nav.gotoLine

leavePopups :: Brick.EventM Types.Name Types.St ()
leavePopups = State.modify (\s -> s {Types.stInbox = Nothing, Types.stTourOverview = Nothing})
