{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Go
  ( goTo
  , highlight
  , dropSpotlightOnEsc
  , dropStaleSpotlight
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Remax.Conversation qualified as Conv
import Remax.DiffModel qualified as DiffModel
import Remax.Feedback qualified as Feedback
import Remax.Git qualified as Git
import Remax.LineRange qualified as LineRange
import Remax.Modal qualified as Modal
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
import System.FilePath qualified as FilePath

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
    (path, line) = case LineRange.parseLineRange word of
      Just LineRange.LineRange {LineRange.lrPath = Just p, LineRange.lrLines = ls} -> (p, Just (LineRange.lnFirst ls))
      _ -> (word, Nothing)
  exists <- Directory.doesFileExist (Types.stRoot st </> T.unpack path)
  pure (if exists then Just (FileAt path line) else Nothing)

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
      Monad.when (maybe False (showsPath path) (Types.currentFileMay opened)) $
        Monad.forM_ line Nav.gotoLine

showsPath :: T.Text -> Types.FileView -> Bool
showsPath path fv = FilePath.normalise (T.unpack path) == FilePath.normalise (Types.fvPath fv)

leavePopups :: Brick.EventM Types.Name Types.St ()
leavePopups = State.modify (\s -> s {Types.stInbox = Nothing, Types.stTourOverview = Nothing})

highlight :: [T.Text] -> Brick.EventM Types.Name Types.St ()
highlight targetWords =
  case targetWords of
    [] -> clearSpotlight >> Types.setMsg "highlight cleared"
    [word]
      | Just range <- LineRange.parseLineRange word -> lightUp range
    _ -> Types.setMsg "highlight what? [PATH:]LINE[-LINE]"

lightUp :: LineRange.LineRange -> Brick.EventM Types.Name Types.St ()
lightUp range = do
  let
    ls = LineRange.lrLines range
  case LineRange.lrPath range of
    Just path -> leavePopups >> arrive (FileAt path (Just (LineRange.lnFirst ls)))
    Nothing -> Nav.gotoLine (LineRange.lnFirst ls)
  st <- State.get
  case Types.currentFileMay st of
    Just fv
      | maybe True (`showsPath` fv) (LineRange.lrPath range) -> do
          State.modify $ \s ->
            s
              { Types.stSpotlight =
                  Just
                    Types.Spotlight
                      { Types.spRoot = Types.stRoot st
                      , Types.spPath = Types.fvPath fv
                      , Types.spLines = ls
                      , Types.spText = linesOf ls fv
                      }
              }
          Types.setMsg ("highlighted " <> LineRange.lineRangeText range {LineRange.lrPath = Just (T.pack (Types.fvPath fv))})
    _ ->
      let
        why = if T.null (Types.stMsg st) then "it is not open here" else Types.stMsg st
      in
        clearSpotlight >> Types.setMsg ("could not highlight " <> LineRange.lineRangeText range <> ": " <> why)

linesOf :: LineRange.Lines -> Types.FileView -> [T.Text]
linesOf ls fv =
  take (LineRange.lnLast ls - LineRange.lnFirst ls + 1) (drop (LineRange.lnFirst ls - 1) (Modal.bufferLines (Types.fvEd fv)))

clearSpotlight :: Brick.EventM Types.Name Types.St ()
clearSpotlight = State.modify (\s -> s {Types.stSpotlight = Nothing})

dropSpotlightOnEsc :: Brick.BrickEvent Types.Name Types.REvent -> Brick.EventM Types.Name Types.St ()
dropSpotlightOnEsc ev = do
  st <- State.get
  let
    idle ed = Maybe.isNothing (Modal.selectionRows ed) && Maybe.isNothing (Modal.charSelection ed) && not (Modal.hasPending ed)
    nothingElse =
      Nav.reviewerFree st
        && Maybe.isNothing (Types.stPullView st)
        && maybe True (idle . Types.fvEd) (Types.currentFileMay st)
  case (ev, Types.stSpotlight st) of
    (Brick.VtyEvent (V.EvKey V.KEsc []), Just _)
      | nothingElse -> clearSpotlight
    _ -> pure ()

dropStaleSpotlight :: Brick.EventM Types.Name Types.St ()
dropStaleSpotlight = do
  st <- State.get
  let
    onIt sp fv = Types.spRoot sp == Types.stRoot st && Types.fvPath fv == Types.spPath sp && linesOf (Types.spLines sp) fv == Types.spText sp
  case (Types.stSpotlight st, Types.currentFileMay st) of
    (Just sp, Just fv)
      | onIt sp fv -> pure ()
    (Just _, _) -> clearSpotlight
    _ -> pure ()
