{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Context
  ( answerContext
  ) where

import Brick qualified as Brick
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.List.NonEmpty qualified as NEL
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.Context qualified as Context
import Remax.Conversation qualified as Conv
import Remax.Feedback qualified as Feedback
import Remax.Git qualified as Git
import Remax.UI.Action.Threads qualified as ThreadAction
import Remax.UI.Core.Anchor qualified as Anchor
import Remax.UI.Core.Types qualified as Types
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

answerContext :: (Context.Answer -> IO ()) -> Brick.EventM Types.Name Types.St ()
answerContext respond = do
  st <- State.get
  MonadIO.liftIO (respond (contextAnswer st))

contextAnswer :: Types.St -> Context.Answer
contextAnswer st =
  case Types.currentFileMay st of
    Nothing -> Context.NoContext "No file open in remax"
    Just fv ->
      case NEL.nonEmpty (Anchor.numberedLinesFor fv (ThreadAction.selectionKeys fv)) of
        Nothing -> Context.NoContext "No lines under the cursor in remax"
        Just numbered -> Context.Attach (pure (attachment st fv numbered (ThreadAction.lineThreads st)))

attachment :: Types.St -> Types.FileView -> NEL.NonEmpty Anchor.NumberedLine -> [Conv.Conversation] -> Context.Attachment
attachment st fv numbered threads =
  let
    path = Types.fvPath fv
    rows = NEL.toList numbered
    oldRange = rangeOf (Maybe.mapMaybe Anchor.nlBase rows)
    newRange = rangeOf (Maybe.mapMaybe Anchor.nlHead rows)
    numbers = T.intercalate ", " (Maybe.catMaybes [fmap ("old " <>) oldRange, fmap ("new " <>) newRange])
    heading = "Lines in remax, " <> T.pack path <> " " <> numbers <> ", " <> comparison (Types.stMode st) <> ":"
    file = "File: " <> T.pack (Types.stRoot st </> path)
    labelRange = Maybe.fromMaybe "" (Maybe.listToMaybe (Maybe.catMaybes [newRange, oldRange]))
    threadNote = if null threads then "" else " \183 thread"
  in
    Context.Attachment
      { Context.attachmentLabel = T.pack (FilePath.takeFileName path) <> ":" <> labelRange <> threadNote
      , Context.attachmentText = T.unlines ([heading, file, diffBlock rows] ++ concatMap threadText threads)
      }

rangeOf :: [Int] -> Maybe T.Text
rangeOf numbers =
  case numbers of
    [] -> Nothing
    _ ->
      let
        lo = minimum numbers
        hi = maximum numbers
      in
        Just (if lo == hi then Types.tshow lo else Types.tshow lo <> "\8211" <> Types.tshow hi)

comparison :: Git.ReviewMode -> T.Text
comparison mode =
  case mode of
    Git.VsRef ref -> "comparing " <> ref <> " with the working tree"
    Git.VsIndex -> "comparing the index with the working tree"
    Git.Untracked -> "an untracked file in the working tree"
    Git.AtRef rr -> "comparing " <> Git.rrBase rr <> " with " <> Git.rrTip rr
    Git.AtPull pr _ ->
      "in pull request #"
        <> Types.tshow (Git.prNumber pr)
        <> " of "
        <> Git.prRepo pr
        <> ", comparing "
        <> Git.prBase pr
        <> " with "
        <> Git.prTip pr

diffBlock :: [Anchor.NumberedLine] -> T.Text
diffBlock rows =
  let
    body =
      if length rows > maxQuotedLines
        then map quoted (take (maxQuotedLines - 1) rows) ++ ["\8230"]
        else map quoted rows
    quoted row = T.cons (Anchor.nlMark row) (Anchor.nlText row)
    fence = T.replicate (max 3 (1 + longestBackticks body)) "`"
  in
    T.intercalate "\n" ([fence <> "diff"] ++ body ++ [fence])

longestBackticks :: [T.Text] -> Int
longestBackticks =
  maximum . (0 :) . map T.length . concatMap (filter (T.isPrefixOf "`") . T.group)

maxQuotedLines :: Int
maxQuotedLines = 40

threadText :: Conv.Conversation -> [T.Text]
threadText conv =
  "Thread on these lines:" : map postText (Conv.convPosts conv)

postText :: Conv.Post -> T.Text
postText post =
  let
    body = T.lines (T.strip (Conv.markdownText (Conv.postBody post)))
  in
    T.intercalate "\n" (map ("> " <>) (("**" <> Feedback.authorWord (Conv.postAuthor post) <> "**:") : body))
