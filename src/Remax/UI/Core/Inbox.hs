{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Core.Inbox
  ( entries
  , selected
  , Summary (..)
  , Tone (..)
  , summary
  ) where

import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Ord qualified as Ord
import Data.Text qualified as T
import Remax.Conversation qualified as Conv
import Remax.Store qualified as Store
import Remax.UI.Core.Markdown qualified as Markdown
import Remax.UI.Core.PullView qualified as PullView
import Remax.UI.Core.Threads qualified as Threads
import Remax.UI.Core.Types qualified as Types

entries :: Types.St -> T.Text -> [Conv.Conversation]
entries st needle =
  let
    folded = T.toCaseFold (T.strip needle)
    matches conv = T.null folded || folded `T.isInfixOf` T.toCaseFold (searchText st conv)
  in
    List.sortOn (Ord.Down . recency) (filter matches (filter worthListing (Types.stConversations st)))

searchText :: Types.St -> Conv.Conversation -> T.Text
searchText st conv =
  T.unwords
    ( placeText st conv
        : map (Threads.authorName . Conv.postAuthor) (Conv.convPosts conv)
        ++ map (Conv.markdownText . Conv.postBody) (Conv.convPosts conv)
    )

placeText :: Types.St -> Conv.Conversation -> T.Text
placeText st conv =
  case Conv.convVenue conv of
    Conv.ReviewSummary _ event -> "review \183 " <> eventText event
    _ -> PullView.placeOf st conv

eventText :: T.Text -> T.Text
eventText event =
  case event of
    "APPROVED" -> "approved"
    "CHANGES_REQUESTED" -> "changes requested"
    "DISMISSED" -> "dismissed"
    _ -> "commented"

recency :: Conv.Conversation -> Recency
recency conv =
  let
    stamp post = case Conv.postState post of
      Conv.Posted _ at edited -> Just (Dated (max at (Maybe.fromMaybe at edited)))
      Conv.Local at -> Just (Dated at)
      Conv.Draft -> Just Unsent
      Conv.Proposal _ _ -> Just Unsent
  in
    Maybe.fromMaybe (Dated "") (Maybe.listToMaybe (List.sortOn Ord.Down (Maybe.mapMaybe stamp (Conv.convPosts conv))))

data Recency = Dated T.Text | Unsent
  deriving (Eq, Ord)

worthListing :: Conv.Conversation -> Bool
worthListing conv =
  let
    said = any (not . T.null . T.strip . Conv.markdownText . Conv.postBody) (Conv.convPosts conv)
  in
    case Conv.convVenue conv of
      Conv.ReviewSummary _ event -> event /= "COMMENTED" || said
      _ -> not (null (Conv.convPosts conv))

selected :: Types.Inbox -> [Conv.Conversation] -> Maybe Conv.Conversation
selected inbox convs =
  case Types.ibSel inbox >>= (`Conv.conversationFor` convs) of
    Just conv -> Just conv
    Nothing -> Maybe.listToMaybe convs

data Summary = Summary
  { suGlyph :: T.Text
  , suPlace :: T.Text
  , suAuthor :: T.Text
  , suAge :: T.Text
  , suText :: T.Text
  , suTone :: Tone
  }

data Tone = Awaiting | Settled | Ordinary
  deriving (Eq)

summary :: Types.St -> Conv.Conversation -> Summary
summary st conv =
  let
    newest = Maybe.listToMaybe (reverse (Conv.convPosts conv))
    age post = case Conv.postState post of
      Conv.Posted _ at edited -> Threads.ageText (Types.stNow st) (Maybe.fromMaybe at edited)
      Conv.Local at -> Threads.ageText (Types.stNow st) at
      Conv.Draft -> "draft"
      Conv.Proposal _ _ -> "proposes"
  in
    Summary
      { suGlyph = glyphOf (Conv.convVenue conv)
      , suPlace = placeText st conv
      , suAuthor = maybe "" (Threads.authorName . Conv.postAuthor) newest
      , suAge = maybe "" age newest
      , suText = maybe "" (Markdown.firstLine . Conv.postBody) newest
      , suTone = toneOf st conv
      }

glyphOf :: Conv.Venue -> T.Text
glyphOf venue =
  case venue of
    Conv.ReviewThread _ _ -> "\8618"
    Conv.PullComments -> "\9679"
    Conv.ReviewSummary _ _ -> "\9679"
    Conv.LocalOnly Store.Flagged -> "!"
    Conv.LocalOnly Store.Questioned -> "?"
    Conv.LocalOnly Store.Reviewed -> "\183"

toneOf :: Types.St -> Conv.Conversation -> Tone
toneOf st conv =
  case Conv.convVenue conv of
    Conv.ReviewThread _ Store.Resolved -> Settled
    _
      | Threads.awaitsYou st conv -> Awaiting
      | otherwise -> Ordinary
