{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Core.Threads
  ( ThreadLine (..)
  , threadLines
  , bandable
  , bandIndexes
  , postKeys
  , postLabel
  , possessive
  , threadState
  , anchorPlace
  , authorName
  , ageText
  , shortAge
  , isResolvedFold
  , lineHint
  ) where

import Brick qualified as Brick
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Clock qualified as Clock
import Data.Time.Format.ISO8601 qualified as ISO8601
import Remax.Config qualified as Config
import Remax.Conversation qualified as Conv
import Remax.Store qualified as Store
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Markdown qualified as Markdown
import Remax.UI.Core.Types qualified as Types
import System.FilePath qualified as FilePath

{- | One drawn row of a conversation under its line. A row that belongs
to a post names its index, so the band can wash it and hang its keys
under it.
-}
data ThreadLine = ThreadLine
  { tlConv :: Conv.ConversationId
  , tlPost :: Maybe Int
  , tlFill :: Maybe Brick.AttrName
  , tlSegs :: [Markdown.Segment]
  }

threadLines :: Types.St -> Int -> Conv.Conversation -> [ThreadLine]
threadLines st width conv =
  let
    cid = Conv.convId conv
    posts = Conv.convPosts conv
    keys = Config.cfgKeys (Types.stConfig st)
    selected = case Types.stBand st of
      Just band | Types.bandConv band == cid -> Just (Types.bandPost band)
      _ -> Nothing
    column = authorColumn posts
    rowsFor (i, post) =
      let
        body = postRows st width column cid i post
        strip =
          if selected == Just i
            then [ThreadLine cid (Just i) Nothing (stripSegs column (postKeys keys (Conv.convVenue conv) post))]
            else []
      in
        body ++ strip
    header = case Conv.convVenue conv of
      Conv.ReviewThread (Just _) _ -> [ThreadLine cid Nothing Nothing (headerSegs conv)]
      _ -> []
  in
    if isResolvedFold st conv
      then [ThreadLine cid Nothing Nothing (resolvedSegs width conv)]
      else header ++ concatMap rowsFor (zip [0 ..] posts)

isResolvedFold :: Types.St -> Conv.Conversation -> Bool
isResolvedFold st conv =
  case Conv.convVenue conv of
    Conv.ReviewThread (Just tid) Store.Resolved -> not (Set.member tid (Types.stUnfolded st))
    _ -> False

resolvedSegs :: Int -> Conv.Conversation -> [Markdown.Segment]
resolvedSegs width conv =
  let
    first = Maybe.listToMaybe (Conv.convPosts conv)
    author = maybe "" (authorName . Conv.postAuthor) first
    line = maybe "" (Markdown.firstLine . Conv.postBody) first
    lead = "\10003 resolved \183 " <> author <> " \183 "
    room = max 8 (width - T.length lead - 11)
  in
    [ Markdown.Segment Attrs.doneMarkAttr "\10003 resolved" Nothing
    , Markdown.Segment Attrs.hintAttr (" \183 " <> author <> " \183 " <> clip room line <> " \183 za opens") Nothing
    ]

clip :: Int -> T.Text -> T.Text
clip room t = if T.length t > room then T.take (max 0 (room - 1)) t <> "\8230" else t

headerSegs :: Conv.Conversation -> [Markdown.Segment]
headerSegs conv =
  let
    posts = Conv.convPosts conv
    count label n = if n == 0 then [] else [Types.tshow n <> " " <> label]
    posted = length (filter isPosted posts)
    drafts = length (filter ((== Conv.Draft) . Conv.postState) posts)
    proposals = length (filter isProposal posts)
    locals = length (filter Conv.isLocalPost posts)
    parts =
      [threadState conv]
        ++ count "on GitHub" posted
        ++ count "draft" drafts
        ++ count "proposal" proposals
        ++ count "local" locals
  in
    [ Markdown.Segment Attrs.replyInlineAttr "\8618 thread" Nothing
    , Markdown.Segment Attrs.hintAttr (" \183 " <> T.intercalate " \183 " parts) Nothing
    ]

isPosted :: Conv.Post -> Bool
isPosted p =
  case Conv.postState p of
    Conv.Posted {} -> True
    _ -> False

isProposal :: Conv.Post -> Bool
isProposal p =
  case Conv.postState p of
    Conv.Proposal _ -> True
    _ -> False

threadState :: Conv.Conversation -> T.Text
threadState conv =
  case Conv.convVenue conv of
    Conv.ReviewThread _ Store.Resolved -> "resolved"
    _ ->
      case reverse (filter (not . Conv.isLocalPost) (Conv.convPosts conv)) of
        (newest : _) ->
          case (Conv.postState newest, Conv.postAuthor newest) of
            (Conv.Draft, _) -> "your reply is pending"
            (Conv.Proposal _, _) -> "awaiting you"
            (_, Conv.Viewer) -> "awaiting them"
            _ -> "awaiting you"
        [] -> "awaiting you"

authorColumn :: [Conv.Post] -> Int
authorColumn posts = max 6 (min columnCap (maximum (0 : map (T.length . authorLabel) posts)))

columnCap :: Int
columnCap = 16

authorLabel :: Conv.Post -> T.Text
authorLabel post =
  case Conv.postState post of
    Conv.Proposal name -> "\10022 " <> Conv.agentNameToText name
    Conv.Local _ -> "\8644 " <> authorName (Conv.postAuthor post)
    _ -> authorName (Conv.postAuthor post)

authorName :: Conv.Author -> T.Text
authorName author =
  case author of
    Conv.Viewer -> "you"
    Conv.Person (Conv.Login l) -> Maybe.fromMaybe l (T.stripSuffix "[bot]" l)
    Conv.Agent (Conv.AgentName n) -> n

data Look = Look
  { lookRail :: T.Text
  , lookSlot :: T.Text
  , lookAuthorAttr :: Brick.AttrName
  , lookRailAttr :: Brick.AttrName
  }

lookOf :: Types.St -> Conv.Post -> Look
lookOf st post =
  case Conv.postState post of
    Conv.Posted _ at _ ->
      Look
        { lookRail = "\9474"
        , lookSlot = ageText (Types.stNow st) at
        , lookAuthorAttr = if Conv.postAuthor post == Conv.Viewer then Attrs.noteInlineAttr else Attrs.replyInlineAttr
        , lookRailAttr = Attrs.hintAttr
        }
    Conv.Draft ->
      Look
        { lookRail = "\9478"
        , lookSlot = "draft"
        , lookAuthorAttr = Attrs.noteInlineAttr
        , lookRailAttr = Attrs.questionMarkAttr
        }
    Conv.Proposal _ ->
      Look
        { lookRail = "\9478"
        , lookSlot = "proposes"
        , lookAuthorAttr = Attrs.guideAttr
        , lookRailAttr = Attrs.guideAttr
        }
    Conv.Local _ ->
      Look
        { lookRail = "\9550"
        , lookSlot = "local"
        , lookAuthorAttr = Attrs.hintAttr
        , lookRailAttr = Attrs.hintAttr
        }

slotWidth :: Int
slotWidth = 8

postRows :: Types.St -> Int -> Int -> Conv.ConversationId -> Int -> Conv.Post -> [ThreadLine]
postRows st width column cid i post =
  let
    look = lookOf st post
    bodyWidth = max 12 (width - column - 3 - slotWidth - 1)
    rendered = Markdown.render bodyWidth (Conv.postBody post)
    rows = if null rendered then [Markdown.plainRow ""] else rendered
    lead j =
      [ Markdown.Segment (lookAuthorAttr look) (T.justifyRight column ' ' (if j == (0 :: Int) then clip column (authorLabel post) else "")) Nothing
      , Markdown.Segment (lookRailAttr look) (" " <> lookRail look <> " ") Nothing
      ]
    trailer j row =
      if j == 0
        then
          [ Markdown.Segment Attrs.hintAttr (T.replicate (max 1 (bodyWidth - T.length (Markdown.renderText row) + 1)) " ") Nothing
          , Markdown.Segment (lookRailAttr look) (T.justifyRight slotWidth ' ' (lookSlot look)) Nothing
          ]
        else []
    line j row = ThreadLine cid (Just i) (Markdown.rowFill row) (lead j ++ Markdown.rowSegments row ++ trailer j row)
    reactionRow =
      case Conv.postReactions post of
        [] -> []
        counts -> [ThreadLine cid (Just i) Nothing (lead 1 ++ reactionSegs counts)]
  in
    zipWith line [0 ..] rows ++ reactionRow

reactionSegs :: Conv.Reactions -> [Markdown.Segment]
reactionSegs counts =
  let
    one c =
      Markdown.Segment
        (if Conv.rcMine c then Attrs.questionMarkAttr else Attrs.hintAttr)
        (Conv.reactionGlyph (Conv.rcReaction c) <> " " <> Types.tshow (Conv.rcCount c))
        Nothing
  in
    List.intersperse (Markdown.Segment Attrs.hintAttr " \183 " Nothing) (map one (filter ((> 0) . Conv.rcCount) counts))

stripSegs :: Int -> T.Text -> [Markdown.Segment]
stripSegs column keys =
  [ Markdown.Segment Attrs.hintAttr (T.replicate (column + 1) " " <> "\9584 " <> keys) Nothing
  ]

{- | The posts the band may land on: everything bound for GitHub, in
order. Local talk with the agent is read, never selected.
-}
bandable :: Conv.Conversation -> [Int]
bandable conv =
  Maybe.mapMaybe (\(i, p) -> if Conv.isLocalPost p then Nothing else Just i) (zip [0 ..] (Conv.convPosts conv))

bandIndexes :: Types.St -> Conv.Conversation -> [Int]
bandIndexes st conv = if isResolvedFold st conv then [] else bandable conv

postKeys :: Config.KeyMode -> Conv.Venue -> Conv.Post -> T.Text
postKeys keys venue post =
  let
    emacs = keys == Config.KeysEmacs
    key plain chord = if emacs then chord else plain
    replyWord = case venue of
      Conv.ReviewThread {} -> "reply"
      _ -> "quote-reply"
    summary = case venue of
      Conv.ReviewSummary {} -> True
      _ -> False
    resolve = case venue of
      Conv.ReviewThread {} -> [key "\9251u resolve" "C-c u resolve"]
      _ -> []
  in
    T.intercalate " \183 " $
      case Conv.postState post of
        Conv.Posted {} ->
          [key ("r " <> replyWord) ("C-c r " <> replyWord), key "? ask" "C-c ? ask"]
            ++ (if Conv.postAuthor post == Conv.Viewer && not summary then [key "e edit" "C-c e edit"] else [])
            ++ (if summary then [] else [key "+ react" "C-c + react"])
            ++ [key "y copy link" "C-c y copy link"]
            ++ resolve
        Conv.Draft -> [key "e edit" "C-c e edit", key "dd discard" "C-c d discard"]
        Conv.Proposal _ -> [key "\9166 take as your reply" "C-c C-c take as your reply", key "dd drop" "C-c d drop"]
        Conv.Local _ -> []

possessive :: Conv.Author -> T.Text
possessive author =
  case author of
    Conv.Viewer -> "your"
    _ -> authorName author <> "'s"

postLabel :: Conv.Venue -> Conv.Post -> T.Text
postLabel venue post =
  let
    who = possessive (Conv.postAuthor post)
    kind = case venue of
      Conv.ReviewThread Nothing _ -> "comment"
      Conv.PullComments -> "comment"
      _ -> "reply"
  in
    case Conv.postState post of
      Conv.Posted {} -> who <> " comment"
      Conv.Draft -> "your pending " <> kind
      Conv.Proposal name -> Conv.agentNameToText name <> "'s proposal"
      Conv.Local _ -> who <> " note"

anchorPlace :: Conv.Anchor -> T.Text
anchorPlace anchor =
  let
    range lo hi = if lo == hi then Types.tshow hi else Types.tshow lo <> "\8211" <> Types.tshow hi
    name = T.pack . FilePath.takeFileName . T.unpack
  in
    case anchor of
      Conv.OnPosition pos -> name (Conv.posPath pos) <> ":" <> range (Maybe.fromMaybe (Conv.posLine pos) (Conv.posStartLine pos)) (Conv.posLine pos)
      Conv.OnOutdated out -> name (Conv.outPath out) <> ":" <> range (Maybe.fromMaybe (Conv.outLine out) (Conv.outStartLine out)) (Conv.outLine out) <> " (outdated)"
      Conv.OnLines lineSpan ->
        let
          numbers = map Conv.slLineNo (List.sortOn Conv.slLineNo (foldr (:) [] (Conv.spanLines lineSpan)))
        in
          name (Conv.spanPath lineSpan) <> ":" <> range (Maybe.fromMaybe 0 (Maybe.listToMaybe numbers)) (Maybe.fromMaybe 0 (Maybe.listToMaybe (reverse numbers)))
      Conv.OnPull -> "PR-level"

lineHint :: Config.KeyMode -> T.Text
lineHint keys =
  case keys of
    Config.KeysEmacs -> "C-n enters the thread \183 C-c g comment \183 C-c ? ask \183 C-c z fold"
    _ -> "j enters the thread \183 @ comment \183 ? ask \183 za fold"

ageText :: Clock.UTCTime -> T.Text -> T.Text
ageText now stamp =
  case ISO8601.iso8601ParseM (T.unpack stamp) of
    Just at -> shortAge (max 0 (floor (Clock.diffUTCTime now at)))
    Nothing -> ""

shortAge :: Int -> T.Text
shortAge secs
  | secs < 60 = Types.tshow secs <> "s"
  | secs < 3600 = Types.tshow (secs `div` 60) <> "m"
  | secs < 86400 = Types.tshow (secs `div` 3600) <> "h"
  | secs < 86400 * 60 = Types.tshow (secs `div` 86400) <> "d"
  | otherwise = Types.tshow (secs `div` (86400 * 30)) <> "mo"
