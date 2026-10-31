{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Core.Threads
  ( ThreadLine (..)
  , threadLines
  , postLines
  , rowLines
  , rowLineCount
  , reactionSegs
  , detailsFold
  , bandable
  , bandIndexes
  , postKeys
  , postLabel
  , possessive
  , threadState
  , awaitingYou
  , awaitsYou
  , proposedComments
  , isProposal
  , isPosted
  , anchorPlace
  , takenAs
  , authorName
  , nameColumn
  , nameCell
  , authorInk
  , railGlyph
  , ageText
  , shortAge
  , isFolded
  , foldedMark
  , onGitHub
  , lineHint
  , noteHint
  , outdatedHint
  ) where

import Brick qualified as Brick
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Clock qualified as Clock
import Data.Time.Format.ISO8601 qualified as ISO8601
import Remax.Config qualified as Config
import Remax.Conversation qualified as Conv
import Remax.PullPoll qualified as PullPoll
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

{- | Every conversation under one display row, the second and later
ones labelled with the lines they cover.
-}
rowLines :: Types.St -> Int -> [Conv.Conversation] -> [ThreadLine]
rowLines st width convs =
  let
    labelled (i, conv) =
      let
        label =
          if i == (0 :: Int)
            then []
            else [ThreadLine (Conv.convId conv) Nothing Nothing [Markdown.Segment Attrs.hintAttr ("\9590 " <> rangeOf (Conv.convAnchor conv)) Nothing]]
      in
        label ++ threadLines st width conv
  in
    concatMap labelled (zip [0 ..] convs)

rowLineCount :: Types.St -> Int -> [Conv.Conversation] -> Int
rowLineCount st width = length . rowLines st width

rangeOf :: Conv.Anchor -> T.Text
rangeOf anchor = snd (T.breakOnEnd ":" (anchorPlace anchor))

threadLines :: Types.St -> Int -> Conv.Conversation -> [ThreadLine]
threadLines st width conv =
  let
    cid = Conv.convId conv
    header = case Conv.convVenue conv of
      Conv.ReviewThread (Just _) _ -> [ThreadLine cid Nothing Nothing (headerSegs st conv)]
      _ -> []
    outdated = case Map.lookup cid (Types.stOutdated st) of
      Just _ -> outdatedRows cid (Conv.convAnchor conv)
      Nothing -> []
  in
    if isFolded st conv
      then [ThreadLine cid Nothing Nothing (foldedSegs st width conv)]
      else outdated ++ header ++ postLines st width conv

postLines :: Types.St -> Int -> Conv.Conversation -> [ThreadLine]
postLines st width conv =
  let
    cid = Conv.convId conv
    posts = Conv.convPosts conv
    bare = case Conv.convVenue conv of
      Conv.LocalOnly _ -> True
      _ -> False
    keys = Config.cfgKeys (Types.stConfig st)
    selected = case Types.currentBand st of
      Just band | Types.bandConv band == cid -> Just (Types.bandPost band)
      _ -> Nothing
    column = authorColumn posts
    rowsFor (i, post) =
      let
        strip =
          if selected == Just i
            then [ThreadLine cid (Just i) Nothing (stripSegs column (lookRailAttr (lookOf st bare post)) (postKeys keys (Types.flagRule st) (Conv.convVenue conv) post))]
            else []
        body = postRows st width column cid bare (length strip) i post
      in
        body ++ strip
  in
    concatMap rowsFor (zip [0 ..] posts)

{- | What a conversation whose lines left the file remembers of them:
the first three, numbered as they were, then how many more.
-}
outdatedRows :: Conv.ConversationId -> Conv.Anchor -> [ThreadLine]
outdatedRows cid anchor =
  let
    line t = ThreadLine cid Nothing Nothing [Markdown.Segment Attrs.hintAttr t Nothing]
    quoted ls =
      let
        shown = take 3 ls
        more = length ls - length shown
      in
        map (\(n, t) -> line ("\9474  " <> Types.tshow n <> " " <> t)) shown
          ++ (if more > 0 then [line ("\9474      \8943 " <> Types.tshow more <> " more lines")] else [])
    header range = line ("\9581 outdated \183 was " <> range <> ", which left the file")
  in
    case anchor of
      Conv.OnLines lineSpan ->
        header (rangeOf anchor)
          : quoted (map (\l -> (Conv.slLineNo l, T.drop 1 (Conv.slText l))) (NEL.toList (Conv.spanLines lineSpan)))
      Conv.OnOutdated out ->
        let
          lo = Maybe.fromMaybe (Conv.outLine out) (Conv.outStartLine out)
          hunk = filter (not . T.isPrefixOf "@@") (T.lines (Conv.outDiffHunk out))
          numbered = zip [lo ..] (reverse (take (Conv.outLine out - lo + 1) (reverse hunk)))
        in
          header (T.replace " (outdated)" "" (rangeOf anchor)) : quoted (map (\(n, t) -> (n, T.drop 1 t)) numbered)
      _ -> []

isFolded :: Types.St -> Conv.Conversation -> Bool
isFolded st conv =
  case Conv.convVenue conv of
    Conv.ReviewThread (Just tid) resolution -> foldsByDefault resolution /= Set.member tid (Types.stFoldFlipped st)
    _ -> False

foldsByDefault :: Store.Resolution -> Bool
foldsByDefault resolution =
  case resolution of
    Store.Resolved -> True
    Store.Unresolved -> False

foldedSegs :: Types.St -> Int -> Conv.Conversation -> [Markdown.Segment]
foldedSegs st width conv =
  let
    first = Maybe.listToMaybe (Conv.convPosts conv)
    author = maybe "" (authorName . Conv.postAuthor) first
    line = maybe "" (Markdown.firstLine . Conv.postBody) first
    (mark, attr) = foldedMark st conv
    lead = mark <> " \183 " <> author <> " \183 "
    room = max 8 (width - T.length lead - 11)
  in
    [ Markdown.Segment attr mark Nothing
    , Markdown.Segment Attrs.hintAttr (" \183 " <> author <> " \183 " <> clip room line <> " \183 za opens") Nothing
    ]

{- | How a folded thread names itself: a resolved one by its check, an
open one by whose turn it is.
-}
foldedMark :: Types.St -> Conv.Conversation -> (T.Text, Brick.AttrName)
foldedMark st conv =
  case Conv.convVenue conv of
    Conv.ReviewThread _ Store.Resolved -> ("\10003 resolved", Attrs.doneMarkAttr)
    _ -> ("\9656 " <> threadState st conv, Attrs.replyInlineAttr)

clip :: Int -> T.Text -> T.Text
clip room t = if T.length t > room then T.take (max 0 (room - 1)) t <> "\8230" else t

headerSegs :: Types.St -> Conv.Conversation -> [Markdown.Segment]
headerSegs st conv =
  let
    posts = Conv.convPosts conv
    count label n = if n == 0 then [] else [Types.tshow n <> " " <> label]
    posted = length (filter isPosted posts)
    drafts = length (filter ((== Conv.Draft) . Conv.postState) posts)
    proposals = length (filter isProposal posts)
    locals = length (filter Conv.isLocalPost posts)
    parts =
      [threadState st conv]
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
    Conv.Proposal _ _ -> True
    _ -> False

data Turn = Settled | ReplyPending | YourTurn | TheirTurn | AuthorsTurn
  deriving (Eq, Show)

data Seat = PullAuthor | Bystander
  deriving (Eq)

seatOf :: Types.St -> Seat
seatOf st =
  let
    authorAndViewer = do
      snap <- Types.ppSnapshot (Types.stPullPoll st)
      viewer <- Conv.remoteViewer (PullPoll.snapshotRemote snap)
      pure (Conv.loginToText viewer, PullPoll.pullHeadAuthor (PullPoll.snapshotHead snap))
  in
    case authorAndViewer of
      Just (viewer, author) | viewer /= author -> Bystander
      _ -> PullAuthor

threadTurn :: Seat -> Conv.Conversation -> Turn
threadTurn seat conv =
  let
    joined = any (\p -> Conv.postAuthor p == Conv.Viewer && not (Conv.isLocalPost p)) (Conv.convPosts conv)
    owed = case (seat, joined) of
      (Bystander, False) -> AuthorsTurn
      _ -> YourTurn
  in
    case Conv.convVenue conv of
      Conv.ReviewThread _ Store.Resolved -> Settled
      _ ->
        case Conv.newestPost (filter (not . Conv.isLocalPost) (Conv.convPosts conv)) of
          Just newest ->
            case (Conv.postState newest, Conv.postAuthor newest) of
              (Conv.Draft, _) -> ReplyPending
              (Conv.Proposal _ _, _) -> YourTurn
              (_, Conv.Viewer) -> TheirTurn
              _ -> owed
          Nothing -> owed

takenAs :: Conv.Target -> T.Text -> T.Text
takenAs target author =
  case target of
    Conv.NewThread _ anchor -> "comment on " <> anchorPlace anchor
    _ -> "reply to " <> author

threadState :: Types.St -> Conv.Conversation -> T.Text
threadState st conv =
  case threadTurn (seatOf st) conv of
    Settled -> "resolved"
    ReplyPending -> case Conv.convVenue conv of
      Conv.ReviewThread Nothing _ -> "your comment is pending"
      _ -> "your reply is pending"
    YourTurn -> "awaiting you"
    TheirTurn -> "awaiting them"
    AuthorsTurn -> "awaiting the author"

awaitingYou :: Types.St -> Int
awaitingYou st =
  length (filter (\c -> onGitHub c && awaitsYou st c) (Types.stConversations st))

onGitHub :: Conv.Conversation -> Bool
onGitHub conv =
  case Conv.convVenue conv of
    Conv.ReviewThread (Just _) _ -> True
    _ -> False

proposedComments :: Types.St -> Int
proposedComments st =
  length (filter (\c -> not (onGitHub c) && any isProposal (Conv.convPosts c)) (Types.stConversations st))

awaitsYou :: Types.St -> Conv.Conversation -> Bool
awaitsYou st conv =
  case Conv.convVenue conv of
    Conv.ReviewThread {} -> threadTurn (seatOf st) conv == YourTurn
    _ -> False

authorColumn :: [Conv.Post] -> Int
authorColumn = nameColumn . map authorLabel

nameColumn :: [T.Text] -> Int
nameColumn names = max 6 (min columnCap (maximum (0 : map T.length names)))

nameCell :: Int -> T.Text -> T.Text
nameCell column name = T.justifyRight column ' ' (clip column name)

columnCap :: Int
columnCap = 16

authorLabel :: Conv.Post -> T.Text
authorLabel post =
  case Conv.postState post of
    Conv.Proposal name _ -> "\10022 " <> Conv.agentNameToText name
    Conv.Local _ -> "\8644 " <> authorName (Conv.postAuthor post)
    Conv.Posted {} | Conv.postAuthor post == Conv.Viewer -> "\8599 " <> authorName Conv.Viewer
    _ -> authorName (Conv.postAuthor post)

bareLabel :: Conv.Post -> T.Text
bareLabel = authorName . Conv.postAuthor

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
  , lookSlotAttr :: Brick.AttrName
  }

lookOf :: Types.St -> Bool -> Conv.Post -> Look
lookOf st bare post =
  let
    ink = authorInk (Conv.postAuthor post)
  in
    case Conv.postState post of
      Conv.Posted _ at _ ->
        Look
          { lookRail = "\9474"
          , lookSlot = ageText (Types.stNow st) at
          , lookAuthorAttr = ink
          , lookRailAttr = ink
          , lookSlotAttr = Attrs.hintAttr
          }
      Conv.Draft ->
        Look
          { lookRail = "\9478"
          , lookSlot = "draft"
          , lookAuthorAttr = Attrs.noteInlineAttr
          , lookRailAttr = Attrs.questionMarkAttr
          , lookSlotAttr = Attrs.questionMarkAttr
          }
      Conv.Proposal _ _ ->
        Look
          { lookRail = "\9478"
          , lookSlot = "proposes"
          , lookAuthorAttr = Attrs.guideAttr
          , lookRailAttr = Attrs.guideAttr
          , lookSlotAttr = Attrs.guideAttr
          }
      Conv.Local at ->
        Look
          { lookRail = "\9550"
          , lookSlot = if bare then ageText (Types.stNow st) at else "local"
          , lookAuthorAttr = ink
          , lookRailAttr = ink
          , lookSlotAttr = Attrs.hintAttr
          }

authorInk :: Conv.Author -> Brick.AttrName
authorInk author =
  case author of
    Conv.Viewer -> Attrs.noteInlineAttr
    Conv.Agent _ -> Attrs.agentInlineAttr
    Conv.Person _ -> Attrs.replyInlineAttr

railGlyph :: T.Text -> Int -> Int -> T.Text
railGlyph rail height j
  | height <= 1 = "\9472"
  | j == 0 = "\9581"
  | otherwise = rail

slotWidth :: Int
slotWidth = 8

detailsFold :: Types.St -> Conv.ConversationId -> Int -> Markdown.Fold
detailsFold st cid i =
  if Set.member (cid, i) (Types.stOpenDetails st) then Markdown.Unfolded else Markdown.Folded

postRows :: Types.St -> Int -> Int -> Conv.ConversationId -> Bool -> Int -> Int -> Conv.Post -> [ThreadLine]
postRows st width column cid bare following i post =
  let
    look = lookOf st bare post
    label = if bare then bareLabel post else authorLabel post
    bodyWidth = max 12 (width - column - 3 - slotWidth - 1)
    rendered = Markdown.render (detailsFold st cid i) bodyWidth (Conv.postBody post)
    rows = if null rendered then [Markdown.plainRow ""] else rendered
    height = length rows + length reactionRow + following
    lead j =
      [ Markdown.Segment (lookAuthorAttr look) (nameCell column (if j == (0 :: Int) then label else "")) Nothing
      , Markdown.Segment (lookRailAttr look) (" " <> railGlyph (lookRail look) height j <> " ") Nothing
      ]
    trailer j row =
      if j == 0
        then
          [ Markdown.Segment Attrs.hintAttr (T.replicate (max 1 (bodyWidth - T.length (Markdown.renderText row) + 1)) " ") Nothing
          , Markdown.Segment (lookSlotAttr look) (T.justifyRight slotWidth ' ' (lookSlot look)) Nothing
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

stripSegs :: Int -> Brick.AttrName -> T.Text -> [Markdown.Segment]
stripSegs column railAttr keys =
  [ Markdown.Segment railAttr (T.replicate (column + 1) " " <> "\9584") Nothing
  , Markdown.Segment Attrs.hintAttr (" " <> keys) Nothing
  ]

{- | The posts the band may land on: everything bound for GitHub, in
order. Local talk with the agent is read, never selected.
-}
bandable :: Conv.Conversation -> [Int]
bandable conv =
  Maybe.mapMaybe (\(i, p) -> if Conv.isLocalPost p then Nothing else Just i) (zip [0 ..] (Conv.convPosts conv))

bandIndexes :: Types.St -> Conv.Conversation -> [Int]
bandIndexes st conv =
  case Conv.convVenue conv of
    Conv.LocalOnly _ -> []
    _ -> if isFolded st conv then [] else bandable conv

postKeys :: Config.KeyMode -> Types.FlagRule -> Conv.Venue -> Conv.Post -> T.Text
postKeys keys rule venue post =
  let
    flag = case rule of
      Types.FlagsAllowed -> [key "! flag" "C-c ! flag"]
      Types.FlagsRefused -> []
    emacs = keys == Config.KeysEmacs
    key plain chord = if emacs then chord else plain
    replyWord = case venue of
      Conv.ReviewThread {} -> "reply"
      _ -> "quote-reply"
    summary = case venue of
      Conv.ReviewSummary {} -> True
      _ -> False
    takenWord = case venue of
      Conv.ReviewThread Nothing _ -> "comment"
      _ -> "reply"
    resolve = case venue of
      Conv.ReviewThread _ Store.Resolved -> [key "\9251u unresolve" "C-c u unresolve"]
      Conv.ReviewThread {} -> [key "\9251u resolve" "C-c u resolve"]
      _ -> []
  in
    T.intercalate " \183 " $
      case Conv.postState post of
        Conv.Posted {} ->
          [key ("r " <> replyWord) ("C-c r " <> replyWord), key "? ask" "C-c ? ask"]
            ++ flag
            ++ (if Conv.postAuthor post == Conv.Viewer && not summary then [key "e edit" "C-c e edit"] else [])
            ++ (if summary then [] else [key "+ react" "C-c + react"])
            ++ [key "y copy link" "C-c y copy link"]
            ++ resolve
        Conv.Draft -> [key "e edit" "C-c e edit", key "dd discard" "C-c d discard"]
        Conv.Proposal _ _ -> [key ("\9166 take as your " <> takenWord) ("C-c C-c take as your " <> takenWord), key "dd drop" "C-c d drop"]
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
      Conv.Proposal name _ -> Conv.agentNameToText name <> "'s proposal"
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

lineHint :: Config.KeyMode -> Types.FlagRule -> T.Text
lineHint keys rule =
  let
    flag chord = case rule of
      Types.FlagsAllowed -> chord <> " flag \183 "
      Types.FlagsRefused -> ""
  in
    case keys of
      Config.KeysEmacs -> "C-n enters the thread \183 C-c g comment \183 C-c ? ask \183 " <> flag "C-c !" <> "C-c z fold"
      _ -> "j enters the thread \183 @ comment \183 ? ask \183 " <> flag "!" <> "za fold"

noteHint :: Config.KeyMode -> T.Text
noteHint keys =
  case keys of
    Config.KeysEmacs -> "C-c ? reply to the agent \183 C-c \9166 accept"
    Config.KeysVi -> "\9251q reply to the agent \183 \9251\9166 accept"
    Config.KeysHelix -> "? reply to the agent \183 \9166 accept"

outdatedHint :: Config.KeyMode -> Int -> Int -> T.Text
outdatedHint keys line n =
  let
    reply = case keys of
      Config.KeysEmacs -> "C-c ? reply to the agent \183 C-c \9166 accept retires it with the hunk"
      Config.KeysVi -> "\9251q reply to the agent \183 \9251\9166 accept retires it with the hunk"
      Config.KeysHelix -> "? reply to the agent \183 \9166 accept retires it with the hunk"
  in
    Types.tshow line <> " \183 " <> Types.tshow n <> " outdated note" <> (if n == 1 then "" else "s") <> "   " <> reply

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
