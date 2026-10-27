{-# LANGUAGE OverloadedStrings #-}

module Remax.Conversation
  ( Login (..)
  , AgentName (..)
  , Author (..)
  , authorText
  , parseAuthor
  , Markdown (..)
  , Side (..)
  , sideText
  , parseSide
  , CommitSha (..)
  , SpanLine (..)
  , LineSpan (..)
  , spanKeys
  , Position (..)
  , Outdated (..)
  , Anchor (..)
  , LocalId (..)
  , WorkId (..)
  , Target (..)
  , targetLocalId
  , Work (..)
  , workBody
  , OwnWork (..)
  , emptyWork
  , Reaction (..)
  , reactionText
  , parseReaction
  , reactionGlyph
  , allReactions
  , ReactionCount (..)
  , Reactions
  , RemoteComment (..)
  , RemoteThread (..)
  , threadAnchor
  , RemotePullComment (..)
  , RemoteReview (..)
  , Remote (..)
  , emptyRemote
  , PostState (..)
  , Post (..)
  , isLocalPost
  , Venue (..)
  , ConversationId (..)
  , conversationIdText
  , parseConversationId
  , Conversation (..)
  , conversations
  , conversationFor
  , comment
  , reply
  , ask
  , edit
  , rewrite
  , discard
  , take
  , newest
  , workOf
  , pendingOf
  ) where

import Prelude hiding (take)

import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.Store qualified as Store

newtype Login = Login {loginToText :: T.Text}
  deriving (Eq, Ord, Show)

newtype AgentName = AgentName {agentNameToText :: T.Text}
  deriving (Eq, Ord, Show)

data Author = Viewer | Person Login | Agent AgentName
  deriving (Eq, Ord, Show)

authorText :: Author -> T.Text
authorText a =
  case a of
    Viewer -> "viewer"
    Person (Login l) -> "person:" <> l
    Agent (AgentName n) -> "agent:" <> n

parseAuthor :: T.Text -> Author
parseAuthor t
  | t == "viewer" = Viewer
  | Just l <- T.stripPrefix "person:" t = Person (Login l)
  | Just n <- T.stripPrefix "agent:" t = Agent (AgentName n)
  | otherwise = Agent (AgentName t)

newtype Markdown = Markdown {markdownText :: T.Text}
  deriving (Eq, Ord, Show)

data Side = BaseSide | HeadSide
  deriving (Eq, Ord, Show)

sideText :: Side -> T.Text
sideText s =
  case s of
    BaseSide -> "base"
    HeadSide -> "head"

parseSide :: T.Text -> Side
parseSide t = if t == "base" then BaseSide else HeadSide

newtype CommitSha = CommitSha {commitShaToText :: T.Text}
  deriving (Eq, Ord, Show)

data SpanLine = SpanLine
  { slKey :: Store.LineKey
  , slLineNo :: Int
  , slText :: T.Text
  }
  deriving (Eq, Ord, Show)

data LineSpan = LineSpan
  { spanPath :: T.Text
  , spanSide :: Side
  , spanLines :: NEL.NonEmpty SpanLine
  , spanAt :: Maybe CommitSha
  }
  deriving (Eq, Ord, Show)

spanKeys :: LineSpan -> [Store.LineKey]
spanKeys = map slKey . NEL.toList . spanLines

data Position = Position
  { posPath :: T.Text
  , posSide :: Side
  , posLine :: Int
  , posStartLine :: Maybe Int
  }
  deriving (Eq, Ord, Show)

data Outdated = Outdated
  { outPath :: T.Text
  , outSide :: Side
  , outLine :: Int
  , outStartLine :: Maybe Int
  , outCommit :: CommitSha
  , outDiffHunk :: T.Text
  }
  deriving (Eq, Ord, Show)

data Anchor
  = OnLines LineSpan
  | OnPosition Position
  | OnOutdated Outdated
  | OnPull
  deriving (Eq, Ord, Show)

newtype LocalId = LocalId {localIdToText :: T.Text}
  deriving (Eq, Ord, Show)

data WorkId
  = StoredWork Int
  | LegacyPost T.Text Int
  deriving (Eq, Ord, Show)

data Target
  = OnThread Store.RemoteThreadId
  | OnComment Store.CommentId
  | NewThread LocalId Anchor
  | OnLinesOnly LocalId LineSpan Store.Verdict
  deriving (Eq, Ord, Show)

targetLocalId :: Target -> Maybe LocalId
targetLocalId t =
  case t of
    NewThread lid _ -> Just lid
    OnLinesOnly lid _ _ -> Just lid
    _ -> Nothing

data Work
  = Proposed AgentName Markdown
  | Pending Author Markdown
  | Aside Author Markdown T.Text
  | Edit Store.CommentId Markdown
  deriving (Eq, Ord, Show)

workBody :: Work -> Markdown
workBody w =
  case w of
    Proposed _ b -> b
    Pending _ b -> b
    Aside _ b _ -> b
    Edit _ b -> b

data OwnWork = OwnWork
  { workItems :: Map.Map Target [(WorkId, Work)]
  , workVerdicts :: Map.Map Store.LineKey Store.Verdict
  , workSubmitted :: Map.Map LocalId Store.RemoteThreadId
  }
  deriving (Eq, Show)

emptyWork :: OwnWork
emptyWork = OwnWork {workItems = Map.empty, workVerdicts = Map.empty, workSubmitted = Map.empty}

data Reaction = ThumbsUp | ThumbsDown | Laugh | Confused | Heart | Hooray | Rocket | Eyes
  deriving (Eq, Ord, Show, Enum, Bounded)

allReactions :: [Reaction]
allReactions = [minBound .. maxBound]

reactionText :: Reaction -> T.Text
reactionText r =
  case r of
    ThumbsUp -> "+1"
    ThumbsDown -> "-1"
    Laugh -> "laugh"
    Confused -> "confused"
    Heart -> "heart"
    Hooray -> "hooray"
    Rocket -> "rocket"
    Eyes -> "eyes"

parseReaction :: T.Text -> Maybe Reaction
parseReaction t = List.find ((== T.toLower t) . reactionText) allReactions

reactionGlyph :: Reaction -> T.Text
reactionGlyph r =
  case r of
    ThumbsUp -> "\128077"
    ThumbsDown -> "\128078"
    Laugh -> "\128516"
    Confused -> "\128533"
    Heart -> "\10084"
    Hooray -> "\127881"
    Rocket -> "\128640"
    Eyes -> "\128064"

data ReactionCount = ReactionCount
  { rcReaction :: Reaction
  , rcCount :: Int
  , rcMine :: Bool
  }
  deriving (Eq, Ord, Show)

type Reactions = [ReactionCount]

data RemoteComment = RemoteComment
  { remoteCommentId :: Store.CommentId
  , remoteCommentAuthor :: Login
  , remoteCommentBody :: Markdown
  , remoteCommentCreatedAt :: T.Text
  , remoteCommentUpdatedAt :: T.Text
  , remoteCommentReview :: Maybe Store.ReviewId
  , remoteCommentUrl :: T.Text
  , remoteCommentReactions :: Reactions
  }
  deriving (Eq, Ord, Show)

data RemoteThread = RemoteThread
  { remoteThreadId :: Store.RemoteThreadId
  , remoteThreadWhere :: Either Outdated Position
  , remoteThreadResolution :: Store.Resolution
  , remoteThreadComments :: NEL.NonEmpty RemoteComment
  }
  deriving (Eq, Show)

threadAnchor :: RemoteThread -> Anchor
threadAnchor t = either OnOutdated OnPosition (remoteThreadWhere t)

data RemotePullComment = RemotePullComment
  { pullCommentId :: Store.CommentId
  , pullCommentAuthor :: Login
  , pullCommentBody :: Markdown
  , pullCommentCreatedAt :: T.Text
  , pullCommentUpdatedAt :: T.Text
  , pullCommentUrl :: T.Text
  , pullCommentReactions :: Reactions
  }
  deriving (Eq, Ord, Show)

data RemoteReview = RemoteReview
  { remoteReviewId :: Store.ReviewId
  , remoteReviewAuthor :: Login
  , remoteReviewState :: T.Text
  , remoteReviewCommit :: T.Text
  , remoteReviewSubmittedAt :: T.Text
  , remoteReviewBody :: Markdown
  , remoteReviewUrl :: T.Text
  }
  deriving (Eq, Ord, Show)

data Remote = Remote
  { remoteViewer :: Maybe Login
  , remoteThreads :: [RemoteThread]
  , remoteComments :: [RemotePullComment]
  , remoteReviews :: [RemoteReview]
  }
  deriving (Eq, Show)

emptyRemote :: Remote
emptyRemote = Remote {remoteViewer = Nothing, remoteThreads = [], remoteComments = [], remoteReviews = []}

data PostState
  = Proposal AgentName
  | Draft
  | Posted Store.CommentId T.Text (Maybe T.Text)
  | Local T.Text
  deriving (Eq, Ord, Show)

data Post = Post
  { postAuthor :: Author
  , postBody :: Markdown
  , postState :: PostState
  , postReactions :: Reactions
  , postWork :: Maybe WorkId
  , postUrl :: Maybe T.Text
  , postAnswers :: Maybe Store.CommentId
  }
  deriving (Eq, Ord, Show)

isLocalPost :: Post -> Bool
isLocalPost p =
  case postState p of
    Local _ -> True
    _ -> False

data Venue
  = ReviewThread (Maybe Store.RemoteThreadId) Store.Resolution
  | PullComments
  | ReviewSummary Store.ReviewId T.Text
  | LocalOnly Store.Verdict
  deriving (Eq, Show)

data ConversationId
  = ThreadConversation Store.RemoteThreadId
  | NoteConversation LocalId
  | PullConversation
  | ReviewConversation Store.ReviewId
  deriving (Eq, Ord, Show)

conversationIdText :: ConversationId -> T.Text
conversationIdText cid =
  case cid of
    ThreadConversation (Store.RemoteThreadId t) -> t
    NoteConversation (LocalId l) -> l
    PullConversation -> "pull"
    ReviewConversation (Store.ReviewId r) -> "review:" <> T.pack (show r)

parseConversationId :: [Conversation] -> T.Text -> Maybe ConversationId
parseConversationId convs t =
  List.find ((== t) . conversationIdText) (map convId convs)

data Conversation = Conversation
  { convId :: ConversationId
  , convAnchor :: Anchor
  , convVenue :: Venue
  , convTarget :: Target
  , convPosts :: [Post]
  }
  deriving (Eq, Show)

conversations :: Remote -> OwnWork -> [Conversation]
conversations remote own =
  let
    viewer = remoteViewer remote
    threads = map (threadConversation viewer own) (remoteThreads remote)
    known = Map.fromList (map (\t -> (remoteThreadId t, ())) (remoteThreads remote))
    unsubmitted (target, items) =
      case target of
        NewThread lid anchor
          | Map.notMember lid (workSubmitted own) || not (Map.member (submittedId lid) known) ->
              Just (newThreadConversation lid anchor items)
        OnLinesOnly lid lineSpan verdict -> Just (noteConversation lid lineSpan verdict items)
        OnThread tid
          | Map.notMember tid known -> Just (orphanThreadConversation tid items)
        _ -> Nothing
    submittedId lid = Maybe.fromMaybe (Store.RemoteThreadId "") (Map.lookup lid (workSubmitted own))
    locals = Maybe.mapMaybe unsubmitted (Map.toList (workItems own))
    pull =
      if null (remoteComments remote) && not (hasPullWork own)
        then []
        else [pullConversation own remote]
    reviews = map (reviewConversation viewer) (remoteReviews remote)
  in
    threads ++ locals ++ pull ++ reviews

conversationFor :: ConversationId -> [Conversation] -> Maybe Conversation
conversationFor cid = List.find ((== cid) . convId)

hasPullWork :: OwnWork -> Bool
hasPullWork own =
  any
    ( \target -> case target of
        NewThread _ OnPull -> True
        OnComment _ -> True
        _ -> False
    )
    (Map.keys (workItems own))

threadConversation :: Maybe Login -> OwnWork -> RemoteThread -> Conversation
threadConversation viewer own t =
  let
    tid = remoteThreadId t
    direct = Map.findWithDefault [] (OnThread tid) (workItems own)
    editOf (_, w) = case w of
      Edit cid b -> Just (cid, b)
      _ -> Nothing
    edits = Map.fromList (Maybe.mapMaybe editOf direct)
    posted = map (remotePost viewer edits) (NEL.toList (remoteThreadComments t))
    carriedFrom (target, items) = case target of
      NewThread lid _ | Map.lookup lid (workSubmitted own) == Just tid -> items
      _ -> []
    carried = foldMap carriedFrom (Map.toList (workItems own))
  in
    Conversation
      { convId = ThreadConversation tid
      , convAnchor = threadAnchor t
      , convVenue = ReviewThread (Just tid) (remoteThreadResolution t)
      , convTarget = OnThread tid
      , convPosts = posted ++ workPosts (List.sortOn fst (direct ++ carried))
      }

orphanThreadConversation :: Store.RemoteThreadId -> [(WorkId, Work)] -> Conversation
orphanThreadConversation tid items =
  Conversation
    { convId = ThreadConversation tid
    , convAnchor = OnPull
    , convVenue = ReviewThread (Just tid) Store.Unresolved
    , convTarget = OnThread tid
    , convPosts = workPosts items
    }

remotePost :: Maybe Login -> Map.Map Store.CommentId Markdown -> RemoteComment -> Post
remotePost viewer edits c =
  let
    cid = remoteCommentId c
    author = if viewer == Just (remoteCommentAuthor c) then Viewer else Person (remoteCommentAuthor c)
    edited =
      if remoteCommentUpdatedAt c /= remoteCommentCreatedAt c
        then Just (remoteCommentUpdatedAt c)
        else Nothing
  in
    Post
      { postAuthor = author
      , postBody = Map.findWithDefault (remoteCommentBody c) cid edits
      , postState = Posted cid (remoteCommentCreatedAt c) edited
      , postReactions = remoteCommentReactions c
      , postWork = Nothing
      , postUrl = Just (remoteCommentUrl c)
      , postAnswers = Nothing
      }

workPosts :: [(WorkId, Work)] -> [Post]
workPosts items =
  let
    post (wid, w) =
      case w of
        Proposed name b -> Just (Post (Agent name) b (Proposal name) [] (Just wid) Nothing Nothing)
        Pending author b -> Just (Post author b Draft [] (Just wid) Nothing Nothing)
        Aside author b at -> Just (Post author b (Local at) [] (Just wid) Nothing Nothing)
        Edit _ _ -> Nothing
    isProposal (_, w) = case w of
      Proposed _ _ -> True
      _ -> False
    (proposals, rest) = List.partition isProposal items
  in
    Maybe.mapMaybe post (rest ++ proposals)

newThreadConversation :: LocalId -> Anchor -> [(WorkId, Work)] -> Conversation
newThreadConversation lid anchor items =
  Conversation
    { convId = NoteConversation lid
    , convAnchor = anchor
    , convVenue = ReviewThread Nothing Store.Unresolved
    , convTarget = NewThread lid anchor
    , convPosts = workPosts items
    }

noteConversation :: LocalId -> LineSpan -> Store.Verdict -> [(WorkId, Work)] -> Conversation
noteConversation lid lineSpan verdict items =
  Conversation
    { convId = NoteConversation lid
    , convAnchor = OnLines lineSpan
    , convVenue = LocalOnly verdict
    , convTarget = OnLinesOnly lid lineSpan verdict
    , convPosts = workPosts items
    }

pullConversation :: OwnWork -> Remote -> Conversation
pullConversation own remote =
  let
    viewer = remoteViewer remote
    posted c =
      let
        author = if viewer == Just (pullCommentAuthor c) then Viewer else Person (pullCommentAuthor c)
        edited =
          if pullCommentUpdatedAt c /= pullCommentCreatedAt c
            then Just (pullCommentUpdatedAt c)
            else Nothing
      in
        Post
          { postAuthor = author
          , postBody = pullCommentBody c
          , postState = Posted (pullCommentId c) (pullCommentCreatedAt c) edited
          , postReactions = pullCommentReactions c
          , postWork = Nothing
          , postUrl = Just (pullCommentUrl c)
          , postAnswers = Nothing
          }
    answering (target, items) =
      case target of
        OnComment cid -> map (\p -> p {postAnswers = Just cid}) (workPosts items)
        NewThread _ OnPull -> workPosts items
        _ -> []
    pullLocal target = case target of
      NewThread l OnPull -> Just l
      _ -> Nothing
    lid = Maybe.fromMaybe (LocalId "pull") (Maybe.listToMaybe (Maybe.mapMaybe pullLocal (Map.keys (workItems own))))
  in
    Conversation
      { convId = PullConversation
      , convAnchor = OnPull
      , convVenue = PullComments
      , convTarget = NewThread lid OnPull
      , convPosts = map posted (remoteComments remote) ++ concatMap answering (Map.toList (workItems own))
      }

reviewConversation :: Maybe Login -> RemoteReview -> Conversation
reviewConversation viewer r =
  Conversation
    { convId = ReviewConversation (remoteReviewId r)
    , convAnchor = OnPull
    , convVenue = ReviewSummary (remoteReviewId r) (remoteReviewState r)
    , convTarget = NewThread (LocalId "pull") OnPull
    , convPosts =
        [ Post
            { postAuthor = if viewer == Just (remoteReviewAuthor r) then Viewer else Person (remoteReviewAuthor r)
            , postBody = remoteReviewBody r
            , postState = Posted (Store.CommentId (Store.reviewIdToInt64 (remoteReviewId r))) (remoteReviewSubmittedAt r) Nothing
            , postReactions = []
            , postWork = Nothing
            , postUrl = Just (remoteReviewUrl r)
            , postAnswers = Nothing
            }
        ]
    }

addWork :: Target -> Work -> OwnWork -> OwnWork
addWork target w own =
  let
    stored (i, _) = case i of
      StoredWork n -> Just n
      LegacyPost _ _ -> Nothing
    wid = StoredWork (1 + maximum (0 : Maybe.mapMaybe stored (concat (Map.elems (workItems own)))))
  in
    own {workItems = Map.insertWith (flip (++)) target [(wid, w)] (workItems own)}

comment :: Anchor -> LocalId -> Markdown -> OwnWork -> OwnWork
comment anchor lid = addWork (NewThread lid anchor) . Pending Viewer

reply :: Target -> Markdown -> OwnWork -> OwnWork
reply target = addWork target . Pending Viewer

ask :: Target -> Author -> T.Text -> Markdown -> OwnWork -> OwnWork
ask target author at body = addWork target (Aside author body at)

edit :: WorkId -> Markdown -> OwnWork -> OwnWork
edit wid body own =
  let
    rewriteItem (i, w) =
      if i == wid
        then
          ( i
          , case w of
              Pending author _ -> Pending author body
              Proposed name _ -> Proposed name body
              Aside author _ at -> Aside author body at
              Edit cid _ -> Edit cid body
          )
        else (i, w)
  in
    own {workItems = Map.map (map rewriteItem) (workItems own)}

rewrite :: Target -> Store.CommentId -> Markdown -> OwnWork -> OwnWork
rewrite target cid body = addWork target (Edit cid body)

discard :: WorkId -> OwnWork -> OwnWork
discard wid own =
  own {workItems = Map.filter (not . null) (Map.map (filter ((/= wid) . fst)) (workItems own))}

take :: WorkId -> OwnWork -> OwnWork
take wid own =
  let
    taken (i, w) =
      case w of
        Proposed _ b | i == wid -> (i, Pending Viewer b)
        _ -> (i, w)
  in
    own {workItems = Map.map (map taken) (workItems own)}

newest :: Target -> OwnWork -> Maybe WorkId
newest target own =
  case reverse (Map.findWithDefault [] target (workItems own)) of
    ((wid, _) : _) -> Just wid
    [] -> Nothing

workOf :: [Conversation] -> [(WorkId, Post)]
workOf convs =
  let
    ownPost p = fmap (\wid -> (wid, p)) (postWork p)
  in
    Maybe.mapMaybe ownPost (concatMap convPosts convs)

pendingOf :: OwnWork -> [(Target, WorkId, Work)]
pendingOf own =
  let
    pendingIn (target, items) =
      Maybe.mapMaybe
        ( \(wid, w) -> case w of
            Pending _ _ -> Just (target, wid, w)
            _ -> Nothing
        )
        items
  in
    foldMap pendingIn (Map.toList (workItems own))
