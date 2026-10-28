{-# LANGUAGE OverloadedStrings #-}

module Remax.PullPoll
  ( PollCache
  , emptyPollCache
  , pollCachePull
  , Snapshot (..)
  , PullHead (..)
  , PullState (..)
  , Draftness (..)
  , ConversationComment (..)
  , ReviewSummary (..)
  , PullCommit (..)
  , ForcePush (..)
  , CheckRow (..)
  , CheckResult (..)
  , Polled (..)
  , Freshness (..)
  , pollPull
  , snapshotMeta
  ) where

import Control.Concurrent qualified as Concurrent
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Data.Int qualified as DataInt
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.Conversation qualified as Conv
import Remax.Git qualified as Git
import Remax.GitHub qualified as GitHub
import Remax.Store qualified as Store

data PullState = PullOpen | PullClosed | PullMerged
  deriving (Eq, Show)

data Draftness = ReadyForReview | Draft
  deriving (Eq, Show)

data PullHead = PullHead
  { pullHeadSha :: T.Text
  , pullHeadTitle :: T.Text
  , pullHeadState :: PullState
  , pullHeadBranch :: T.Text
  , pullHeadBaseBranch :: T.Text
  , pullHeadAuthor :: T.Text
  , pullHeadBody :: T.Text
  , pullHeadDraft :: Draftness
  }
  deriving (Eq, Show)

data ConversationComment = ConversationComment
  { conversationId :: Store.CommentId
  , conversationAuthor :: T.Text
  , conversationCreatedAt :: T.Text
  , conversationUpdatedAt :: T.Text
  , conversationBody :: T.Text
  , conversationUrl :: T.Text
  , conversationReactions :: Conv.Reactions
  }
  deriving (Eq, Show)

data ReviewSummary = ReviewSummary
  { reviewSummaryId :: Store.ReviewId
  , reviewSummaryAuthor :: T.Text
  , reviewSummaryState :: T.Text
  , reviewSummaryCommit :: T.Text
  , reviewSummarySubmittedAt :: T.Text
  , reviewSummaryBody :: T.Text
  , reviewSummaryUrl :: T.Text
  }
  deriving (Eq, Show)

data PullCommit = PullCommit
  { pullCommitSha :: T.Text
  , pullCommitSubject :: T.Text
  , pullCommitDate :: T.Text
  }
  deriving (Eq, Show)

data ForcePush = ForcePush
  { forcePushAt :: T.Text
  , forcePushBefore :: T.Text
  , forcePushAfter :: T.Text
  }
  deriving (Eq, Show)

data CheckResult = CheckPassed | CheckFailed | CheckPending | CheckSkipped
  deriving (Eq, Show)

data CheckRow = CheckRow
  { checkName :: T.Text
  , checkResult :: CheckResult
  }
  deriving (Eq, Show)

data Snapshot = Snapshot
  { snapshotPull :: Int
  , snapshotHead :: PullHead
  , snapshotChecks :: GitHub.Checks
  , snapshotCheckRows :: [CheckRow]
  , snapshotMergeable :: T.Text
  , snapshotLabels :: [T.Text]
  , snapshotConversation :: [ConversationComment]
  , snapshotReviews :: [ReviewSummary]
  , snapshotCommits :: [PullCommit]
  , snapshotForcePushes :: [ForcePush]
  , snapshotRemote :: Conv.Remote
  }
  deriving (Eq, Show)

snapshotMeta :: Snapshot -> GitHub.PullMeta
snapshotMeta snap =
  let
    h = snapshotHead snap
  in
    GitHub.PullMeta
      { GitHub.metaNumber = snapshotPull snap
      , GitHub.metaTitle = pullHeadTitle h
      , GitHub.metaState = pullStateText (pullHeadState h)
      , GitHub.metaHeadBranch = pullHeadBranch h
      , GitHub.metaBaseBranch = pullHeadBaseBranch h
      , GitHub.metaAuthor = pullHeadAuthor h
      , GitHub.metaChecks = snapshotChecks snap
      }

pullStateText :: PullState -> T.Text
pullStateText s =
  case s of
    PullOpen -> "OPEN"
    PullClosed -> "CLOSED"
    PullMerged -> "MERGED"

data ReviewComment = ReviewComment
  { reviewCommentId :: Store.CommentId
  , reviewCommentAuthor :: T.Text
  , reviewCommentReplyTo :: Maybe Store.CommentId
  , reviewCommentPath :: T.Text
  , reviewCommentLine :: Maybe Int
  , reviewCommentSide :: Conv.Side
  , reviewCommentStartLine :: Maybe Int
  , reviewCommentCreatedAt :: T.Text
  , reviewCommentUpdatedAt :: T.Text
  , reviewCommentBody :: T.Text
  , reviewCommentUrl :: T.Text
  , reviewCommentReview :: Maybe Store.ReviewId
  , reviewCommentOriginalLine :: Maybe Int
  , reviewCommentOriginalStartLine :: Maybe Int
  , reviewCommentOriginalCommit :: T.Text
  , reviewCommentDiffHunk :: T.Text
  , reviewCommentReactions :: Conv.Reactions
  }
  deriving (Eq, Show)

data ThreadRow = ThreadRow
  { threadRowId :: Store.RemoteThreadId
  , threadRowResolution :: Store.Resolution
  , threadRowRoot :: Store.CommentId
  }
  deriving (Eq, Show)

data GraphAnswer = GraphAnswer
  { graphRaw :: T.Text
  , graphThreads :: [ThreadRow]
  , graphForcePushes :: [ForcePush]
  , graphChecks :: GitHub.Checks
  , graphCheckRows :: [CheckRow]
  , graphMergeable :: T.Text
  , graphLabels :: [T.Text]
  , graphReactions :: Map.Map Store.CommentId Conv.Reactions
  }

data Cached a = Cached
  { cachedEtag :: Maybe T.Text
  , cachedValue :: a
  }

data PollCache = PollCache
  { pollCachePull :: Int
  , cacheViewer :: Maybe Conv.Login
  , cacheHead :: Maybe (Cached PullHead)
  , cacheReviewComments :: Maybe [Cached [ReviewComment]]
  , cacheConversation :: Maybe [Cached [ConversationComment]]
  , cacheReviews :: Maybe [Cached [ReviewSummary]]
  , cacheCommits :: Maybe [Cached [PullCommit]]
  , cacheGraph :: Maybe GraphAnswer
  }

emptyPollCache :: Int -> PollCache
emptyPollCache n =
  PollCache
    { pollCachePull = n
    , cacheViewer = Nothing
    , cacheHead = Nothing
    , cacheReviewComments = Nothing
    , cacheConversation = Nothing
    , cacheReviews = Nothing
    , cacheCommits = Nothing
    , cacheGraph = Nothing
    }

data Freshness = Unchanged | Changed
  deriving (Eq, Show)

instance Semigroup Freshness where
  Unchanged <> Unchanged = Unchanged
  _ <> _ = Changed

data Polled = Polled
  { polledCache :: PollCache
  , polledSnapshot :: Snapshot
  , polledFreshness :: Freshness
  , polledHead :: GitHub.RefsMove
  }

pollPull :: FilePath -> Git.PullRequest -> PollCache -> IO (Either GitHub.GhFailure Polled)
pollPull root pr cache = do
  fetched <- fetchAll root pr cache
  case fetched of
    Left failure -> pure (Left failure)
    Right (freshness, fresh) ->
      case freshSnapshot pr fresh of
        Nothing -> pure (Left (GitHub.GhFailed ("unexpected answer for pull request #" <> T.pack (show (Git.prNumber pr)))))
        Just snap -> do
          moved <-
            GitHub.refreshPullRefs root (Git.prRepo pr) (Git.prNumber pr) (pullHeadBaseBranch (snapshotHead snap)) (pullHeadSha (snapshotHead snap))
          case moved of
            Left failure -> pure (Left failure)
            Right refs ->
              pure
                ( Right
                    Polled
                      { polledCache = fresh
                      , polledSnapshot = snap
                      , polledFreshness = freshness
                      , polledHead = refs
                      }
                )

freshSnapshot :: Git.PullRequest -> PollCache -> Maybe Snapshot
freshSnapshot pr cache = do
  h <- fmap cachedValue (cacheHead cache)
  graph <- cacheGraph cache
  Just
    Snapshot
      { snapshotPull = Git.prNumber pr
      , snapshotHead = h
      , snapshotChecks = graphChecks graph
      , snapshotCheckRows = graphCheckRows graph
      , snapshotMergeable = graphMergeable graph
      , snapshotLabels = graphLabels graph
      , snapshotConversation = pagedRows (cacheConversation cache)
      , snapshotReviews = pagedRows (cacheReviews cache)
      , snapshotCommits = pagedRows (cacheCommits cache)
      , snapshotForcePushes = graphForcePushes graph
      , snapshotRemote = freshRemote cache
      }

freshRemote :: PollCache -> Conv.Remote
freshRemote cache =
  let
    reactions = maybe Map.empty graphReactions (cacheGraph cache)
    withViewer cid rest = Map.findWithDefault rest cid reactions
    thread t = t {Conv.remoteThreadComments = fmap comment (Conv.remoteThreadComments t)}
    comment c = c {Conv.remoteCommentReactions = withViewer (Conv.remoteCommentId c) (Conv.remoteCommentReactions c)}
    pullComment c = c {Conv.pullCommentReactions = withViewer (Conv.pullCommentId c) (Conv.pullCommentReactions c)}
  in
    Conv.Remote
      { Conv.remoteViewer = cacheViewer cache
      , Conv.remoteThreads = map thread (remoteThreads (pagedRows (cacheReviewComments cache)) (maybe [] graphThreads (cacheGraph cache)))
      , Conv.remoteComments = map pullComment (map remotePullComment (pagedRows (cacheConversation cache)))
      , Conv.remoteReviews = map remoteReview (pagedRows (cacheReviews cache))
      }

remoteThreads :: [ReviewComment] -> [ThreadRow] -> [Conv.RemoteThread]
remoteThreads comments threads =
  let
    rootOf c = Maybe.fromMaybe (reviewCommentId c) (reviewCommentReplyTo c)
    byRoot = Map.fromListWith (flip (++)) (map (\c -> (rootOf c, [c])) comments)
    thread t = do
      cs <- Map.lookup (threadRowRoot t) byRoot
      let
        (firsts, replies) = List.partition (Maybe.isNothing . reviewCommentReplyTo) cs
      first <- Maybe.listToMaybe firsts
      Just
        Conv.RemoteThread
          { Conv.remoteThreadId = threadRowId t
          , Conv.remoteThreadWhere = commentPlace first
          , Conv.remoteThreadResolution = threadRowResolution t
          , Conv.remoteThreadComments = remoteComment first NEL.:| map remoteComment (List.sortOn reviewCommentCreatedAt replies)
          }
  in
    Maybe.mapMaybe thread threads

commentPlace :: ReviewComment -> Either Conv.Outdated Conv.Position
commentPlace c =
  case reviewCommentLine c of
    Just line ->
      Right
        Conv.Position
          { Conv.posPath = reviewCommentPath c
          , Conv.posSide = reviewCommentSide c
          , Conv.posLine = line
          , Conv.posStartLine = reviewCommentStartLine c
          }
    Nothing ->
      Left
        Conv.Outdated
          { Conv.outPath = reviewCommentPath c
          , Conv.outSide = reviewCommentSide c
          , Conv.outLine = Maybe.fromMaybe 0 (reviewCommentOriginalLine c)
          , Conv.outStartLine = reviewCommentOriginalStartLine c
          , Conv.outCommit = Conv.CommitSha (reviewCommentOriginalCommit c)
          , Conv.outDiffHunk = reviewCommentDiffHunk c
          }

remoteComment :: ReviewComment -> Conv.RemoteComment
remoteComment c =
  Conv.RemoteComment
    { Conv.remoteCommentId = reviewCommentId c
    , Conv.remoteCommentAuthor = Conv.Login (reviewCommentAuthor c)
    , Conv.remoteCommentBody = Conv.Markdown (reviewCommentBody c)
    , Conv.remoteCommentCreatedAt = reviewCommentCreatedAt c
    , Conv.remoteCommentUpdatedAt = reviewCommentUpdatedAt c
    , Conv.remoteCommentReview = reviewCommentReview c
    , Conv.remoteCommentUrl = reviewCommentUrl c
    , Conv.remoteCommentReactions = reviewCommentReactions c
    , Conv.remoteCommentDiffHunk = reviewCommentDiffHunk c
    }

remotePullComment :: ConversationComment -> Conv.RemotePullComment
remotePullComment c =
  Conv.RemotePullComment
    { Conv.pullCommentId = conversationId c
    , Conv.pullCommentAuthor = Conv.Login (conversationAuthor c)
    , Conv.pullCommentBody = Conv.Markdown (conversationBody c)
    , Conv.pullCommentCreatedAt = conversationCreatedAt c
    , Conv.pullCommentUpdatedAt = conversationUpdatedAt c
    , Conv.pullCommentUrl = conversationUrl c
    , Conv.pullCommentReactions = conversationReactions c
    }

remoteReview :: ReviewSummary -> Conv.RemoteReview
remoteReview r =
  Conv.RemoteReview
    { Conv.remoteReviewId = reviewSummaryId r
    , Conv.remoteReviewAuthor = Conv.Login (reviewSummaryAuthor r)
    , Conv.remoteReviewState = reviewSummaryState r
    , Conv.remoteReviewCommit = reviewSummaryCommit r
    , Conv.remoteReviewSubmittedAt = reviewSummarySubmittedAt r
    , Conv.remoteReviewBody = Conv.Markdown (reviewSummaryBody r)
    , Conv.remoteReviewUrl = reviewSummaryUrl r
    }

fetchAll :: FilePath -> Git.PullRequest -> PollCache -> IO (Either GitHub.GhFailure (Freshness, PollCache))
fetchAll root pr cache = do
  let
    repo = Git.prRepo pr
    n = T.pack (show (Git.prNumber pr))
    pulls = "repos/" <> repo <> "/pulls/" <> n
    issue = "repos/" <> repo <> "/issues/" <> n
  awaitViewer <- inBackground (viewerOnce root (cacheViewer cache))
  awaitHeads <- inBackground (fetchCached root pulls headJq parseHead (cacheHead cache))
  awaitComments <- inBackground (fetchPages root (pulls <> "/comments") reviewCommentJq reviewCommentRow (cacheReviewComments cache))
  awaitConversation <- inBackground (fetchPages root (issue <> "/comments") conversationJq conversationRow (cacheConversation cache))
  awaitReviews <- inBackground (fetchPages root (pulls <> "/reviews") reviewJq reviewRow (cacheReviews cache))
  awaitCommits <- inBackground (fetchPages root (pulls <> "/commits") commitJq commitRow (cacheCommits cache))
  graph <- fetchGraph root pr (cacheGraph cache)
  viewer <- awaitViewer
  heads <- awaitHeads
  comments <- awaitComments
  conversation <- awaitConversation
  reviews <- awaitReviews
  commits <- awaitCommits
  pure $ do
    (f1, h) <- heads
    (f2, c) <- comments
    (f3, v) <- conversation
    (f4, r) <- reviews
    (f5, m) <- commits
    (f6, g) <- graph
    Right
      ( f1 <> f2 <> f3 <> f4 <> f5 <> f6
      , cache
          { cacheViewer = viewer
          , cacheHead = Just h
          , cacheReviewComments = Just c
          , cacheConversation = Just v
          , cacheReviews = Just r
          , cacheCommits = Just m
          , cacheGraph = Just g
          }
      )

viewerOnce :: FilePath -> Maybe Conv.Login -> IO (Maybe Conv.Login)
viewerOnce root known =
  case known of
    Just login -> pure (Just login)
    Nothing -> fmap (either (const Nothing) (Just . Conv.Login)) (GitHub.viewerLogin root)

inBackground :: IO a -> IO (IO a)
inBackground act = do
  slot <- Concurrent.newEmptyMVar
  Monad.void (Concurrent.forkIO (Exception.try act >>= Concurrent.putMVar slot))
  pure $ do
    got <- Concurrent.readMVar slot
    either (\e -> Exception.throwIO (e :: Exception.SomeException)) pure got

pagedRows :: Maybe [Cached [a]] -> [a]
pagedRows = foldMap (concatMap cachedValue)

pageSize :: Int
pageSize = 100

fetchPages ::
  FilePath ->
  T.Text ->
  T.Text ->
  ([T.Text] -> Maybe a) ->
  Maybe [Cached [a]] ->
  IO (Either GitHub.GhFailure (Freshness, [Cached [a]]))
fetchPages root path jq row known = do
  let
    pagePath k = path <> "?per_page=" <> T.pack (show pageSize) <> "&page=" <> T.pack (show (k :: Int))
    go k remembered = do
      got <- fetchCached root (pagePath k) jq (traverse row) (Maybe.listToMaybe remembered)
      case got of
        Right (freshness, page)
          | length (cachedValue page) == pageSize -> fmap (fmap ((freshness, page) :)) (go (k + 1) (drop 1 remembered))
          | otherwise -> pure (Right [(freshness, page)])
        Left failure -> pure (Left failure)
    sameCount pages = if fmap length known == Just (length pages) then Unchanged else Changed
  fetched <- go 1 (Maybe.fromMaybe [] known)
  pure $ do
    pages <- fetched
    Right (foldr ((<>) . fst) (sameCount pages) pages, map snd pages)

fetchCached ::
  FilePath ->
  T.Text ->
  T.Text ->
  ([[T.Text]] -> Maybe a) ->
  Maybe (Cached a) ->
  IO (Either GitHub.GhFailure (Freshness, Cached a))
fetchCached root path jq parse known = do
  got <- GitHub.conditionalGet root path (known >>= cachedEtag) (Just jq)
  pure $ case (got, known) of
    (Left failure, _) -> Left failure
    (Right GitHub.NotModified, Just cached) -> Right (Unchanged, cached)
    (Right GitHub.NotModified, Nothing) -> Left (unexpected path)
    (Right (GitHub.Modified response), _) ->
      case parse (GitHub.tsvRows (GitHub.responseBody response)) of
        Nothing -> Left (unexpected path)
        Just value -> Right (Changed, Cached {cachedEtag = GitHub.responseEtag response, cachedValue = value})

unexpected :: T.Text -> GitHub.GhFailure
unexpected path = GitHub.GhFailed ("unexpected answer from " <> path)

headJq :: T.Text
headJq =
  "[.head.sha, .title, .state, (.merged | tostring), .head.ref, .base.ref, .user.login\
  \, (.body // \"\"), (.draft | tostring)] | @tsv"

parseHead :: [[T.Text]] -> Maybe PullHead
parseHead rows =
  case rows of
    [[sha, title, state, merged, headBranch, baseBranch, author, body, draft]] ->
      Just
        PullHead
          { pullHeadSha = sha
          , pullHeadTitle = title
          , pullHeadState = case (state, merged) of
              (_, "true") -> PullMerged
              ("closed", _) -> PullClosed
              _ -> PullOpen
          , pullHeadBranch = headBranch
          , pullHeadBaseBranch = baseBranch
          , pullHeadAuthor = author
          , pullHeadBody = body
          , pullHeadDraft = if draft == "true" then Draft else ReadyForReview
          }
    _ -> Nothing

reviewCommentJq :: T.Text
reviewCommentJq =
  ".[] | [(.id | tostring), .user.login, ((.in_reply_to_id // \"\") | tostring), .path\
  \, ((.line // \"\") | tostring), (.side // \"\"), ((.start_line // \"\") | tostring)\
  \, .created_at, .updated_at, .body, (.html_url // \"\"), ((.pull_request_review_id // \"\") | tostring)\
  \, ((.original_line // \"\") | tostring), ((.original_start_line // \"\") | tostring)\
  \, (.original_commit_id // \"\"), (.diff_hunk // \"\"), "
    <> restReactionsJq
    <> "] | @tsv"

restReactionsJq :: T.Text
restReactionsJq =
  "((.reactions // {}) | [.[\"+1\"], .[\"-1\"], .laugh, .confused, .heart, .hooray, .rocket, .eyes]\
  \ | map((. // 0) | tostring) | join(\",\"))"

parseReactions :: T.Text -> Conv.Reactions
parseReactions t =
  let
    count (r, n) = case Git.readCount n of
      Just k | k > 0 -> Just Conv.ReactionCount {Conv.rcReaction = r, Conv.rcCount = k, Conv.rcMine = False}
      _ -> Nothing
  in
    Maybe.mapMaybe count (zip Conv.allReactions (T.splitOn "," t))

reviewCommentRow :: [T.Text] -> Maybe ReviewComment
reviewCommentRow cells =
  case cells of
    [cid, author, replyTo, path, line, side, startLine, createdAt, updatedAt, body, url, review, origLine, origStart, origCommit, hunk, reactions] -> do
      i <- readId cid
      Just
        ReviewComment
          { reviewCommentId = Store.CommentId i
          , reviewCommentAuthor = author
          , reviewCommentReplyTo = fmap Store.CommentId (readId replyTo)
          , reviewCommentPath = path
          , reviewCommentLine = Git.readCount line
          , reviewCommentSide = if side == "LEFT" then Conv.BaseSide else Conv.HeadSide
          , reviewCommentStartLine = Git.readCount startLine
          , reviewCommentCreatedAt = createdAt
          , reviewCommentUpdatedAt = updatedAt
          , reviewCommentBody = body
          , reviewCommentUrl = url
          , reviewCommentReview = fmap Store.ReviewId (readId review)
          , reviewCommentOriginalLine = Git.readCount origLine
          , reviewCommentOriginalStartLine = Git.readCount origStart
          , reviewCommentOriginalCommit = origCommit
          , reviewCommentDiffHunk = hunk
          , reviewCommentReactions = parseReactions reactions
          }
    _ -> Nothing

conversationJq :: T.Text
conversationJq =
  ".[] | [(.id | tostring), .user.login, .created_at, .updated_at, (.body // \"\"), (.html_url // \"\"), "
    <> restReactionsJq
    <> "] | @tsv"

conversationRow :: [T.Text] -> Maybe ConversationComment
conversationRow cells =
  case cells of
    [cid, author, createdAt, updatedAt, body, url, reactions] -> do
      i <- readId cid
      Just
        ConversationComment
          { conversationId = Store.CommentId i
          , conversationAuthor = author
          , conversationCreatedAt = createdAt
          , conversationUpdatedAt = updatedAt
          , conversationBody = body
          , conversationUrl = url
          , conversationReactions = parseReactions reactions
          }
    _ -> Nothing

reviewJq :: T.Text
reviewJq =
  ".[] | [(.id | tostring), .user.login, .state, (.commit_id // \"\"), (.submitted_at // \"\")\
  \, (.body // \"\"), (.html_url // \"\")] | @tsv"

reviewRow :: [T.Text] -> Maybe ReviewSummary
reviewRow cells =
  case cells of
    [rid, author, state, commit, submittedAt, body, url] -> do
      i <- readId rid
      Just
        ReviewSummary
          { reviewSummaryId = Store.ReviewId i
          , reviewSummaryAuthor = author
          , reviewSummaryState = state
          , reviewSummaryCommit = commit
          , reviewSummarySubmittedAt = submittedAt
          , reviewSummaryBody = body
          , reviewSummaryUrl = url
          }
    _ -> Nothing

commitJq :: T.Text
commitJq = ".[] | [.sha, (.commit.message | split(\"\\n\") | .[0]), (.commit.author.date // \"\")] | @tsv"

commitRow :: [T.Text] -> Maybe PullCommit
commitRow cells =
  case cells of
    [sha, subject, date] -> Just PullCommit {pullCommitSha = sha, pullCommitSubject = subject, pullCommitDate = date}
    _ -> Nothing

readId :: T.Text -> Maybe DataInt.Int64
readId t = fmap fromIntegral (Git.readCount t)

threadsSelection :: T.Text -> T.Text
threadsSelection after =
  "reviewThreads(first: "
    <> T.pack (show pageSize)
    <> after
    <> ") { pageInfo { hasNextPage endCursor }\
       \ nodes { id isResolved comments(first: 100) { nodes { databaseId "
    <> reactionsSelection
    <> " } } } }"

reactionsSelection :: T.Text
reactionsSelection = "reactionGroups { content viewerHasReacted reactors { totalCount } }"

reactionsJq :: T.Text -> T.Text
reactionsJq comments =
  "["
    <> comments
    <> ".nodes[] | (.databaseId | tostring) as $d | .reactionGroups[]\
       \ | select(.reactors.totalCount > 0 or .viewerHasReacted)\
       \ | [\"R\", $d, .content, (.viewerHasReacted | tostring), (.reactors.totalCount | tostring)]]"

graphQuery :: T.Text
graphQuery =
  "query($owner: String!, $name: String!, $number: Int!) {\
  \ repository(owner: $owner, name: $name) { pullRequest(number: $number) { "
    <> threadsSelection ""
    <> " comments(last: 100) { nodes { databaseId "
    <> reactionsSelection
    <> " } }\
       \ timelineItems(itemTypes: [HEAD_REF_FORCE_PUSHED_EVENT], last: 50) { nodes {\
       \ ... on HeadRefForcePushedEvent { createdAt beforeCommit { oid } afterCommit { oid } } } }\
       \ mergeable labels(first: 20) { nodes { name } }\
       \ commits(last: 1) { nodes { commit { oid statusCheckRollup { state\
       \ contexts(first: 100) { nodes { __typename ... on CheckRun { name conclusion status } ... on StatusContext { context state } } } } } } } } } }"

threadsQuery :: T.Text
threadsQuery =
  "query($owner: String!, $name: String!, $number: Int!, $cursor: String!) {\
  \ repository(owner: $owner, name: $name) { pullRequest(number: $number) { "
    <> threadsSelection ", after: $cursor"
    <> " } } }"

threadsJq :: T.Text -> T.Text
threadsJq threads =
  "["
    <> threads
    <> ".nodes[] | [\"T\", .id, (.isResolved | tostring), ((.comments.nodes[0].databaseId // \"\") | tostring)]]\
       \ + "
    <> reactionsJq (threads <> ".nodes[].comments")
    <> " + [[\"P\", ("
    <> threads
    <> ".pageInfo.hasNextPage | tostring), ("
    <> threads
    <> ".pageInfo.endCursor // \"\")]]"

graphJq :: T.Text
graphJq =
  ".data.repository.pullRequest as $p | ("
    <> threadsJq "$p.reviewThreads"
    <> " + "
    <> reactionsJq "$p.comments"
    <> " + [$p.timelineItems.nodes[] | [\"F\", (.createdAt // \"\"), (.beforeCommit.oid // \"\"), (.afterCommit.oid // \"\")]]\
       \ + [$p.commits.nodes[] | [\"C\", .commit.oid, (.commit.statusCheckRollup.state // \"\")]]\
       \ + [[\"M\", ($p.mergeable // \"\")]]\
       \ + [$p.labels.nodes[] | [\"L\", .name]]\
       \ + [$p.commits.nodes[].commit.statusCheckRollup.contexts.nodes[]?\
       \ | [\"K\", (.name // .context // \"\"), (.conclusion // .state // .status // \"\")]])\
       \ | .[] | @tsv"

threadsPageJq :: T.Text
threadsPageJq = "(" <> threadsJq ".data.repository.pullRequest.reviewThreads" <> ") | .[] | @tsv"

nextThreadCursor :: T.Text -> Maybe T.Text
nextThreadCursor raw =
  let
    cursor cells = case cells of
      ["P", "true", c] | not (T.null c) -> Just c
      _ -> Nothing
  in
    Maybe.listToMaybe (Maybe.mapMaybe cursor (GitHub.tsvRows raw))

fetchGraph :: FilePath -> Git.PullRequest -> Maybe GraphAnswer -> IO (Either GitHub.GhFailure (Freshness, GraphAnswer))
fetchGraph root pr known = do
  let
    (owner, rest) = T.breakOn "/" (Git.prRepo pr)
    fields = [("owner", owner), ("name", T.drop 1 rest), ("number", T.pack (show (Git.prNumber pr)))]
  got <- graphPages root fields Nothing
  pure $ case fmap (T.intercalate "\n") got of
    Left failure -> Left failure
    Right raw
      | fmap graphRaw known == Just raw -> Right (Unchanged, Maybe.fromMaybe (graphAnswer raw) known)
      | otherwise -> Right (Changed, graphAnswer raw)

graphPages :: FilePath -> [(T.Text, T.Text)] -> Maybe T.Text -> IO (Either GitHub.GhFailure [T.Text])
graphPages root fields after = do
  got <- case after of
    Nothing -> GitHub.graphql root graphQuery fields (Just graphJq)
    Just cursor -> GitHub.graphql root threadsQuery (fields ++ [("cursor", cursor)]) (Just threadsPageJq)
  case got of
    Left failure -> pure (Left failure)
    Right raw ->
      case nextThreadCursor raw of
        Nothing -> pure (Right [raw])
        Just cursor -> fmap (fmap (raw :)) (graphPages root fields (Just cursor))

graphAnswer :: T.Text -> GraphAnswer
graphAnswer raw =
  let
    rows = GitHub.tsvRows raw
    thread cells = case cells of
      ["T", tid, resolved, root] -> do
        i <- readId root
        Just
          ThreadRow
            { threadRowId = Store.RemoteThreadId tid
            , threadRowResolution = if resolved == "true" then Store.Resolved else Store.Unresolved
            , threadRowRoot = Store.CommentId i
            }
      _ -> Nothing
    forcePush cells = case cells of
      ["F", at, before, after] -> Just ForcePush {forcePushAt = at, forcePushBefore = before, forcePushAfter = after}
      _ -> Nothing
    checks cells = case cells of
      ["C", _, state] -> Just (GitHub.checksFromRollup state)
      _ -> Nothing
    reaction cells = case cells of
      ["R", cid, content, mine, count] -> do
        i <- readId cid
        r <- graphReaction content
        n <- Git.readCount count
        Just (Store.CommentId i, [Conv.ReactionCount {Conv.rcReaction = r, Conv.rcCount = n, Conv.rcMine = mine == "true"}])
      _ -> Nothing
    reacted = Map.fromListWith (flip (++)) (Maybe.mapMaybe reaction rows)
    checkRow cells = case cells of
      ["K", name, result] -> Just CheckRow {checkName = name, checkResult = checkResultOf result}
      _ -> Nothing
    mergeable cells = case cells of
      ["M", m] -> Just m
      _ -> Nothing
    label cells = case cells of
      ["L", l] -> Just l
      _ -> Nothing
  in
    GraphAnswer
      { graphRaw = raw
      , graphThreads = Maybe.mapMaybe thread rows
      , graphForcePushes = Maybe.mapMaybe forcePush rows
      , graphChecks = Maybe.fromMaybe GitHub.ChecksNone (Maybe.listToMaybe (Maybe.mapMaybe checks rows))
      , graphCheckRows = Maybe.mapMaybe checkRow rows
      , graphMergeable = Maybe.fromMaybe "" (Maybe.listToMaybe (Maybe.mapMaybe mergeable rows))
      , graphLabels = Maybe.mapMaybe label rows
      , graphReactions = reacted
      }

checkResultOf :: T.Text -> CheckResult
checkResultOf t =
  case T.toUpper t of
    "SUCCESS" -> CheckPassed
    "NEUTRAL" -> CheckPassed
    "SKIPPED" -> CheckSkipped
    "FAILURE" -> CheckFailed
    "ERROR" -> CheckFailed
    "TIMED_OUT" -> CheckFailed
    "CANCELLED" -> CheckFailed
    "ACTION_REQUIRED" -> CheckFailed
    "STARTUP_FAILURE" -> CheckFailed
    _ -> CheckPending

graphReaction :: T.Text -> Maybe Conv.Reaction
graphReaction content =
  case content of
    "THUMBS_UP" -> Just Conv.ThumbsUp
    "THUMBS_DOWN" -> Just Conv.ThumbsDown
    "LAUGH" -> Just Conv.Laugh
    "HOORAY" -> Just Conv.Hooray
    "CONFUSED" -> Just Conv.Confused
    "HEART" -> Just Conv.Heart
    "ROCKET" -> Just Conv.Rocket
    "EYES" -> Just Conv.Eyes
    _ -> Nothing
