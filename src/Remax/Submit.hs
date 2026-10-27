{-# LANGUAGE OverloadedStrings #-}

module Remax.Submit
  ( Item (..)
  , ItemKind (..)
  , outgoing
  , Placed (..)
  , PlacedAs (..)
  , placeItems
  , Submission (..)
  , Sent (..)
  , Accepted (..)
  , submit
  , countsText
  ) where

import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Remax.Conversation qualified as Conv
import Remax.DiffModel qualified as DiffModel
import Remax.Git qualified as Git
import Remax.GitHub qualified as GitHub
import Remax.Store qualified as Store

{- | One pending post and what it is: a new comment on lines or on the
pull request, or a reply under a thread or a PR-level comment.
-}
data Item = Item
  { itemWork :: Conv.WorkId
  , itemTarget :: Conv.Target
  , itemBody :: Conv.Markdown
  , itemKind :: ItemKind
  }
  deriving (Eq, Show)

data ItemKind
  = NewComment Conv.LineSpan
  | NewPullComment
  | ReplyUnder Store.RemoteThreadId Store.CommentId T.Text
  | QuoteReply Store.CommentId
  | ReplyUnknown
  deriving (Eq, Show)

{- | Every pending post, new comments first in file order, then replies,
then PR-level comments.
-}
outgoing :: Conv.Remote -> Conv.OwnWork -> [Item]
outgoing remote own =
  let
    threads = Map.fromList (map (\t -> (Conv.remoteThreadId t, t)) (Conv.remoteThreads remote))
    rootOf t = Conv.remoteCommentId (NEL.head (Conv.remoteThreadComments t))
    authorOf t = Conv.loginToText (Conv.remoteCommentAuthor (NEL.last (Conv.remoteThreadComments t)))
    item (target, wid, work) =
      case work of
        Conv.Pending _ body ->
          let
            kind = case target of
              Conv.NewThread _ (Conv.OnLines lineSpan) -> Just (NewComment lineSpan)
              Conv.NewThread _ Conv.OnPull -> Just NewPullComment
              Conv.NewThread _ _ -> Nothing
              Conv.OnThread tid -> Just (maybe ReplyUnknown (\t -> ReplyUnder tid (rootOf t) (authorOf t)) (Map.lookup tid threads))
              Conv.OnComment cid -> Just (QuoteReply cid)
              Conv.OnLinesOnly {} -> Nothing
          in
            fmap (\k -> Item {itemWork = wid, itemTarget = target, itemBody = body, itemKind = k}) kind
        _ -> Nothing
    items = Maybe.mapMaybe item (Conv.pendingOf own)
    rank i = case itemKind i of
      NewComment lineSpan -> (0 :: Int, Conv.spanPath lineSpan, Conv.slLineNo (NEL.head (Conv.spanLines lineSpan)))
      ReplyUnder {} -> (1, "", 0)
      ReplyUnknown -> (1, "", 0)
      QuoteReply _ -> (2, "", 0)
      NewPullComment -> (2, "", 1)
  in
    List.sortOn rank items

countsText :: [Item] -> T.Text
countsText items =
  let
    isNew i = case itemKind i of
      NewComment _ -> True
      _ -> False
    isReply i = case itemKind i of
      ReplyUnder {} -> True
      ReplyUnknown -> True
      _ -> False
    news = length (filter isNew items)
    replies = length (filter isReply items)
    pulls = length items - news - replies
    part n word = if n == 0 then [] else [T.pack (show n) <> " " <> word <> (if n == 1 then "" else "s")]
  in
    T.intercalate " \183 " (part (min 1 news) "review" ++ part replies "reply" ++ part pulls "PR-level comment")

{- | Where each item posts: inline on the head's diff, under a thread, or
as a PR-level comment naming its file and line.
-}
data Placed = Placed
  { placedItem :: Item
  , placedAs :: PlacedAs
  }
  deriving (Eq, Show)

data PlacedAs
  = Inline GitHub.InlineComment
  | PullLevel T.Text
  | Under Store.CommentId
  | Unplaceable T.Text
  deriving (Eq, Show)

placeItems :: FilePath -> Git.PullRequest -> [Item] -> IO [Placed]
placeItems root pr items = do
  let
    n = Git.prNumber pr
    paths = List.nub (Maybe.mapMaybe spanPathOf items)
    spanPathOf i = case itemKind i of
      NewComment lineSpan -> Just (Conv.spanPath lineSpan)
      _ -> Nothing
  diffs <- mapM (\p -> fmap (\d -> (p, d)) (headDiff root n (T.unpack p))) paths
  let
    byPath = Map.fromList diffs
    place i =
      Placed i $ case itemKind i of
        NewComment lineSpan ->
          case Map.lookup (Conv.spanPath lineSpan) byPath >>= id >>= inlineFor lineSpan (Conv.markdownText (itemBody i)) of
            Just inline -> Inline inline
            Nothing -> PullLevel (offDiffBody lineSpan (Conv.markdownText (itemBody i)))
        NewPullComment -> PullLevel (Conv.markdownText (itemBody i))
        ReplyUnder _ rootId _ -> Under rootId
        QuoteReply _ -> PullLevel (Conv.markdownText (itemBody i))
        ReplyUnknown -> Unplaceable "its thread is not in the last poll"
  pure (map place items)

offDiffBody :: Conv.LineSpan -> T.Text -> T.Text
offDiffBody lineSpan body =
  let
    numbers = map Conv.slLineNo (NEL.toList (Conv.spanLines lineSpan))
    range = case (Maybe.listToMaybe numbers, Maybe.listToMaybe (reverse numbers)) of
      (Just lo, Just hi) | lo /= hi -> T.pack (show lo) <> "\8211" <> T.pack (show hi)
      (Just lo, _) -> T.pack (show lo)
      _ -> ""
  in
    "`" <> Conv.spanPath lineSpan <> ":" <> range <> "`\n\n" <> body

{- | The lines of a file the head's diff holds, by side, each with the
text it has there.
-}
data HeadDiff = HeadDiff
  { headLines :: Map.Map Int T.Text
  , baseLines :: Map.Map Int T.Text
  }

headDiff :: FilePath -> Int -> FilePath -> IO (Maybe HeadDiff)
headDiff root n path = do
  base <- Git.refContent root (Git.pullBaseRef n) path
  tip <- Git.refContent root (Git.pullHeadRef n) path
  let
    textOf c = case c of
      Just (Git.RefText ls) -> Just ls
      Just Git.RefBinary -> Nothing
      Nothing -> Just []
  pure $ do
    baseText <- textOf base
    headText <- textOf tip
    Just (inDiff (DiffModel.diffRows DiffModel.WhitespaceMatters baseText headText))

{- | GitHub takes a comment on any line of a hunk, including the three
context lines either side of a change.
-}
inDiff :: [(Char, Maybe Int, T.Text)] -> HeadDiff
inDiff rows =
  let
    numbered = snd (List.mapAccumL (\base (mark, new, text) -> (if mark /= '+' then base + 1 else base, (mark, if mark /= '+' then Just base else Nothing, new, text))) 1 rows)
    changedAt = Set.fromList (Maybe.mapMaybe (\(i, (mark, _, _, _)) -> if mark == ' ' then Nothing else Just i) (zip [0 :: Int ..] numbered))
    near i = any (\d -> Set.member (i + d) changedAt) [-3 .. 3]
    kept = Maybe.mapMaybe (\(i, row) -> if near i then Just row else Nothing) (zip [0 ..] numbered)
  in
    HeadDiff
      { headLines = Map.fromList (Maybe.mapMaybe (\(_, _, new, text) -> fmap (\r -> (r + 1, text)) new) kept)
      , baseLines = Map.fromList (Maybe.mapMaybe (\(mark, base, _, text) -> if mark == '+' then Nothing else fmap (\b -> (b, text)) base) kept)
      }

inlineFor :: Conv.LineSpan -> T.Text -> HeadDiff -> Maybe GitHub.InlineComment
inlineFor lineSpan body diff =
  let
    (side, held) = case Conv.spanSide lineSpan of
      Conv.HeadSide -> ("RIGHT", headLines diff)
      Conv.BaseSide -> ("LEFT", baseLines diff)
    onSide l = not (T.isPrefixOf (case Conv.spanSide lineSpan of Conv.HeadSide -> "-"; Conv.BaseSide -> "+") (Conv.slText l))
    spanLines = filter onSide (NEL.toList (Conv.spanLines lineSpan))
    found l =
      let
        wanted = T.drop 1 (Conv.slText l)
        matches = Map.keys (Map.filter (== wanted) held)
      in
        Maybe.listToMaybe (List.sortOn (\n -> abs (n - Conv.slLineNo l)) matches)
    lines_ = Maybe.mapMaybe found spanLines
  in
    case (Maybe.listToMaybe lines_, Maybe.listToMaybe (reverse lines_)) of
      (Just lo, Just hi)
        | length lines_ == length spanLines ->
            Just
              GitHub.InlineComment
                { GitHub.inlineCommentPath = Conv.spanPath lineSpan
                , GitHub.inlineCommentLine = hi
                , GitHub.inlineCommentSide = side
                , GitHub.inlineCommentStartLine = if lo < hi then Just lo else Nothing
                , GitHub.inlineCommentBody = body
                }
      _ -> Nothing

data Submission = Submission
  { submissionRoot :: FilePath
  , submissionPull :: Git.PullRequest
  , submissionEvent :: GitHub.ReviewEvent
  , submissionBody :: T.Text
  , submissionItems :: [Placed]
  }

{- | An item GitHub took: the work to drop, and for a new thread the
comment id its later work hangs off.
-}
data Accepted = Accepted
  { acceptedWork :: Conv.WorkId
  , acceptedAs :: Maybe (Conv.LocalId, Store.CommentId)
  }
  deriving (Eq, Show)

data Sent = Sent
  { sentAccepted :: [Accepted]
  , sentReview :: Maybe GitHub.SubmittedReview
  , sentFailure :: Maybe GitHub.GhFailure
  }

{- | One review with the inline comments and the summary, then each
reply, then each PR-level comment; the first refusal stops the rest.
Each accepted item is handed on as GitHub takes it, so a session that
dies mid-send has still recorded what went out.
-}
submit :: (Accepted -> IO ()) -> Submission -> IO Sent
submit landed submission = do
  let
    root = submissionRoot submission
    pr = submissionPull submission
    repo = Git.prRepo pr
    n = Git.prNumber pr
    placed = submissionItems submission
    inline = Maybe.mapMaybe (\p -> case placedAs p of Inline c -> Just (p, c); _ -> Nothing) placed
    replies = Maybe.mapMaybe (\p -> case placedAs p of Under cid -> Just (p, cid); _ -> Nothing) placed
    pullLevel = Maybe.mapMaybe (\p -> case placedAs p of PullLevel body -> Just (p, body); _ -> Nothing) placed
    saysNothing = submissionEvent submission == GitHub.EventComment && T.null (submissionBody submission) && null inline
  headSha <- Git.resolveFull root (Git.pullHeadRef n)
  case headSha of
    Nothing -> pure Sent {sentAccepted = [], sentReview = Nothing, sentFailure = Just (GitHub.GhFailed "the pull request's head is not fetched \8212 :pr again")}
    Just sha -> do
      reviewed <-
        if saysNothing
          then pure (Right (Nothing, []))
          else sendReview root repo n sha submission inline
      case reviewed of
        Left failure -> pure Sent {sentAccepted = [], sentReview = Nothing, sentFailure = Just failure}
        Right (review, fromReview) -> do
          mapM_ landed fromReview
          (later, failure) <- sendEach landed root repo n (replies, pullLevel)
          pure Sent {sentAccepted = fromReview ++ later, sentReview = review, sentFailure = failure}

sendReview ::
  FilePath ->
  T.Text ->
  Int ->
  T.Text ->
  Submission ->
  [(Placed, GitHub.InlineComment)] ->
  IO (Either GitHub.GhFailure (Maybe GitHub.SubmittedReview, [Accepted]))
sendReview root repo n sha submission inline = do
  let
    request =
      GitHub.ReviewRequest
        { GitHub.reviewRequestCommit = sha
        , GitHub.reviewRequestEvent = submissionEvent submission
        , GitHub.reviewRequestBody = submissionBody submission
        , GitHub.reviewRequestComments = map snd inline
        }
  posted <- GitHub.postReview root repo n request
  case posted of
    Left failure -> pure (Left failure)
    Right review -> do
      listed <- if null inline then pure (Right []) else GitHub.reviewComments root repo n (GitHub.submittedReviewId review)
      let
        comments = either (const []) id listed
      pure (Right (Just review, matchComments inline comments))

{- | Each inline comment finds the posted comment on its path and line;
one GitHub did not list is still dropped, since the review took it.
-}
matchComments :: [(Placed, GitHub.InlineComment)] -> [GitHub.PostedComment] -> [Accepted]
matchComments inline comments =
  let
    step (used, acc) (p, c) =
      let
        fits posted =
          GitHub.postedCommentPath posted == GitHub.inlineCommentPath c
            && GitHub.postedCommentLine posted == Just (GitHub.inlineCommentLine c)
            && not (Set.member (GitHub.postedCommentId posted) used)
        lid = Conv.targetLocalId (itemTarget (placedItem p))
      in
        case List.find fits comments of
          Just posted ->
            ( Set.insert (GitHub.postedCommentId posted) used
            , Accepted {acceptedWork = itemWork (placedItem p), acceptedAs = fmap (\l -> (l, GitHub.postedCommentId posted)) lid} : acc
            )
          Nothing -> (used, Accepted {acceptedWork = itemWork (placedItem p), acceptedAs = Nothing} : acc)
  in
    reverse (snd (List.foldl' step (Set.empty, []) inline))

sendEach :: (Accepted -> IO ()) -> FilePath -> T.Text -> Int -> ([(Placed, Store.CommentId)], [(Placed, T.Text)]) -> IO ([Accepted], Maybe GitHub.GhFailure)
sendEach landed root repo n (replies, pullLevel) =
  let
    sends =
      map (\(p, cid) -> (p, GitHub.postReply root repo n cid (Conv.markdownText (itemBody (placedItem p))))) replies
        ++ map (\(p, body) -> (p, GitHub.postIssueComment root repo n body)) pullLevel
    go acc list =
      case list of
        [] -> pure (reverse acc, Nothing)
        ((p, send) : rest) -> do
          got <- send
          case got of
            Left failure -> pure (reverse acc, Just failure)
            Right posted ->
              let
                lid = Conv.targetLocalId (itemTarget (placedItem p))
                accepted = Accepted {acceptedWork = itemWork (placedItem p), acceptedAs = fmap (\l -> (l, GitHub.postedCommentId posted)) lid}
              in
                landed accepted >> go (accepted : acc) rest
  in
    go [] sends
