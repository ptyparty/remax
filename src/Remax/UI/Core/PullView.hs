{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Core.PullView
  ( Row (..)
  , Gutter (..)
  , timeline
  , stops
  , checksSummary
  , checksListing
  ) where

import Brick qualified as Brick
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.Config qualified as Config
import Remax.Conversation qualified as Conv
import Remax.PullPoll qualified as PullPoll
import Remax.Store qualified as Store
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Markdown qualified as Markdown
import Remax.UI.Core.Threads qualified as Threads
import Remax.UI.Core.Types qualified as Types

{- | One row of the timeline: its gutter glyph and its text, and the
post it belongs to when the band can land on it.
-}
data Row = Row
  { rowGutter :: Gutter
  , rowSegs :: [Markdown.Segment]
  , rowPost :: Maybe (Conv.ConversationId, Int)
  , rowFill :: Maybe Markdown.Row
  }

data Gutter = Dot | Pending | Rail | Branch | Blank

-- | Every post the band may visit, in timeline order.
stops :: Types.St -> Int -> [Types.Band]
stops st width =
  let
    once acc band = case acc of
      (prev : _) | prev == band -> acc
      _ -> band : acc
  in
    reverse (List.foldl' once [] (Maybe.mapMaybe (\r -> fmap (\(cid, i) -> Types.Band cid i False) (rowPost r)) (timeline st width)))

timeline :: Types.St -> Int -> [Row]
timeline st width =
  let
    snap = Types.ppSnapshot (Types.stPullPoll st)
    convs = Types.stConversations st
    reviews = List.sortOn Conv.remoteReviewSubmittedAt (maybe [] (Conv.remoteReviews . PullPoll.snapshotRemote) snap)
    pullConv = List.find ((== Conv.PullConversation) . Conv.convId) convs
    threads = Maybe.mapMaybe (\c -> case Conv.convVenue c of Conv.ReviewThread (Just _) _ -> Just c; _ -> Nothing) convs
    byReview = Map.fromListWith (flip (++)) (Maybe.mapMaybe (\c -> fmap (\r -> (r, [c])) (reviewOf st c)) threads)
    orphans = filter (Maybe.isNothing . reviewOf st) threads
    entries =
      List.sortOn fst $
        maybe [] (\c -> Maybe.mapMaybe (\(i, p) -> fmap (\t -> (t, CommentEntry c i p)) (postedAt p)) (zip [0 ..] (Conv.convPosts c))) pullConv
          ++ Maybe.mapMaybe reviewEntry reviews
    reviewEntry r =
      let
        under = Map.findWithDefault [] (Conv.remoteReviewId r) byReview
        says = Conv.remoteReviewState r /= "COMMENTED" || not (T.null (T.strip (Conv.markdownText (Conv.remoteReviewBody r))))
      in
        if says || not (null under) then Just (Conv.remoteReviewSubmittedAt r, ReviewEntry r under) else Nothing
    drafts = draftRows st width convs
    bodyRows = concatMap (entryRows st width) (map snd entries ++ [ThreadsEntry orphans | not (null orphans)])
  in
    headerRows st width ++ bodyRows ++ drafts ++ footerRows st

data Entry
  = CommentEntry Conv.Conversation Int Conv.Post
  | ReviewEntry Conv.RemoteReview [Conv.Conversation]
  | ThreadsEntry [Conv.Conversation]

postedAt :: Conv.Post -> Maybe T.Text
postedAt p =
  case Conv.postState p of
    Conv.Posted _ at _ -> Just at
    _ -> Nothing

reviewOf :: Types.St -> Conv.Conversation -> Maybe Store.ReviewId
reviewOf st conv =
  case Conv.convId conv of
    Conv.ThreadConversation tid -> do
      snap <- Types.ppSnapshot (Types.stPullPoll st)
      thread <- List.find ((== tid) . Conv.remoteThreadId) (Conv.remoteThreads (PullPoll.snapshotRemote snap))
      Conv.remoteCommentReview (NEL.head (Conv.remoteThreadComments thread))
    _ -> Nothing

text :: Gutter -> Brick.AttrName -> T.Text -> Row
text g attr t = Row g [Markdown.Segment attr t Nothing] Nothing Nothing

hint :: Gutter -> T.Text -> Row
hint g = text g Attrs.hintAttr

blank :: Row
blank = Row Rail [] Nothing Nothing

headerRows :: Types.St -> Int -> [Row]
headerRows st width =
  case Types.ppSnapshot (Types.stPullPoll st) of
    Nothing -> [hint Blank "waiting for the first poll\8230"]
    Just snap ->
      let
        h = PullPoll.snapshotHead snap
        n = Types.tshow (PullPoll.snapshotPull snap)
        reviewers = List.nub (map (Threads.authorName . Conv.Person . Conv.remoteReviewAuthor) (Conv.remoteReviews (PullPoll.snapshotRemote snap)))
        count k word = Types.tshow k <> " " <> word <> (if k == 1 then "" else if T.isSuffixOf "push" word then "es" else "s")
        parts =
          [ "#" <> n
          , PullPoll.pullHeadAuthor h
          , count (length (PullPoll.snapshotCommits snap)) "commit"
          ]
            ++ (if null (PullPoll.snapshotForcePushes snap) then [] else [count (length (PullPoll.snapshotForcePushes snap)) "force-push"])
            ++ (if null reviewers then [] else [T.intercalate ", " reviewers])
            ++ (if null (PullPoll.snapshotLabels snap) then [] else [T.intercalate ", " (PullPoll.snapshotLabels snap)])
            ++ (if PullPoll.pullHeadDraft h == PullPoll.Draft then ["draft"] else [])
        description = map (\r -> Row Blank (Markdown.rowSegments r) Nothing (Just r)) (Markdown.render Markdown.Folded (width - 4) (Conv.Markdown (PullPoll.pullHeadBody h)))
        emacs = Config.cfgKeys (Types.stConfig st) == Config.KeysEmacs
        checksKey = if emacs then "    C-c k lists them" else "    c lists them"
        conversationKeys = if emacs then "conversation \183 C-n C-p move by post \183 C-c [ ] threads awaiting you \183 C-c g comment on the PR" else "conversation \183 j/k move by post \183 [ ] threads awaiting you \183 @ comment on the PR"
      in
        [ Row Blank [Markdown.Segment Attrs.trayFileAttr (PullPoll.pullHeadTitle h) Nothing] Nothing Nothing
        , hint Blank (T.intercalate " \183 " parts)
        , blank {rowGutter = Blank}
        ]
          ++ description
          ++ [ blank {rowGutter = Blank}
             , Row Blank [Markdown.Segment Attrs.hintAttr "checks  " Nothing, Markdown.Segment Attrs.msgAttr (checksSummary snap) Nothing, Markdown.Segment Attrs.hintAttr checksKey Nothing] Nothing Nothing
             , blank {rowGutter = Blank}
             , hint Blank conversationKeys
             ]

checksSummary :: PullPoll.Snapshot -> T.Text
checksSummary snap =
  let
    rows = PullPoll.snapshotCheckRows snap
    counted result = length (filter ((== result) . PullPoll.checkResult) rows)
    passed = counted PullPoll.CheckPassed
    failed = counted PullPoll.CheckFailed
    pending = counted PullPoll.CheckPending
    mergeable = case PullPoll.snapshotMergeable snap of
      "MERGEABLE" -> " \183 mergeable"
      "CONFLICTING" -> " \183 conflicts"
      _ -> ""
    summary
      | null rows = "no checks"
      | failed > 0 = "\10007 " <> Types.tshow failed <> " failed \183 " <> Types.tshow passed <> " passed"
      | pending > 0 = "\9675 " <> Types.tshow pending <> " running \183 " <> Types.tshow passed <> " passed"
      | otherwise = "\10003 " <> Types.tshow passed <> " passed"
  in
    summary <> mergeable

checksListing :: PullPoll.Snapshot -> T.Text
checksListing snap =
  let
    glyph r = case PullPoll.checkResult r of
      PullPoll.CheckPassed -> "\10003"
      PullPoll.CheckFailed -> "\10007"
      PullPoll.CheckPending -> "\9675"
      PullPoll.CheckSkipped -> "\8211"
  in
    case PullPoll.snapshotCheckRows snap of
      [] -> "no checks on the head commit"
      rows -> T.unlines (map (\r -> glyph r <> " " <> PullPoll.checkName r) rows)

entryRows :: Types.St -> Int -> Entry -> [Row]
entryRows st width entry =
  case entry of
    CommentEntry conv i post ->
      dotRow st post Nothing ++ postBody st width (Conv.convId conv) i post ++ [blank]
    ReviewEntry review threads ->
      let
        convId = Conv.ReviewConversation (Conv.remoteReviewId review)
        summary = List.find ((== convId) . Conv.convId) (Types.stConversations st)
        post = summary >>= Maybe.listToMaybe . Conv.convPosts
        event = case Conv.remoteReviewState review of
          "APPROVED" -> Just "\10003 approved"
          "CHANGES_REQUESTED" -> Just "\10007 changes requested"
          "DISMISSED" -> Just "dismissed"
          _ -> Just "commented"
      in
        case post of
          Just p ->
            dotRow st p event
              ++ (if T.null (T.strip (Conv.markdownText (Conv.postBody p))) then [] else postBody st width convId 0 p ++ (if null threads then [] else [blank]))
              ++ concatMap (threadRows st width) threads
              ++ [blank]
          Nothing -> concatMap (threadRows st width) threads
    ThreadsEntry threads -> concatMap (threadRows st width) threads ++ [blank]

dotRow :: Types.St -> Conv.Post -> Maybe T.Text -> [Row]
dotRow st post event =
  let
    age = case Conv.postState post of
      Conv.Posted _ at _ -> Threads.ageText (Types.stNow st) at
      _ -> ""
    attr = Threads.authorInk (Conv.postAuthor post)
  in
    [ Row
        Dot
        ( [Markdown.Segment attr (Threads.authorName (Conv.postAuthor post)) Nothing, Markdown.Segment Attrs.hintAttr (" \183 " <> age) Nothing]
            ++ maybe [] (\e -> [Markdown.Segment Attrs.hintAttr (" \183 " <> e) Nothing]) event
        )
        Nothing
        Nothing
    ]

{- | A post's body under its dot, with its reactions and, when the band
is on it, its keys.
-}
postBody :: Types.St -> Int -> Conv.ConversationId -> Int -> Conv.Post -> [Row]
postBody st width cid i post =
  let
    rendered = Markdown.render (Threads.detailsFold st cid i) (width - 6) (Conv.postBody post)
    at = case Conv.postState post of
      Conv.Local _ -> Nothing
      _ -> Just (cid, i)
    body = map (\r -> Row Rail (Markdown.Segment Attrs.msgAttr "  " Nothing : Markdown.rowSegments r) at (Just r)) rendered
    reactions = case Conv.postReactions post of
      [] -> []
      counts -> [Row Rail (Markdown.Segment Attrs.msgAttr "  " Nothing : Threads.reactionSegs counts) at Nothing]
  in
    body ++ reactions ++ stripRow st cid i post

stripRow :: Types.St -> Conv.ConversationId -> Int -> Conv.Post -> [Row]
stripRow st cid i post =
  case Types.stBand st of
    Just band
      | Types.bandConv band == cid && Types.bandPost band == i ->
          let
            venue = maybe Conv.PullComments Conv.convVenue (Conv.conversationFor cid (Types.stConversations st))
            keys = Config.cfgKeys (Types.stConfig st)
          in
            [Row Rail [Markdown.Segment Attrs.hintAttr ("  \9584 " <> Threads.postKeys keys (Types.flagRule st) venue post) Nothing] (Just (cid, i)) Nothing]
    _ -> []

{- | A review thread under the review that started it: its place and
state, the diff lines of a range, then its log.
-}
threadRows :: Types.St -> Int -> Conv.Conversation -> [Row]
threadRows st width conv =
  let
    cid = Conv.convId conv
    header = Row Branch [Markdown.Segment Attrs.hintAttr " \8618 " Nothing, Markdown.Segment Attrs.trayFileAttr (Threads.anchorPlace (Conv.convAnchor conv)) Nothing, Markdown.Segment Attrs.hintAttr (" \183 " <> Threads.threadState st conv) Nothing] Nothing Nothing
    folded = Threads.isResolvedFold st conv
    resolvedLine =
      Row
        Branch
        [ Markdown.Segment Attrs.doneMarkAttr " \10003 " Nothing
        , Markdown.Segment Attrs.hintAttr (Threads.anchorPlace (Conv.convAnchor conv) <> " \183 resolved \183 " <> Types.tshow (length (Conv.convPosts conv)) <> " comments \183 za opens") Nothing
        ]
        (Just (cid, 0))
        Nothing
    resolvedStrip = maybe [] (stripRow st cid 0) (Maybe.listToMaybe (Conv.convPosts conv))
    rangeLines = map (\(n, t) -> Row Rail [Markdown.Segment Attrs.hintAttr ("       " <> T.justifyRight 4 ' ' (Types.tshow n) <> " " <> t) Nothing] Nothing Nothing) (rangeOf st conv)
    logRows = map (\tl -> Row Rail (Markdown.Segment Attrs.msgAttr "  " Nothing : Threads.tlSegs tl) (fmap (\i -> (cid, i)) (Threads.tlPost tl)) Nothing) (threadLog st width conv)
  in
    if folded
      then resolvedLine : resolvedStrip ++ [blank]
      else header : rangeLines ++ logRows ++ [blank]

{- | The thread's posts as the body draws them, without the header the
timeline already has.
-}
threadLog :: Types.St -> Int -> Conv.Conversation -> [Threads.ThreadLine]
threadLog st width conv = drop 1 (Threads.threadLines st (width - 4) conv)

{- | The diff lines a range thread sits on, numbered as GitHub shows
them, from the hunk GitHub keeps with the comment.
-}
rangeOf :: Types.St -> Conv.Conversation -> [(Int, T.Text)]
rangeOf st conv =
  case (Conv.convId conv, Conv.convAnchor conv) of
    (Conv.ThreadConversation tid, Conv.OnPosition pos)
      | Just start <- Conv.posStartLine pos -> do
          let
            hunk = do
              snap <- Types.ppSnapshot (Types.stPullPoll st)
              thread <- List.find ((== tid) . Conv.remoteThreadId) (Conv.remoteThreads (PullPoll.snapshotRemote snap))
              Just (Conv.remoteCommentDiffHunk (NEL.head (Conv.remoteThreadComments thread)))
            count = Conv.posLine pos - start + 1
          maybe [] (\h -> zip [start ..] (reverse (take count (reverse (filter (not . T.isPrefixOf "@@") (T.lines h)))))) hunk
    _ -> []

{- | Your pending comments and the agent's proposals, on the pull request
and on lines, after the posted entries; pending replies sit inside their
threads already.
-}
draftRows :: Types.St -> Int -> [Conv.Conversation] -> [Row]
draftRows st width convs =
  let
    newThreads = Maybe.mapMaybe (\c -> case Conv.convVenue c of Conv.ReviewThread Nothing _ -> Just c; _ -> Nothing) convs
    unsent c = Maybe.mapMaybe (\(i, p) -> if isUnsent p then Just (c, i, p) else Nothing) (zip [0 ..] (Conv.convPosts c))
    isUnsent p = case Conv.postState p of
      Conv.Draft -> True
      Conv.Proposal _ -> True
      _ -> False
    pullDrafts = maybe [] unsent (List.find ((== Conv.PullConversation) . Conv.convId) convs)
    place c = Threads.anchorPlace (Conv.convAnchor c)
    heading c p = case Conv.postState p of
      Conv.Proposal name ->
        [ Markdown.Segment Attrs.guideAttr ("\10022 " <> Conv.agentNameToText name) Nothing
        , Markdown.Segment Attrs.guideAttr " \183 proposes" Nothing
        , Markdown.Segment Attrs.hintAttr (" \183 " <> place c <> " \183 yours to take or drop") Nothing
        ]
      _ ->
        [ Markdown.Segment Attrs.noteInlineAttr "you" Nothing
        , Markdown.Segment Attrs.questionMarkAttr " \183 draft" Nothing
        , Markdown.Segment Attrs.hintAttr (" \183 " <> place c <> " \183 sends with :submit") Nothing
        ]
    draft (c, i, p) =
      Row Pending (heading c p) Nothing Nothing
        : postBody st width (Conv.convId c) i p
        ++ [blank]
  in
    concatMap draft (pullDrafts ++ concatMap unsent newThreads)

footerRows :: Types.St -> [Row]
footerRows st =
  let
    pending = length (Conv.pendingOf (Types.stWork st))
  in
    if pending == 0
      then []
      else [hint Blank ("drafts \183 " <> Types.tshow pending <> " to send \183 :submit sends")]
