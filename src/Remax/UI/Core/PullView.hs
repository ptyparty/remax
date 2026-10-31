{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Core.PullView
  ( Row (..)
  , Gutter (..)
  , selectedStop
  , timeline
  , stops
  , checksSummary
  , checksListing
  , hunkBody
  , conversationRows
  , placeOf
  , gitHubAuthor
  ) where

import Brick qualified as Brick
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Remax.Config qualified as Config
import Remax.Conversation qualified as Conv
import Remax.GitHub qualified as GitHub
import Remax.Highlight qualified as Highlight
import Remax.PullPoll qualified as PullPoll
import Remax.SoftWrap qualified as SoftWrap
import Remax.Store qualified as Store
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Markdown qualified as Markdown
import Remax.UI.Core.Threads qualified as Threads
import Remax.UI.Core.Types qualified as Types

{- | One row of the timeline: its gutter glyph and its text, and the
post or commit it belongs to when the cursor can land on it.
-}
data Row = Row
  { rowGutter :: Gutter
  , rowSegs :: [Markdown.Segment]
  , rowStop :: Maybe Types.Stop
  , rowFill :: Maybe Markdown.Row
  }

data Gutter = Dot | Pending | Rail | Branch | Blank

selectedStop :: Types.St -> Maybe Types.Stop
selectedStop st = Types.stPullView st >>= Types.pvStop

-- | Every post and commit the cursor may visit, in timeline order.
stops :: Types.St -> Int -> [Types.Stop]
stops st width =
  let
    once acc stop = case acc of
      (prev : _) | prev == stop -> acc
      _ -> stop : acc
  in
    reverse (List.foldl' once [] (Maybe.mapMaybe rowStop (timeline st width)))

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
      groupCommits . map snd . List.sortOn fst $
        maybe [] (\c -> Maybe.mapMaybe (\(i, p) -> fmap (\t -> (t, CommentEntry c i p)) (postedAt p)) (zip [0 ..] (Conv.convPosts c))) pullConv
          ++ Maybe.mapMaybe reviewEntry reviews
          ++ maybe [] historyEntries snap
    reviewEntry r =
      let
        under = Map.findWithDefault [] (Conv.remoteReviewId r) byReview
        says = Conv.remoteReviewState r /= "COMMENTED" || not (T.null (T.strip (Conv.markdownText (Conv.remoteReviewBody r))))
      in
        if says || not (null under) then Just (Conv.remoteReviewSubmittedAt r, ReviewEntry r under) else Nothing
    drafts = draftRows st width convs
    bodyRows = concatMap (entryRows st width) (entries ++ [ThreadsEntry orphans | not (null orphans)])
  in
    headerRows st width ++ bodyRows ++ drafts ++ footerRows st

data Entry
  = CommentEntry Conv.Conversation Int Conv.Post
  | ReviewEntry Conv.RemoteReview [Conv.Conversation]
  | ThreadsEntry [Conv.Conversation]
  | CommitsEntry Conv.Author (NEL.NonEmpty PullPoll.PullCommit)
  | ForcePushEntry Conv.Author PullPoll.ForcePush

historyEntries :: PullPoll.Snapshot -> [(T.Text, Entry)]
historyEntries snap =
  let
    commit c = (PullPoll.pullCommitCommittedAt c, CommitsEntry (gitHubAuthor snap (PullPoll.pullCommitAuthor c)) (NEL.singleton c))
    push p = (PullPoll.forcePushAt p, ForcePushEntry (gitHubAuthor snap (PullPoll.forcePushActor p)) p)
  in
    map commit (PullPoll.snapshotCommits snap) ++ map push (PullPoll.snapshotForcePushes snap)

gitHubAuthor :: PullPoll.Snapshot -> T.Text -> Conv.Author
gitHubAuthor snap name =
  Conv.remoteAuthor (Conv.remoteViewer (PullPoll.snapshotRemote snap)) (Conv.Login (if T.null name then "ghost" else name))

groupCommits :: [Entry] -> [Entry]
groupCommits entries =
  case entries of
    CommitsEntry author commits : rest ->
      let
        byAuthor e = case e of
          CommitsEntry next more | next == author -> Just (NEL.toList more)
          _ -> Nothing
        (run, others) = List.span (Maybe.isJust . byAuthor) rest
      in
        CommitsEntry author (NEL.appendList commits (concat (Maybe.mapMaybe byAuthor run))) : groupCommits others
    entry : rest -> entry : groupCommits rest
    [] -> []

countText :: Int -> T.Text -> T.Text
countText k word = Types.tshow k <> " " <> word <> (if k == 1 then "" else if T.isSuffixOf "push" word then "es" else "s")

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
        parts =
          [ "#" <> n
          , PullPoll.pullHeadAuthor h
          , countText (length (PullPoll.snapshotCommits snap)) "commit"
          ]
            ++ (if null (PullPoll.snapshotForcePushes snap) then [] else [countText (length (PullPoll.snapshotForcePushes snap)) "force-push"])
            ++ (if null reviewers then [] else [T.intercalate ", " reviewers])
            ++ (if null (PullPoll.snapshotLabels snap) then [] else [T.intercalate ", " (PullPoll.snapshotLabels snap)])
            ++ (if PullPoll.pullHeadDraft h == PullPoll.Draft then ["draft"] else [])
        emacs = Config.cfgKeys (Types.stConfig st) == Config.KeysEmacs
        checksKey = if emacs then "    C-c k lists them" else "    c lists them"
        conversationKeys = if emacs then "conversation \183 C-n C-p move \183 C-c [ ] threads awaiting you \183 C-c g comment on the PR" else "conversation \183 j/k move \183 [ ] threads awaiting you \183 @ comment on the PR"
      in
        [ Row Blank [Markdown.Segment Attrs.trayFileAttr (PullPoll.pullHeadTitle h) Nothing] Nothing Nothing
        , hint Blank (T.intercalate " \183 " parts)
        , blank {rowGutter = Blank}
        ]
          ++ descriptionRows st width snap
          ++ [ blank {rowGutter = Blank}
             , Row Blank [Markdown.Segment Attrs.hintAttr "checks  " Nothing, Markdown.Segment Attrs.msgAttr (checksSummary snap) Nothing, Markdown.Segment Attrs.hintAttr checksKey Nothing] Nothing Nothing
             , blank {rowGutter = Blank}
             , hint Blank conversationKeys
             ]

descriptionRows :: Types.St -> Int -> PullPoll.Snapshot -> [Row]
descriptionRows st width snap =
  let
    body = PullPoll.pullHeadBody (PullPoll.snapshotHead snap)
    at = Just Types.DescriptionStop
    rendered
      | T.null (T.strip body) = [Row Blank [Markdown.Segment Attrs.hintAttr "no description provided" Nothing] at Nothing]
      | otherwise = map (\r -> Row Blank (Markdown.rowSegments r) at (Just r)) (Markdown.render Markdown.Folded (width - 4) (Conv.Markdown body))
    reactions = case PullPoll.snapshotReactions snap of
      [] -> []
      counts -> [Row Blank (Threads.reactionSegs counts) at Nothing]
    react = if Config.cfgKeys (Types.stConfig st) == Config.KeysEmacs then "C-c + react" else "+ react"
    strip = if selectedStop st == at then [(keyStrip Types.DescriptionStop react) {rowGutter = Blank}] else []
  in
    rendered ++ reactions ++ strip

checksSummary :: PullPoll.Snapshot -> T.Text
checksSummary snap =
  let
    rows = PullPoll.snapshotCheckRows snap
    counted result = length (filter ((== result) . PullPoll.checkResult) rows)
    passed = counted PullPoll.CheckPassed
    failed = counted PullPoll.CheckFailed
    pending = counted PullPoll.CheckPending
    mergeable = case (GitHub.endedLabel (PullPoll.pullHeadState (PullPoll.snapshotHead snap)), PullPoll.snapshotMergeable snap) of
      (Just label, _) -> " \183 " <> label
      (Nothing, "MERGEABLE") -> " \183 mergeable"
      (Nothing, "CONFLICTING") -> " \183 conflicts"
      (Nothing, _) -> ""
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
    CommitsEntry author commits ->
      authorDot st author (Just (PullPoll.pullCommitCommittedAt (NEL.last commits))) (Just ("added " <> countText (length commits) "commit"))
        : concatMap (commitRows st width) (NEL.toList commits)
        ++ [blank]
    ForcePushEntry author push ->
      let
        moved = "force-pushed " <> T.take 7 (PullPoll.forcePushBefore push) <> " \8594 " <> T.take 7 (PullPoll.forcePushAfter push)
      in
        [authorDot st author (Just (PullPoll.forcePushAt push)) (Just moved), blank]

dotRow :: Types.St -> Conv.Post -> Maybe T.Text -> [Row]
dotRow st post event = [authorDot st (Conv.postAuthor post) (postedAt post) event]

authorDot :: Types.St -> Conv.Author -> Maybe T.Text -> Maybe T.Text -> Row
authorDot st author at event =
  Row
    Dot
    ( [ Markdown.Segment (Threads.authorInk author) (Threads.authorName author) Nothing
      , Markdown.Segment Attrs.hintAttr (" \183 " <> maybe "" (Threads.ageText (Types.stNow st)) at) Nothing
      ]
        ++ maybe [] (\e -> [Markdown.Segment Attrs.hintAttr (" \183 " <> e) Nothing]) event
    )
    Nothing
    Nothing

commitRows :: Types.St -> Int -> PullPoll.PullCommit -> [Row]
commitRows st width commit =
  let
    sha = PullPoll.pullCommitSha commit
    stop = Types.CommitStop sha
    body = PullPoll.pullCommitBody commit
    opened = Set.member sha (Types.stOpenCommits st)
    folded = if T.null body || opened then [] else [Markdown.Segment Attrs.hintAttr " \8230" Nothing]
    subject =
      Row
        Rail
        ( [ Markdown.Segment Attrs.msgAttr "  " Nothing
          , Markdown.Segment Attrs.rebaseShaAttr (T.take 7 sha) Nothing
          , Markdown.Segment Attrs.msgAttr ("  " <> PullPoll.pullCommitSubject commit) Nothing
          ]
            ++ folded
        )
        (Just stop)
        Nothing
    messageRow r = Row Rail [Markdown.Segment Attrs.msgAttr (T.replicate 11 " " <> SoftWrap.srText r) Nothing] (Just stop) Nothing
    message = if opened then map messageRow (SoftWrap.softRows (max 20 (width - 15)) (T.lines (T.replace "\t" "    " body))) else []
    fold = if Config.cfgKeys (Types.stConfig st) == Config.KeysEmacs then "C-c z" else "za"
    messageKey
      | T.null body = []
      | opened = [fold <> " hides the message"]
      | otherwise = [fold <> " shows the message"]
    strip = if selectedStop st == Just stop then [keyStrip stop (T.intercalate " \183 " (messageKey ++ ["\9166 opens the commit"]))] else []
  in
    subject : message ++ strip

keyStrip :: Types.Stop -> T.Text -> Row
keyStrip stop keys = Row Rail [Markdown.Segment Attrs.hintAttr ("  \9584 " <> keys) Nothing] (Just stop) Nothing

{- | A post's body under its dot, with its reactions and, when the band
is on it, its keys.
-}
postBody :: Types.St -> Int -> Conv.ConversationId -> Int -> Conv.Post -> [Row]
postBody st width cid i post =
  let
    rendered = Markdown.render (Threads.detailsFold st cid i) (width - 6) (Conv.postBody post)
    at = case Conv.postState post of
      Conv.Local _ -> Nothing
      _ -> Just (Types.PostStop cid i)
    body = map (\r -> Row Rail (Markdown.Segment Attrs.msgAttr "  " Nothing : Markdown.rowSegments r) at (Just r)) rendered
    reactions = case Conv.postReactions post of
      [] -> []
      counts -> [Row Rail (Markdown.Segment Attrs.msgAttr "  " Nothing : Threads.reactionSegs counts) at Nothing]
  in
    body ++ reactions ++ stripRow st cid i post

stripRow :: Types.St -> Conv.ConversationId -> Int -> Conv.Post -> [Row]
stripRow st cid i post =
  case Types.currentBand st of
    Just band
      | Types.bandConv band == cid && Types.bandPost band == i ->
          let
            venue = maybe Conv.PullComments Conv.convVenue (Conv.conversationFor cid (Types.stConversations st))
            keys = Config.cfgKeys (Types.stConfig st)
          in
            [keyStrip (Types.PostStop cid i) (Threads.postKeys keys (Types.flagRule st) venue post)]
    _ -> []

{- | A review thread under the review that started it: its place and
state, the diff lines of a range, then its log.
-}
threadRows :: Types.St -> Int -> Conv.Conversation -> [Row]
threadRows st width conv =
  let
    cid = Conv.convId conv
    header = Row Branch [Markdown.Segment Attrs.hintAttr " \8618 " Nothing, Markdown.Segment Attrs.trayFileAttr (placeOf st conv) Nothing, Markdown.Segment Attrs.hintAttr (" \183 " <> Threads.threadState st conv) Nothing] Nothing Nothing
    folded = Threads.isFolded st conv
    (mark, markAttr) = Threads.foldedMark st conv
    foldedLine =
      Row
        Branch
        [ Markdown.Segment markAttr (" " <> mark) Nothing
        , Markdown.Segment Attrs.hintAttr (" \183 " <> placeOf st conv <> " \183 " <> Types.tshow (length (Conv.convPosts conv)) <> " comments \183 za opens") Nothing
        ]
        (Just (Types.PostStop cid 0))
        Nothing
    foldedStrip = maybe [] (stripRow st cid 0) (Maybe.listToMaybe (Conv.convPosts conv))
    rangeLines = map excerptRow (rangeOf st conv)
    logRows = map (\tl -> Row Rail (Markdown.Segment Attrs.msgAttr "  " Nothing : Threads.tlSegs tl) (fmap (Types.PostStop cid) (Threads.tlPost tl)) Nothing) (threadLog st width conv)
  in
    if folded
      then foldedLine : foldedStrip ++ [blank]
      else header : rangeLines ++ logRows ++ [blank]

noteDirection :: Conv.Conversation -> T.Text
noteDirection conv =
  case fmap Conv.postAuthor (Maybe.listToMaybe (Conv.convPosts conv)) of
    Just (Conv.Agent _) -> "note from the agent"
    _ -> "note to the agent"

conversationRows :: Types.St -> Int -> Conv.Conversation -> [Row]
conversationRows st width conv =
  let
    cid = Conv.convId conv
    (lead, state) = case Conv.convVenue conv of
      Conv.LocalOnly _ -> (" \9550 ", noteDirection conv)
      _ -> (" \8618 ", Threads.threadState st conv)
    header = Row Branch [Markdown.Segment Attrs.hintAttr lead Nothing, Markdown.Segment Attrs.trayFileAttr (placeOf st conv) Nothing, Markdown.Segment Attrs.hintAttr (" \183 " <> state) Nothing] Nothing Nothing
    logRows = map (\tl -> Row Rail (Markdown.Segment Attrs.msgAttr "  " Nothing : Threads.tlSegs tl) (fmap (Types.PostStop cid) (Threads.tlPost tl)) Nothing) (threadLog st width conv)
    postRows (i, post) = dotRow st post Nothing ++ postBody st width cid i post ++ [blank]
  in
    case Conv.convAnchor conv of
      Conv.OnPull -> concatMap postRows (zip [0 ..] (Conv.convPosts conv))
      _ -> header : map excerptRow (rangeOf st conv) ++ logRows

{- | The thread's posts as the body draws them, without the header the
timeline already has.
-}
threadLog :: Types.St -> Int -> Conv.Conversation -> [Threads.ThreadLine]
threadLog st width = Threads.postLines st (width - 4)

data Mark = Context | Added | Removed
  deriving (Eq)

data ExcerptLine = ExcerptLine
  { exMark :: Mark
  , exNumber :: Maybe Int
  , exText :: T.Text
  , exToks :: Maybe [Highlight.Seg]
  }

{- | The diff lines a thread sits on. A posted thread's come from the hunk
GitHub keeps with its first comment, which ends at the comment's line as
it was when written, so the excerpt counts back from there and renumbers
to where the thread sits now; an outdated thread keeps the numbers it
had.
-}
rangeOf :: Types.St -> Conv.Conversation -> [ExcerptLine]
rangeOf st conv =
  case Conv.convId conv of
    Conv.ThreadConversation tid -> Maybe.fromMaybe [] $ do
      thread <- remoteThread st tid
      let
        (side, start, end) = case Conv.remoteThreadWhere thread of
          Right pos -> (Conv.posSide pos, Maybe.fromMaybe (Conv.posLine pos) (Conv.posStartLine pos), Conv.posLine pos)
          Left out -> (Conv.outSide out, Maybe.fromMaybe (Conv.outLine out) (Conv.outStartLine out), Conv.outLine out)
      excerpt side start end (Map.lookup tid (Types.ppExcerptToks (Types.stPullPoll st))) (Conv.remoteCommentDiffHunk (NEL.head (Conv.remoteThreadComments thread)))
    _ -> case Conv.convAnchor conv of
      Conv.OnLines lineSpan -> map (spanExcerpt st lineSpan) (NEL.toList (Conv.spanLines lineSpan))
      _ -> []

remoteThread :: Types.St -> Store.RemoteThreadId -> Maybe Conv.RemoteThread
remoteThread st tid = do
  snap <- Types.ppSnapshot (Types.stPullPoll st)
  List.find ((== tid) . Conv.remoteThreadId) (Conv.remoteThreads (PullPoll.snapshotRemote snap))

{- | Where a thread sits on GitHub, which the PR view names the same way
whatever the checkout holds.
-}
placeOf :: Types.St -> Conv.Conversation -> T.Text
placeOf st conv =
  Threads.anchorPlace $ case Conv.convId conv of
    Conv.ThreadConversation tid -> maybe (Conv.convAnchor conv) Conv.threadAnchor (remoteThread st tid)
    _ -> Conv.convAnchor conv

spanExcerpt :: Types.St -> Conv.LineSpan -> Conv.SpanLine -> ExcerptLine
spanExcerpt st lineSpan line =
  let
    (mark, content) = case T.uncons (Conv.slText line) of
      Just ('+', rest) -> (Added, rest)
      Just ('-', rest) -> (Removed, rest)
      Just (_, rest) -> (Context, rest)
      Nothing -> (Context, "")
    onSide = case (Conv.spanSide lineSpan, mark) of
      (Conv.HeadSide, Removed) -> False
      (Conv.BaseSide, Added) -> False
      _ -> True
    toks = do
      fv <- List.find ((== Conv.spanPath lineSpan) . T.pack . Types.fvPath) (Types.stFiles st)
      segs <- Types.safeIdx (Types.fvToks fv) (Conv.slLineNo line - 1)
      if Conv.spanSide lineSpan == Conv.HeadSide && onSide && T.concat (map snd segs) == content then Just segs else Nothing
  in
    ExcerptLine mark (if onSide then Just (Conv.slLineNo line) else Nothing) content toks

excerpt :: Conv.Side -> Int -> Int -> Maybe [[Highlight.Seg]] -> T.Text -> Maybe [ExcerptLine]
excerpt side start end toks hunk =
  case T.lines hunk of
    header : _ -> do
      (base0, head0) <- hunkStarts header
      let
        numbered = number base0 head0 (hunkBody hunk)
        tokensAt i content = do
          segs <- toks >>= (`Types.safeIdx` i)
          if T.concat (map snd segs) == content then Just segs else Nothing
        onSide (i, (mark, base, headNo, content)) =
          let
            n = case side of
              Conv.BaseSide -> if mark == Added then Nothing else base
              Conv.HeadSide -> if mark == Removed then Nothing else headNo
          in
            ExcerptLine mark n content (tokensAt i content)
        sided = map onSide (zip [0 ..] numbered)
        backTo :: Int -> [ExcerptLine] -> [ExcerptLine]
        backTo left ls =
          case ls of
            l : rest
              | left > 0 -> l : backTo (if Maybe.isJust (exNumber l) then left - 1 else left) rest
            _ -> []
        picked = reverse (backTo (end - start + 1) (reverse sided))
      lastNo <- Maybe.listToMaybe (reverse (Maybe.mapMaybe exNumber picked))
      Just (map (\l -> l {exNumber = fmap (+ (end - lastNo)) (exNumber l)}) picked)
    [] -> Nothing

hunkBody :: T.Text -> [T.Text]
hunkBody hunk = filter (not . T.isPrefixOf "\\") (drop 1 (T.lines hunk))

hunkStarts :: T.Text -> Maybe (Int, Int)
hunkStarts header =
  case T.words header of
    "@@" : base : headSide : _ -> (,) <$> startOf '-' base <*> startOf '+' headSide
    _ -> Nothing
 where
  startOf sign t = do
    (c, rest) <- T.uncons t
    if c == sign then readInt (T.takeWhile (/= ',') rest) else Nothing
  readInt t = case reads (T.unpack t) of
    [(n, "")] -> Just n
    _ -> Nothing

number :: Int -> Int -> [T.Text] -> [(Mark, Maybe Int, Maybe Int, T.Text)]
number base headNo ls =
  case ls of
    [] -> []
    l : rest ->
      let
        content = T.drop 1 l
      in
        case T.take 1 l of
          "+" -> (Added, Nothing, Just headNo, content) : number base (headNo + 1) rest
          "-" -> (Removed, Just base, Nothing, content) : number (base + 1) headNo rest
          _ -> (Context, Just base, Just headNo, content) : number (base + 1) (headNo + 1) rest

excerptRow :: ExcerptLine -> Row
excerptRow line =
  let
    numberCol = maybe "    " (\n -> T.justifyRight 3 ' ' (Types.tshow n) <> " ") (exNumber line)
    sign = case exMark line of
      Added -> Markdown.Segment Attrs.addedMarkAttr "+ " Nothing
      Removed -> Markdown.Segment Attrs.removedMarkAttr "- " Nothing
      Context -> Markdown.Segment Attrs.mdCodeAttr "  " Nothing
    code = case exMark line of
      Removed -> [Markdown.Segment Attrs.removedTextAttr (exText line) Nothing]
      _ -> map (\(attr, t) -> Markdown.Segment attr t Nothing) (Maybe.fromMaybe [(Attrs.mdCodeAttr, exText line)] (exToks line))
  in
    Row
      Rail
      (Markdown.Segment Attrs.mdCodeAttr " " Nothing : Markdown.Segment Attrs.lineNrAttr numberCol Nothing : sign : code)
      Nothing
      (Just (Markdown.Row (Just Attrs.mdCodeAttr) []))

{- | Your pending comments on the pull request and the agent's proposals
for it, then the review your line comments will go out in, with the
agent's proposed line comments beside them; pending replies sit inside
their threads already.
-}
draftRows :: Types.St -> Int -> [Conv.Conversation] -> [Row]
draftRows st width convs =
  let
    newThreads = Maybe.mapMaybe (\c -> case Conv.convVenue c of Conv.ReviewThread Nothing _ -> Just c; _ -> Nothing) convs
    unsent c = Maybe.mapMaybe (\(i, p) -> if Conv.isUnsent p then Just (c, i, p) else Nothing) (zip [0 ..] (Conv.convPosts c))
    pullDrafts = maybe [] unsent (List.find ((== Conv.PullConversation) . Conv.convId) convs)
    heading p = case Conv.postState p of
      Conv.Proposal name _ ->
        [ Markdown.Segment Attrs.guideAttr ("\10022 " <> Conv.agentNameToText name) Nothing
        , Markdown.Segment Attrs.guideAttr " \183 proposes" Nothing
        , Markdown.Segment Attrs.hintAttr " \183 yours to take or drop" Nothing
        ]
      _ ->
        [ Markdown.Segment Attrs.noteInlineAttr "you" Nothing
        , Markdown.Segment Attrs.questionMarkAttr " \183 draft" Nothing
        , Markdown.Segment Attrs.hintAttr " \183 sends with :submit" Nothing
        ]
    draft (c, i, p) =
      Row Pending (heading p) Nothing Nothing
        : postBody st width (Conv.convId c) i p
        ++ [blank]
    posts = concatMap Conv.convPosts newThreads
    count label n = if n == 0 then [] else [Types.tshow n <> " " <> label]
    tally =
      count (if drafted == 1 then "comment" else "comments") drafted
        ++ count "proposed" (length (filter Threads.isProposal posts))
    drafted = length (filter ((== Conv.Draft) . Conv.postState) posts)
    review =
      Row
        Pending
        [ Markdown.Segment Attrs.noteInlineAttr "you" Nothing
        , Markdown.Segment Attrs.questionMarkAttr " \183 pending review" Nothing
        , Markdown.Segment Attrs.hintAttr (T.concat (map (" \183 " <>) (tally ++ ["sends with :submit"]))) Nothing
        ]
        Nothing
        Nothing
  in
    concatMap draft pullDrafts
      ++ (if null newThreads then [] else review : concatMap (threadRows st width) newThreads ++ [blank])

footerRows :: Types.St -> [Row]
footerRows st =
  let
    pending = length (Conv.pendingOf (Types.stWork st))
  in
    if pending == 0
      then []
      else [hint Blank ("drafts \183 " <> Types.tshow pending <> " to send \183 :submit sends")]
