{-# LANGUAGE OverloadedStrings #-}

module Remax.Feedback
  ( Listed (..)
  , ListedLine (..)
  , Party (..)
  , partyText
  , parseParty
  , listedParty
  , listFor
  , digestOf
  , Reply (..)
  , replyTo
  , commentOnHunk
  , proposeOn
  , listedJson
  , agentName
  ) where

import Control.Monad qualified as Monad
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Ord qualified as Ord
import Data.Text qualified as T
import Database.SQLite.Simple qualified as SQLite
import Remax.Conversation qualified as Conv
import Remax.Git qualified as Git
import Remax.Hunks qualified as Hunks
import Remax.Json qualified as Json
import Remax.Key qualified as Key
import Remax.RemoteCache qualified as RemoteCache
import Remax.Store qualified as Store
import Remax.WorkStore qualified as WorkStore

agentName :: Conv.AgentName
agentName = Conv.AgentName "claude"

data ListedLine = ListedLine
  { listedLineNo :: Maybe Int
  , listedLineText :: T.Text
  , listedLineVerdict :: Maybe Store.Verdict
  }
  deriving (Eq, Show)

{- | One conversation as the agent sees it: a GitHub thread, a note, or
a line with a verdict and nothing said about it yet.
-}
data Listed = Listed
  { listedId :: T.Text
  , listedRepo :: FilePath
  , listedKind :: T.Text
  , listedFile :: Maybe T.Text
  , listedLines :: [ListedLine]
  , listedPosts :: [Conv.Post]
  , listedCommit :: Maybe T.Text
  , listedUpdatedAt :: T.Text
  , listedTarget :: Maybe Conv.Target
  , listedVerdictKeys :: [Store.LineKey]
  }
  deriving (Eq, Show)

{- | Whose turn it is: the agent's when the newest post is the
reviewer's or a GitHub reviewer's, the reviewer's after the agent
spoke, and nobody's on a verdict with no note yet.
-}
data Party = AwaitingNote | AwaitingAgent | AwaitingReviewer
  deriving (Eq, Show)

partyText :: Party -> T.Text
partyText p =
  case p of
    AwaitingNote -> "note"
    AwaitingAgent -> "agent"
    AwaitingReviewer -> "reviewer"

parseParty :: T.Text -> Maybe Party
parseParty t =
  case t of
    "note" -> Just AwaitingNote
    "agent" -> Just AwaitingAgent
    "reviewer" -> Just AwaitingReviewer
    _ -> Nothing

listedParty :: Listed -> Party
listedParty l =
  case newestPost l of
    Nothing -> AwaitingNote
    Just newest -> case Conv.postAuthor newest of
      Conv.Agent _ -> AwaitingReviewer
      _ -> AwaitingAgent

newestPost :: Listed -> Maybe Conv.Post
newestPost =
  fmap (List.maximumBy (Ord.comparing (\p -> Conv.storedId =<< Conv.postWork p))) . NEL.nonEmpty . listedPosts

{- | Everything to list for a repository: the conversations the session
draws, from the store and the last poll's cache, then the lines with a
verdict that no conversation covers.
-}
listFor :: SQLite.Connection -> FilePath -> IO [Listed]
listFor conn root = fmap snd (listScoped conn root)

listScoped :: SQLite.Connection -> FilePath -> IO (Maybe Int, [Listed])
listScoped conn root = do
  let
    repo = T.pack root
  cached <- RemoteCache.readRemote repo
  let
    (pull, remote) = case cached of
      Just c -> (Just (RemoteCache.cachedPull c), RemoteCache.cachedRemote c)
      Nothing -> (Nothing, Conv.emptyRemote)
  own <- WorkStore.loadWork conn WorkStore.Scope {WorkStore.scopeRepo = repo, WorkStore.scopePull = pull}
  rows <- Store.punchRows conn repo
  let
    convs = filter speaks (Conv.conversations remote own)
    speaks conv = case Conv.convVenue conv of
      Conv.ReviewSummary _ state -> state /= "COMMENTED" || any (not . T.null . T.strip . Conv.markdownText . Conv.postBody) (Conv.convPosts conv)
      _ -> True
  texts <- positionTexts root pull convs
  let
    listedConvs = map (fromConversation root rows texts) convs
    covered = Map.fromList (map (\k -> (k, ())) (concatMap listedVerdictKeys listedConvs))
    bare = filter (\r -> Map.notMember (Store.punchKey r) covered) rows
    bareGroups = Map.fromListWith (flip (++)) (map (\r -> ((Store.punchFile r, Store.punchCommitSha r), [r])) bare)
  pure (pull, listedConvs ++ map (bareListed root) (Map.elems bareGroups))

positionTexts :: FilePath -> Maybe Int -> [Conv.Conversation] -> IO (Map.Map (Conv.Side, T.Text) [T.Text])
positionTexts root pull convs =
  case pull of
    Nothing -> pure Map.empty
    Just n -> do
      let
        wanted = List.nub (Maybe.mapMaybe (\c -> case Conv.convAnchor c of Conv.OnPosition pos -> Just (Conv.posSide pos, Conv.posPath pos); _ -> Nothing) convs)
        refOf side = if side == Conv.HeadSide then Git.pullHeadRef n else Git.pullBaseRef n
      found <- Monad.forM wanted $ \(side, path) -> do
        got <- Git.refContent root (refOf side) (T.unpack path)
        pure $ case got of
          Just (Git.RefText ls) -> Just ((side, path), ls)
          _ -> Nothing
      pure (Map.fromList (Maybe.catMaybes found))

fromConversation :: FilePath -> [Store.PunchRow] -> Map.Map (Conv.Side, T.Text) [T.Text] -> Conv.Conversation -> Listed
fromConversation root rows texts conv =
  let
    textAt pos n = Maybe.fromMaybe "" (Maybe.listToMaybe (drop (n - 1) (Map.findWithDefault [] (Conv.posSide pos, Conv.posPath pos) texts)))
    verdictOf k = Maybe.listToMaybe (map Store.punchVerdict (filter ((== k) . Store.punchKey) rows))
    (file, lines_, keys, commit) = case Conv.convAnchor conv of
      Conv.OnLines lineSpan ->
        ( Just (Conv.spanPath lineSpan)
        , map (\l -> ListedLine (Just (Conv.slLineNo l)) (Conv.slText l) (verdictOf (Conv.slKey l))) (NEL.toList (Conv.spanLines lineSpan))
        , Conv.spanKeys lineSpan
        , fmap Conv.commitShaToText (Conv.spanAt lineSpan)
        )
      Conv.OnPosition pos ->
        ( Just (Conv.posPath pos)
        , map (\n -> ListedLine (Just n) (textAt pos n) Nothing) [Maybe.fromMaybe (Conv.posLine pos) (Conv.posStartLine pos) .. Conv.posLine pos]
        , []
        , Nothing
        )
      Conv.OnOutdated out -> (Just (Conv.outPath out), [ListedLine (Just (Conv.outLine out)) "" Nothing], [], Just (Conv.commitShaToText (Conv.outCommit out)))
      Conv.OnPull -> (Nothing, [], [], Nothing)
    kind = case Conv.convVenue conv of
      Conv.ReviewThread (Just _) _ -> "thread"
      Conv.ReviewThread Nothing _ -> "draft"
      Conv.PullComments -> "pull"
      Conv.ReviewSummary _ _ -> "review"
      Conv.LocalOnly _ -> "note"
    updated = Maybe.fromMaybe "" (Maybe.listToMaybe (reverse (Maybe.mapMaybe postTime (Conv.convPosts conv))))
  in
    Listed
      { listedId = Conv.conversationIdText (Conv.convId conv)
      , listedRepo = root
      , listedKind = kind
      , listedFile = file
      , listedLines = lines_
      , listedPosts = Conv.convPosts conv
      , listedCommit = commit
      , listedUpdatedAt = updated
      , listedTarget = Just (Conv.convTarget conv)
      , listedVerdictKeys = keys
      }

postTime :: Conv.Post -> Maybe T.Text
postTime p =
  case Conv.postState p of
    Conv.Posted _ at _ -> Just at
    Conv.Local at -> Just at
    _ -> Nothing

{- | Lines with a verdict and no note draw no conversation yet; they
list under the name a note on them would take.
-}
bareListed :: FilePath -> [Store.PunchRow] -> Listed
bareListed root rows =
  let
    keys = map (Store.lineKeyToText . Store.punchKey) rows
  in
    Listed
      { listedId = Key.noteGroupId (T.pack root) keys
      , listedRepo = root
      , listedKind = "note"
      , listedFile = fmap Store.punchFile (Maybe.listToMaybe rows)
      , listedLines = map (\r -> ListedLine (Store.punchLineNo r) (Store.punchLine r) (Just (Store.punchVerdict r))) rows
      , listedPosts = []
      , listedCommit = Store.punchCommitSha =<< Maybe.listToMaybe rows
      , listedUpdatedAt = maximum ("" : map Store.punchAt rows)
      , listedTarget = Nothing
      , listedVerdictKeys = map Store.punchKey rows
      }

{- | What a wait watches: each conversation's newest post, so a new one
from the reviewer or from GitHub wakes it.
-}
digestOf :: Maybe Party -> [Listed] -> [T.Text]
digestOf party listed =
  let
    wanted l = maybe True (== listedParty l) party
    line l =
      listedId l
        <> "\t"
        <> maybe "" (\p -> Conv.authorText (Conv.postAuthor p) <> "\t" <> Conv.markdownText (Conv.postBody p)) (newestPost l)
  in
    map line (filter wanted listed)

data Reply = Replied Int | NoSuchConversation

{- | The agent's local post on a conversation named by its id. A verdict
with no note yet takes a fresh note on its lines.
-}
replyTo :: SQLite.Connection -> FilePath -> T.Text -> T.Text -> IO Reply
replyTo conn root target text = do
  (pull, listed) <- listScoped conn root
  let
    repo = T.pack root
    scope = WorkStore.Scope {WorkStore.scopeRepo = repo, WorkStore.scopePull = pull}
    body = Conv.Markdown text
  case List.find ((== target) . listedId) listed of
    Nothing -> pure NoSuchConversation
    Just l ->
      case listedTarget l of
        Just t -> do
          _ <- WorkStore.addAside conn scope t (Conv.Agent agentName) body
          pure (Replied (max 1 (length (listedLines l))))
        Nothing ->
          case (listedFile l, NEL.nonEmpty (Maybe.mapMaybe spanLine (zip (listedVerdictKeys l) (listedLines l)))) of
            (Just file, Just spanLines) -> do
              let
                verdict = Maybe.fromMaybe Store.Questioned (Maybe.listToMaybe (Maybe.mapMaybe listedLineVerdict (listedLines l)))
                lineSpan = Conv.LineSpan {Conv.spanPath = file, Conv.spanSide = Conv.HeadSide, Conv.spanLines = spanLines, Conv.spanAt = fmap Conv.CommitSha (listedCommit l)}
              _ <- WorkStore.addAside conn scope (Conv.OnLinesOnly (Conv.LocalId target) lineSpan verdict) (Conv.Agent agentName) body
              pure (Replied (length (listedLines l)))
            _ -> pure NoSuchConversation

spanLine :: (Store.LineKey, ListedLine) -> Maybe Conv.SpanLine
spanLine (key, line) = do
  n <- listedLineNo line
  Just (Conv.SpanLine key n (listedLineText line))

{- | A comment the agent proposes on a hunk, which the reviewer takes as
their own before it can be submitted.
-}
commentOnHunk :: SQLite.Connection -> FilePath -> Maybe Int -> Hunks.Hunk -> T.Text -> IO Conv.LocalId
commentOnHunk conn root pull hunk text = do
  let
    repo = T.pack root
    scope = WorkStore.Scope {WorkStore.scopeRepo = repo, WorkStore.scopePull = pull}
    numbers = zip [0 :: Int ..] (Hunks.hunkNumbers hunk)
    heads = Maybe.mapMaybe (\(i, n) -> case n of Hunks.OnHead h -> Just (i, h); _ -> Nothing) numbers
    side = if null heads then Conv.BaseSide else Conv.HeadSide
    lineNo (i, n) = case n of
      Hunks.OnHead h -> h
      Hunks.OnBase b
        | side == Conv.BaseSide -> b
        | otherwise -> Maybe.fromMaybe b (Maybe.listToMaybe (map snd (filter ((> i) . fst) heads) ++ reverse (map snd (filter ((< i) . fst) heads))))
    spanLines = zipWith (\n (k, l) -> Conv.SpanLine (Store.LineKey k) (lineNo n) l) numbers (zip (Hunks.hunkKeys hunk) (Hunks.hunkLines hunk))
  lid <- WorkStore.newLocalId repo (map Store.LineKey (Hunks.hunkKeys hunk))
  case NEL.nonEmpty spanLines of
    Nothing -> pure lid
    Just ne -> do
      let
        lineSpan = Conv.LineSpan {Conv.spanPath = T.pack (Hunks.hunkFile hunk), Conv.spanSide = side, Conv.spanLines = ne, Conv.spanAt = Nothing}
      _ <- WorkStore.setProposal conn scope (Conv.NewThread lid (Conv.OnLines lineSpan)) agentName (Conv.Markdown text)
      pure lid

-- | The agent's proposed text on a conversation; the next replaces it.
proposeOn :: SQLite.Connection -> FilePath -> T.Text -> T.Text -> IO Reply
proposeOn conn root target text = do
  (pull, listed) <- listScoped conn root
  let
    scope = WorkStore.Scope {WorkStore.scopeRepo = T.pack root, WorkStore.scopePull = pull}
  case List.find ((== target) . listedId) listed >>= listedTarget of
    Nothing -> pure NoSuchConversation
    Just t -> do
      _ <- WorkStore.setProposal conn scope t agentName (Conv.Markdown text)
      pure (Replied 1)

listedJson :: Listed -> Json.Json
listedJson l =
  let
    str = Json.JString
    lineJson line =
      Json.JObject
        [ ("line_no", maybe Json.JNull (Json.JNumber . T.pack . show) (listedLineNo line))
        , ("line", str (listedLineText line))
        , ("verdict", maybe Json.JNull (str . Store.verdictText) (listedLineVerdict line))
        ]
    postJson p =
      Json.JObject
        [ ("author", str (authorWord (Conv.postAuthor p)))
        , ("body", str (Conv.markdownText (Conv.postBody p)))
        , ("state", str (stateWord (Conv.postState p)))
        ]
  in
    Json.JObject
      [ ("id", str (listedId l))
      , ("repo", str (T.pack (listedRepo l)))
      , ("kind", str (listedKind l))
      , ("file", maybe Json.JNull str (listedFile l))
      , ("commit_sha", maybe Json.JNull str (listedCommit l))
      , ("lines", Json.JArray (map lineJson (listedLines l)))
      , ("thread", Json.JArray (map postJson (listedPosts l)))
      , ("awaiting", str (partyText (listedParty l)))
      , ("updated_at", str (listedUpdatedAt l))
      ]

authorWord :: Conv.Author -> T.Text
authorWord a =
  case a of
    Conv.Viewer -> "reviewer"
    Conv.Person (Conv.Login login) -> login
    Conv.Agent (Conv.AgentName n) -> n

stateWord :: Conv.PostState -> T.Text
stateWord s =
  case s of
    Conv.Posted {} -> "posted"
    Conv.Draft -> "pending"
    Conv.Proposal _ -> "proposal"
    Conv.Local _ -> "local"
