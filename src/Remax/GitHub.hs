{-# LANGUAGE OverloadedStrings #-}

module Remax.GitHub
  ( GhFailure (..)
  , failureText
  , Conditional (..)
  , Response (..)
  , conditionalGet
  , graphql
  , viewerLogin
  , pullNumber
  , RepoName (..)
  , cachedRepoName
  , rememberRepoName
  , resolveRepo
  , Checks (..)
  , checksText
  , ReviewState (..)
  , PullRow (..)
  , listPulls
  , PullMeta (..)
  , viewPull
  , Opened (..)
  , openedMode
  , PullStart (..)
  , Authorship (..)
  , pullStart
  , startedMode
  , openPull
  , checkoutPull
  , checksFromRollup
  , tsvRows
  , RefsMove (..)
  , refreshPullRefs
  , Kept (..)
  , OnGitHub (..)
  , PushedHead (..)
  , pushedHeadState
  , keepPushedHead
  , setThreadResolution
  , CommentKind (..)
  , addReaction
  , removeReaction
  , editComment
  , markReady
  , markDraft
  , InlineComment (..)
  , ReviewEvent (..)
  , reviewEventText
  , ReviewRequest (..)
  , SubmittedReview (..)
  , PostedComment (..)
  , postReview
  , reviewComments
  , postReply
  , postIssueComment
  ) where

import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Database.SQLite.Simple qualified as SQLite
import Remax.Conversation qualified as Conv
import Remax.Git qualified as Git
import Remax.Json qualified as Json
import Remax.Store qualified as Store
import System.Environment qualified as Env
import System.Exit qualified as Exit
import System.Process qualified as Process

data GhFailure
  = GhMissing
  | GhLoggedOut
  | GhOffline T.Text
  | GhNotFound T.Text
  | GhFailed T.Text
  | GhRefused Int T.Text
  deriving (Eq, Show)

failureText :: GhFailure -> T.Text
failureText failure =
  case failure of
    GhMissing -> "gh is not installed \8212 remax reaches GitHub through the GitHub CLI"
    GhLoggedOut -> "gh is not logged in \8212 run gh auth login"
    GhOffline why -> "cannot reach GitHub \8212 check the network (" <> why <> ")"
    GhNotFound why -> "not found on GitHub: " <> why
    GhFailed why -> "gh: " <> why
    GhRefused status why -> "GitHub " <> T.pack (show status) <> " \183 " <> why

runGh :: FilePath -> [T.Text] -> IO (Either GhFailure T.Text)
runGh root args = do
  got <- runGhOutput root args
  pure $ case got of
    Left failure -> Left failure
    Right (Exit.ExitSuccess, out, _) -> Right out
    Right (Exit.ExitFailure _, _, err) -> Left (classify (T.strip err))

runGhOutput :: FilePath -> [T.Text] -> IO (Either GhFailure (Exit.ExitCode, T.Text, T.Text))
runGhOutput root args = runGhInput root args ""

runGhInput :: FilePath -> [T.Text] -> T.Text -> IO (Either GhFailure (Exit.ExitCode, T.Text, T.Text))
runGhInput root args input = do
  environment <- Env.getEnvironment
  let
    quiet = [("GH_PROMPT_DISABLED", "1"), ("GH_NO_UPDATE_NOTIFIER", "1")]
    p =
      (Process.proc "gh" (map T.unpack args))
        { Process.cwd = Just root
        , Process.env = Just (quiet ++ filter ((`notElem` map fst quiet) . fst) environment)
        }
  got <- Exception.try (Process.readCreateProcessWithExitCode p (T.unpack input))
  pure $ case got of
    Left e -> let _ = (e :: Exception.IOException) in Left GhMissing
    Right (code, out, err) -> Right (code, T.pack out, T.pack err)

classify :: T.Text -> GhFailure
classify err =
  let
    lower = T.toLower err
    mentions = any (`T.isInfixOf` lower)
  in
    case () of
      _
        | mentions ["gh auth login", "not logged into", "authentication required", "bad credentials", "http 401"] -> GhLoggedOut
        | mentions ["could not resolve host", "no such host", "dial tcp", "connection refused", "network is unreachable", "timeout"] -> GhOffline (firstLine err)
        | mentions ["http 404", "not found", "could not resolve to"] -> GhNotFound (firstLine err)
        | otherwise -> GhFailed (firstLine err)

firstLine :: T.Text -> T.Text
firstLine t = Maybe.fromMaybe "" (Maybe.listToMaybe (T.lines t))

data Conditional
  = NotModified
  | Modified Response
  deriving (Eq, Show)

data Response = Response
  { responseEtag :: Maybe T.Text
  , responseBody :: T.Text
  }
  deriving (Eq, Show)

conditionalGet :: FilePath -> T.Text -> Maybe T.Text -> Maybe T.Text -> IO (Either GhFailure Conditional)
conditionalGet root path etag jq = do
  let
    etagArgs = maybe [] (\e -> ["-H", "If-None-Match: " <> e]) etag
    jqArgs = maybe [] (\q -> ["--jq", q]) jq
  got <- runGhOutput root (["api", "-i"] ++ etagArgs ++ jqArgs ++ [path])
  pure $ case got of
    Left failure -> Left failure
    Right (Exit.ExitSuccess, out, _) -> Right (readConditional out)
    Right (Exit.ExitFailure _, out, err) ->
      case readConditional out of
        NotModified -> Right NotModified
        Modified _ -> Left (classify (T.strip err))

readConditional :: T.Text -> Conditional
readConditional out =
  let
    (headers, body) = T.breakOn "\n\n" (T.replace "\r\n" "\n" out)
    headerLines = T.lines headers
    status = case headerLines of
      (statusLine : _) -> Maybe.listToMaybe (drop 1 (T.words statusLine))
      [] -> Nothing
    header name =
      Maybe.listToMaybe
        [ T.strip value
        | l <- headerLines
        , let
            (key, rest) = T.breakOn ":" l
        , T.toLower (T.strip key) == name
        , let
            value = T.drop 1 rest
        ]
  in
    if status == Just "304"
      then NotModified
      else Modified (Response {responseEtag = header "etag", responseBody = T.drop 2 body})

graphql :: FilePath -> T.Text -> [(T.Text, T.Text)] -> Maybe T.Text -> IO (Either GhFailure T.Text)
graphql root query fields jq =
  let
    fieldArgs = concatMap (\(k, v) -> ["-F", k <> "=" <> v]) fields
    jqArgs = maybe [] (\q -> ["--jq", q]) jq
  in
    runGh root (["api", "graphql", "-f", "query=" <> query] ++ fieldArgs ++ jqArgs)

setThreadResolution :: FilePath -> Store.Resolution -> Store.RemoteThreadId -> IO (Either GhFailure ())
setThreadResolution root resolution tid =
  let
    mutation = case resolution of
      Store.Resolved -> "resolveReviewThread"
      Store.Unresolved -> "unresolveReviewThread"
  in
    fmap
      (fmap (const ()))
      ( graphql
          root
          ("mutation($thread: ID!) { " <> mutation <> "(input: {threadId: $thread}) { thread { isResolved } } }")
          [("thread", Store.remoteThreadIdToText tid)]
          Nothing
      )

{- | A review comment sits on a line of the diff; an issue comment is
PR-level. GitHub keeps them under different paths.
-}
data CommentKind = ReviewCommentKind | IssueCommentKind
  deriving (Eq, Show)

commentPath :: T.Text -> CommentKind -> Store.CommentId -> T.Text
commentPath repo kind cid =
  let
    middle = case kind of
      ReviewCommentKind -> "/pulls/comments/"
      IssueCommentKind -> "/issues/comments/"
  in
    "repos/" <> repo <> middle <> T.pack (show (Store.commentIdToInt64 cid))

addReaction :: FilePath -> T.Text -> CommentKind -> Store.CommentId -> Conv.Reaction -> IO (Either GhFailure ())
addReaction root repo kind cid reaction =
  fmap
    (fmap (const ()))
    (runGh root ["api", "-X", "POST", commentPath repo kind cid <> "/reactions", "-f", "content=" <> Conv.reactionText reaction, "--jq", ".id"])

{- | Taking a reaction back means finding the one you left: the list
names each reaction's author.
-}
removeReaction :: FilePath -> T.Text -> CommentKind -> Store.CommentId -> Conv.Login -> Conv.Reaction -> IO (Either GhFailure ())
removeReaction root repo kind cid (Conv.Login viewer) reaction = do
  let
    path = commentPath repo kind cid <> "/reactions"
    jq = ".[] | select(.user.login == \"" <> viewer <> "\" and .content == \"" <> Conv.reactionText reaction <> "\") | .id"
  found <- runGh root ["api", path <> "?per_page=100", "--jq", jq]
  case fmap (Maybe.listToMaybe . T.lines . T.strip) found of
    Left failure -> pure (Left failure)
    Right Nothing -> pure (Right ())
    Right (Just rid) -> fmap (fmap (const ())) (runGh root ["api", "-X", "DELETE", path <> "/" <> rid])

editComment :: FilePath -> T.Text -> CommentKind -> Store.CommentId -> T.Text -> IO (Either GhFailure ())
editComment root repo kind cid body =
  fmap
    (fmap (const ()))
    (runGh root ["api", "-X", "PATCH", commentPath repo kind cid, "-f", "body=" <> body, "--jq", ".id"])

data InlineComment = InlineComment
  { inlineCommentPath :: T.Text
  , inlineCommentLine :: Int
  , inlineCommentSide :: T.Text
  , inlineCommentStartLine :: Maybe Int
  , inlineCommentBody :: T.Text
  }
  deriving (Eq, Show)

data ReviewEvent = EventComment | EventApprove | EventRequestChanges
  deriving (Eq, Show)

reviewEventText :: ReviewEvent -> T.Text
reviewEventText e =
  case e of
    EventComment -> "COMMENT"
    EventApprove -> "APPROVE"
    EventRequestChanges -> "REQUEST_CHANGES"

data ReviewRequest = ReviewRequest
  { reviewRequestCommit :: T.Text
  , reviewRequestEvent :: ReviewEvent
  , reviewRequestBody :: T.Text
  , reviewRequestComments :: [InlineComment]
  }
  deriving (Eq, Show)

data SubmittedReview = SubmittedReview
  { submittedReviewId :: Store.ReviewId
  , submittedReviewAt :: T.Text
  }
  deriving (Eq, Show)

postReview :: FilePath -> T.Text -> Int -> ReviewRequest -> IO (Either GhFailure SubmittedReview)
postReview root repo n request = do
  got <- postJson root (pullPath repo n <> "/reviews") (reviewJson request) "[(.id | tostring), (.submitted_at // \"\")] | @tsv"
  pure $ case fmap tsvRows got of
    Left failure -> Left failure
    Right [[rid, at]]
      | Just k <- Git.readCount rid -> Right SubmittedReview {submittedReviewId = Store.ReviewId (fromIntegral k), submittedReviewAt = at}
    Right _ -> Left (GhFailed "unexpected answer to the review")

reviewJson :: ReviewRequest -> Json.Json
reviewJson request =
  let
    comment c =
      Json.JObject
        ( [ ("path", Json.JString (inlineCommentPath c))
          , ("line", jsonInt (inlineCommentLine c))
          , ("side", Json.JString (inlineCommentSide c))
          , ("body", Json.JString (inlineCommentBody c))
          ]
            ++ maybe [] (\s -> [("start_line", jsonInt s), ("start_side", Json.JString (inlineCommentSide c))]) (inlineCommentStartLine c)
        )
  in
    Json.JObject
      ( [ ("commit_id", Json.JString (reviewRequestCommit request))
        , ("event", Json.JString (reviewEventText (reviewRequestEvent request)))
        , ("comments", Json.JArray (map comment (reviewRequestComments request)))
        ]
          ++ (if T.null (reviewRequestBody request) then [] else [("body", Json.JString (reviewRequestBody request))])
      )

jsonInt :: Int -> Json.Json
jsonInt = Json.JNumber . T.pack . show

data PostedComment = PostedComment
  { postedCommentId :: Store.CommentId
  , postedCommentPath :: T.Text
  , postedCommentLine :: Maybe Int
  , postedCommentCreatedAt :: T.Text
  , postedCommentUpdatedAt :: T.Text
  }
  deriving (Eq, Show)

postedCommentJq :: T.Text
postedCommentJq =
  "[(.id | tostring), (.path // \"\"), ((.line // .original_line // \"\") | tostring), .created_at, .updated_at] | @tsv"

postedCommentRow :: [T.Text] -> Maybe PostedComment
postedCommentRow cells =
  case cells of
    [cid, path, line, created, updated] -> do
      k <- Git.readCount cid
      Just
        PostedComment
          { postedCommentId = Store.CommentId (fromIntegral k)
          , postedCommentPath = path
          , postedCommentLine = Git.readCount line
          , postedCommentCreatedAt = created
          , postedCommentUpdatedAt = updated
          }
    _ -> Nothing

{- | The comments a review posted, so each pending comment can be matched
to the id GitHub gave it.
-}
reviewComments :: FilePath -> T.Text -> Int -> Store.ReviewId -> IO (Either GhFailure [PostedComment])
reviewComments root repo n rid = do
  got <-
    runGh
      root
      [ "api"
      , "--paginate"
      , pullPath repo n <> "/comments?per_page=100"
      , "--jq"
      , ".[] | select(.pull_request_review_id == " <> T.pack (show (Store.reviewIdToInt64 rid)) <> ") | " <> postedCommentJq
      ]
  pure (fmap (Maybe.mapMaybe postedCommentRow . tsvRows) got)

postReply :: FilePath -> T.Text -> Int -> Store.CommentId -> T.Text -> IO (Either GhFailure PostedComment)
postReply root repo n cid body =
  postComment root (pullPath repo n <> "/comments/" <> T.pack (show (Store.commentIdToInt64 cid)) <> "/replies") body

postIssueComment :: FilePath -> T.Text -> Int -> T.Text -> IO (Either GhFailure PostedComment)
postIssueComment root repo n body =
  postComment root ("repos/" <> repo <> "/issues/" <> T.pack (show n) <> "/comments") body

postComment :: FilePath -> T.Text -> T.Text -> IO (Either GhFailure PostedComment)
postComment root path body = do
  got <- postJson root path (Json.JObject [("body", Json.JString body)]) postedCommentJq
  pure $ case fmap tsvRows got of
    Left failure -> Left failure
    Right [cells] | Just posted <- postedCommentRow cells -> Right posted
    Right _ -> Left (GhFailed ("unexpected answer from " <> path))

pullPath :: T.Text -> Int -> T.Text
pullPath repo n = "repos/" <> repo <> "/pulls/" <> T.pack (show n)

postJson :: FilePath -> T.Text -> Json.Json -> T.Text -> IO (Either GhFailure T.Text)
postJson root path payload jq = do
  got <- runGhInput root ["api", "-X", "POST", path, "--input", "-", "--jq", jq] (Json.encode payload)
  pure $ case got of
    Left failure -> Left failure
    Right (Exit.ExitSuccess, out, _) -> Right out
    Right (Exit.ExitFailure _, out, err) -> Left (refusal out (T.strip err))

{- | What GitHub said no to, when it said: the status from gh's last
line and the message from the JSON body.
-}
refusal :: T.Text -> T.Text -> GhFailure
refusal body err =
  let
    status = do
      (_, rest) <- Just (T.breakOnEnd "(HTTP " err)
      Git.readCount (T.takeWhile (/= ')') rest)
    reasons json =
      case Json.field "errors" json of
        Just (Json.JArray es) -> Maybe.mapMaybe errorText es
        _ -> []
    errorText e =
      case e of
        Json.JString t -> Just t
        _ -> case Json.field "message" e of
          Just (Json.JString t) -> Just t
          _ -> Nothing
    message json =
      case (reasons json, Json.field "message" json) of
        (r : rs, _) -> Just (T.intercalate "; " (r : rs))
        ([], Just (Json.JString t)) -> Just t
        _ -> Nothing
  in
    case (status, Json.decode body >>= message) of
      (Just code, Just why) -> GhRefused code why
      _ -> classify err

viewerLogin :: FilePath -> IO (Either GhFailure T.Text)
viewerLogin root = fmap (fmap T.strip) (runGh root ["api", "user", "--jq", ".login"])

pullNumber :: T.Text -> Maybe Int
pullNumber t =
  case Git.readCount (Maybe.fromMaybe t (T.stripPrefix "#" t)) of
    Just n | n > 0 -> Just n
    _ -> Nothing

data RepoName = RepoName
  { repoOrigin :: Maybe T.Text
  , repoCached :: Maybe T.Text
  }

cachedRepoName :: SQLite.Connection -> FilePath -> IO RepoName
cachedRepoName conn root = do
  url <- originUrl root
  cached <- maybe (pure Nothing) (Store.cachedRepoName conn) url
  pure RepoName {repoOrigin = url, repoCached = cached}

rememberRepoName :: SQLite.Connection -> RepoName -> T.Text -> IO ()
rememberRepoName conn known name =
  Monad.forM_ (repoOrigin known) (\url -> Store.saveRepoName conn url name)

originUrl :: FilePath -> IO (Maybe T.Text)
originUrl root = do
  (code, out, _) <- Process.readProcessWithExitCode "git" ["-C", root, "remote", "get-url", "origin"] ""
  pure $ case (code, T.strip (T.pack out)) of
    (Exit.ExitSuccess, url) | not (T.null url) -> Just url
    _ -> Nothing

resolveRepo :: FilePath -> IO (Either GhFailure T.Text)
resolveRepo root =
  fmap (fmap T.strip) (runGh root ["repo", "view", "--json", "nameWithOwner", "--jq", ".nameWithOwner"])

data Checks = ChecksNone | ChecksRunning | ChecksPass | ChecksFail
  deriving (Eq, Show)

checksText :: Checks -> T.Text
checksText checks =
  case checks of
    ChecksNone -> "none"
    ChecksRunning -> "running"
    ChecksPass -> "pass"
    ChecksFail -> "fail"

checksFromText :: T.Text -> Checks
checksFromText t =
  case t of
    "pass" -> ChecksPass
    "fail" -> ChecksFail
    "running" -> ChecksRunning
    _ -> ChecksNone

checksJq :: T.Text
checksJq =
  "((.statusCheckRollup // []) as $c\
  \ | if ($c | length) == 0 then \"none\"\
  \ elif any($c[]; (.conclusion // .state // \"\") as $s\
  \   | [\"FAILURE\",\"ERROR\",\"TIMED_OUT\",\"CANCELLED\",\"ACTION_REQUIRED\",\"STARTUP_FAILURE\"] | index([$s]) != null) then \"fail\"\
  \ elif any($c[]; ((.status // \"COMPLETED\") != \"COMPLETED\") or ((.state // \"\") == \"PENDING\") or ((.state // \"\") == \"EXPECTED\")) then \"running\"\
  \ else \"pass\" end)"

checksFromRollup :: T.Text -> Checks
checksFromRollup state =
  case state of
    "SUCCESS" -> ChecksPass
    "FAILURE" -> ChecksFail
    "ERROR" -> ChecksFail
    "PENDING" -> ChecksRunning
    "EXPECTED" -> ChecksRunning
    _ -> ChecksNone

data ReviewState
  = NotReviewed
  | Commented
  | Approved
  | ChangesRequested
  | ReviewRequested
  deriving (Eq, Show)

data PullRow = PullRow
  { rowNumber :: Int
  , rowAuthor :: T.Text
  , rowTitle :: T.Text
  , rowAgeSeconds :: Int
  , rowDraft :: Bool
  , rowChecks :: Checks
  , rowReview :: ReviewState
  }
  deriving (Eq, Show)

listPulls :: FilePath -> T.Text -> T.Text -> IO (Either GhFailure [PullRow])
listPulls root repo viewer = do
  let
    jq =
      ".[] | [ (.number | tostring), .author.login, .title\
      \, ((now - (.createdAt | fromdateiso8601)) | floor | tostring)\
      \, (.isDraft | tostring), "
        <> checksJq
        <> ", ([.latestReviews[]? | \"\\(.author.login):\\(.state)\"] | join(\",\"))\
           \, ([.reviewRequests[]? | (.login // .name // \"\")] | join(\",\")) ] | @tsv"
  got <-
    runGh
      root
      [ "pr"
      , "list"
      , "-R"
      , repo
      , "--state"
      , "open"
      , "--limit"
      , "200"
      , "--json"
      , "number,author,title,createdAt,isDraft,statusCheckRollup,latestReviews,reviewRequests"
      , "--jq"
      , jq
      ]
  pure (fmap (Maybe.mapMaybe (pullRow viewer . T.splitOn "\t") . filter (not . T.null) . T.lines) got)

pullRow :: T.Text -> [T.Text] -> Maybe PullRow
pullRow viewer cells =
  case cells of
    [numText, author, title, ageText, draftText, checks, reviews, requested] -> do
      n <- Git.readCount numText
      age <- Git.readCount ageText
      let
        mine =
          Maybe.listToMaybe
            [ state
            | entry <- T.splitOn "," reviews
            , let
                (login, rest) = T.breakOn ":" entry
                state = T.drop 1 rest
            , login == viewer
            ]
        review = case mine of
          Just "APPROVED" -> Approved
          Just "CHANGES_REQUESTED" -> ChangesRequested
          Just "COMMENTED" -> Commented
          _
            | viewer `elem` T.splitOn "," requested -> ReviewRequested
            | otherwise -> NotReviewed
      Just
        PullRow
          { rowNumber = n
          , rowAuthor = author
          , rowTitle = unescapeTsv title
          , rowAgeSeconds = age
          , rowDraft = draftText == "true"
          , rowChecks = checksFromText checks
          , rowReview = review
          }
    _ -> Nothing

unescapeTsv :: T.Text -> T.Text
unescapeTsv = T.replace "\t" " " . T.replace "\n" " " . T.replace "\r" "" . tsvField

tsvRows :: T.Text -> [[T.Text]]
tsvRows = map (map tsvField . T.splitOn "\t") . filter (not . T.null) . T.lines

tsvField :: T.Text -> T.Text
tsvField =
  let
    go cs =
      case cs of
        '\\' : 'n' : rest -> '\n' : go rest
        '\\' : 't' : rest -> '\t' : go rest
        '\\' : 'r' : rest -> '\r' : go rest
        '\\' : '\\' : rest -> '\\' : go rest
        c : rest -> c : go rest
        [] -> []
  in
    T.pack . go . T.unpack

data PullMeta = PullMeta
  { metaNumber :: Int
  , metaTitle :: T.Text
  , metaState :: T.Text
  , metaHeadBranch :: T.Text
  , metaBaseBranch :: T.Text
  , metaAuthor :: T.Text
  , metaChecks :: Checks
  }
  deriving (Eq, Show)

viewPull :: FilePath -> T.Text -> Int -> IO (Either GhFailure PullMeta)
viewPull root repo n = do
  let
    jq = "[ .title, .state, .headRefName, .baseRefName, .author.login, " <> checksJq <> " ] | @tsv"
  got <-
    runGh
      root
      [ "pr"
      , "view"
      , T.pack (show n)
      , "-R"
      , repo
      , "--json"
      , "title,state,headRefName,baseRefName,author,statusCheckRollup"
      , "--jq"
      , jq
      ]
  pure $ case fmap (T.splitOn "\t" . T.strip) got of
    Left failure -> Left failure
    Right [title, state, headBranch, baseBranch, author, checks] ->
      Right
        PullMeta
          { metaNumber = n
          , metaTitle = unescapeTsv title
          , metaState = state
          , metaHeadBranch = headBranch
          , metaBaseBranch = baseBranch
          , metaAuthor = author
          , metaChecks = checksFromText checks
          }
    Right _ -> Left (GhFailed ("unexpected answer for pull request #" <> T.pack (show n)))

data Opened = Opened
  { openedRepo :: T.Text
  , openedPull :: Git.PullRequest
  , openedMeta :: PullMeta
  }

openedMode :: Opened -> Git.ReviewMode
openedMode opened = Git.AtPull (openedPull opened) Git.ReadOnly

data Authorship = YourPull | TheirPull

data PullStart = EditYourPull | ReviewPull Authorship

pullStart :: FilePath -> Opened -> IO PullStart
pullStart root opened = do
  viewer <- viewerLogin root
  branch <- Git.currentBranch root
  let
    authorship = case viewer of
      Right login | login == metaAuthor (openedMeta opened) -> YourPull
      _ -> TheirPull
  pure $ case authorship of
    YourPull | branch == Just (Git.prHeadBranch (openedPull opened)) -> EditYourPull
    _ -> ReviewPull authorship

startedMode :: PullStart -> Opened -> Git.ReviewMode
startedMode start opened =
  case start of
    EditYourPull -> Git.AtPull (openedPull opened) Git.Checkout
    ReviewPull _ -> openedMode opened

openPull :: FilePath -> Maybe T.Text -> Int -> IO (Either GhFailure Opened)
openPull root cached n = do
  firstRepo <- maybe (resolveRepo root) (pure . Right) cached
  viewed <- either (pure . Left) (\repo -> fmap (fmap (\m -> (repo, m))) (viewPull root repo n)) firstRepo
  found <- case (viewed, cached) of
    (Left (GhNotFound _), Just stale) -> do
      fresh <- resolveRepo root
      case fresh of
        Right repo | repo /= stale -> fmap (fmap (\m -> (repo, m))) (viewPull root repo n)
        _ -> pure viewed
    _ -> pure viewed
  case found of
    Left failure -> pure (Left failure)
    Right (repo, meta) -> do
      remote <- remoteFor root repo
      fetched <- fetchPull root remote n (metaBaseBranch meta)
      pure $ case fetched of
        Left e -> Left (classify e)
        Right () ->
          Right
            Opened
              { openedRepo = repo
              , openedMeta = meta
              , openedPull =
                  Git.PullRequest
                    { Git.prNumber = n
                    , Git.prRepo = repo
                    , Git.prHeadBranch = metaHeadBranch meta
                    , Git.prBaseBranch = metaBaseBranch meta
                    , Git.prTip = Git.pullHeadRef n
                    , Git.prBase = Git.pullBaseRef n
                    }
              }

remoteFor :: FilePath -> T.Text -> IO T.Text
remoteFor root repo = do
  (code, out, _) <- Process.readProcessWithExitCode "git" ["-C", root, "remote", "-v"] ""
  let
    wanted = T.toLower repo
    names url =
      let
        trimmed = T.toLower (stripSuffix ".git" (stripSuffix "/" url))
      in
        any (`T.isSuffixOf` trimmed) ["/" <> wanted, ":" <> wanted]
    candidates =
      [ name
      | l <- T.lines (T.pack out)
      , (name : url : _) <- [T.words l]
      , names url
      ]
  pure $ case (code, candidates) of
    (Exit.ExitSuccess, name : _) -> name
    _ -> "origin"

stripSuffix :: T.Text -> T.Text -> T.Text
stripSuffix suffix t = Maybe.fromMaybe t (T.stripSuffix suffix t)

fetchPull :: FilePath -> T.Text -> Int -> T.Text -> IO (Either T.Text ())
fetchPull root remote n baseBranch = do
  let
    numText = T.pack (show n)
    headRef = Git.pullHeadRef n
    baseRef = Git.pullBaseRef n
    git = gitIn root
  fetched <-
    git
      [ "fetch"
      , "--quiet"
      , "--no-tags"
      , "--no-write-fetch-head"
      , remote
      , "+refs/pull/" <> numText <> "/head:" <> headRef
      , "+refs/heads/" <> baseBranch <> ":" <> baseRef
      ]
  case fetched of
    Left e -> pure (Left e)
    Right _ -> do
      forked <- git ["merge-base", headRef, baseRef]
      case forked of
        Left e -> pure (Left e)
        Right sha -> fmap (fmap (const ())) (git ["update-ref", baseRef, sha])

gitIn :: FilePath -> [T.Text] -> IO (Either T.Text T.Text)
gitIn root args = do
  (code, out, err) <- Process.readProcessWithExitCode "git" (["-C", root] ++ map T.unpack args) ""
  pure $ case code of
    Exit.ExitSuccess -> Right (T.strip (T.pack out))
    Exit.ExitFailure _ -> Left (firstLine (T.strip (T.pack err)))

markReady :: FilePath -> T.Text -> Int -> IO (Either GhFailure ())
markReady root repo n = fmap (fmap (const ())) (runGh root ["pr", "ready", T.pack (show n), "-R", repo])

markDraft :: FilePath -> T.Text -> Int -> IO (Either GhFailure ())
markDraft root repo n = fmap (fmap (const ())) (runGh root ["pr", "ready", T.pack (show n), "-R", repo, "--undo"])

checkoutPull :: FilePath -> T.Text -> Int -> IO (Either GhFailure ())
checkoutPull root repo n = fmap (fmap (const ())) (runGh root ["pr", "checkout", T.pack (show n), "-R", repo])

data RefsMove = RefsInPlace | RefsFetched

refreshPullRefs :: FilePath -> T.Text -> Int -> T.Text -> T.Text -> IO (Either GhFailure RefsMove)
refreshPullRefs root repo n baseBranch headSha = do
  (code, out, _) <- Process.readProcessWithExitCode "git" ["-C", root, "rev-parse", "--verify", "--quiet", T.unpack (Git.pullHeadRef n)] ""
  case (code, T.strip (T.pack out)) of
    (Exit.ExitSuccess, sha) | sha == headSha -> pure (Right RefsInPlace)
    _ -> do
      remote <- remoteFor root repo
      fetched <- fetchPull root remote n baseBranch
      pure $ case fetched of
        Left e -> Left (classify e)
        Right () -> Right RefsFetched

data Kept = KeptHere | NotKept
  deriving (Eq, Show)

data OnGitHub = StillOnGitHub | GoneFromGitHub
  deriving (Eq, Show)

data PushedHead = PushedHead
  { pushedKept :: Kept
  , pushedOnGitHub :: OnGitHub
  }
  deriving (Eq, Show)

pushedHeadState :: FilePath -> T.Text -> T.Text -> IO (Either GhFailure PushedHead)
pushedHeadState root repo sha = do
  local <- Git.refExists root sha
  remote <- runGhOutput root ["api", "repos/" <> repo <> "/commits/" <> sha, "--jq", ".sha"]
  let
    kept = if local then KeptHere else NotKept
    gone = PushedHead {pushedKept = kept, pushedOnGitHub = GoneFromGitHub}
  pure $ case remote of
    Left failure -> Left failure
    Right (Exit.ExitSuccess, _, _) -> Right PushedHead {pushedKept = kept, pushedOnGitHub = StillOnGitHub}
    Right (Exit.ExitFailure _, _, err)
      | "no commit found" `T.isInfixOf` T.toLower err -> Right gone
      | otherwise -> case classify (T.strip err) of
          GhNotFound _ -> Right gone
          failure -> Left failure

keepPushedHead :: FilePath -> T.Text -> Int -> T.Text -> IO (Either GhFailure T.Text)
keepPushedHead root repo n sha = do
  local <- Git.refExists root sha
  let
    ref = Git.pushedHeadRef n sha
  kept <-
    if local
      then gitIn root ["update-ref", ref, sha]
      else do
        remote <- remoteFor root repo
        gitIn root ["fetch", "--quiet", "--no-tags", "--no-write-fetch-head", remote, "+" <> sha <> ":" <> ref]
  pure $ case kept of
    Left e -> Left (classify e)
    Right _ -> Right ref
