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
  , openPull
  , checkoutPull
  ) where

import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Database.SQLite.Simple qualified as SQLite
import Remax.Git qualified as Git
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
  deriving (Eq, Show)

failureText :: GhFailure -> T.Text
failureText failure =
  case failure of
    GhMissing -> "gh is not installed \8212 remax reaches GitHub through the GitHub CLI"
    GhLoggedOut -> "gh is not logged in \8212 run gh auth login"
    GhOffline why -> "cannot reach GitHub \8212 check the network (" <> why <> ")"
    GhNotFound why -> "not found on GitHub: " <> why
    GhFailed why -> "gh: " <> why

runGh :: FilePath -> [T.Text] -> IO (Either GhFailure T.Text)
runGh root args = do
  environment <- Env.getEnvironment
  let
    quiet = [("GH_PROMPT_DISABLED", "1"), ("GH_NO_UPDATE_NOTIFIER", "1")]
    p =
      (Process.proc "gh" (map T.unpack args))
        { Process.cwd = Just root
        , Process.env = Just (quiet ++ filter ((`notElem` map fst quiet) . fst) environment)
        }
  got <- Exception.try (Process.readCreateProcessWithExitCode p "")
  pure $ case got of
    Left e -> let _ = (e :: Exception.IOException) in Left GhMissing
    Right (Exit.ExitSuccess, out, _) -> Right (T.pack out)
    Right (Exit.ExitFailure _, _, err) -> Left (classify (T.strip (T.pack err)))

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
  got <- runGh root (["api", "-i"] ++ etagArgs ++ jqArgs ++ [path])
  pure (fmap readConditional got)

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
unescapeTsv = T.replace "\\t" " " . T.replace "\\n" " " . T.replace "\\r" "" . T.replace "\\\\" "\\"

data PullMeta = PullMeta
  { metaNumber :: Int
  , metaTitle :: T.Text
  , metaState :: T.Text
  , metaHeadBranch :: T.Text
  , metaBaseBranch :: T.Text
  , metaChecks :: Checks
  }
  deriving (Eq, Show)

viewPull :: FilePath -> T.Text -> Int -> IO (Either GhFailure PullMeta)
viewPull root repo n = do
  let
    jq = "[ .title, .state, .headRefName, .baseRefName, " <> checksJq <> " ] | @tsv"
  got <-
    runGh
      root
      [ "pr"
      , "view"
      , T.pack (show n)
      , "-R"
      , repo
      , "--json"
      , "title,state,headRefName,baseRefName,statusCheckRollup"
      , "--jq"
      , jq
      ]
  pure $ case fmap (T.splitOn "\t" . T.strip) got of
    Left failure -> Left failure
    Right [title, state, headBranch, baseBranch, checks] ->
      Right
        PullMeta
          { metaNumber = n
          , metaTitle = unescapeTsv title
          , metaState = state
          , metaHeadBranch = headBranch
          , metaBaseBranch = baseBranch
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
    git args = do
      (code, out, err) <- Process.readProcessWithExitCode "git" (["-C", root] ++ map T.unpack args) ""
      pure $ case code of
        Exit.ExitSuccess -> Right (T.strip (T.pack out))
        Exit.ExitFailure _ -> Left (firstLine (T.strip (T.pack err)))
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

checkoutPull :: FilePath -> T.Text -> Int -> IO (Either GhFailure ())
checkoutPull root repo n = fmap (fmap (const ())) (runGh root ["pr", "checkout", T.pack (show n), "-R", repo])
