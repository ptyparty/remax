{-# LANGUAGE OverloadedStrings #-}

module Remax.RemoteCache
  ( cachePath
  , writeRemote
  , readRemote
  , Cached (..)
  ) where

import Control.Exception qualified as Exception
import Data.Digest.Pure.SHA qualified as SHA
import Data.List.NonEmpty qualified as NEL
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Remax.Conversation qualified as Conv
import Remax.Json qualified as Json
import Remax.Store qualified as Store
import System.Directory qualified as Directory
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

{- | The last poll's GitHub state for the pull request a session has
open, written for the agent CLI to read.
-}
data Cached = Cached
  { cachedPull :: Int
  , cachedRemote :: Conv.Remote
  }

cachePath :: T.Text -> IO FilePath
cachePath repo = do
  dir <- Directory.getXdgDirectory Directory.XdgCache "remax"
  let
    name = T.unpack (T.take 16 (T.pack (SHA.showDigest (SHA.sha256 (TLE.encodeUtf8 (TL.fromStrict repo))))))
  pure (dir </> "pull" </> name <> ".json")

writeRemote :: T.Text -> Int -> Conv.Remote -> IO ()
writeRemote repo pull remote = do
  path <- cachePath repo
  Directory.createDirectoryIfMissing True (FilePath.takeDirectory path)
  let
    tmp = path <> ".tmp"
  TIO.writeFile tmp (Json.encode (encodeCached (Cached pull remote)))
  Directory.renameFile tmp path

readRemote :: T.Text -> IO (Maybe Cached)
readRemote repo = do
  path <- cachePath repo
  got <- Exception.try (TIO.readFile path) :: IO (Either Exception.IOException T.Text)
  pure $ case got of
    Left _ -> Nothing
    Right raw -> Json.decode raw >>= decodeCached

-- Encoding ----------------------------------------------------------------

str :: T.Text -> Json.Json
str = Json.JString

int :: Int -> Json.Json
int = Json.JNumber . T.pack . show

encodeCached :: Cached -> Json.Json
encodeCached c =
  let
    remote = cachedRemote c
  in
    Json.JObject
      [ ("version", int 1)
      , ("pull", int (cachedPull c))
      , ("viewer", maybe Json.JNull (str . Conv.loginToText) (Conv.remoteViewer remote))
      , ("threads", Json.JArray (map encodeThread (Conv.remoteThreads remote)))
      , ("comments", Json.JArray (map encodePullComment (Conv.remoteComments remote)))
      , ("reviews", Json.JArray (map encodeReview (Conv.remoteReviews remote)))
      ]

encodeThread :: Conv.RemoteThread -> Json.Json
encodeThread t =
  Json.JObject
    ( [ ("id", str (Store.remoteThreadIdToText (Conv.remoteThreadId t)))
      , ("resolved", Json.JBool (Conv.remoteThreadResolution t == Store.Resolved))
      , ("comments", Json.JArray (map encodeComment (NEL.toList (Conv.remoteThreadComments t))))
      ]
        ++ encodeWhere (Conv.remoteThreadWhere t)
    )

encodeWhere :: Either Conv.Outdated Conv.Position -> [(T.Text, Json.Json)]
encodeWhere place =
  case place of
    Right pos ->
      [ ("path", str (Conv.posPath pos))
      , ("side", str (Conv.sideText (Conv.posSide pos)))
      , ("line", int (Conv.posLine pos))
      , ("start_line", maybe Json.JNull int (Conv.posStartLine pos))
      ]
    Left out ->
      [ ("path", str (Conv.outPath out))
      , ("side", str (Conv.sideText (Conv.outSide out)))
      , ("outdated", Json.JBool True)
      , ("line", int (Conv.outLine out))
      , ("start_line", maybe Json.JNull int (Conv.outStartLine out))
      , ("commit", str (Conv.commitShaToText (Conv.outCommit out)))
      , ("diff_hunk", str (Conv.outDiffHunk out))
      ]

encodeComment :: Conv.RemoteComment -> Json.Json
encodeComment c =
  Json.JObject
    [ ("id", int (fromIntegral (Store.commentIdToInt64 (Conv.remoteCommentId c))))
    , ("author", str (Conv.loginToText (Conv.remoteCommentAuthor c)))
    , ("body", str (Conv.markdownText (Conv.remoteCommentBody c)))
    , ("created_at", str (Conv.remoteCommentCreatedAt c))
    , ("updated_at", str (Conv.remoteCommentUpdatedAt c))
    , ("review_id", maybe Json.JNull (int . fromIntegral . Store.reviewIdToInt64) (Conv.remoteCommentReview c))
    , ("url", str (Conv.remoteCommentUrl c))
    , ("diff_hunk", str (Conv.remoteCommentDiffHunk c))
    , ("reactions", Json.JArray (map encodeReaction (Conv.remoteCommentReactions c)))
    ]

encodeReaction :: Conv.ReactionCount -> Json.Json
encodeReaction r =
  Json.JObject
    [ ("content", str (Conv.reactionText (Conv.rcReaction r)))
    , ("count", int (Conv.rcCount r))
    , ("mine", Json.JBool (Conv.rcMine r))
    ]

encodePullComment :: Conv.RemotePullComment -> Json.Json
encodePullComment c =
  Json.JObject
    [ ("id", int (fromIntegral (Store.commentIdToInt64 (Conv.pullCommentId c))))
    , ("author", str (Conv.loginToText (Conv.pullCommentAuthor c)))
    , ("body", str (Conv.markdownText (Conv.pullCommentBody c)))
    , ("created_at", str (Conv.pullCommentCreatedAt c))
    , ("updated_at", str (Conv.pullCommentUpdatedAt c))
    , ("url", str (Conv.pullCommentUrl c))
    , ("reactions", Json.JArray (map encodeReaction (Conv.pullCommentReactions c)))
    ]

encodeReview :: Conv.RemoteReview -> Json.Json
encodeReview r =
  Json.JObject
    [ ("id", int (fromIntegral (Store.reviewIdToInt64 (Conv.remoteReviewId r))))
    , ("author", str (Conv.loginToText (Conv.remoteReviewAuthor r)))
    , ("state", str (Conv.remoteReviewState r))
    , ("commit", str (Conv.remoteReviewCommit r))
    , ("submitted_at", str (Conv.remoteReviewSubmittedAt r))
    , ("body", str (Conv.markdownText (Conv.remoteReviewBody r)))
    , ("url", str (Conv.remoteReviewUrl r))
    ]

-- Decoding ----------------------------------------------------------------

text :: T.Text -> Json.Json -> Maybe T.Text
text key json =
  case Json.field key json of
    Just (Json.JString t) -> Just t
    _ -> Nothing

number :: T.Text -> Json.Json -> Maybe Int
number key json =
  case Json.field key json of
    Just (Json.JNumber n) -> case reads (T.unpack n) of
      [(k, "")] -> Just k
      _ -> Nothing
    _ -> Nothing

array :: T.Text -> Json.Json -> [Json.Json]
array key json =
  case Json.field key json of
    Just (Json.JArray xs) -> xs
    _ -> []

bool :: T.Text -> Json.Json -> Bool
bool key json =
  case Json.field key json of
    Just (Json.JBool b) -> b
    _ -> False

decodeCached :: Json.Json -> Maybe Cached
decodeCached json = do
  pull <- number "pull" json
  Just
    Cached
      { cachedPull = pull
      , cachedRemote =
          Conv.Remote
            { Conv.remoteViewer = fmap Conv.Login (text "viewer" json)
            , Conv.remoteThreads = Maybe.mapMaybe decodeThread (array "threads" json)
            , Conv.remoteComments = Maybe.mapMaybe decodePullComment (array "comments" json)
            , Conv.remoteReviews = Maybe.mapMaybe decodeReview (array "reviews" json)
            }
      }

decodeThread :: Json.Json -> Maybe Conv.RemoteThread
decodeThread json = do
  tid <- text "id" json
  path <- text "path" json
  line <- number "line" json
  comments <- NEL.nonEmpty (Maybe.mapMaybe decodeComment (array "comments" json))
  let
    side = Conv.parseSide (Maybe.fromMaybe "head" (text "side" json))
    place =
      if bool "outdated" json
        then
          Left
            Conv.Outdated
              { Conv.outPath = path
              , Conv.outSide = side
              , Conv.outLine = line
              , Conv.outStartLine = number "start_line" json
              , Conv.outCommit = Conv.CommitSha (Maybe.fromMaybe "" (text "commit" json))
              , Conv.outDiffHunk = Maybe.fromMaybe "" (text "diff_hunk" json)
              }
        else Right Conv.Position {Conv.posPath = path, Conv.posSide = side, Conv.posLine = line, Conv.posStartLine = number "start_line" json}
  Just
    Conv.RemoteThread
      { Conv.remoteThreadId = Store.RemoteThreadId tid
      , Conv.remoteThreadWhere = place
      , Conv.remoteThreadResolution = if bool "resolved" json then Store.Resolved else Store.Unresolved
      , Conv.remoteThreadComments = comments
      }

decodeReactions :: Json.Json -> Conv.Reactions
decodeReactions json =
  Maybe.mapMaybe
    ( \r -> do
        content <- text "content" r >>= Conv.parseReaction
        count <- number "count" r
        Just Conv.ReactionCount {Conv.rcReaction = content, Conv.rcCount = count, Conv.rcMine = bool "mine" r}
    )
    (array "reactions" json)

decodeComment :: Json.Json -> Maybe Conv.RemoteComment
decodeComment json = do
  cid <- number "id" json
  author <- text "author" json
  Just
    Conv.RemoteComment
      { Conv.remoteCommentId = Store.CommentId (fromIntegral cid)
      , Conv.remoteCommentAuthor = Conv.Login author
      , Conv.remoteCommentBody = Conv.Markdown (Maybe.fromMaybe "" (text "body" json))
      , Conv.remoteCommentCreatedAt = Maybe.fromMaybe "" (text "created_at" json)
      , Conv.remoteCommentUpdatedAt = Maybe.fromMaybe "" (text "updated_at" json)
      , Conv.remoteCommentReview = fmap (Store.ReviewId . fromIntegral) (number "review_id" json)
      , Conv.remoteCommentUrl = Maybe.fromMaybe "" (text "url" json)
      , Conv.remoteCommentReactions = decodeReactions json
      , Conv.remoteCommentDiffHunk = Maybe.fromMaybe "" (text "diff_hunk" json)
      }

decodePullComment :: Json.Json -> Maybe Conv.RemotePullComment
decodePullComment json = do
  cid <- number "id" json
  author <- text "author" json
  Just
    Conv.RemotePullComment
      { Conv.pullCommentId = Store.CommentId (fromIntegral cid)
      , Conv.pullCommentAuthor = Conv.Login author
      , Conv.pullCommentBody = Conv.Markdown (Maybe.fromMaybe "" (text "body" json))
      , Conv.pullCommentCreatedAt = Maybe.fromMaybe "" (text "created_at" json)
      , Conv.pullCommentUpdatedAt = Maybe.fromMaybe "" (text "updated_at" json)
      , Conv.pullCommentUrl = Maybe.fromMaybe "" (text "url" json)
      , Conv.pullCommentReactions = decodeReactions json
      }

decodeReview :: Json.Json -> Maybe Conv.RemoteReview
decodeReview json = do
  rid <- number "id" json
  author <- text "author" json
  Just
    Conv.RemoteReview
      { Conv.remoteReviewId = Store.ReviewId (fromIntegral rid)
      , Conv.remoteReviewAuthor = Conv.Login author
      , Conv.remoteReviewState = Maybe.fromMaybe "" (text "state" json)
      , Conv.remoteReviewCommit = Maybe.fromMaybe "" (text "commit" json)
      , Conv.remoteReviewSubmittedAt = Maybe.fromMaybe "" (text "submitted_at" json)
      , Conv.remoteReviewBody = Conv.Markdown (Maybe.fromMaybe "" (text "body" json))
      , Conv.remoteReviewUrl = Maybe.fromMaybe "" (text "url" json)
      }
