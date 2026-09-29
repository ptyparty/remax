module Remax.Punch
  ( PunchLine (..)
  , Conversation (..)
  , conversations
  , commitSubjects
  ) where

import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Remax.Key (noteGroupId)
import Remax.Store
  ( GroupId (..)
  , LineKey (..)
  , Post
  , PunchRow (..)
  , Verdict
  )

data PunchLine = PunchLine
  { punchLineNumber :: Maybe Int
  , punchLineText :: Text
  , punchLineVerdict :: Verdict
  , punchLineKey :: LineKey
  }
  deriving (Eq, Show)

data Conversation = Conversation
  { conversationId :: GroupId
  , conversationRepo :: FilePath
  , conversationFile :: Text
  , conversationCommitSha :: Maybe Text
  , conversationLines :: NonEmpty PunchLine
  , conversationThread :: [Post]
  , conversationUpdatedAt :: Text
  }
  deriving (Eq, Show)

rowConversationId :: FilePath -> PunchRow -> GroupId
rowConversationId repo row =
  let
    ownGroup = GroupId (noteGroupId (T.pack repo) [lineKeyToText (punchKey row)])
  in
    fromMaybe ownGroup (punchGroup row)

conversations :: FilePath -> [PunchRow] -> [Conversation]
conversations repo rows =
  let
    groupingKey row =
      (rowConversationId repo row, punchFile row, punchCommitSha row, punchThread row)
    appendLater (_, later) (first, earlier) = (first, earlier <> later)
    grouped =
      Map.fromListWith
        appendLater
        ( zipWith
            (\position row -> (groupingKey row, (position, row :| [])))
            [0 :: Int ..]
            rows
        )
  in
    map (toConversation repo . snd) (sortOn fst (Map.elems grouped))

toConversation :: FilePath -> NonEmpty PunchRow -> Conversation
toConversation repo members@(first :| _) =
  Conversation
    { conversationId = rowConversationId repo first
    , conversationRepo = repo
    , conversationFile = punchFile first
    , conversationCommitSha = punchCommitSha first
    , conversationLines = fmap toLine members
    , conversationThread = punchThread first
    , conversationUpdatedAt = maximum (fmap punchAt members)
    }

toLine :: PunchRow -> PunchLine
toLine row =
  PunchLine
    { punchLineNumber = punchLineNo row
    , punchLineText = punchLine row
    , punchLineVerdict = punchVerdict row
    , punchLineKey = punchKey row
    }

commitSubjects :: [PunchRow] -> Map Text Text
commitSubjects rows =
  let
    subjectOf row = do
      sha <- punchCommitSha row
      subject <- punchCommitSubject row
      pure (sha, subject)
  in
    Map.fromList (mapMaybe subjectOf rows)
