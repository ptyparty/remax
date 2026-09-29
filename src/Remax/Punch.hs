{-# LANGUAGE OverloadedStrings #-}

module Remax.Punch
  ( PunchLine (..)
  , Conversation (..)
  , conversationAwaiting
  , conversations
  , commitSubjects
  , LinesInFile (..)
  , linesInFileText
  , linesInFile
  ) where

import Data.Char (isAlphaNum)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Remax.Key (noteGroupId)
import Remax.Store
  ( Awaiting
  , GroupId (..)
  , LineKey (..)
  , Post
  , PunchRow (..)
  , Verdict
  , awaiting
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

conversationAwaiting :: Conversation -> Awaiting
conversationAwaiting = awaiting . conversationThread

rowConversationId :: FilePath -> Map Text Text -> PunchRow -> GroupId
rowConversationId repo groups row =
  let
    key = lineKeyToText (punchKey row)
  in
    GroupId (Map.findWithDefault (noteGroupId (T.pack repo) [key]) key groups)

conversations :: FilePath -> Map Text Text -> [PunchRow] -> [Conversation]
conversations repo groups rows =
  let
    conversationIdOf = rowConversationId repo groups
    groupingKey row =
      (conversationIdOf row, punchFile row, punchCommitSha row, punchThread row)
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
    map (toConversation repo conversationIdOf . snd) (sortOn fst (Map.elems grouped))

toConversation :: FilePath -> (PunchRow -> GroupId) -> NonEmpty PunchRow -> Conversation
toConversation repo conversationIdOf members@(first :| _) =
  Conversation
    { conversationId = conversationIdOf first
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

data LinesInFile = AllInFile | SomeInFile | NoneInFile
  deriving (Eq, Show)

linesInFileText :: LinesInFile -> Text
linesInFileText l =
  case l of
    AllInFile -> "all"
    SomeInFile -> "some"
    NoneInFile -> "none"

linesInFile :: [Text] -> NonEmpty PunchLine -> Maybe LinesInFile
linesInFile fileLines marked =
  let
    lineAt = Map.fromList (zip [1 ..] fileLines)
    positionsOf = Map.fromListWith (flip (<>)) (zip fileLines (map pure [1 ..]))
    kept line = do
      n <- punchLineNumber line
      case T.uncons (punchLineText line) of
        Just ('+', rest) -> Just (n, rest)
        Just (' ', rest) -> Just (n, rest)
        _ -> Nothing
    placed = mapMaybe kept (toList marked)
    anchors = filter (T.any isAlphaNum . snd) placed
    shifts =
      nubOrd
        ( concatMap
            (\(n, text) -> map (subtract n) (Map.findWithDefault [] text positionsOf))
            anchors
        )
    matchesAt shift =
      length (filter (\(n, text) -> Map.lookup (n + shift) lineAt == Just text) placed)
    best = maximum (0 : map matchesAt shifts)
  in
    if null anchors
      then Nothing
      else
        Just $
          if best == length placed
            then AllInFile
            else
              if best > 0
                then SomeInFile
                else NoneInFile
