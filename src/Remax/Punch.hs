{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

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

import Data.Char qualified as Char
import Data.Containers.ListUtils qualified as ListUtils
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T

import Remax.Key qualified as Key
import Remax.Store qualified as Store

data PunchLine = PunchLine
  { punchLineNumber :: Maybe Int
  , punchLineText :: T.Text
  , punchLineVerdict :: Store.Verdict
  , punchLineKey :: Store.LineKey
  }
  deriving (Eq, Show)

data Conversation = Conversation
  { conversationId :: Store.GroupId
  , conversationRepo :: FilePath
  , conversationFile :: T.Text
  , conversationCommitSha :: Maybe T.Text
  , conversationLines :: NEL.NonEmpty PunchLine
  , conversationThread :: [Store.Post]
  , conversationUpdatedAt :: T.Text
  }
  deriving (Eq, Show)

conversationAwaiting :: Conversation -> Store.Awaiting
conversationAwaiting = Store.awaiting . conversationThread

rowConversationId :: FilePath -> Map.Map T.Text T.Text -> Store.PunchRow -> Store.GroupId
rowConversationId repo groups row =
  let
    key = Store.lineKeyToText (Store.punchKey row)
  in
    Store.GroupId (Map.findWithDefault (Key.noteGroupId (T.pack repo) [key]) key groups)

conversations :: FilePath -> Map.Map T.Text T.Text -> [Store.PunchRow] -> [Conversation]
conversations repo groups rows =
  let
    conversationIdOf = rowConversationId repo groups
    groupingKey row =
      (conversationIdOf row, Store.punchFile row, Store.punchCommitSha row, Store.punchThread row)
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
    map (toConversation repo conversationIdOf . snd) (List.sortOn fst (Map.elems grouped))

toConversation :: FilePath -> (Store.PunchRow -> Store.GroupId) -> NEL.NonEmpty Store.PunchRow -> Conversation
toConversation repo conversationIdOf members@(first :| _) =
  Conversation
    { conversationId = conversationIdOf first
    , conversationRepo = repo
    , conversationFile = Store.punchFile first
    , conversationCommitSha = Store.punchCommitSha first
    , conversationLines = fmap toLine members
    , conversationThread = Store.punchThread first
    , conversationUpdatedAt = maximum (fmap Store.punchAt members)
    }

toLine :: Store.PunchRow -> PunchLine
toLine row =
  PunchLine
    { punchLineNumber = Store.punchLineNo row
    , punchLineText = Store.punchLine row
    , punchLineVerdict = Store.punchVerdict row
    , punchLineKey = Store.punchKey row
    }

commitSubjects :: [Store.PunchRow] -> Map.Map T.Text T.Text
commitSubjects rows =
  let
    subjectOf row = do
      sha <- Store.punchCommitSha row
      subject <- Store.punchCommitSubject row
      pure (sha, subject)
  in
    Map.fromList (Maybe.mapMaybe subjectOf rows)

data LinesInFile = AllInFile | SomeInFile | NoneInFile
  deriving (Eq, Show)

linesInFileText :: LinesInFile -> T.Text
linesInFileText l =
  case l of
    AllInFile -> "all"
    SomeInFile -> "some"
    NoneInFile -> "none"

linesInFile :: [T.Text] -> NEL.NonEmpty PunchLine -> Maybe LinesInFile
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
    placed = Maybe.mapMaybe kept (Foldable.toList marked)
    anchors = filter (T.any Char.isAlphaNum . snd) placed
    shifts =
      ListUtils.nubOrd
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
