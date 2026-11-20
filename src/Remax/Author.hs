{-# LANGUAGE OverloadedStrings #-}

module Remax.Author
  ( Login (..)
  , AgentName (..)
  , Author (..)
  , authorText
  , parseAuthor
  , legacyAuthor
  ) where

import Data.Text qualified as T

newtype Login = Login {loginToText :: T.Text}
  deriving (Eq, Ord, Show)

newtype AgentName = AgentName {agentNameToText :: T.Text}
  deriving (Eq, Ord, Show)

data Author = Viewer | Person Login | Agent AgentName
  deriving (Eq, Ord, Show)

authorText :: Author -> T.Text
authorText a =
  case a of
    Viewer -> "viewer"
    Person (Login l) -> "person:" <> l
    Agent (AgentName n) -> "agent:" <> n

parseAuthor :: T.Text -> Author
parseAuthor t
  | t == "viewer" = Viewer
  | Just l <- T.stripPrefix "person:" t = Person (Login l)
  | Just n <- T.stripPrefix "agent:" t = Agent (AgentName n)
  | otherwise = Agent (AgentName t)

legacyAuthor :: T.Text -> Author
legacyAuthor name =
  if name == "reviewer"
    then Viewer
    else Agent (AgentName name)
