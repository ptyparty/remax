{-# LANGUAGE OverloadedStrings #-}

module Remax.Komux
  ( PaneId (..)
  , answerTimeoutUs
  , AgentPid (..)
  , AgentProcess (..)
  , AgentLookupError (..)
  , renderAgentLookupError
  , Prompted (..)
  , PaneSession (..)
  , findAgent
  , agentAlive
  , promptAgent
  , paneSession
  ) where

import Control.Exception qualified as Exception
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBS
import System.Environment qualified as Env
import System.Posix.Process qualified as Posix
import System.Timeout qualified as Timeout
import Text.Read qualified as Read

import Remax.Context qualified as Context
import Remax.Json qualified as Json

newtype PaneId = PaneId {paneIdToInt :: Int}
  deriving (Eq)

newtype AgentPid = AgentPid {agentPidToInt :: Int}
  deriving (Eq)

data AgentProcess = AgentProcess
  { agentPid :: AgentPid
  , agentStarted :: T.Text
  }

data AgentLookupError
  = NoClaudeAncestor

renderAgentLookupError :: AgentLookupError -> T.Text
renderAgentLookupError e =
  case e of
    NoClaudeAncestor ->
      "could not find the Claude that ran this command among its parent processes; a sandboxed shell hides it, and a watch started from one would die when the command returns"

data Prompted
  = Prompted
  | PromptBusy
  | PromptUnanswered
  | PromptUnreachable
  | PromptRefused T.Text

data PaneSession
  = PaneSessionUnknown
  | PaneSessionNone
  | PaneSessionIs T.Text

data ProcStat = ProcStat
  { procStatName :: T.Text
  , procStatParent :: Int
  , procStatStarted :: T.Text
  }

findAgent :: IO (Either AgentLookupError AgentProcess)
findAgent = do
  parent <- Posix.getParentProcessID
  declared <- Env.lookupEnv "CLAUDE_PID"
  ancestors <- ancestry (fromIntegral parent)
  let
    declaredPid = declared >>= Read.readMaybe
    isAgent (pid, stat) =
      case declaredPid of
        Just wanted -> pid == wanted
        Nothing -> procStatName stat == "claude"
  pure $ case filter isAgent ancestors of
    (pid, stat) : _ -> Right AgentProcess {agentPid = AgentPid pid, agentStarted = procStatStarted stat}
    [] -> Left NoClaudeAncestor

ancestry :: Int -> IO [(Int, ProcStat)]
ancestry =
  let
    climb depth pid
      | depth > (32 :: Int) || pid <= 1 = pure []
      | otherwise = do
          stat <- readProcStat pid
          case stat of
            Nothing -> pure []
            Just found -> fmap ((pid, found) :) (climb (depth + 1) (procStatParent found))
  in
    climb 0

agentAlive :: AgentProcess -> IO Bool
agentAlive agent = do
  stat <- readProcStat (agentPidToInt (agentPid agent))
  pure (fmap procStatStarted stat == Just (agentStarted agent))

readProcStat :: Int -> IO (Maybe ProcStat)
readProcStat pid = do
  got <- Exception.try (TIO.readFile ("/proc/" <> show pid <> "/stat")) :: IO (Either Exception.IOException T.Text)
  pure $ case got of
    Left _ -> Nothing
    Right raw -> parseProcStat raw

parseProcStat :: T.Text -> Maybe ProcStat
parseProcStat raw =
  let
    (beforeName, fromName) = T.breakOn "(" raw
    (name, afterName) = T.breakOnEnd ")" (T.drop 1 fromName)
    fields = T.words afterName
  in
    case (T.null beforeName, drop 1 fields, drop 19 fields) of
      (False, parentField : _, started : _)
        | Just parent <- Read.readMaybe (T.unpack parentField) ->
            Just
              ProcStat
                { procStatName = T.dropEnd 1 name
                , procStatParent = parent
                , procStatStarted = started
                }
      _ -> Nothing

promptAgent :: FilePath -> PaneId -> T.Text -> IO Prompted
promptAgent socketPath pane text = do
  got <-
    request
      socketPath
      "agent.prompt"
      [("paneId", Json.JNumber (T.pack (show (paneIdToInt pane)))), ("text", Json.JString text)]
  pure $ case got of
    Unreachable -> PromptUnreachable
    Unanswered -> PromptUnanswered
    Garbled -> PromptRefused "komux gave an answer remax does not understand"
    Answered answer -> case (Json.field "ok" answer, Json.field "error" answer) of
      (Just (Json.JBool True), _) -> Prompted
      (_, Just (Json.JString "busy")) -> PromptBusy
      (_, Just (Json.JString why)) -> PromptRefused why
      _ -> PromptRefused "komux gave an answer remax does not understand"

paneSession :: FilePath -> PaneId -> IO PaneSession
paneSession socketPath pane = do
  got <- request socketPath "claude.sessions" []
  let
    entry json =
      case (Json.field "paneId" json, Json.field "sessionId" json) of
        (Just (Json.JNumber n), Just (Json.JString session))
          | Read.readMaybe (T.unpack n) == Just (paneIdToInt pane) -> Just session
        _ -> Nothing
  pure $ case got of
    Answered answer
      | Just (Json.JBool True) <- Json.field "ok" answer
      , Just (Json.JArray entries) <- Json.field "result" answer ->
          case Maybe.mapMaybe entry entries of
            session : _ -> PaneSessionIs session
            [] -> PaneSessionNone
    _ -> PaneSessionUnknown

data Answer
  = Answered Json.Json
  | Garbled
  | Unanswered
  | Unreachable

request :: FilePath -> T.Text -> [(T.Text, Json.Json)] -> IO Answer
request socketPath method params =
  let
    line =
      Json.encode
        ( Json.JObject
            ( ("method", Json.JString method)
                : (if null params then [] else [("params", Json.JObject params)])
            )
        )
  in
    do
      got <- Exception.try (Timeout.timeout answerTimeoutUs (exchange socketPath line)) :: IO (Either Exception.IOException (Maybe T.Text))
      pure $ case got of
        Left _ -> Unreachable
        Right Nothing -> Unanswered
        Right (Just answer) -> maybe Garbled Answered (Json.decode answer)

answerTimeoutUs :: Int
answerTimeoutUs = 5000000

exchange :: FilePath -> T.Text -> IO T.Text
exchange socketPath line =
  Exception.bracket
    (Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol)
    Socket.close
    ( \sock -> do
        Socket.connect sock (Socket.SockAddrUnix socketPath)
        SocketBS.sendAll sock (TE.encodeUtf8 (line <> "\n"))
        Context.receiveLine sock
    )
