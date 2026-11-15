{-# LANGUAGE OverloadedStrings #-}

module Remax.Komux
  ( AgentProcess (..)
  , Prompted (..)
  , findAgent
  , agentAlive
  , promptAgent
  ) where

import Control.Exception qualified as Exception
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Text.IO qualified as TIO
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBS
import System.Posix.Process qualified as Posix
import System.Timeout qualified as Timeout

import Remax.Json qualified as Json

data AgentProcess = AgentProcess
  { agentPid :: Int
  , agentStarted :: T.Text
  }

data Prompted
  = Prompted
  | PromptBusy
  | PromptGone

data ProcStat = ProcStat
  { procStatName :: T.Text
  , procStatParent :: Int
  , procStatStarted :: T.Text
  }

findAgent :: IO (Maybe AgentProcess)
findAgent =
  let
    climb depth pid
      | depth > (32 :: Int) || pid <= 1 = pure Nothing
      | otherwise = do
          stat <- readProcStat pid
          case stat of
            Nothing -> pure Nothing
            Just found
              | procStatName found == "claude" ->
                  pure (Just AgentProcess {agentPid = pid, agentStarted = procStatStarted found})
              | otherwise -> climb (depth + 1) (procStatParent found)
  in
    do
      parent <- Posix.getParentProcessID
      climb 0 (fromIntegral parent)

agentAlive :: AgentProcess -> IO Bool
agentAlive agent = do
  stat <- readProcStat (agentPid agent)
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
        | [(parent, "")] <- reads (T.unpack parentField) ->
            Just
              ProcStat
                { procStatName = T.dropEnd 1 name
                , procStatParent = parent
                , procStatStarted = started
                }
      _ -> Nothing

promptAgent :: FilePath -> Int -> T.Text -> IO Prompted
promptAgent socketPath pane text =
  let
    request =
      Json.JObject
        [ ("method", Json.JString "agent.prompt")
        , ("params", Json.JObject [("paneId", Json.JNumber (T.pack (show pane))), ("text", Json.JString text)])
        ]
  in
    do
      got <- Exception.try (Timeout.timeout answerTimeoutUs (exchange socketPath (Json.encode request))) :: IO (Either Exception.IOException (Maybe T.Text))
      pure $ case got of
        Left _ -> PromptGone
        Right Nothing -> PromptBusy
        Right (Just answer) -> readAnswer answer

readAnswer :: T.Text -> Prompted
readAnswer answer =
  let
    json = Json.decode answer
  in
    case (json >>= Json.field "ok", json >>= Json.field "error") of
      (Just (Json.JBool True), _) -> Prompted
      (_, Just (Json.JString "busy")) -> PromptBusy
      _ -> PromptGone

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
        fmap (TE.decodeUtf8With TEE.lenientDecode) (receiveLine sock)
    )

receiveLine :: Socket.Socket -> IO BS.ByteString
receiveLine sock =
  let
    go acc = do
      chunk <- SocketBS.recv sock 4096
      let
        got = acc <> chunk
      if BS.null chunk || BS.elem 10 chunk
        then pure (BS.takeWhile (/= 10) got)
        else go got
  in
    go BS.empty
