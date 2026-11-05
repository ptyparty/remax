{-# LANGUAGE OverloadedStrings #-}

module Remax.Context
  ( Attachment (..)
  , Answer (..)
  , KeyGroup (..)
  , KeyCommand (..)
  , Handlers (..)
  , withListener
  ) where

import Control.Concurrent qualified as Concurrent
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Word qualified as DataWord
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBS
import Remax.Json qualified as Json
import System.Directory qualified as Directory
import System.Environment qualified as Env
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

data Attachment = Attachment
  { attachmentLabel :: T.Text
  , attachmentText :: T.Text
  }

data Answer
  = Attach (NEL.NonEmpty Attachment)
  | NoContext T.Text

data KeyGroup = KeyGroup
  { keyGroupName :: T.Text
  , keyGroupCommands :: [KeyCommand]
  }

data KeyCommand = KeyCommand
  { keyCommandLabel :: T.Text
  , keyCommandKeys :: T.Text
  , keyCommandSend :: Maybe T.Text
  }

data Handlers = Handlers
  { handlersContext :: IO Answer
  , handlersKeys :: IO [KeyGroup]
  }

data Request = AskContext | AskKeys

socketPath :: IO (Maybe FilePath)
socketPath = do
  tab <- Env.lookupEnv "KOMUX_TAB_DIR"
  pane <- Env.lookupEnv "KOMUX_PANE_ID"
  pure $ case (tab, pane) of
    (Just dir, Just paneId)
      | not (null dir)
      , not (null paneId)
      , all Char.isDigit paneId ->
          Just (dir </> "context" </> (paneId <> ".sock"))
    _ -> Nothing

withListener :: Handlers -> IO a -> IO a
withListener handlers action = do
  found <- socketPath
  case found of
    Nothing -> action
    Just path -> do
      Directory.createDirectoryIfMissing True (FilePath.takeDirectory path)
      removeQuietly path
      opened <- Exception.try (bindAt path) :: IO (Either Exception.IOException Socket.Socket)
      case opened of
        Left _ -> action
        Right sock ->
          Exception.bracket
            (Concurrent.forkIO (acceptLoop sock handlers))
            (\loop -> Concurrent.killThread loop >> Socket.close sock >> removeQuietly path)
            (const action)

bindAt :: FilePath -> IO Socket.Socket
bindAt path = do
  sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
  Socket.bind sock (Socket.SockAddrUnix path)
  Socket.listen sock 4
  pure sock

acceptLoop :: Socket.Socket -> Handlers -> IO ()
acceptLoop sock handlers =
  Monad.forever $ do
    (conn, _) <- Socket.accept sock
    Concurrent.forkFinally (serve conn handlers) (const (Socket.close conn))

serve :: Socket.Socket -> Handlers -> IO ()
serve conn handlers = do
  line <- receiveLine conn
  case requestOf line of
    Nothing -> pure ()
    Just AskContext -> do
      reply <- handlersContext handlers
      sendLine conn (encodeAnswer reply)
    Just AskKeys -> do
      groups <- handlersKeys handlers
      sendLine conn (encodeKeys groups)

sendLine :: Socket.Socket -> Json.Json -> IO ()
sendLine conn json =
  SocketBS.sendAll conn (TE.encodeUtf8 (Json.encode json <> "\n"))

requestOf :: T.Text -> Maybe Request
requestOf line = do
  json <- Json.decode line
  case (Json.field "context" json, Json.field "keys" json) of
    (Just _, _) -> Just AskContext
    (Nothing, Just _) -> Just AskKeys
    (Nothing, Nothing) -> Nothing

receiveLine :: Socket.Socket -> IO T.Text
receiveLine sock =
  let
    go acc = do
      chunk <- SocketBS.recv sock 4096
      let
        got = BS.concat (reverse (chunk : acc))
      if BS.null chunk || BS.elem newline chunk || BS.length got > maxMessage
        then pure (TE.decodeUtf8With TEE.lenientDecode (BS.takeWhile (/= newline) got))
        else go (chunk : acc)
  in
    go []

newline :: DataWord.Word8
newline = 10

maxMessage :: Int
maxMessage = 65536

encodeAnswer :: Answer -> Json.Json
encodeAnswer reply =
  case reply of
    Attach attachments -> Json.JObject [("attach", Json.JArray (map encodeAttachment (NEL.toList attachments)))]
    NoContext reason -> Json.JObject [("none", Json.JString reason)]

encodeKeys :: [KeyGroup] -> Json.Json
encodeKeys groups =
  Json.JObject
    [
      ( "keys"
      , Json.JObject
          [ ("app", Json.JString "remax")
          , ("groups", Json.JArray (map encodeGroup groups))
          ]
      )
    ]

encodeGroup :: KeyGroup -> Json.Json
encodeGroup group =
  Json.JObject
    [ ("name", Json.JString (keyGroupName group))
    , ("commands", Json.JArray (map encodeCommand (keyGroupCommands group)))
    ]

encodeCommand :: KeyCommand -> Json.Json
encodeCommand command =
  Json.JObject
    ( [ ("label", Json.JString (keyCommandLabel command))
      , ("keys", Json.JString (keyCommandKeys command))
      ]
        ++ maybe [] (\send -> [("send", Json.JString send)]) (keyCommandSend command)
    )

encodeAttachment :: Attachment -> Json.Json
encodeAttachment attachment =
  Json.JObject
    [ ("label", Json.JString (attachmentLabel attachment))
    , ("text", Json.JString (attachmentText attachment))
    ]

removeQuietly :: FilePath -> IO ()
removeQuietly path = Monad.void (Exception.try (Directory.removeFile path) :: IO (Either Exception.IOException ()))
