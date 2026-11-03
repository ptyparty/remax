{-# LANGUAGE OverloadedStrings #-}

module Remax.Context
  ( Attachment (..)
  , Answer (..)
  , withListener
  ) where

import Control.Concurrent qualified as Concurrent
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.List.NonEmpty qualified as NEL
import Data.Maybe qualified as Maybe
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

withListener :: IO Answer -> IO a -> IO a
withListener answer action = do
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
            (Concurrent.forkIO (acceptLoop sock answer))
            (\loop -> Concurrent.killThread loop >> Socket.close sock >> removeQuietly path)
            (const action)

bindAt :: FilePath -> IO Socket.Socket
bindAt path = do
  sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
  Socket.bind sock (Socket.SockAddrUnix path)
  Socket.listen sock 4
  pure sock

acceptLoop :: Socket.Socket -> IO Answer -> IO ()
acceptLoop sock answer =
  Monad.forever $ do
    (conn, _) <- Socket.accept sock
    Concurrent.forkFinally (serve conn answer) (const (Socket.close conn))

serve :: Socket.Socket -> IO Answer -> IO ()
serve conn answer = do
  line <- receiveLine conn
  Monad.when (asksForContext line) $ do
    reply <- answer
    SocketBS.sendAll conn (TE.encodeUtf8 (Json.encode (encodeAnswer reply) <> "\n"))

asksForContext :: T.Text -> Bool
asksForContext line =
  Maybe.isJust (Json.field "context" =<< Json.decode line)

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

encodeAttachment :: Attachment -> Json.Json
encodeAttachment attachment =
  Json.JObject
    [ ("label", Json.JString (attachmentLabel attachment))
    , ("text", Json.JString (attachmentText attachment))
    ]

removeQuietly :: FilePath -> IO ()
removeQuietly path = Monad.void (Exception.try (Directory.removeFile path) :: IO (Either Exception.IOException ()))
