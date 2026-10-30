{-# LANGUAGE OverloadedStrings #-}

module Remax.Remote
  ( Request (..)
  , Reply (..)
  , Delivery (..)
  , withListener
  , send
  , replyText
  ) where

import Control.Concurrent qualified as Concurrent
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Data.ByteString qualified as BS
import Data.Either qualified as Either
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBS
import System.Directory qualified as Directory
import System.Environment qualified as Env
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import System.Posix.Process qualified as Posix
import System.Timeout qualified as Timeout

data Request = Request
  { rqCommand :: T.Text
  , rqRoots :: [FilePath]
  }

data Reply
  = Ran T.Text
  | Held
  | Refused T.Text
  | NotHere

data Delivery
  = Answered Reply
  | NobodyOpen

replyText :: Reply -> T.Text
replyText reply =
  case reply of
    Ran said -> said
    Held -> "held for the reviewer: they will be asked once they are free"
    Refused why -> why
    NotHere -> "no remax is open on this checkout"

socketDir :: IO FilePath
socketDir = do
  tab <- Env.lookupEnv "QMUX_TAB_DIR"
  runtime <- Env.lookupEnv "XDG_RUNTIME_DIR"
  temp <- Directory.getTemporaryDirectory
  pure $ case (tab, runtime) of
    (Just dir, _) | not (null dir) -> dir </> "remax"
    (_, Just dir) | not (null dir) -> dir </> "remax"
    _ -> temp </> "remax"

withListener :: (Request -> IO Reply) -> IO a -> IO a
withListener handle action = do
  dir <- socketDir
  pid <- Posix.getProcessID
  let
    path = dir </> (show pid <> ".sock")
  Directory.createDirectoryIfMissing True dir
  removeQuietly path
  opened <- Exception.try (bindAt path) :: IO (Either Exception.IOException Socket.Socket)
  case opened of
    Left _ -> action
    Right sock ->
      Exception.bracket
        (Concurrent.forkIO (acceptLoop sock handle))
        (\loop -> Concurrent.killThread loop >> Socket.close sock >> removeQuietly path)
        (const action)

bindAt :: FilePath -> IO Socket.Socket
bindAt path = do
  sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
  Socket.bind sock (Socket.SockAddrUnix path)
  Socket.listen sock 4
  pure sock

acceptLoop :: Socket.Socket -> (Request -> IO Reply) -> IO ()
acceptLoop sock handle =
  Monad.forever $ do
    (conn, _) <- Socket.accept sock
    Concurrent.forkFinally (answer conn handle) (const (Socket.close conn))

answer :: Socket.Socket -> (Request -> IO Reply) -> IO ()
answer conn handle = do
  raw <- receiveAll conn
  case decodeRequest raw of
    Nothing -> pure ()
    Just request -> do
      reply <- handle request
      SocketBS.sendAll conn (TE.encodeUtf8 (encodeReply reply))

send :: Request -> IO Delivery
send request = do
  dir <- socketDir
  listed <- Exception.try (Directory.listDirectory dir) :: IO (Either Exception.IOException [FilePath])
  let
    sockets = map (dir </>) (filter ((== ".sock") . FilePath.takeExtension) (Either.fromRight [] listed))
  replies <- mapM (ask request) sockets
  pure $ case List.find answered (Maybe.catMaybes replies) of
    Just reply -> Answered reply
    Nothing -> NobodyOpen

answered :: Reply -> Bool
answered reply =
  case reply of
    NotHere -> False
    _ -> True

ask :: Request -> FilePath -> IO (Maybe Reply)
ask request path = do
  got <- Exception.try (Timeout.timeout askTimeoutUs (exchange request path)) :: IO (Either Exception.IOException (Maybe Reply))
  case got of
    Left _ -> removeQuietly path >> pure Nothing
    Right reply -> pure reply

askTimeoutUs :: Int
askTimeoutUs = 6000000

exchange :: Request -> FilePath -> IO Reply
exchange request path =
  Exception.bracket
    (Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol)
    Socket.close
    ( \sock -> do
        Socket.connect sock (Socket.SockAddrUnix path)
        SocketBS.sendAll sock (TE.encodeUtf8 (encodeRequest request))
        Socket.shutdown sock Socket.ShutdownSend
        raw <- receiveAll sock
        pure (Maybe.fromMaybe NotHere (decodeReply raw))
    )

receiveAll :: Socket.Socket -> IO BS.ByteString
receiveAll sock =
  let
    go acc = do
      chunk <- SocketBS.recv sock 4096
      if BS.null chunk || sum (map BS.length acc) > maxMessage
        then pure (BS.concat (reverse acc))
        else go (chunk : acc)
  in
    go []

maxMessage :: Int
maxMessage = 65536

encodeRequest :: Request -> T.Text
encodeRequest request = T.unlines (rqCommand request : map T.pack (rqRoots request))

decodeRequest :: BS.ByteString -> Maybe Request
decodeRequest raw =
  case T.lines (TE.decodeUtf8With TEE.lenientDecode raw) of
    (command : roots) -> Just Request {rqCommand = command, rqRoots = map T.unpack roots}
    [] -> Nothing

encodeReply :: Reply -> T.Text
encodeReply reply =
  case reply of
    Ran said -> "ran\t" <> said
    Held -> "held"
    Refused why -> "refused\t" <> why
    NotHere -> "nothere"

decodeReply :: BS.ByteString -> Maybe Reply
decodeReply raw =
  let
    (tag, rest) = T.breakOn "\t" (T.strip (TE.decodeUtf8With TEE.lenientDecode raw))
    said = T.drop 1 rest
  in
    case tag of
      "ran" -> Just (Ran said)
      "held" -> Just Held
      "refused" -> Just (Refused said)
      "nothere" -> Just NotHere
      _ -> Nothing

removeQuietly :: FilePath -> IO ()
removeQuietly path = Monad.void (Exception.try (Directory.removeFile path) :: IO (Either Exception.IOException ()))
