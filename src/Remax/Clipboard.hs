{-# LANGUAGE OverloadedStrings #-}

-- Talking to the system clipboard, by whichever route the machine
-- actually offers. A platform helper is preferred when one is on PATH,
-- because it is the only route that can be *read* back; with none,
-- copying still works through OSC 52, the escape sequence the terminal
-- itself forwards — which is what makes a copy survive an ssh session
-- where no helper exists at the far end.
--
-- Nothing here knows about vty: the OSC 52 writer is passed in, so the
-- terminal stays the caller's business.
module Remax.Clipboard
  ( Clipboard (..)
  , clipboardName
  , clipboardReadable
  , discoverClipboard
  , readClipboard
  , writeClipboard
  ) where

import Control.Exception qualified as Exception
import Data.Bits ((.&.), (.|.))
import Data.Bits qualified as Bits
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word qualified as DataWord
import System.Directory qualified as Directory
import System.Environment qualified as Env
import System.Exit qualified as Exit
import System.Process qualified as Process
import System.Timeout qualified as Timeout

-- A helper that can copy and paste, or the escape-sequence route that
-- can only copy.
data Clipboard
  = ClipTool T.Text (String, [String]) (String, [String]) -- label, copy, paste
  | ClipOsc52

clipboardName :: Clipboard -> T.Text
clipboardName clip =
  case clip of
    ClipTool label _ _ -> label
    ClipOsc52 -> "OSC 52"

-- OSC 52 is write-only in practice: terminals that implement the read
-- side almost all disable it, since it lets any program on the far end
-- of an ssh pipe exfiltrate whatever you last copied.
clipboardReadable :: Clipboard -> Bool
clipboardReadable clip =
  case clip of
    ClipTool _ _ _ -> True
    ClipOsc52 -> False

-- Decided once, at startup: probing PATH on every yank would be a
-- process spawn per keystroke. Wayland first when the session is
-- Wayland, since xclip may also be installed there and would talk to
-- XWayland's clipboard instead of the real one.
discoverClipboard :: IO Clipboard
discoverClipboard = do
  wayland <- Env.lookupEnv "WAYLAND_DISPLAY"
  x11 <- Env.lookupEnv "DISPLAY"
  let
    onWayland = maybe False (not . null) wayland
    onX11 = maybe False (not . null) x11
    -- A helper is only a candidate when the display server it talks to
    -- is actually there. xclip installed on a machine reached over ssh
    -- has no X to reach, and picking it would mean every copy failing
    -- quietly where OSC 52 would have worked.
    candidates =
      (if onWayland then [wlTool] else [])
        ++ [pbTool]
        ++ (if onX11 then [xclipTool, xselTool] else [])
    firstAvailable [] = pure ClipOsc52
    firstAvailable (c : rest) = do
      let
        (bin, _, _) = c
      found <- Directory.findExecutable bin
      case found of
        Just _ -> pure (toClipboard c)
        Nothing -> firstAvailable rest
  firstAvailable candidates

-- (binary probed on PATH, copy command, paste command)
type Candidate = (String, (String, [String]), (String, [String]))

toClipboard :: Candidate -> Clipboard
toClipboard (bin, copy, paste) = ClipTool (T.pack bin) copy paste

wlTool :: Candidate
wlTool = ("wl-copy", ("wl-copy", []), ("wl-paste", ["--no-newline"]))

pbTool :: Candidate
pbTool = ("pbcopy", ("pbcopy", []), ("pbpaste", []))

xclipTool :: Candidate
xclipTool =
  ( "xclip"
  , ("xclip", ["-selection", "clipboard"])
  , ("xclip", ["-selection", "clipboard", "-o"])
  )

xselTool :: Candidate
xselTool =
  ( "xsel"
  , ("xsel", ["--clipboard", "--input"])
  , ("xsel", ["--clipboard", "--output"])
  )

-- Helpers are held to a short leash: xclip and friends keep running to
-- own the selection, and a wedged one must not freeze the session.
toolTimeoutMicros :: Int
toolTimeoutMicros = 2000000

-- Terminals cap what they will accept in one escape sequence, and a
-- truncated paste is worse than a refused one.
osc52Limit :: Int
osc52Limit = 100000

-- Put text on the clipboard. False means it went nowhere and the caller
-- should say so.
writeClipboard :: Clipboard -> (BS.ByteString -> IO ()) -> T.Text -> IO Bool
writeClipboard clip emit text =
  case clip of
    ClipOsc52 ->
      let
        payload = osc52 text
      in
        if BS.length payload > osc52Limit
          then pure False
          else do
            attempted <- Exception.try (emit payload)
            case attempted of
              Right () -> pure True
              Left e -> const (pure False) (e :: Exception.IOException)
    ClipTool _ (bin, args) _ -> do
      done <-
        Timeout.timeout
          toolTimeoutMicros
          (Exception.try (Process.readProcessWithExitCode bin args (T.unpack text)))
      case done of
        -- a helper still running is the normal case for xclip, which
        -- stays alive to serve the selection it was just handed
        Nothing -> pure True
        Just (Right (Exit.ExitSuccess, _, _)) -> pure True
        Just (Right _) -> pure False
        Just (Left e) -> const (pure False) (e :: Exception.IOException)

-- What the clipboard currently holds, or Nothing when there is no way to
-- ask (or nothing there).
readClipboard :: Clipboard -> IO (Maybe T.Text)
readClipboard clip =
  case clip of
    ClipOsc52 -> pure Nothing
    ClipTool _ _ (bin, args) -> do
      done <-
        Timeout.timeout toolTimeoutMicros (Exception.try (Process.readProcessWithExitCode bin args ""))
      case done of
        Just (Right (Exit.ExitSuccess, out, _)) ->
          let
            text = T.pack out
          in
            if T.null text then pure Nothing else pure (Just text)
        Just (Left e) -> const (pure Nothing) (e :: Exception.IOException)
        _ -> pure Nothing

-- ESC ] 52 ; c ; <base64> BEL — the terminal takes it from here.
osc52 :: T.Text -> BS.ByteString
osc52 text =
  BS.concat ["\ESC]52;c;", base64 (TE.encodeUtf8 text), "\a"]

base64Alphabet :: BS.ByteString
base64Alphabet =
  BSC.pack (['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "+/")

-- Hand-rolled rather than pulling in a dependency for twenty lines.
base64 :: BS.ByteString -> BS.ByteString
base64 input =
  let
    pad = 61 :: DataWord.Word8 -- '='
    sym i = BS.index base64Alphabet i
    triple :: DataWord.Word8 -> DataWord.Word8 -> DataWord.Word8 -> Int
    triple a b c =
      Bits.shiftL (fromIntegral a) 16 .|. Bits.shiftL (fromIntegral b) 8 .|. fromIntegral c
    chunk n = map (\s -> sym (Bits.shiftR n s .&. 63)) [18, 12, 6, 0]
    go bytes =
      case bytes of
        [] -> []
        (a : b : c : rest) -> chunk (triple a b c) ++ go rest
        [a, b] -> take 3 (chunk (triple a b 0)) ++ [pad]
        [a] -> take 2 (chunk (triple a 0 0)) ++ [pad, pad]
  in
    BS.pack (go (BS.unpack input))
