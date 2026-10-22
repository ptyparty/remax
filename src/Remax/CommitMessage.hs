{-# LANGUAGE OverloadedStrings #-}

module Remax.CommitMessage
  ( unfill
  , fill
  ) where

import Data.Char qualified as Char
import Data.List qualified as List
import Data.Text qualified as T
import Remax.Modal qualified as Modal

data BodyLine
  = Prose
  | Item Int
  | Verbatim

data Run = Run BodyLine [T.Text]

bodyWidth :: Int
bodyWidth = 72

classify :: T.Text -> BodyLine
classify l =
  case itemHang l of
    Just hang -> Item hang
    Nothing
      | "#" `T.isPrefixOf` l || T.any Char.isSpace (T.take 1 l) -> Verbatim
      | otherwise -> Prose

itemHang :: T.Text -> Maybe Int
itemHang l =
  let
    indent = T.length (T.takeWhile (== ' ') l)
    rest = T.drop indent l
    digits = T.takeWhile Char.isDigit rest
    afterDigits = T.drop (T.length digits) rest
  in
    case T.uncons rest of
      Just (c, r)
        | c `elem` ['-', '*', '+'] && " " `T.isPrefixOf` r -> Just (indent + 2)
      _
        | not (T.null digits) && any (`T.isPrefixOf` afterDigits) [". ", ") "] ->
            Just (indent + T.length digits + 2)
      _ -> Nothing

isTrailer :: T.Text -> Bool
isTrailer l =
  let
    (key, rest) = T.breakOn ": " l
  in
    not (T.null key)
      && T.all (\c -> Char.isAlphaNum c || c == '-') key
      && T.length rest > 2

paragraphs :: [T.Text] -> [[T.Text]]
paragraphs = List.groupBy (\a b -> isBlank a == isBlank b)

isBlank :: T.Text -> Bool
isBlank = T.null . T.strip

keepsLineBreaks :: [T.Text] -> Bool
keepsLineBreaks para = all isBlank para || all isTrailer para

unfill :: T.Text -> [T.Text]
unfill msg =
  case T.lines msg of
    [] -> [""]
    (subject : body) -> subject : concatMap unfillParagraph (paragraphs body)

unfillParagraph :: [T.Text] -> [T.Text]
unfillParagraph para
  | keepsLineBreaks para = para
  | otherwise =
      let
        (done, open) = List.foldl' unfillStep ([], Nothing) para
      in
        reverse (closeRun open done)

unfillStep :: ([T.Text], Maybe Run) -> T.Text -> ([T.Text], Maybe Run)
unfillStep (done, open) l =
  let
    extend (Run kind parts) = (done, Just (Run kind (T.strip l : parts)))
    start kind = (closeRun open done, Just (Run kind [T.stripEnd l]))
  in
    case (classify l, open) of
      (Item hang, _) -> start (Item hang)
      (Prose, Just run@(Run Prose _)) -> extend run
      (Prose, _) -> start Prose
      (Verbatim, Just run@(Run (Item hang) _))
        | T.length (T.takeWhile (== ' ') l) == hang -> extend run
      _ -> (l : closeRun open done, Nothing)

closeRun :: Maybe Run -> [T.Text] -> [T.Text]
closeRun open done =
  case open of
    Nothing -> done
    Just (Run _ parts) -> T.unwords (reverse parts) : done

fill :: T.Text -> T.Text
fill msg =
  case T.lines msg of
    [] -> ""
    (subject : body) ->
      T.intercalate "\n" (subject : concatMap fillParagraph (paragraphs body))

fillParagraph :: [T.Text] -> [T.Text]
fillParagraph para
  | keepsLineBreaks para = para
  | otherwise = concatMap fillLine para

fillLine :: T.Text -> [T.Text]
fillLine l
  | T.length l <= bodyWidth = [l]
  | otherwise =
      case classify l of
        Prose -> hangingWrap 0 l
        Item hang -> hangingWrap hang l
        Verbatim -> [l]

hangingWrap :: Int -> T.Text -> [T.Text]
hangingWrap hang l =
  let
    (lead, content) = T.splitAt hang l
    indent = T.replicate hang " "
  in
    case Modal.greedyWrap (max 1 (bodyWidth - hang)) (T.words content) of
      [] -> [l]
      (first : rest) -> (lead <> T.unwords first) : map ((indent <>) . T.unwords) rest
