{-# LANGUAGE OverloadedStrings #-}

module Remax.FeedbackText
  ( Detail (..)
  , Change (..)
  , listedText
  , scopeText
  ) where

import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.Conversation qualified as Conv
import Remax.Feedback qualified as Feedback
import Remax.Store qualified as Store
import Remax.WorkStore qualified as WorkStore

data Detail = Compact | Whole
  deriving (Eq, Show)

data Change = Unchanged | New | Changed
  deriving (Eq, Show)

listedText :: Detail -> T.Text -> Change -> Feedback.Listed -> [T.Text]
listedText detail shownId change l =
  headerLine detail shownId change l : linesText detail l ++ threadText detail (Feedback.listedPosts l)

headerLine :: Detail -> T.Text -> Change -> Feedback.Listed -> T.Text
headerLine detail shownId change l =
  let
    place = case (Feedback.listedFile l, Feedback.listedScope l) of
      (Just file, _) -> file
      (Nothing, WorkStore.InPull n) -> "PR #" <> T.pack (show n)
      (Nothing, WorkStore.RepoWide) -> "PR-level"
    commit = maybe [] (\sha -> ["@" <> T.take 8 sha]) (Feedback.listedCommit l)
    awaiting = ["awaiting " <> Feedback.partyText (Feedback.listedParty l)]
    whole = case detail of
      Compact -> []
      Whole -> [scopeText (Feedback.listedScope l), Feedback.listedUpdatedAt l]
    mark = case change of
      Unchanged -> []
      New -> ["new"]
      Changed -> ["changed"]
  in
    T.intercalate "  " (filter (not . T.null) ([shownId, Feedback.listedKind l, place] ++ commit ++ awaiting ++ whole ++ mark))

scopeText :: WorkStore.ConversationScope -> T.Text
scopeText scope =
  case scope of
    WorkStore.RepoWide -> "repo-wide"
    WorkStore.InPull n -> "on PR #" <> T.pack (show n)

linesText :: Detail -> Feedback.Listed -> [T.Text]
linesText detail l =
  let
    shown = filter (not . T.null . Feedback.listedLineText) (Feedback.listedLines l)
    glyph v = case v of
      Just Store.Flagged -> "!"
      Just Store.Questioned -> "?"
      _ -> " "
    one line =
      "  "
        <> glyph (Feedback.listedLineVerdict line)
        <> " "
        <> maybe "" (\n -> T.pack (show n) <> " ") (Feedback.listedLineNo line)
        <> cut detail lineWidth (Feedback.listedLineText line)
  in
    case detail of
      Whole -> map one shown
      Compact ->
        map one (take shownLines shown)
          ++ countedRest (length shown - shownLines)

countedRest :: Int -> [T.Text]
countedRest more =
  if more > 0
    then ["  \8230 " <> T.pack (show more) <> " more lines"]
    else []

threadText :: Detail -> [Conv.Post] -> [T.Text]
threadText detail posts =
  let
    earlier = case detail of
      Compact -> max 0 (length posts - shownPosts)
      Whole -> 0
    counted = if earlier > 0 then ["  (" <> T.pack (show earlier) <> " earlier posts)"] else []
  in
    counted ++ concatMap (postText detail) (drop earlier posts)

postText :: Detail -> Conv.Post -> [T.Text]
postText detail post =
  let
    tag = case Conv.postState post of
      Conv.Draft -> " [pending]"
      Conv.Proposal _ _ -> " [proposal]"
      _ -> ""
    label = Feedback.authorWord (Conv.postAuthor post) <> tag <> ": "
    indent = T.replicate (T.length label) " "
    body = Conv.markdownText (Conv.postBody post)
    bodyLines = case detail of
      Compact
        | Feedback.isBot (Conv.postAuthor post) -> maybe [] (\t -> [cut Compact botWidth t]) (botGist body)
      _ -> T.lines body
  in
    case bodyLines of
      [] -> []
      (firstLine : more) -> ("  " <> label <> firstLine) : map (\ln -> "  " <> indent <> ln) more

botGist :: T.Text -> Maybe T.Text
botGist body =
  Maybe.listToMaybe
    . filter (not . T.null)
    . map (T.strip . stripTags . stripImages)
    . T.lines
    . stripBetween "<!--" "-->"
    $ body

stripImages :: T.Text -> T.Text
stripImages t =
  case T.breakOn "![" t of
    (before, rest)
      | T.null rest -> t
      | otherwise ->
          let
            afterAlt = snd (T.breakOn "](" rest)
          in
            if T.null afterAlt
              then t
              else before <> stripImages (T.drop 1 (snd (T.breakOn ")" afterAlt)))

stripTags :: T.Text -> T.Text
stripTags = stripBetween "<" ">"

stripBetween :: T.Text -> T.Text -> T.Text -> T.Text
stripBetween open close t =
  case T.breakOn open t of
    (before, rest)
      | T.null rest -> t
      | otherwise ->
          case T.breakOn close (T.drop (T.length open) rest) of
            (_, after)
              | T.null after -> before
              | otherwise -> before <> stripBetween open close (T.drop (T.length close) after)

cut :: Detail -> Int -> T.Text -> T.Text
cut detail width t =
  case detail of
    Compact | T.length t > width -> T.take (width - 1) t <> "\8230"
    _ -> t

lineWidth :: Int
lineWidth = 160

botWidth :: Int
botWidth = 200

shownLines :: Int
shownLines = 6

shownPosts :: Int
shownPosts = 2
