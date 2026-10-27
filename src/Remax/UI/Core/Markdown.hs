{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Core.Markdown
  ( Segment (..)
  , Row (..)
  , render
  , renderText
  , firstLine
  , plainRow
  ) where

import Brick qualified as Brick
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.Conversation qualified as Conv
import Remax.UI.Core.Attrs qualified as Attrs

{- | One styled run of a rendered row. A link or image carries the URL
it opens.
-}
data Segment = Segment
  { segAttr :: Brick.AttrName
  , segText :: T.Text
  , segUrl :: Maybe T.Text
  }
  deriving (Eq, Show)

{- | One screen row of a rendered body. A code block's rows fill to the
width with their tint; every other row ends where its text does.
-}
data Row = Row
  { rowFill :: Maybe Brick.AttrName
  , rowSegments :: [Segment]
  }
  deriving (Eq, Show)

plainRow :: T.Text -> Row
plainRow t = Row Nothing [Segment Attrs.msgAttr t Nothing]

render :: Int -> Conv.Markdown -> [Row]
render width = concatMap (layoutBlock (max 8 width)) . blocks . stripComments . Conv.markdownText

renderText :: Row -> T.Text
renderText = T.concat . map segText . rowSegments

firstLine :: Conv.Markdown -> T.Text
firstLine body =
  case render 200 body of
    [] -> ""
    (r : _) -> renderText r

-- Blocks ---------------------------------------------------------------

data Block
  = Paragraph [Inline]
  | Heading Int [Inline]
  | Bullet [Inline]
  | Quote [Inline]
  | Code [T.Text]
  | Table [[Inline]] [[[Inline]]]
  | Details T.Text
  | Blank

data Inline
  = Plain T.Text
  | Strong T.Text
  | Mono T.Text
  | Link T.Text T.Text
  | Image T.Text T.Text

stripComments :: T.Text -> T.Text
stripComments t =
  case T.breakOn "<!--" t of
    (before, rest)
      | T.null rest -> before
      | otherwise -> before <> stripComments (T.drop 3 (snd (T.breakOn "-->" rest)))

blocks :: T.Text -> [Block]
blocks = collapseBlanks . blocksOf . map T.stripEnd . T.lines . T.replace "\r" ""

blocksOf :: [T.Text] -> [Block]
blocksOf ls =
  let
    go = blocksOf
    paragraphLine l =
      not (T.null (T.strip l))
        && not (isFence l)
        && Maybe.isNothing (heading l)
        && Maybe.isNothing (bullet l)
        && not (T.isPrefixOf ">" l)
        && not (T.isPrefixOf "<details" (T.toLower (T.strip l)))
    continuation l = not (T.null (T.strip l)) && T.isPrefixOf " " l && Maybe.isNothing (bullet l)
  in
    case ls of
      [] -> []
      (l : rest)
        | isFence l ->
            let
              (body, after) = break isFence rest
            in
              Code body : go (drop 1 after)
        | T.null (T.strip l) -> Blank : go rest
        | "<details" `T.isPrefixOf` T.toLower (T.strip l) ->
            let
              (inside, after) = break (T.isPrefixOf "</details" . T.toLower . T.strip) (l : rest)
            in
              Details (summaryOf inside) : go (drop 1 after)
        | Just (level, text) <- heading l -> Heading level (inlines text) : go rest
        | Just item <- bullet l ->
            let
              (continued, after) = span continuation rest
            in
              Bullet (inlines (T.unwords (item : map T.strip continued))) : go after
        | T.isPrefixOf ">" l ->
            let
              (more, after) = span (T.isPrefixOf ">") rest
              quoted = map (T.strip . T.drop 1) (l : more)
            in
              map (Quote . inlines . T.unwords) (quoteParagraphs quoted) ++ go after
        | isTableRow l && any isTableRule (take 1 rest) ->
            let
              (body, after) = span isTableRow (drop 1 rest)
            in
              Table (map inlines (cells l)) (map (map inlines . cells) body) : go after
        | otherwise ->
            let
              (more, after) = span paragraphLine rest
            in
              Paragraph (inlines (T.unwords (map T.strip (l : more)))) : go after

collapseBlanks :: [Block] -> [Block]
collapseBlanks bs =
  case bs of
    (Blank : Blank : rest) -> collapseBlanks (Blank : rest)
    (b : rest) -> b : collapseBlanks rest
    [] -> []

isFence :: T.Text -> Bool
isFence l = T.isPrefixOf "```" (T.stripStart l) || T.isPrefixOf "~~~" (T.stripStart l)

heading :: T.Text -> Maybe (Int, T.Text)
heading l =
  let
    hashes = T.takeWhile (== '#') l
    rest = T.drop (T.length hashes) l
  in
    if T.length hashes >= 1 && T.length hashes <= 6 && T.isPrefixOf " " rest
      then Just (T.length hashes, T.strip rest)
      else Nothing

bullet :: T.Text -> Maybe T.Text
bullet l =
  let
    s = T.stripStart l
    numbered =
      let
        (digits, rest) = T.span Char.isDigit s
      in
        if not (T.null digits) && (T.isPrefixOf ". " rest || T.isPrefixOf ") " rest)
          then Just (T.drop 2 rest)
          else Nothing
  in
    case Maybe.mapMaybe (\m -> T.stripPrefix m s) ["- [ ] ", "- [x] ", "- ", "* ", "+ "] of
      (item : _) -> Just item
      [] -> numbered

isTableRow :: T.Text -> Bool
isTableRow l = T.isPrefixOf "|" (T.stripStart l) && T.count "|" l >= 2

isTableRule :: T.Text -> Bool
isTableRule l = isTableRow l && T.all (\c -> c `elem` ("|-: " :: String)) l

cells :: T.Text -> [T.Text]
cells l =
  let
    inner = T.dropWhileEnd (== '|') (T.dropWhile (== '|') (T.strip l))
  in
    map T.strip (T.splitOn "|" inner)

quoteParagraphs :: [T.Text] -> [[T.Text]]
quoteParagraphs ls =
  case dropWhile T.null ls of
    [] -> []
    rest ->
      let
        (paragraph, after) = break T.null rest
      in
        paragraph : quoteParagraphs after

summaryOf :: [T.Text] -> T.Text
summaryOf ls =
  let
    joined = T.unwords ls
    afterOpen = snd (T.breakOn "<summary>" joined)
  in
    if T.null afterOpen
      then "details"
      else T.strip (fst (T.breakOn "</summary>" (T.drop 9 afterOpen)))

-- Inlines ----------------------------------------------------------------

inlines :: T.Text -> [Inline]
inlines t
  | T.null t = []
  | Just rest <- T.stripPrefix "![" t
  , Just (alt, url, after) <- linkParts rest =
      Image alt url : inlines after
  | Just rest <- T.stripPrefix "[" t
  , Just (label, url, after) <- linkParts rest =
      Link label url : inlines after
  | Just rest <- T.stripPrefix "**" t
  , (strong, after) <- T.breakOn "**" rest
  , not (T.null after) =
      Strong strong : inlines (T.drop 2 after)
  | Just rest <- T.stripPrefix "`" t
  , (mono, after) <- T.breakOn "`" rest
  , not (T.null after) =
      Mono mono : inlines (T.drop 1 after)
  | Just url <- bareUrl t =
      Link url url : inlines (T.drop (T.length url) t)
  | Just rest <- T.stripPrefix "<" t
  , (tag, after) <- T.breakOn ">" rest
  , not (T.null after) && T.all (\c -> Char.isAlphaNum c || c `elem` ("/ =\"'-" :: String)) tag =
      inlines (T.drop 1 after)
  | otherwise =
      let
        (plain, rest) = T.span (\c -> c `notElem` ("[!*`<h" :: String)) t
        (plainMore, restMore) = T.span (\c -> c `notElem` ("[!*`<h" :: String)) (T.drop 1 rest)
        stepOver = plain <> T.take 1 rest <> plainMore
      in
        if T.null plain || (T.isPrefixOf "h" rest && Maybe.isNothing (bareUrl rest))
          then Plain stepOver : inlines restMore
          else Plain plain : inlines rest

linkParts :: T.Text -> Maybe (T.Text, T.Text, T.Text)
linkParts rest =
  let
    (label, afterLabel) = T.breakOn "](" rest
    (url, afterUrl) = T.breakOn ")" (T.drop 2 afterLabel)
  in
    if T.null afterLabel || T.null afterUrl
      then Nothing
      else Just (label, T.strip (fst (T.breakOn " " url)), T.drop 1 afterUrl)

bareUrl :: T.Text -> Maybe T.Text
bareUrl t =
  if T.isPrefixOf "http://" t || T.isPrefixOf "https://" t
    then Just (T.takeWhile (\c -> not (Char.isSpace c) && c `notElem` (")]>" :: String)) t)
    else Nothing

-- Layout -----------------------------------------------------------------

layoutBlock :: Int -> Block -> [Row]
layoutBlock width block =
  case block of
    Blank -> [Row Nothing []]
    Paragraph ins -> wrapInlines width Attrs.msgAttr "" "" ins
    Heading level ins ->
      wrapInlines width (if level <= 2 then Attrs.mdHeadingAttr else Attrs.mdBoldAttr) "" "" ins
    Bullet ins -> wrapInlines width Attrs.msgAttr "\8226 " "  " ins
    Quote ins -> wrapInlines width Attrs.hintAttr "\9474 " "\9474 " ins
    Code ls -> map (\l -> Row (Just Attrs.mdCodeAttr) [Segment Attrs.mdCodeAttr (" " <> T.take (width - 1) l) Nothing]) (if null ls then [""] else ls)
    Details summary -> [Row Nothing [Segment Attrs.hintAttr ("\9656 " <> summary <> "  za opens") Nothing]]
    Table header body -> tableRows width header body

wrapInlines :: Int -> Brick.AttrName -> T.Text -> T.Text -> [Inline] -> [Row]
wrapInlines width base lead continueLead ins =
  let
    ws = wordsOf (concatMap (atomsOf base) ins)
    firstWidth = width - T.length lead
    restWidth = width - T.length continueLead
    rows = wrapWords base firstWidth restWidth ws
    withLead i segs =
      let
        l = if i == (0 :: Int) then lead else continueLead
      in
        Row Nothing (coalesce (if T.null l then segs else Segment base l Nothing : segs))
  in
    case rows of
      [] -> [withLead 0 []]
      _ -> zipWith withLead [0 ..] rows

data Atom = Space | Chunk Segment

atomsOf :: Brick.AttrName -> Inline -> [Atom]
atomsOf base inline =
  let
    spaced attr t =
      List.intercalate [Space] (map (\w -> if T.null w then [] else [Chunk (Segment attr w Nothing)]) (T.split (== ' ') t))
  in
    case inline of
      Plain t -> spaced base t
      Strong t -> spaced (bold base) t
      Mono t -> [Chunk (Segment Attrs.mdCodeAttr t Nothing)]
      Link label url -> [Chunk (Segment Attrs.mdLinkAttr label (Just url))]
      Image alt url ->
        [ Chunk (Segment Attrs.mdLinkAttr ("\9635 " <> alt) (Just url))
        , Space
        , Chunk (Segment Attrs.hintAttr "image" (Just url))
        ]

{- | Chunks with no space between them are one word, however many
styles they span.
-}
wordsOf :: [Atom] -> [[Segment]]
wordsOf atoms =
  let
    (word, rest) = span isChunk atoms
    segs = Maybe.mapMaybe chunkSegment word
  in
    case atoms of
      [] -> []
      (Space : more) -> wordsOf more
      _ -> segs : wordsOf rest

isChunk :: Atom -> Bool
isChunk a =
  case a of
    Chunk _ -> True
    Space -> False

chunkSegment :: Atom -> Maybe Segment
chunkSegment a =
  case a of
    Chunk seg -> Just seg
    Space -> Nothing

bold :: Brick.AttrName -> Brick.AttrName
bold base = if base == Attrs.mdHeadingAttr then base else Attrs.mdBoldAttr

wrapWords :: Brick.AttrName -> Int -> Int -> [[Segment]] -> [[Segment]]
wrapWords base firstWidth restWidth =
  let
    wordWidth = sum . map (T.length . segText)
    space = Segment base " " Nothing
    go _ acc _ [] = if null acc then [] else [concat (reverse acc)]
    go limit acc used (w : ws) =
      let
        width = wordWidth w
      in
        if null acc
          then go limit [w] width ws
          else
            if used + 1 + width <= limit
              then go limit (w : [space] : acc) (used + 1 + width) ws
              else concat (reverse acc) : go restWidth [w] width ws
  in
    go firstWidth [] 0

coalesce :: [Segment] -> [Segment]
coalesce segs =
  case segs of
    (a : b : rest)
      | segAttr a == segAttr b && segUrl a == segUrl b -> coalesce (a {segText = segText a <> segText b} : rest)
    (a : rest) -> a : coalesce rest
    [] -> []

tableRows :: Int -> [[Inline]] -> [[[Inline]]] -> [Row]
tableRows width header body =
  let
    cellText = T.concat . map inlineText
    rows = map (map cellText) (header : body)
    columns = maximum (1 : map length rows)
    widths = map (\i -> maximum (1 : map (\r -> T.length (Maybe.fromMaybe "" (List.lookup i (zip [0 ..] r)))) rows)) [0 .. columns - 1]
    padded r = zipWith (\w c -> T.justifyLeft w ' ' c) widths (r ++ replicate (columns - length r) "")
    line attr r = Row Nothing [Segment attr (T.take width (T.intercalate "  " (padded r))) Nothing]
  in
    line Attrs.mdTableHeaderAttr (map cellText header) : map (line Attrs.msgAttr . map cellText) body

inlineText :: Inline -> T.Text
inlineText i =
  case i of
    Plain t -> t
    Strong t -> t
    Mono t -> t
    Link label _ -> label
    Image alt _ -> "\9635 " <> alt
