{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Core.Anchor
  ( anchorIn
  , anchorRows
  , anchorCovers
  , conversationsAt
  , conversationsIn
  , lineSpanFor
  ) where

import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Remax.Conversation qualified as Conv
import Remax.DiffModel qualified as DiffModel
import Remax.Store qualified as Store
import Remax.UI.Core.Types qualified as Types

{- | Which display row each conversation of this file draws under: a
GitHub thread under the line GitHub numbers it at, a local anchor
under the last of its lines still in the file. Rows map to the
conversations that end on them, oldest first.
-}
anchorIn :: [Conv.Conversation] -> Types.FileView -> Map.Map Int [Conv.ConversationId]
anchorIn convs fv =
  let
    path = T.pack (Types.fvPath fv)
    rows = Types.fvRows fv
    located = zip [0 ..] (locate rows)
    byHead = Map.fromList (Maybe.mapMaybe (\(i, l) -> fmap (\n -> (n, i)) (locHead l)) located)
    byBase = Map.fromList (Maybe.mapMaybe (\(i, l) -> fmap (\n -> (n, i)) (locBase l)) located)
    byKey =
      Map.fromList
        ( concatMap
            (\(i, l) -> map (\k -> (Store.LineKey k, i)) (Maybe.catMaybes [DiffModel.rKey (locRow l), DiffModel.rRemarkKey (locRow l)]))
            located
        )
    rowFor conv = do
      rowIx <- anchorRow path byHead byBase byKey (Conv.convAnchor conv)
      Just (rowIx, [Conv.convId conv])
    oldestFirst = List.sortOn Conv.conversationTime convs
  in
    Map.fromListWith (flip (++)) (Maybe.mapMaybe rowFor oldestFirst)

anchorRow ::
  T.Text ->
  Map.Map Int Int ->
  Map.Map Int Int ->
  Map.Map Store.LineKey Int ->
  Conv.Anchor ->
  Maybe Int
anchorRow path byHead byBase byKey anchor =
  case anchor of
    Conv.OnPosition pos
      | Conv.posPath pos == path ->
          case Conv.posSide pos of
            Conv.HeadSide -> Map.lookup (Conv.posLine pos) byHead
            Conv.BaseSide -> Map.lookup (Conv.posLine pos) byBase
    Conv.OnLines lineSpan
      | Conv.spanPath lineSpan == path ->
          Maybe.listToMaybe (reverse (List.sort (Maybe.mapMaybe (`Map.lookup` byKey) (Conv.spanKeys lineSpan))))
    _ -> Nothing

-- | The display rows a local anchor still covers, for washing them.
anchorRows :: Types.FileView -> Conv.Anchor -> [Int]
anchorRows fv anchor =
  let
    path = T.pack (Types.fvPath fv)
    rows = zip [0 ..] (locate (Types.fvRows fv))
    keyed keys = Maybe.mapMaybe (\(i, l) -> if any (`elem` keys) (rowKeys (locRow l)) then Just i else Nothing) rows
  in
    case anchor of
      Conv.OnLines lineSpan | Conv.spanPath lineSpan == path -> keyed (map Store.lineKeyToText (Conv.spanKeys lineSpan))
      Conv.OnPosition pos
        | Conv.posPath pos == path ->
            let
              lo = Maybe.fromMaybe (Conv.posLine pos) (Conv.posStartLine pos)
              hi = Conv.posLine pos
              lineOf l = case Conv.posSide pos of
                Conv.HeadSide -> locHead l
                Conv.BaseSide -> locBase l
            in
              Maybe.mapMaybe (\(i, l) -> if maybe False (\n -> n >= lo && n <= hi) (lineOf l) then Just i else Nothing) rows
      _ -> []

{- | Whether a row sits inside an anchor's range, by its keys for a local
span or by its head line for a GitHub position. A base-side position
is left to the row it ends on.
-}
anchorCovers :: T.Text -> DiffModel.RowR -> Conv.Anchor -> Bool
anchorCovers path row anchor =
  case anchor of
    Conv.OnLines lineSpan
      | Conv.spanPath lineSpan == path -> any (`elem` map Store.lineKeyToText (Conv.spanKeys lineSpan)) (rowKeys row)
    Conv.OnPosition pos
      | Conv.posPath pos == path && Conv.posSide pos == Conv.HeadSide ->
          case DiffModel.rNewRow row of
            Just n -> n + 1 >= Maybe.fromMaybe (Conv.posLine pos) (Conv.posStartLine pos) && n + 1 <= Conv.posLine pos
            Nothing -> False
    _ -> False

rowKeys :: DiffModel.RowR -> [T.Text]
rowKeys r = Maybe.catMaybes [DiffModel.rKey r, DiffModel.rRemarkKey r]

conversationsAt :: Types.St -> Types.FileView -> Int -> [Conv.Conversation]
conversationsAt st fv rowIx =
  conversationsIn st (Map.findWithDefault [] rowIx (Types.fvThreads fv))

conversationsIn :: Types.St -> [Conv.ConversationId] -> [Conv.Conversation]
conversationsIn st ids =
  Maybe.mapMaybe (\cid -> Conv.conversationFor cid (Types.stConversations st)) ids

lineSpanFor :: Types.St -> Types.FileView -> [T.Text] -> Maybe Conv.LineSpan
lineSpanFor st fv keys =
  let
    wanted = Set.fromList keys
    indexed = zip [0 :: Int ..] (locate (Types.fvRows fv))
    keyOf l = Maybe.listToMaybe (filter (`Set.member` wanted) (rowKeys (locRow l)))
    picked = Maybe.mapMaybe (\(i, l) -> fmap (\k -> (i, l, k)) (keyOf l)) indexed
    allRemoved = all (\(_, l, _) -> DiffModel.rMark (locRow l) == '-') picked
    side = if allRemoved then Conv.BaseSide else Conv.HeadSide
    headNear i = Maybe.listToMaybe (Maybe.mapMaybe (locHead . snd) (drop i indexed ++ reverse (take i indexed)))
    numberOf i l = case side of
      Conv.BaseSide -> Maybe.fromMaybe 0 (locBase l)
      Conv.HeadSide -> Maybe.fromMaybe 0 (Maybe.listToMaybe (Maybe.catMaybes [locHead l, headNear i]))
    spanLine (i, l, k) = Conv.SpanLine (Store.LineKey k) (numberOf i l) (T.cons (DiffModel.rMark (locRow l)) (DiffModel.rText (locRow l)))
  in
    fmap
      ( \ne ->
          Conv.LineSpan
            { Conv.spanPath = T.pack (Types.fvPath fv)
            , Conv.spanSide = side
            , Conv.spanLines = ne
            , Conv.spanAt = fmap (Conv.CommitSha . fst) (Types.stHead st)
            }
      )
      (NEL.nonEmpty (map spanLine picked))

data Located = Located
  { locRow :: DiffModel.RowR
  , locBase :: Maybe Int
  , locHead :: Maybe Int
  }

{- | Each display row with its one-based line number on the base side
and on the head side, whichever of the two it has.
-}
locate :: [DiffModel.RowR] -> [Located]
locate =
  let
    go _ [] = []
    go base (r : rs) =
      let
        onBase = DiffModel.rMark r /= '+'
      in
        Located
          { locRow = r
          , locBase = if onBase then Just base else Nothing
          , locHead = fmap (+ 1) (DiffModel.rNewRow r)
          }
          : go (if onBase then base + 1 else base) rs
  in
    go 1
