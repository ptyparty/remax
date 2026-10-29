{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Uses
  ( usesUnderCursor
  , findUses
  ) where

import Brick qualified as Brick
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Control.Monad.State qualified as State
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.DiffModel qualified as DiffModel
import Remax.UI.Action.Picker qualified as PickerAction
import Remax.UI.Action.Search qualified as Search
import Remax.UI.Core.Jobs qualified as Jobs
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.View qualified as View
import Remax.UI.Core.Widgets qualified as Widgets
import Remax.UI.Model.Nav qualified as Nav

data Scanned = Scanned
  { scPath :: FilePath
  , scRuns :: [DiffModel.Run]
  , scDecls :: [T.Text]
  }

data Mention = Mention
  { mnPath :: FilePath
  , mnHunk :: Int
  , mnKeys :: [T.Text]
  , mnLineKey :: T.Text
  , mnLine :: Int
  , mnMark :: Char
  , mnColumn :: Int
  , mnDecl :: T.Text
  , mnText :: T.Text
  }

data Request = Request
  { rqName :: T.Text
  , rqEpoch :: Int
  , rqOrigin :: Maybe FilePath
  }

usesUnderCursor :: Brick.EventM Types.Name Types.St ()
usesUnderCursor = do
  st <- State.get
  case Types.currentFileMay st >>= Search.wordUnderCursor . Types.fvEd of
    Nothing -> Types.setMsg "no name under the cursor"
    Just name -> findUses name

findUses :: T.Text -> Brick.EventM Types.Name Types.St ()
findUses name
  | T.null (T.strip name) = Types.setMsg "mentions of what? put the cursor on a name"
  | otherwise = do
      st <- State.get
      let
        request =
          Request
            { rqName = name
            , rqEpoch = Types.stEpoch st
            , rqOrigin = fmap Types.fvPath (Types.currentFileMay st)
            }
        here = do
          fv <- Types.currentFileMay st
          h <- Nav.hunkUnderCursor st
          pure (Types.fvPath fv, h)
        scans = map scanOf (Types.stFiles st)
      State.modify (\s -> s {Types.stFindingUses = Just name})
      Jobs.startJob ("finding " <> name) (forceMentions (mentionsIn name here scans)) (finishUses request)

finishUses :: Request -> Either T.Text [Mention] -> Brick.EventM Types.Name Types.St ()
finishUses request outcome = do
  latest <- State.get
  Monad.when (Types.stEpoch latest == rqEpoch request && Types.stFindingUses latest == Just (rqName request)) $ do
    State.modify (\s -> s {Types.stFindingUses = Nothing})
    case outcome of
      Left err -> Jobs.jobFailed ("finding " <> rqName request) err
      Right found
        | movedOn latest request -> Types.setMsg ("moved on before the hunks mentioning " <> rqName request <> " were found")
        | otherwise -> presentUses (rqName request) found

movedOn :: Types.St -> Request -> Bool
movedOn st request =
  Maybe.isJust (Types.stPicker st)
    || Maybe.isJust (Types.stTree st)
    || Maybe.isJust (Types.stPrompt st)
    || fmap Types.fvPath (Types.currentFileMay st) /= rqOrigin request

scanOf :: Types.FileView -> Scanned
scanOf fv =
  Scanned
    { scPath = Types.fvPath fv
    , scRuns = Types.fvRuns fv
    , scDecls = Types.fvRunDecls fv
    }

forceMentions :: [Mention] -> IO [Mention]
forceMentions found = do
  _ <- Exception.evaluate (List.foldl' (\n m -> n + mnLine m + mnColumn m + T.length (mnText m)) 0 found)
  pure found

mentionsIn :: T.Text -> Maybe (FilePath, Int) -> [Scanned] -> [Mention]
mentionsIn name here scans =
  let
    elsewhere m = Just (mnPath m, mnHunk m) /= here
  in
    take (hitCap + 1) (filter elsewhere (concatMap (fileMentions name) scans))

hitCap :: Int
hitCap = 200

fileMentions :: T.Text -> Scanned -> [Mention]
fileMentions name sc =
  Maybe.catMaybes (zipWith (runMention name sc) [0 ..] (scRuns sc))

runMention :: T.Text -> Scanned -> Int -> DiffModel.Run -> Maybe Mention
runMention name sc h run = do
  let
    marked = zip3 [0 :: Int ..] (DiffModel.runKeys run) (DiffModel.runLines run)
    hit (i, key, line) = do
      (mark, body) <- T.uncons line
      col <- mentionColumn name body
      pure (i, key, mark, body, col)
  (i, key, mark, body, col) <- Maybe.listToMaybe (Maybe.mapMaybe hit marked)
  let
    addedBefore = length (filter (T.isPrefixOf "+") (take i (DiffModel.runLines run)))
  pure
    Mention
      { mnPath = scPath sc
      , mnHunk = h
      , mnKeys = DiffModel.runKeys run
      , mnLineKey = key
      , mnLine = DiffModel.runAnchor run + addedBefore + 1
      , mnMark = mark
      , mnColumn = col
      , mnDecl = Maybe.fromMaybe "" (Types.safeIdx (scDecls sc) h)
      , mnText = T.strip body
      }

mentionColumn :: T.Text -> T.Text -> Maybe Int
mentionColumn name line =
  let
    wordy ch = Char.isAlphaNum ch || ch == '_'
    before pre = fmap snd (T.unsnoc pre)
    after from = fmap fst (T.uncons (T.drop (T.length name) from))
    quoted pre from = before pre == Just '\'' && after from == Just '\''
    joins (pre, from) =
      maybe False wordy (before pre)
        || (maybe False Search.isIdentChar (after from) && not (quoted pre from))
  in
    fmap (T.length . fst) (List.find (not . joins) (T.breakOnAll name line))

presentUses :: T.Text -> [Mention] -> Brick.EventM Types.Name Types.St ()
presentUses name found =
  let
    hits = map (usesHit name) (take hitCap found)
    extent = if length found > hitCap then Types.UsesCapped else Types.UsesComplete
  in
    case hits of
      [] -> Types.setMsg ("no other hunk mentions " <> name)
      [hit] -> do
        Nav.parkOnLine (Types.uhAnchor hit) (Types.uhLine hit)
        Types.setMsg ("the one other hunk mentioning " <> name)
      _ -> PickerAction.openUsesPicker (Types.UsesPicker {Types.upName = name, Types.upHits = hits, Types.upExtent = extent})

usesHit :: T.Text -> Mention -> Types.UsesHit
usesHit name m =
  Types.UsesHit
    { Types.uhRow = usesRow m
    , Types.uhAnchor =
        Types.Anchor
          { Types.anPath = mnPath m
          , Types.anHunk = mnHunk m
          , Types.anKeys = mnKeys m
          }
    , Types.uhLine =
        Types.LineMark
          { Types.lmKey = mnLineKey m
          , Types.lmSpan =
              if mnMark m == '+'
                then Just (mnColumn m, T.length name)
                else Nothing
          }
    }

usesRow :: Mention -> T.Text
usesRow m =
  let
    decl =
      if T.null (mnDecl m)
        then ""
        else "  " <> T.take 24 (View.declName (mnDecl m))
  in
    Widgets.shortPath 48 (T.pack (mnPath m))
      <> ":"
      <> Types.tshow (mnLine m)
      <> "  "
      <> T.singleton (mnMark m)
      <> decl
      <> "  \183  "
      <> T.take 160 (mnText m)
