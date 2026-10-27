{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Model.Work
  ( scopeOf
  , scopeFor
  , loadFor
  , reloadWork
  , withConversations
  , placeOutdated
  , remoteOf
  ) where

import Brick qualified as Brick
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Database.SQLite.Simple qualified as SQLite
import Remax.Conversation qualified as Conv
import Remax.DiffModel qualified as DiffModel
import Remax.Git qualified as Git
import Remax.PullPoll qualified as PullPoll
import Remax.Store qualified as Store
import Remax.UI.Core.Anchor qualified as Anchor
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.View qualified as View
import Remax.WorkStore qualified as WorkStore

scopeFor :: FilePath -> Git.ReviewMode -> WorkStore.Scope
scopeFor root mode =
  WorkStore.Scope
    { WorkStore.scopeRepo = T.pack root
    , WorkStore.scopePull = case Git.subjectOf mode of
        Git.Pull pr _ -> Just (Git.prNumber pr)
        _ -> Nothing
    }

scopeOf :: Types.St -> WorkStore.Scope
scopeOf st = scopeFor (Types.stRoot st) (Types.stMode st)

loadFor :: SQLite.Connection -> FilePath -> Git.ReviewMode -> IO Conv.OwnWork
loadFor conn root mode = WorkStore.loadWork conn (scopeFor root mode)

reloadWork :: Brick.EventM Types.Name Types.St ()
reloadWork = do
  st <- State.get
  own <- MonadIO.liftIO (WorkStore.loadWork (Types.stConn st) (scopeOf st))
  State.modify (\s -> withConversations s {Types.stWork = own})
  placeOutdated

{- | Find where each conversation whose lines left a file draws now: the
line the diff from its commit's copy of the file to the buffer maps
its first old line to.
-}
placeOutdated :: Brick.EventM Types.Name Types.St ()
placeOutdated = do
  st <- State.get
  placed <- MonadIO.liftIO (mapM (placeIn (Types.stRoot st) (Types.stConversations st)) (Types.stFiles st))
  State.modify (\s -> withConversations s {Types.stOutdated = Map.unions placed})

placeIn :: FilePath -> [Conv.Conversation] -> Types.FileView -> IO (Map.Map Conv.ConversationId Types.Outdated)
placeIn root convs fv = do
  let
    path = T.pack (Types.fvPath fv)
    byKey = Map.fromList (concatMap (\(i, r) -> map (\k -> (Store.LineKey k, i)) (Maybe.catMaybes [DiffModel.rKey r, DiffModel.rRemarkKey r])) (zip [0 :: Int ..] (Types.fvRows fv)))
    gone conv =
      case Conv.convAnchor conv of
        Conv.OnLines lineSpan
          | Conv.spanPath lineSpan == path
          , Just sha <- Conv.spanAt lineSpan ->
              case Anchor.survives byKey lineSpan of
                Anchor.Gone -> Just (sha, map (\l -> (Conv.slLineNo l, T.drop 1 (Conv.slText l))) (NEL.toList (Conv.spanLines lineSpan)))
                Anchor.Survives _ -> Nothing
        Conv.OnOutdated out
          | Conv.outPath out == path ->
              let
                lo = Maybe.fromMaybe (Conv.outLine out) (Conv.outStartLine out)
                hunk = filter (not . T.isPrefixOf "@@") (T.lines (Conv.outDiffHunk out))
                shown = reverse (take (Conv.outLine out - lo + 1) (reverse hunk))
              in
                Just (Conv.outCommit out, zip [lo ..] (map (T.drop 1) shown))
        _ -> Nothing
    candidates = Maybe.mapMaybe (\c -> fmap (\g -> (Conv.convId c, g)) (gone c)) convs
    current = View.newSideLines fv
  placed <- mapM (\(cid, (sha, oldLines)) -> fmap (fmap (\n -> (cid, Types.Outdated path n))) (mapOldLines root path current sha oldLines)) candidates
  pure (Map.fromList (Maybe.catMaybes placed))

{- | The commit's copy of the file is the old side of the diff. Lines
the commit does not hold, written in a working tree, are put back at
their numbers first, so the diff can see them leave.
-}
mapOldLines :: FilePath -> T.Text -> [T.Text] -> Conv.CommitSha -> [(Int, T.Text)] -> IO (Maybe Int)
mapOldLines root path current (Conv.CommitSha sha) oldLines = do
  old <- Git.refContent root sha (T.unpack path)
  pure $ case (old, oldLines) of
    (Just (Git.RefText ls), (firstNo, _) : _) ->
      let
        held = all (\(n, t) -> Maybe.listToMaybe (drop (n - 1) ls) == Just t) oldLines
        withLines = if held then ls else take (firstNo - 1) ls ++ map snd oldLines ++ drop (firstNo - 1) ls
      in
        Just (restingLine (DiffModel.diffRows DiffModel.WhitespaceMatters withLines current) firstNo)
    _ -> Nothing

{- | The current line an old line answers to: its own when it survived,
else the first line that follows where it was.
-}
restingLine :: [(Char, Maybe Int, T.Text)] -> Int -> Int
restingLine rows oldLine =
  let
    numbered = snd (List.mapAccumL (\base (mark, new, _) -> (if mark /= '+' then base + 1 else base, (mark, if mark /= '+' then Just base else Nothing, new))) 1 rows)
    after = dropWhile (\(_, base, _) -> maybe True (< oldLine) base) numbered
  in
    case Maybe.mapMaybe (\(_, _, new) -> new) after of
      (n : _) -> n + 1
      [] -> length (filter (\(m, _, _) -> m /= '-') rows)

remoteOf :: Types.St -> Conv.Remote
remoteOf st =
  case Types.ppSnapshot (Types.stPullPoll st) of
    Just snap -> PullPoll.snapshotRemote snap
    Nothing -> Conv.emptyRemote

withConversations :: Types.St -> Types.St
withConversations st =
  let
    convs = Conv.conversations (remoteOf st) (Types.stWork st)
  in
    st
      { Types.stConversations = convs
      , Types.stFiles = map (\fv -> fv {Types.fvThreads = Anchor.anchorIn (Types.stOutdated st) convs fv}) (Types.stFiles st)
      }
