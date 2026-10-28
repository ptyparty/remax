{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Inbox
  ( openInbox
  , handleInboxKey
  , jumpTo
  ) where

import Brick qualified as Brick
import Control.Applicative ((<|>))
import Control.Monad qualified as Monad
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Remax.Conversation qualified as Conv
import Remax.DiffModel qualified as DiffModel
import Remax.Git qualified as Git
import Remax.UI.Action.Open qualified as Open
import Remax.UI.Action.Threads qualified as ThreadAction
import Remax.UI.Core.Inbox qualified as Inbox
import Remax.UI.Core.PullView qualified as PullView
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.View qualified as View
import Remax.UI.Model.Nav qualified as Nav

openInbox :: Brick.EventM Types.Name Types.St ()
openInbox = do
  st <- State.get
  case Inbox.entries st "" of
    [] -> Types.setMsg "no conversations yet"
    (first : _) ->
      setInbox
        Types.Inbox
          { Types.ibSel = Just (Conv.convId first)
          , Types.ibFilter = ""
          , Types.ibFocus = Types.InboxList
          , Types.ibScroll = 0
          }

setInbox :: Types.Inbox -> Brick.EventM Types.Name Types.St ()
setInbox inbox = State.modify (\s -> s {Types.stInbox = Just inbox})

handleInboxKey :: V.Event -> Types.Inbox -> Brick.EventM Types.Name Types.St ()
handleInboxKey ev inbox = do
  chord <- Types.readChord ev
  case chord of
    Types.CancelKey -> closeInbox
    Types.PrefixStarted -> pure ()
    Types.PrefixedKey _ -> Types.setMsg "C-c C-k closes"
    Types.PlainKey _ ->
      case Types.ibFocus inbox of
        Types.InboxList -> listKey ev inbox
        Types.InboxFiltering -> filterKey ev inbox

closeInbox :: Brick.EventM Types.Name Types.St ()
closeInbox = State.modify (\s -> s {Types.stInbox = Nothing})

listKey :: V.Event -> Types.Inbox -> Brick.EventM Types.Name Types.St ()
listKey ev inbox = do
  st <- State.get
  let
    convs = Inbox.entries st (Types.ibFilter inbox)
    here = Inbox.selected inbox convs
    at = Maybe.fromMaybe 0 (here >>= \c -> List.findIndex ((== Conv.convId c) . Conv.convId) convs)
    moveTo i =
      setInbox
        inbox
          { Types.ibSel = fmap Conv.convId (Types.safeIdx convs (max 0 (min (length convs - 1) i)))
          , Types.ibScroll = 0
          }
    scrollBy d = setInbox inbox {Types.ibScroll = max 0 (Types.ibScroll inbox + d)}
  case ev of
    V.EvKey (V.KChar 'j') [] -> moveTo (at + 1)
    V.EvKey V.KDown [] -> moveTo (at + 1)
    V.EvKey (V.KChar 'n') [V.MCtrl] -> moveTo (at + 1)
    V.EvKey (V.KChar 'k') [] -> moveTo (at - 1)
    V.EvKey V.KUp [] -> moveTo (at - 1)
    V.EvKey (V.KChar 'p') [V.MCtrl] -> moveTo (at - 1)
    V.EvKey (V.KChar 'g') [] -> moveTo 0
    V.EvKey (V.KChar 'G') [] -> moveTo (length convs - 1)
    V.EvKey (V.KChar 'd') [V.MCtrl] -> scrollBy 10
    V.EvKey (V.KChar 'u') [V.MCtrl] -> scrollBy (-10)
    V.EvKey (V.KChar '/') [] -> setInbox inbox {Types.ibFocus = Types.InboxFiltering}
    V.EvKey V.KEnter [] -> Monad.forM_ here jumpTo
    V.EvKey (V.KChar 'r') [] -> Monad.forM_ here replyTo
    V.EvKey (V.KChar 'q') [] -> closeInbox
    V.EvKey V.KEsc []
      | T.null (Types.ibFilter inbox) -> closeInbox
      | otherwise -> setInbox inbox {Types.ibFilter = "", Types.ibScroll = 0}
    _ -> pure ()

jumpTo :: Conv.Conversation -> Brick.EventM Types.Name Types.St ()
jumpTo conv = do
  st <- State.get
  case drawnLanding st (Conv.convId conv) <|> anchorLanding st conv of
    Nothing -> showInPullView conv
    Just landing -> do
      State.modify (\s -> s {Types.stInbox = Nothing, Types.stPullView = Nothing, Types.stBand = Nothing})
      Open.openFilePath (laPath landing)
      opened <- State.get
      Monad.when (fmap (T.pack . Types.fvPath) (Types.currentFileMay opened) == Just (laPath landing)) $
        land conv (Maybe.fromMaybe landing (drawnLanding opened (Conv.convId conv)))

data Landing = Landing
  { laPath :: T.Text
  , laLine :: Int
  , laFit :: Fit
  }

data Fit = OnItsLines | Nearby

drawnLanding :: Types.St -> Conv.ConversationId -> Maybe Landing
drawnLanding st cid =
  let
    fit = if Map.member cid (Types.stOutdated st) then Nearby else OnItsLines
    drawsIn fv = do
      (row, _) <- List.find (elem cid . snd) (Map.toList (Types.fvThreads fv))
      point <- Types.safeIdx (View.rowRestPoints fv) row
      Just (Landing (T.pack (Types.fvPath fv)) (View.rowAt point + 1) fit)
  in
    Maybe.listToMaybe (Maybe.mapMaybe drawsIn (Types.stFiles st))

anchorLanding :: Types.St -> Conv.Conversation -> Maybe Landing
anchorLanding st conv =
  case (Map.lookup (Conv.convId conv) (Types.stOutdated st), Conv.convAnchor conv) of
    (_, Conv.OnPull) -> Nothing
    (Just out, _) -> Just (Landing (Types.outdatedPath out) (max 1 (Types.outdatedLine out)) Nearby)
    (Nothing, Conv.OnPosition pos) -> Just (Landing (Conv.posPath pos) (Conv.posLine pos) Nearby)
    (Nothing, Conv.OnOutdated out) -> Just (Landing (Conv.outPath out) (Conv.outLine out) Nearby)
    (Nothing, Conv.OnLines lineSpan) -> Just (Landing (Conv.spanPath lineSpan) (Conv.slLineNo (NEL.last (Conv.spanLines lineSpan))) Nearby)

showInPullView :: Conv.Conversation -> Brick.EventM Types.Name Types.St ()
showInPullView conv = do
  st <- State.get
  case Git.subjectOf (Types.stMode st) of
    Git.Pull _ _ -> do
      State.modify (\s -> Types.showPullView s {Types.stInbox = Nothing})
      ThreadAction.bandNewest (Conv.convId conv)
    _ -> Types.setMsg "no pull request open \8212 :pr opens one"

land :: Conv.Conversation -> Landing -> Brick.EventM Types.Name Types.St ()
land conv landing = do
  Nav.gotoLine (laLine landing)
  st <- State.get
  case (Nav.hunkUnderCursor st, nearestHunk st (laLine landing - 1)) of
    (Just h, _) -> State.modify (\s -> s {Types.stCurH = h})
    (Nothing, Just h) -> State.modify (\s -> s {Types.stCurH = h})
    (Nothing, Nothing) -> pure ()
  ThreadAction.bandNewest (Conv.convId conv)
  Types.setMsg $ case laFit landing of
    OnItsLines -> "at " <> PullView.placeOf st conv
    Nearby -> "that hunk is gone \183 nearest line " <> Types.tshow (laLine landing)

nearestHunk :: Types.St -> Int -> Maybe Int
nearestHunk st row = do
  fv <- Types.currentFileMay st
  let
    distance (_, run) = abs (DiffModel.runAnchor run - row)
  fmap fst (Maybe.listToMaybe (List.sortOn distance (zip [0 ..] (Types.fvRuns fv))))

replyTo :: Conv.Conversation -> Brick.EventM Types.Name Types.St ()
replyTo conv =
  let
    posted (i, post) = case Conv.postState post of
      Conv.Posted {} -> Just i
      _ -> Nothing
    newestPosted = Maybe.listToMaybe (reverse (Maybe.mapMaybe posted (zip [0 ..] (Conv.convPosts conv))))
  in
    case (Conv.convVenue conv, newestPosted) of
      (Conv.LocalOnly _, _) -> Types.setMsg "a note is talk with the agent \183 \9166 goes to its lines"
      (_, Nothing) -> Types.setMsg "nothing posted to reply to yet"
      (_, Just i) -> do
        before <- State.gets Types.stBand
        State.modify (\s -> s {Types.stBand = Just (Types.Band (Conv.convId conv) i False)})
        Monad.void ThreadAction.replyKey
        State.modify (\s -> s {Types.stBand = before})

filterKey :: V.Event -> Types.Inbox -> Brick.EventM Types.Name Types.St ()
filterKey ev inbox =
  let
    retyped f = setInbox inbox {Types.ibFilter = f, Types.ibSel = Nothing, Types.ibScroll = 0}
  in
    case ev of
      V.EvKey V.KEnter [] -> setInbox inbox {Types.ibFocus = Types.InboxList}
      V.EvKey V.KEsc [] -> setInbox inbox {Types.ibFocus = Types.InboxList, Types.ibFilter = "", Types.ibSel = Nothing}
      V.EvKey V.KBS [] -> retyped (T.dropEnd 1 (Types.ibFilter inbox))
      V.EvKey (V.KChar c) [] -> retyped (T.snoc (Types.ibFilter inbox) c)
      _ -> pure ()
