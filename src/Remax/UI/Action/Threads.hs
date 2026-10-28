{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Threads
  ( cursorConversations
  , bandPost
  , enterThread
  , moveBand
  , leaveBand
  , Handled (..)
  , replyKey
  , askKey
  , flagKey
  , commentKey
  , editKey
  , discardKey
  , takeKey
  , toggleFold
  , reactKey
  , handleReactionKey
  , copyKey
  , resolveKey
  , editPostedKey
  , githubJob
  , pollSoon
  , aimAt
  , retireNotesOn
  , openWorkPopup
  , openWorkPopupAt
  , toggleFoldOf
  , toggleFoldAtBand
  , switchAction
  , saveWork
  , reloadAndBand
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Remax.Clipboard qualified as Clipboard
import Remax.Conversation qualified as Conv
import Remax.DiffModel qualified as DiffModel
import Remax.Git qualified as Git
import Remax.GitHub qualified as GitHub
import Remax.Modal qualified as Modal
import Remax.PullPoll qualified as PullPoll
import Remax.Store qualified as Store
import Remax.UI.Core.Anchor qualified as Anchor
import Remax.UI.Core.Jobs qualified as Jobs
import Remax.UI.Core.Markdown qualified as Markdown
import Remax.UI.Core.Threads qualified as Threads
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.View qualified as View
import Remax.UI.Model.Nav qualified as Nav
import Remax.UI.Model.Work qualified as WorkModel
import Remax.Vi qualified as Vi
import Remax.WorkStore qualified as WorkStore
import Remax.Zipper qualified as Z

data Handled = Handled | Unhandled

{- | The conversations the cursor's line belongs to: those drawn under
it, then those whose range it falls inside.
-}
cursorConversations :: Types.St -> [Conv.Conversation]
cursorConversations st =
  case Types.currentFileMay st of
    Nothing -> []
    Just fv ->
      let
        rowIx = cursorRow fv
        ending = Anchor.conversationsAt st fv rowIx
        inside = case Types.safeIdx (Types.fvRows fv) rowIx of
          Just row -> filter (Anchor.anchorCovers (T.pack (Types.fvPath fv)) row . Conv.convAnchor) (Types.stConversations st)
          Nothing -> []
      in
        List.nubBy (\a b -> Conv.convId a == Conv.convId b) (ending ++ inside)

cursorRow :: Types.FileView -> Int
cursorRow fv = View.cursorDisplayRow (Types.fvRows fv) (fst (Modal.cursorRC (Types.fvEd fv)))

bandPost :: Types.St -> Maybe (Conv.Conversation, Int, Conv.Post)
bandPost st = do
  band <- Types.stBand st
  conv <- Conv.conversationFor (Types.bandConv band) (Types.stConversations st)
  post <- Types.safeIdx (Conv.convPosts conv) (Types.bandPost band)
  Just (conv, Types.bandPost band, post)

{- | Every post the band can visit under the cursor's line, in drawing
order.
-}
bandStops :: Types.St -> [Types.Band]
bandStops st =
  case Types.currentFileMay st of
    Nothing -> []
    Just fv ->
      concatMap
        (\c -> map (\i -> Types.Band (Conv.convId c) i False) (Threads.bandIndexes st c))
        (Anchor.conversationsAt st fv (cursorRow fv))

enterThread :: Brick.EventM Types.Name Types.St Handled
enterThread = do
  st <- State.get
  case bandStops st of
    (first : _) -> do
      State.modify (\s -> s {Types.stBand = Just first})
      pure Handled
    [] -> pure Unhandled

{- | Step the band by one post; past either end the keys return to the
line, and past the last post the cursor moves on down.
-}
moveBand :: Int -> Brick.EventM Types.Name Types.St ()
moveBand d = do
  st <- State.get
  let
    stops = bandStops st
    here = Types.stBand st >>= \b -> List.elemIndex b {Types.bandDeleting = False} stops
  case here of
    Nothing -> leaveBand
    Just i ->
      case Types.safeIdx stops (i + d) of
        Just next | i + d >= 0 -> State.modify (\s -> s {Types.stBand = Just next})
        _ -> do
          leaveBand
          Monad.when (d > 0) $
            case Types.currentFileMay st of
              Just fv -> Nav.setEd (Types.stCurF st) (Modal.plainMove Z.moveDown (Types.fvEd fv))
              Nothing -> pure ()

leaveBand :: Brick.EventM Types.Name Types.St ()
leaveBand = State.modify (\s -> s {Types.stBand = Nothing})

-- | The GitHub threads under the cursor's line, oldest first.
lineThreads :: Types.St -> [Conv.Conversation]
lineThreads st =
  List.sortOn
    Conv.conversationTime
    ( filter
        ( \c -> case Conv.convVenue c of
            Conv.ReviewThread _ _ -> True
            Conv.PullComments -> True
            _ -> False
        )
        (cursorConversations st)
    )

lineThread :: Types.St -> Maybe Conv.Conversation
lineThread = Maybe.listToMaybe . lineThreads

repliedAuthor :: Conv.Conversation -> Maybe Conv.Post -> T.Text
repliedAuthor conv post =
  let
    isPosted p = case Conv.postState p of
      Conv.Posted {} -> True
      _ -> False
    posted = filter isPosted (Conv.convPosts conv)
    fallback = case reverse posted of
      (p : _) -> Threads.authorName (Conv.postAuthor p)
      [] -> "the thread"
  in
    maybe fallback (Threads.authorName . Conv.postAuthor) post

replyKey :: Brick.EventM Types.Name Types.St Handled
replyKey = do
  st <- State.get
  case bandPost st of
    Just (conv, _, post) ->
      case Conv.postState post of
        Conv.Posted cid _ _ ->
          case Conv.convVenue conv of
            Conv.ReviewThread {} -> openWorkPopup conv (Types.ReplyTo (Conv.convTarget conv) (repliedAuthor conv (Just post))) ""
            _ -> quoteReply conv cid post
        _ -> pure Handled
    Nothing ->
      case lineThreads st of
        [] -> pure Unhandled
        threads@(conv : _) -> do
          handled <- openWorkPopup conv (Types.ReplyTo (Conv.convTarget conv) (repliedAuthor conv Nothing)) ""
          offerThreads threads
          pure handled

offerThreads :: [Conv.Conversation] -> Brick.EventM Types.Name Types.St ()
offerThreads threads =
  Monad.when (length threads > 1) $
    State.modify $ \s ->
      s
        { Types.stNotePopup =
            fmap
              ( \np -> case Types.npPurpose np of
                  Types.WorkFor wp -> np {Types.npPurpose = Types.WorkFor wp {Types.wpChoices = threads, Types.wpChosen = 0}}
                  _ -> np
              )
              (Types.stNotePopup s)
        }

-- | Point a reply popup at another of the threads under its line.
aimAt :: Types.WorkPopup -> Conv.Conversation -> Types.WorkPopup
aimAt wp conv =
  wp
    { Types.wpConv = Just (Conv.convId conv)
    , Types.wpAction = case Types.wpAction wp of
        Types.ReplyTo _ _ -> Types.ReplyTo (Conv.convTarget conv) (repliedAuthor conv Nothing)
        Types.AskOn r _ -> Types.AskOn r (Conv.convTarget conv)
        other -> other
    }

quoteReply :: Conv.Conversation -> Store.CommentId -> Conv.Post -> Brick.EventM Types.Name Types.St Handled
quoteReply conv cid post =
  let
    quoted = T.unlines (map ("> " <>) (T.lines (Conv.markdownText (Conv.postBody post)))) <> "\n"
  in
    openWorkPopup conv (Types.ReplyTo (Conv.OnComment cid) (Threads.authorName (Conv.postAuthor post))) quoted

askKey :: Brick.EventM Types.Name Types.St Handled
askKey = remarkKey Types.AskRemark

flagKey :: Brick.EventM Types.Name Types.St Handled
flagKey = do
  st <- State.get
  if Types.flagRule st == Types.FlagsRefused
    then do
      Types.setMsg "read-only: @ drafts a comment"
      pure Handled
    else remarkKey Types.FlagRemark

remarkKey :: Types.AgentRemark -> Brick.EventM Types.Name Types.St Handled
remarkKey remark = do
  st <- State.get
  case bandPost st of
    Just (conv, _, _) -> openWorkPopup conv (Types.AskOn remark (Conv.convTarget conv)) ""
    Nothing ->
      case lineThreads st of
        [] -> pure Unhandled
        threads@(conv : _) -> do
          handled <- openWorkPopup conv (Types.AskOn remark (Conv.convTarget conv)) ""
          offerThreads threads
          pure handled

{- | @: a new comment on the selection or the cursor's line, a selection
pre-filling a suggestion fence with its lines.
-}
commentKey :: Brick.EventM Types.Name Types.St Handled
commentKey = do
  st <- State.get
  case Types.currentFileMay st of
    Nothing -> pure Unhandled
    Just fv ->
      case selectionSpan st fv of
        Nothing -> do
          Types.setMsg "nothing to comment on here"
          pure Handled
        Just (lineSpan, selected) -> do
          let
            fence =
              if selected
                then "```suggestion\n" <> T.unlines (map (T.drop 1 . Conv.slText) (NEL.toList (Conv.spanLines lineSpan))) <> "```\n"
                else ""
          openWorkPopupAt st (Threads.anchorPlace (Conv.OnLines lineSpan) <> declOf fv) Nothing (Types.CommentOn (Conv.OnLines lineSpan)) fence

{- | The lines a comment covers: the selection, else the cursor's line,
as content keys with the text and number they have now.
-}
selectionSpan :: Types.St -> Types.FileView -> Maybe (Conv.LineSpan, Bool)
selectionSpan st fv =
  let
    ed = Types.fvEd fv
    (lo, hi, selected) = case View.activeSelectionRows ed of
      Just (a, b) -> (a, b, True)
      Nothing -> let r = fst (Modal.cursorRC ed) in (r, r, False)
    keys = map (\(k, _, _) -> k) (Nav.selectedChanges True fv lo hi)
  in
    fmap (\lineSpan -> (lineSpan, selected)) (Anchor.lineSpanFor st fv keys)

declOf :: Types.FileView -> T.Text
declOf fv =
  case View.declAt (Modal.bufferLines (Types.fvEd fv)) (fst (Modal.cursorRC (Types.fvEd fv))) of
    Just d -> " \183 " <> View.declName d
    Nothing -> ""

editKey :: Brick.EventM Types.Name Types.St Handled
editKey = do
  st <- State.get
  case bandPost st of
    Just (conv, _, post)
      | Conv.Draft <- Conv.postState post
      , Just wid <- Conv.postWork post ->
          openWorkPopup conv (Types.EditPending wid (originOf conv post)) (Conv.markdownText (Conv.postBody post))
    _ -> editPostedKey

originOf :: Conv.Conversation -> Conv.Post -> Types.WorkOrigin
originOf conv post =
  case Conv.convVenue conv of
    Conv.ReviewThread Nothing _ -> Types.CommentOrigin
    Conv.PullComments | Maybe.isNothing (Conv.postAnswers post) -> Types.CommentOrigin
    _ -> Types.ReplyOrigin (repliedAuthor conv Nothing)

discardKey :: Brick.EventM Types.Name Types.St Handled
discardKey = do
  st <- State.get
  case bandPost st of
    Just (_, _, post)
      | Just wid <- Conv.postWork post ->
          case Conv.postState post of
            Conv.Draft
              | T.null (T.strip (Conv.markdownText (Conv.postBody post))) -> dropWork wid "discarded" >> pure Handled
              | otherwise -> do
                  State.modify (\s -> s {Types.stConfirm = Just (Types.Confirm "discard the text you typed? y / n" (dropWork wid "discarded"))})
                  pure Handled
            Conv.Proposal _ -> dropWork wid "proposal dropped" >> pure Handled
            _ -> pure Unhandled
    _ -> pure Unhandled

dropWork :: Conv.WorkId -> T.Text -> Brick.EventM Types.Name Types.St ()
dropWork wid msg = do
  st <- State.get
  MonadIO.liftIO (WorkStore.discardWork (Types.stConn st) wid)
  leaveBand
  WorkModel.reloadWork
  Types.setMsg msg

takeKey :: Brick.EventM Types.Name Types.St Handled
takeKey = do
  st <- State.get
  case bandPost st of
    Just (conv, _, post)
      | Conv.Proposal _ <- Conv.postState post
      , Just wid <- Conv.postWork post ->
          openWorkPopup conv (Types.TakeProposal wid (Conv.convTarget conv) (repliedAuthor conv Nothing)) (Conv.markdownText (Conv.postBody post))
    _ -> pure Unhandled

toggleFold :: Brick.EventM Types.Name Types.St ()
toggleFold = do
  st <- State.get
  let
    folding = filter resolved (cursorConversations st)
    resolved conv = case Conv.convVenue conv of
      Conv.ReviewThread (Just _) Store.Resolved -> True
      _ -> False
  case cursorConversations st of
    [] -> Types.setMsg "nothing folded here"
    _
      | null folding -> Types.setMsg "only a resolved thread folds"
      | otherwise -> mapM_ toggleFoldOf folding

toggleFoldOf :: Conv.Conversation -> Brick.EventM Types.Name Types.St ()
toggleFoldOf conv =
  case Conv.convVenue conv of
    Conv.ReviewThread (Just tid) Store.Resolved ->
      State.modify $ \s ->
        let
          folding = Set.member tid (Types.stUnfolded s)
          band = case (folding, Types.stPullView s) of
            (True, Just _) -> Just (Types.Band (Conv.convId conv) 0 False)
            (True, Nothing) -> Nothing
            (False, _) -> Types.stBand s
        in
          s {Types.stUnfolded = toggleMember tid (Types.stUnfolded s), Types.stBand = band}
    _ -> Types.setMsg "only a resolved thread folds"

toggleFoldAtBand :: Brick.EventM Types.Name Types.St ()
toggleFoldAtBand = do
  st <- State.get
  case bandPost st of
    Just (conv, _, _)
      | Threads.isResolvedFold st conv -> toggleFoldOf conv
    Just (conv, i, post)
      | Markdown.hasDetails (Conv.postBody post) ->
          State.modify (\s -> s {Types.stOpenDetails = toggleMember (Conv.convId conv, i) (Types.stOpenDetails s)})
      | otherwise -> toggleFoldOf conv
    Nothing -> toggleFold

toggleMember :: Ord a => a -> Set.Set a -> Set.Set a
toggleMember x xs = if Set.member x xs then Set.delete x xs else Set.insert x xs

openWorkPopup :: Conv.Conversation -> Types.WorkAction -> T.Text -> Brick.EventM Types.Name Types.St Handled
openWorkPopup conv action initial = do
  st <- State.get
  let
    place = Threads.anchorPlace (Conv.convAnchor conv) <> maybe "" declOf (Types.currentFileMay st)
  openWorkPopupAt st place (Just (Conv.convId conv)) action initial

openWorkPopupAt :: Types.St -> T.Text -> Maybe Conv.ConversationId -> Types.WorkAction -> T.Text -> Brick.EventM Types.Name Types.St Handled
openWorkPopupAt _ place cid action initial = do
  let
    ls = if T.null initial then [""] else T.lines initial
    lastRow = length ls - 1
    lastCol = maybe 0 T.length (Maybe.listToMaybe (reverse ls))
    ed0 = Modal.newEdState lastRow ls
    ed1 = ed0 {Modal.edZipper = Z.moveCursor (lastRow, lastCol) (Modal.edZipper ed0), Modal.edMode = Modal.Insert}
    popup =
      Types.WorkPopup
        { Types.wpAction = action
        , Types.wpConv = cid
        , Types.wpPlace = place
        , Types.wpOpenedWith = initial
        , Types.wpChoices = []
        , Types.wpChosen = 0
        , Types.wpReturnToSubmit = False
        , Types.wpLog = Types.FoldedLog
        }
  State.modify (\s -> s {Types.stNotePopup = Just (Types.NotePopup ed1 (Types.WorkFor popup)), Types.stVi = Vi.viIdle})
  Brick.vScrollToEnd (Brick.viewportScroll Types.PopupLogVP)
  pure Handled

{- | In the popup, ? turns a reply into a question for the agent and r
turns the question back into a reply.
-}
switchAction :: Types.WorkPopup -> Types.WorkAction -> Brick.EventM Types.Name Types.St ()
switchAction wp action =
  State.modify $ \s ->
    s
      { Types.stNotePopup =
          fmap (\np -> np {Types.npPurpose = Types.WorkFor wp {Types.wpAction = action}}) (Types.stNotePopup s)
      }

saveWork :: Types.WorkPopup -> T.Text -> Brick.EventM Types.Name Types.St ()
saveWork wp text = do
  st <- State.get
  let
    conn = Types.stConn st
    scope = WorkModel.scopeOf st
    body = Conv.Markdown text
  if T.null text
    then Types.setMsg "nothing written \8212 dd discards a pending post"
    else case Types.wpAction wp of
      Types.ReplyTo target author -> do
        Monad.void (MonadIO.liftIO (WorkStore.addPending conn scope target Conv.Viewer body))
        reloadAndBand (Types.wpConv wp)
        Types.setMsg ("reply to " <> author <> " waits for :submit")
      Types.AskOn remark target -> do
        Monad.void (MonadIO.liftIO (WorkStore.addAside conn scope target Conv.Viewer body))
        reloadAndBand (Types.wpConv wp)
        Types.setMsg (case remark of Types.FlagRemark -> "flagged for the agent"; Types.AskRemark -> "asked the agent")
      Types.CommentOn anchor -> do
        lid <- MonadIO.liftIO (WorkStore.newLocalId (Types.repoText st) (anchorKeys anchor))
        Monad.void (MonadIO.liftIO (WorkStore.addPending conn scope (Conv.NewThread lid anchor) Conv.Viewer body))
        reloadAndBand (Just (Conv.NoteConversation lid))
        Types.setMsg ("comment on " <> Threads.anchorPlace anchor <> " waits for :submit")
      Types.EditPending wid _ -> do
        MonadIO.liftIO (WorkStore.editWork conn wid body)
        reloadAndBand (Types.wpConv wp)
        Types.setMsg "edited \183 still waits for :submit"
      Types.TakeProposal wid _ author -> do
        MonadIO.liftIO (WorkStore.takeWork conn wid)
        MonadIO.liftIO (WorkStore.editWork conn wid body)
        reloadAndBand (Types.wpConv wp)
        Types.setMsg ("taken as your reply to " <> author <> " \183 waits for :submit")
      Types.EditPosted kind cid ->
        case pullOf st of
          Just pr -> githubJob "editing the comment" (GitHub.editComment (Types.stRoot st) (Git.prRepo pr) kind cid text) "edited on GitHub"
          Nothing -> Types.setMsg "no pull request open"

-- GitHub writes --------------------------------------------------------

{- | Run a call that writes to GitHub off the event loop, then say how it
went and ask the poll to read the result back soon.
-}
githubJob :: T.Text -> IO (Either GitHub.GhFailure ()) -> T.Text -> Brick.EventM Types.Name Types.St ()
githubJob label work done = do
  st <- State.get
  let
    epoch = Types.stEpoch st
  Jobs.startJob label work $ \outcome -> do
    latest <- State.get
    Monad.when (Types.stEpoch latest == epoch) $
      case outcome of
        Left err -> Jobs.jobFailed label err
        Right (Left failure) -> Types.setMsg ("! " <> GitHub.failureText failure)
        Right (Right ()) -> do
          Types.setMsg done
          pollSoon

pollSoon :: Brick.EventM Types.Name Types.St ()
pollSoon = State.modify (\s -> s {Types.stPullPoll = (Types.stPullPoll s) {Types.ppStarted = 0}})

pullOf :: Types.St -> Maybe Git.PullRequest
pullOf st =
  case Git.subjectOf (Types.stMode st) of
    Git.Pull pr _ -> Just pr
    _ -> Nothing

commentKindOf :: Conv.Venue -> GitHub.CommentKind
commentKindOf venue =
  case venue of
    Conv.PullComments -> GitHub.IssueCommentKind
    Conv.ReviewSummary _ _ -> GitHub.IssueCommentKind
    _ -> GitHub.ReviewCommentKind

reactKey :: Brick.EventM Types.Name Types.St Handled
reactKey = do
  st <- State.get
  case bandPost st of
    Just (conv, _, _)
      | Conv.ReviewSummary {} <- Conv.convVenue conv -> do
          Types.setMsg "GitHub takes no reactions on a review summary"
          pure Handled
    Just (conv, _, post)
      | Conv.Posted cid _ _ <- Conv.postState post -> do
          State.modify $ \s ->
            s
              { Types.stReacting =
                  Just
                    Types.ReactionPicker
                      { Types.rkKind = commentKindOf (Conv.convVenue conv)
                      , Types.rkComment = cid
                      , Types.rkOwner = Threads.possessive (Conv.postAuthor post)
                      , Types.rkReactions = Conv.postReactions post
                      , Types.rkPreview = Markdown.firstLine (Conv.postBody post)
                      }
              }
          pure Handled
    _ -> pure Unhandled

handleReactionKey :: V.Event -> Types.ReactionPicker -> Brick.EventM Types.Name Types.St ()
handleReactionKey ev picker = do
  State.modify (\s -> s {Types.stReacting = Nothing})
  st <- State.get
  let
    chosen = case ev of
      V.EvKey (V.KChar c) [] | c >= '1' && c <= '8' -> Types.safeIdx Conv.allReactions (fromEnum c - fromEnum '1')
      _ -> Nothing
  case (chosen, pullOf st) of
    (Just reaction, Just pr) -> do
      let
        repo = Git.prRepo pr
        kind = Types.rkKind picker
        cid = Types.rkComment picker
        mine = any (\c -> Conv.rcReaction c == reaction && Conv.rcMine c) (Types.rkReactions picker)
        viewer = Types.ppSnapshot (Types.stPullPoll st) >>= Conv.remoteViewer . PullPoll.snapshotRemote
      case (mine, viewer) of
        (True, Just login) ->
          githubJob "removing the reaction" (GitHub.removeReaction (Types.stRoot st) repo kind cid login reaction) "reaction removed"
        (True, Nothing) -> Types.setMsg "who you are on GitHub is still loading"
        (False, _) ->
          githubJob "reacting" (GitHub.addReaction (Types.stRoot st) repo kind cid reaction) ("reacted " <> Conv.reactionGlyph reaction)
    (Just _, Nothing) -> Types.setMsg "no pull request open"
    (Nothing, _) -> pure ()

copyKey :: Brick.EventM Types.Name Types.St Handled
copyKey = do
  st <- State.get
  case bandPost st of
    Just (_, _, post)
      | Just url <- Conv.postUrl post -> do
          vty <- Brick.getVtyHandle
          ok <- MonadIO.liftIO (Clipboard.writeClipboard (Types.stClip st) (V.outputByteBuffer (V.outputIface vty)) url)
          State.modify (\s -> s {Types.stKill = url, Types.stClipSent = if ok then url else Types.stClipSent s})
          Types.setMsg (if ok then "copied " <> url else "no clipboard helper here \8212 the link is in the yank slot")
          pure Handled
    _ -> pure Unhandled

-- | Resolve or unresolve the selected post's thread, or the thread under the line.
resolveKey :: Brick.EventM Types.Name Types.St Handled
resolveKey = do
  st <- State.get
  let
    conv = fmap (\(c, _, _) -> c) (bandPost st)
  case conv of
    Just c -> resolveConversation c
    Nothing ->
      case lineThread st of
        Just c -> resolveConversation c
        Nothing -> pure Unhandled

resolveConversation :: Conv.Conversation -> Brick.EventM Types.Name Types.St Handled
resolveConversation conv = do
  st <- State.get
  case Conv.convVenue conv of
    Conv.ReviewThread (Just tid) Store.Unresolved -> do
      leaveBand
      githubJob "resolving the thread" (GitHub.setThreadResolution (Types.stRoot st) Store.Resolved tid) "resolved on GitHub"
      pure Handled
    Conv.ReviewThread (Just tid) Store.Resolved -> do
      leaveBand
      githubJob "unresolving the thread" (GitHub.setThreadResolution (Types.stRoot st) Store.Unresolved tid) "unresolved on GitHub"
      pure Handled
    _ -> pure Unhandled

editPostedKey :: Brick.EventM Types.Name Types.St Handled
editPostedKey = do
  st <- State.get
  case bandPost st of
    Just (conv, _, post)
      | Conv.ReviewSummary {} <- Conv.convVenue conv
      , Conv.postAuthor post == Conv.Viewer -> do
          Types.setMsg "edit the review summary on GitHub"
          pure Handled
    Just (conv, _, post)
      | Conv.Posted cid _ _ <- Conv.postState post
      , Conv.postAuthor post == Conv.Viewer ->
          openWorkPopup conv (Types.EditPosted (commentKindOf (Conv.convVenue conv)) cid) (Conv.markdownText (Conv.postBody post))
    _ -> pure Unhandled

{- | Accepting lines retires the notes written on them, the outdated
notes drawn under them, and the talk with the agent on GitHub threads
over them.
-}
retireNotesOn :: [Store.LineKey] -> Brick.EventM Types.Name Types.St ()
retireNotesOn keys = do
  st <- State.get
  let
    outdatedHere = case Types.currentFileMay st of
      Nothing -> []
      Just fv ->
        let
          rowIxs = Maybe.mapMaybe (\(i, r) -> if maybe False (`elem` map Store.lineKeyToText keys) (DiffModel.rKey r) then Just i else Nothing) (zip [0 ..] (Types.fvRows fv))
          drawn = Anchor.conversationsIn st (concatMap (\i -> Map.findWithDefault [] i (Types.fvThreads fv)) rowIxs)
        in
          Maybe.mapMaybe (\c -> if Map.member (Conv.convId c) (Types.stOutdated st) then Conv.targetLocalId (Conv.convTarget c) else Nothing) drawn
    threadsHere = case Types.currentFileMay st of
      Nothing -> []
      Just fv ->
        let
          path = T.pack (Types.fvPath fv)
          rows = filter (\r -> any (`elem` map Store.lineKeyToText keys) (Maybe.catMaybes [DiffModel.rKey r, DiffModel.rRemarkKey r])) (Types.fvRows fv)
          covers c = any (\r -> Anchor.anchorCovers path r (Conv.convAnchor c)) rows
          threadId c = case Conv.convVenue c of
            Conv.ReviewThread (Just tid) _ | covers c -> Just tid
            _ -> Nothing
        in
          Maybe.mapMaybe threadId (Types.stConversations st)
  MonadIO.liftIO (WorkStore.retireNotesOn (Types.stConn st) (Types.repoText st) keys)
  MonadIO.liftIO (mapM_ (WorkStore.retireNote (Types.stConn st) (Types.repoText st)) outdatedHere)
  MonadIO.liftIO (WorkStore.retireAsides (Types.stConn st) (Types.repoText st) threadsHere)
  WorkModel.reloadWork

anchorKeys :: Conv.Anchor -> [Store.LineKey]
anchorKeys anchor =
  case anchor of
    Conv.OnLines lineSpan -> Conv.spanKeys lineSpan
    _ -> []

{- | After a write, read the work back and land the band on the newest
post of the conversation it went to.
-}
reloadAndBand :: Maybe Conv.ConversationId -> Brick.EventM Types.Name Types.St ()
reloadAndBand cid = do
  WorkModel.reloadWork
  st <- State.get
  let
    newest = do
      c <- cid
      conv <- Conv.conversationFor c (Types.stConversations st)
      i <- Maybe.listToMaybe (reverse (Threads.bandIndexes st conv))
      Just (Types.Band c i False)
  State.modify (\s -> s {Types.stBand = newest})
