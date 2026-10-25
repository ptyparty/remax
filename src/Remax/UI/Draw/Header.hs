{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Header
  ( gitBar
  , pullBar
  , rebaseBar
  , tourBar
  , headerBar
  ) where

import Brick ((<+>))
import Brick qualified as Brick
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Remax.Git qualified as Git
import Remax.GitHub qualified as GitHub
import Remax.Store qualified as Store
import Remax.UI.Action.Pull qualified as PullAction
import Remax.UI.Core.Attrs qualified as Attrs
import Remax.UI.Core.Tour qualified as Tour
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Verdict qualified as Verdict
import Remax.UI.Core.Widgets qualified as Widgets
import System.FilePath qualified as FilePath

-- Outside a rebase the same full-width slot answers "where am I in git":
-- the branch, and the commit HEAD is on. Same shape as the rebase bar,
-- because it is the same question.
gitBar :: Types.St -> Brick.Widget Types.Name
gitBar st =
  let
    -- with more than one repository in reach, which one you are in is
    -- the first thing the bar has to answer
    repoSeg =
      if length (Types.stRoots st) > 1
        then
          [ Brick.withAttr Attrs.headerFileAttr (Brick.txt (" " <> T.pack (FilePath.takeFileName (Types.stRoot st))))
          , Brick.withAttr Attrs.headerAttr (Brick.txt " ·")
          ]
        else []
    placeWord = case Git.sourceOf (Types.stMode st) of
      Git.FromWorkingTree -> " on "
      Git.FromRef _ -> " at "
    place = case Types.stBranch st of
      Just b ->
        Brick.hBox
          ( repoSeg
              ++ [ Brick.withAttr Attrs.headerAttr (Brick.txt placeWord)
                 , Brick.withAttr Attrs.headerFileAttr (Brick.txt b)
                 ]
          )
      Nothing -> Brick.hBox (repoSeg ++ [Brick.withAttr Attrs.conflictAttr (Brick.txt " detached")])
    commitSeg = case Types.stHead st of
      Nothing -> [Brick.withAttr Attrs.hintAttr (Brick.txt "   no commits yet")]
      Just (sha, subject) ->
        [ Brick.txt "  "
        , Brick.withAttr Attrs.rebaseShaAttr (Brick.txt sha)
        , Brick.txt "  "
        , Brick.txt subject
        , Brick.withAttr Attrs.hintAttr (Brick.txt "   \9251c full message")
        ]
  in
    Brick.withDefAttr Attrs.headerRebaseAttr . Brick.padRight Brick.Max $
      Brick.hBox (place : commitSeg)

pullBar :: Types.St -> Git.PullRequest -> Git.Editing -> Brick.Widget Types.Name
pullBar st pr editing =
  let
    dot = Brick.withAttr Attrs.headerAttr (Brick.txt " \183 ")
    meta = case Types.stPullMeta st of
      Just m | GitHub.metaNumber m == Git.prNumber pr -> Just m
      _ -> Nothing
    chip = case editing of
      Git.ReadOnly -> " review "
      Git.Checkout -> " edit "
    checksSeg = case fmap GitHub.metaChecks meta of
      Just GitHub.ChecksPass -> Brick.withAttr Attrs.headerOkAttr (Brick.txt "\10003 checks")
      Just GitHub.ChecksFail -> Brick.withAttr Attrs.conflictAttr (Brick.txt "\10007 checks")
      Just GitHub.ChecksRunning -> Brick.txt "\9675 checks"
      Just GitHub.ChecksNone -> Brick.withAttr Attrs.hintAttr (Brick.txt "no checks")
      Nothing -> Brick.withAttr Attrs.hintAttr (Brick.txt "checks \8230")
  in
    Brick.withDefAttr Attrs.headerRebaseAttr . Brick.padRight Brick.Max $
      Brick.hBox
        [ Brick.withAttr Attrs.headerFileAttr (Brick.txt (" #" <> Types.tshow (Git.prNumber pr) <> " " <> Git.prHeadBranch pr <> "\8594" <> Git.prBaseBranch pr))
        , dot
        , Brick.withAttr Attrs.rebaseShaAttr (Brick.txt (maybe "?" fst (Types.stHead st) <> "\8230" <> Maybe.fromMaybe "?" (Types.stBaseAt st)))
        , dot
        , Brick.withAttr Attrs.guideAttr (Brick.txt chip)
        , dot
        , Brick.txt "0@ 0\8801"
        , dot
        , checksSeg
        , Brick.withAttr Attrs.hintAttr (Brick.txt "   \9251c PR")
        ]

-- The commit under review gets its own full-width bar during a rebase:
-- step, sha, and the whole subject line. Space c shows the full
-- message.
rebaseBar :: Git.RebaseStatus -> Brick.Widget Types.Name
rebaseBar rb =
  let
    conflictNote =
      if Git.rebaseConflicted rb
        then Brick.withAttr Attrs.conflictAttr (Brick.txt "  · conflict")
        else Brick.txt ""
    shaNote =
      if T.null (Git.rebaseSha rb)
        then Brick.txt ""
        else Brick.withAttr Attrs.rebaseShaAttr (Brick.txt (Git.rebaseSha rb)) <+> Brick.txt "  "
  in
    Brick.withDefAttr Attrs.headerRebaseAttr . Brick.padRight Brick.Max $
      Brick.hBox
        [ Brick.withAttr Attrs.headerAttr $
            Brick.txt
              ( " \10227 commit "
                  <> Types.tshow (Git.rebaseStep rb)
                  <> "/"
                  <> Types.tshow (Git.rebaseTotal rb)
                  <> "  "
              )
        , shaNote
        , Brick.txt (Git.rebaseSubject rb)
        , conflictNote
        , Brick.withAttr Attrs.hintAttr (Brick.txt "   \9251c full message")
        ]

headerBar :: Types.St -> Brick.Widget Types.Name
headerBar st =
  let
    lead = " remax · " <> Git.modeLabel (Types.stMode st) <> " · "
    trailing = [rebaseSeg st, pullSeg st, watchSeg st, busySeg st]
  in
    Brick.withDefAttr Attrs.headerAttr . Brick.padRight Brick.Max $
      case Types.currentFileMay st of
        Nothing -> Brick.hBox (Brick.txt (lead <> "all clear") : trailing)
        Just fv -> Brick.hBox (fileHeader st lead fv ++ trailing)

rebaseSeg :: Types.St -> Brick.Widget Types.Name
rebaseSeg st =
  case Types.stRebase st of
    Nothing -> Brick.txt ""
    Just _ -> Brick.withAttr Attrs.headerRebaseAttr (Brick.txt "  ·  rebasing")

-- Somebody is waiting on this review. Worth saying, because the
-- alternative is flagging a line and wondering whether anyone is
-- listening — and because when it stops, nothing else would say so.
--
-- It breathes rather than sits still: a mark that never moves is one
-- you stop believing, and this one is a claim about right now.
watchSeg :: Types.St -> Brick.Widget Types.Name
watchSeg st =
  let
    (waiting, working) = Types.stAttending st
    here = Types.repoText st
    mine
      | here `elem` waiting = Just (Widgets.throbGlyph (Types.stThrob st), "watching")
      | here `elem` working = Just (Widgets.busyGlyph (Types.stThrob st), "working")
      | otherwise = Nothing
  in
    case mine of
      Nothing -> Brick.txt ""
      Just (glyph, label) -> glyphSeg glyph label

pullSeg :: Types.St -> Brick.Widget Types.Name
pullSeg st =
  case PullAction.pullFreshness st of
    Nothing -> Brick.txt ""
    Just PullAction.AwaitingFirstPoll -> Brick.txt "  \183  \8635 \8230"
    Just (PullAction.SyncedAgo secs) -> Brick.txt ("  \183  \8635 " <> PullAction.shortAge secs)
    Just (PullAction.StaleSince age _) ->
      Brick.txt "  \183  " <+> Brick.withAttr Attrs.conflictAttr (Brick.txt (PullAction.staleText age))

busySeg :: Types.St -> Brick.Widget Types.Name
busySeg st =
  case Types.stBusy st of
    [] -> Brick.txt ""
    (label : _) -> glyphSeg (Widgets.busyGlyph (Types.stThrob st)) label

glyphSeg :: T.Text -> T.Text -> Brick.Widget Types.Name
glyphSeg glyph label =
  Brick.hBox
    [ Brick.withAttr Attrs.headerAttr (Brick.txt "  ·  ")
    , Brick.withAttr Attrs.watchingAttr (Brick.txt glyph)
    , Brick.withAttr Attrs.headerAttr (Brick.txt (" " <> label))
    ]

fileHeader :: Types.St -> T.Text -> Types.FileView -> [Brick.Widget Types.Name]
fileHeader st lead fv =
  let
    rs = Types.fvRuns fv
    h = Types.clampH rs (Types.stCurH st)
    shownH = if null rs then 0 else h + 1
    counts =
      "  ·  file "
        <> Types.tshow (Types.stCurF st + 1)
        <> "/"
        <> Types.tshow (length (Types.stFiles st))
        <> "  ·  hunk "
        <> Types.tshow shownH
        <> "/"
        <> Types.tshow (length rs)
        <> pendingSeg st fv
    doneBadge = case Types.safeIdx rs h of
      Just r
        | not (Verdict.runPending (Types.stVerdicts st) r) ->
            Brick.withAttr Attrs.headerOkAttr (Brick.txt "  ✓ done")
      _ -> Brick.txt " "
    -- no working copy behind this screen: what is on it is the
    -- file as it was, every line of it on its way out
    deletedSeg =
      if Types.fvDeleted fv
        then Brick.withAttr Attrs.removedMarkAttr (Brick.txt "  ·  deleted")
        else Brick.txt ""
    -- a filtered session hides hunks; say so, or an empty-looking
    -- tray reads as a finished review
    walkSeg = case Types.stWalk st of
      Types.WalkUnresolved -> Brick.withAttr Attrs.headerRebaseAttr (Brick.txt "  ·  unresolved only")
      Types.WalkTour -> Brick.withAttr Attrs.headerRebaseAttr (Brick.txt ("  ·  " <> tourPosition st))
      _ -> Brick.txt ""
  in
    [ Brick.txt lead
    , Brick.withAttr Attrs.headerFileAttr (Brick.txt (T.pack (Types.fvPath fv)))
    , deletedSeg
    , walkSeg
    , Brick.withAttr Attrs.headerCountAttr (Brick.txt counts)
    , doneBadge
    ]

-- The tour's own bar, under the git bar for as long as the walk follows
-- a tour: which stop this is, where in the stop the cursor sits, and
-- the stop's note, so the trail stays visible on every hunk of a stop
-- and not only on the one that carries the note.
tourBar :: Types.St -> Maybe (Brick.Widget Types.Name)
tourBar st =
  case (Types.stWalk st, Types.stTour st) of
    (Types.WalkTour, steps@(_ : _)) ->
      let
        here = (Types.stCurF st, Types.clampH (Types.fvRuns (Types.currentFile st)) (Types.stCurH st))
        total = Types.tshow (length steps)
        text = case Tour.stepPosition st here of
          Just (i, j, size) ->
            let
              step = Types.safeIdx steps i
              title = maybe "" ("  \183  " <>) (step >>= Store.tourTitle)
              stop = Maybe.fromMaybe "" (fmap stepLead step)
              within = if size > 1 then "  \183  hunk " <> Types.tshow (j + 1) <> "/" <> Types.tshow size <> " of this stop" else ""
            in
              " \9656 stop " <> Types.tshow (i + 1) <> "/" <> total <> title <> within <> "  \183  " <> stop
          Nothing
            | null (Tour.tourOrder st) ->
                " \9656 tour of " <> total <> " stops  \183  none of its hunks are in this view any more"
            | otherwise ->
                " \9656 tour of " <> total <> " stops  \183  off the trail \8212 :tour returns to it, ] goes to the first stop"
      in
        Just (Brick.withDefAttr Attrs.guideAttr (Brick.padRight Brick.Max (Brick.txt text)))
    _ -> Nothing

stepLead :: Store.TourStep -> T.Text
stepLead step = Maybe.fromMaybe "" (Maybe.listToMaybe (T.lines (Store.tourNote step)))

tourPosition :: Types.St -> T.Text
tourPosition st =
  let
    here = (Types.stCurF st, Types.clampH (Types.fvRuns (Types.currentFile st)) (Types.stCurH st))
    total = Types.tshow (length (Types.stTour st))
  in
    case Tour.stepPosition st here of
      Just (i, j, size)
        | size > 1 -> "tour " <> Types.tshow (i + 1) <> "/" <> total <> " · " <> Types.tshow (j + 1) <> "/" <> Types.tshow size
        | otherwise -> "tour " <> Types.tshow (i + 1) <> "/" <> total
      Nothing -> "tour · " <> total <> " steps"

pendingSeg :: Types.St -> Types.FileView -> T.Text
pendingSeg st fv =
  let
    pendingAll = sum (map Types.fvPending (Types.stFiles st))
  in
    if length (Types.stFiles st) > 1
      then
        "  ·  "
          <> Types.tshow (Types.fvPending fv)
          <> " pending here · "
          <> Types.tshow pendingAll
          <> " overall"
      else "  ·  " <> Types.tshow pendingAll <> " pending"
