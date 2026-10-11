{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Header
  ( gitBar
  , rebaseBar
  , headerBar
  ) where

import Brick ((<+>))
import Brick qualified as Brick
import Data.Text qualified as T
import Remax.Git qualified as Git
import Remax.UI.Core.Attrs qualified as Attrs
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
    place = case Types.stBranch st of
      Just b ->
        Brick.hBox
          ( repoSeg
              ++ [ Brick.withAttr Attrs.headerAttr (Brick.txt " on ")
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
    trailing = [rebaseSeg st, watchSeg st, busySeg st]
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
    filterSeg =
      if Types.stRunFilter st == Types.FilterAll
        then Brick.txt ""
        else
          Brick.withAttr
            Attrs.headerRebaseAttr
            (Brick.txt ("  ·  " <> Types.runFilterLabel (Types.stRunFilter st) <> " only"))
  in
    [ Brick.txt lead
    , Brick.withAttr Attrs.headerFileAttr (Brick.txt (T.pack (Types.fvPath fv)))
    , deletedSeg
    , filterSeg
    , Brick.withAttr Attrs.headerCountAttr (Brick.txt counts)
    , doneBadge
    ]

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
