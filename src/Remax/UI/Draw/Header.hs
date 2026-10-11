{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Draw.Header
  ( gitBar
  , rebaseBar
  , headerBar
  ) where

import Brick
  ( Padding (Max)
  , Widget
  , hBox
  , padRight
  , txt
  , withAttr
  , withDefAttr
  , (<+>)
  )
import Data.Text (Text)
import qualified Data.Text as T
import Remax.Git
  ( RebaseStatus (..)
  , modeLabel
  )
import Remax.UI.Core.Attrs
import Remax.UI.Core.Types
import Remax.UI.Core.Verdict
import Remax.UI.Core.Widgets
import System.FilePath
  ( takeFileName
  )

-- Outside a rebase the same full-width slot answers "where am I in git":
-- the branch, and the commit HEAD is on. Same shape as the rebase bar,
-- because it is the same question.
gitBar :: St -> Widget Name
gitBar st =
  let
    -- with more than one repository in reach, which one you are in is
    -- the first thing the bar has to answer
    repoSeg =
      if length (stRoots st) > 1
        then
          [ withAttr headerFileAttr (txt (" " <> T.pack (takeFileName (stRoot st))))
          , withAttr headerAttr (txt " ·")
          ]
        else []
    place = case stBranch st of
      Just b ->
        hBox
          ( repoSeg
              ++ [ withAttr headerAttr (txt " on ")
                 , withAttr headerFileAttr (txt b)
                 ]
          )
      Nothing -> hBox (repoSeg ++ [withAttr conflictAttr (txt " detached")])
    commitSeg = case stHead st of
      Nothing -> [withAttr hintAttr (txt "   no commits yet")]
      Just (sha, subject) ->
        [ txt "  "
        , withAttr rebaseShaAttr (txt sha)
        , txt "  "
        , txt subject
        , withAttr hintAttr (txt "   \9251c full message")
        ]
  in
    withDefAttr headerRebaseAttr . padRight Max $
      hBox (place : commitSeg)

-- The commit under review gets its own full-width bar during a rebase:
-- step, sha, and the whole subject line. Space c shows the full
-- message.
rebaseBar :: RebaseStatus -> Widget Name
rebaseBar rb =
  let
    conflictNote =
      if rebaseConflicted rb
        then withAttr conflictAttr (txt "  · conflict")
        else txt ""
    shaNote =
      if T.null (rebaseSha rb)
        then txt ""
        else withAttr rebaseShaAttr (txt (rebaseSha rb)) <+> txt "  "
  in
    withDefAttr headerRebaseAttr . padRight Max $
      hBox
        [ withAttr headerAttr $
            txt
              ( " \10227 commit "
                  <> tshow (rebaseStep rb)
                  <> "/"
                  <> tshow (rebaseTotal rb)
                  <> "  "
              )
        , shaNote
        , txt (rebaseSubject rb)
        , conflictNote
        , withAttr hintAttr (txt "   \9251c full message")
        ]

headerBar :: St -> Widget Name
headerBar st =
  let
    lead = " remax · " <> modeLabel (stMode st) <> " · "
    trailing = [rebaseSeg st, watchSeg st, busySeg st]
  in
    withDefAttr headerAttr . padRight Max $
      case currentFileMay st of
        Nothing -> hBox (txt (lead <> "all clear") : trailing)
        Just fv -> hBox (fileHeader st lead fv ++ trailing)

rebaseSeg :: St -> Widget Name
rebaseSeg st =
  case stRebase st of
    Nothing -> txt ""
    Just _ -> withAttr headerRebaseAttr (txt "  ·  rebasing")

-- Somebody is waiting on this review. Worth saying, because the
-- alternative is flagging a line and wondering whether anyone is
-- listening — and because when it stops, nothing else would say so.
--
-- It breathes rather than sits still: a mark that never moves is one
-- you stop believing, and this one is a claim about right now.
watchSeg :: St -> Widget Name
watchSeg st =
  let
    (waiting, working) = stAttending st
    here = repoText st
    mine
      | here `elem` waiting = Just (throbGlyph (stThrob st), "watching")
      | here `elem` working = Just (busyGlyph (stThrob st), "working")
      | otherwise = Nothing
  in
    case mine of
      Nothing -> txt ""
      Just (glyph, label) -> glyphSeg glyph label

busySeg :: St -> Widget Name
busySeg st =
  case stBusy st of
    [] -> txt ""
    (label : _) -> glyphSeg (busyGlyph (stThrob st)) label

glyphSeg :: Text -> Text -> Widget Name
glyphSeg glyph label =
  hBox
    [ withAttr headerAttr (txt "  ·  ")
    , withAttr watchingAttr (txt glyph)
    , withAttr headerAttr (txt (" " <> label))
    ]

fileHeader :: St -> Text -> FileView -> [Widget Name]
fileHeader st lead fv =
  let
    rs = fvRuns fv
    h = clampH rs (stCurH st)
    shownH = if null rs then 0 else h + 1
    counts =
      "  ·  file "
        <> tshow (stCurF st + 1)
        <> "/"
        <> tshow (length (stFiles st))
        <> "  ·  hunk "
        <> tshow shownH
        <> "/"
        <> tshow (length rs)
        <> pendingSeg st fv
    doneBadge = case safeIdx rs h of
      Just r
        | not (runPending (stVerdicts st) r) ->
            withAttr headerOkAttr (txt "  ✓ done")
      _ -> txt " "
    -- no working copy behind this screen: what is on it is the
    -- file as it was, every line of it on its way out
    deletedSeg =
      if fvDeleted fv
        then withAttr removedMarkAttr (txt "  ·  deleted")
        else txt ""
    -- a filtered session hides hunks; say so, or an empty-looking
    -- tray reads as a finished review
    filterSeg =
      if stRunFilter st == FilterAll
        then txt ""
        else
          withAttr
            headerRebaseAttr
            (txt ("  ·  " <> runFilterLabel (stRunFilter st) <> " only"))
  in
    [ txt lead
    , withAttr headerFileAttr (txt (T.pack (fvPath fv)))
    , deletedSeg
    , filterSeg
    , withAttr headerCountAttr (txt counts)
    , doneBadge
    ]

pendingSeg :: St -> FileView -> Text
pendingSeg st fv =
  let
    pendingAll = sum (map fvPending (stFiles st))
  in
    if length (stFiles st) > 1
      then
        "  ·  "
          <> tshow (fvPending fv)
          <> " pending here · "
          <> tshow pendingAll
          <> " overall"
      else "  ·  " <> tshow pendingAll <> " pending"
