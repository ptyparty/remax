{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Core.Attrs
  ( headerAttr
  , headerFileAttr
  , headerOkAttr
  , cursorMarkAttr
  , addedMarkAttr
  , removedMarkAttr
  , ctxMarkAttr
  , removedTextAttr
  , watchingAttr
  , wordChangeAttr
  , selAttr
  , pendingWashAttr
  , flaggedWashAttr
  , questionedWashAttr
  , settleAttr
  , settleInkAttr
  , settleFrames
  , curRunAttr
  , msgAttr
  , hintAttr
  , statusAttr
  , borderAttr
  , lineNrAttr
  , lineNrCurAttr
  , trayFileAttr
  , trayFileCurAttr
  , flagMarkAttr
  , questionMarkAttr
  , doneMarkAttr
  , pendingMarkAttr
  , lineNrDoneAttr
  , headerRebaseAttr
  , trayDeclAttr
  , headerCountAttr
  , rebaseShaAttr
  , conflictAttr
  , watchPollAttr
  , modeDotNorAttr
  , modeDotInsAttr
  , noteInlineAttr
  , replyInlineAttr
  , guideAttr
  , floatAttr
  , floatSelAttr
  , floatSelBarAttr
  , floatTitleAttr
  , matchCharAttr
  , meterOnAttr
  , meterOffAttr
  , hairAttr
  , mdHeadingAttr
  , mdBoldAttr
  , mdCodeAttr
  , mdLinkAttr
  , mdTableHeaderAttr
  , fgOnly
  , mkAttrMap
  ) where

import Brick qualified as Brick
import Brick.Util qualified as BrickUtil
import Brick.Widgets.Border qualified as Border
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Remax.Highlight qualified as Highlight
import Remax.Theme qualified as Theme

headerAttr
  , headerFileAttr
  , headerOkAttr
  , cursorMarkAttr
  , addedMarkAttr
  , removedMarkAttr
  , ctxMarkAttr
  , removedTextAttr
  , watchingAttr
  , wordChangeAttr
  , selAttr
  , pendingWashAttr
  , flaggedWashAttr
  , questionedWashAttr
  , dimAttr
  , curRunAttr
  , norAttr
  , insAttr
  , msgAttr
  , hintAttr
  , statusAttr
  , borderAttr
  , lineNrAttr
  , lineNrCurAttr
  , trayFileAttr
  , trayFileCurAttr
  , flagMarkAttr
  , questionMarkAttr
  , doneMarkAttr
  , pendingMarkAttr
  , lineNrDoneAttr
  , headerRebaseAttr
  , trayDeclAttr
  , headerCountAttr
  , rebaseShaAttr
  , conflictAttr
  , watchPollAttr
  , modeDotNorAttr
  , modeDotInsAttr
  , noteInlineAttr
  , replyInlineAttr
  , guideAttr
  , floatAttr
  , floatSelAttr
  , floatSelBarAttr
  , floatTitleAttr
  , matchCharAttr
  , meterOnAttr
  , meterOffAttr
  , hairAttr
  , mdHeadingAttr
  , mdBoldAttr
  , mdCodeAttr
  , mdLinkAttr
  , mdTableHeaderAttr ::
    Brick.AttrName
headerAttr = Brick.attrName "header"
headerFileAttr = headerAttr <> Brick.attrName "file"
headerOkAttr = headerAttr <> Brick.attrName "ok"
headerRebaseAttr = headerAttr <> Brick.attrName "rebase"
trayDeclAttr = Brick.attrName "trayDecl"
headerCountAttr = headerAttr <> Brick.attrName "count"
rebaseShaAttr = Brick.attrName "rebaseSha"
conflictAttr = Brick.attrName "conflict"
watchPollAttr = Brick.attrName "watchPoll"
modeDotNorAttr = Brick.attrName "modeDotNor"
modeDotInsAttr = Brick.attrName "modeDotIns"
noteInlineAttr = Brick.attrName "noteInline"
replyInlineAttr = Brick.attrName "replyInline"
guideAttr = Brick.attrName "guide"
floatAttr = Brick.attrName "float"
floatSelAttr = floatAttr <> Brick.attrName "sel"
floatSelBarAttr = floatAttr <> Brick.attrName "selBar"
floatTitleAttr = floatAttr <> Brick.attrName "title"
matchCharAttr = Brick.attrName "matchChar"
meterOnAttr = Brick.attrName "meterOn"
meterOffAttr = Brick.attrName "meterOff"
hairAttr = Brick.attrName "hairline"
mdHeadingAttr = Brick.attrName "mdHeading"
mdBoldAttr = Brick.attrName "mdBold"
mdCodeAttr = Brick.attrName "mdCode"
mdLinkAttr = Brick.attrName "mdLink"
mdTableHeaderAttr = Brick.attrName "mdTableHeader"
cursorMarkAttr = Brick.attrName "cursorMark"
addedMarkAttr = Brick.attrName "addedMark"
removedMarkAttr = Brick.attrName "removedMark"
ctxMarkAttr = Brick.attrName "ctxMark"
removedTextAttr = Brick.attrName "removedText"
watchingAttr = Brick.attrName "watching"
wordChangeAttr = Brick.attrName "wordChange"
pendingWashAttr = Brick.attrName "pendingWash"
flaggedWashAttr = Brick.attrName "flaggedWash"
questionedWashAttr = Brick.attrName "questionedWash"

settleFrames :: Int
settleFrames = 6

settleAttr :: Int -> Brick.AttrName
settleAttr frame = Brick.attrName ("settle" ++ show (max 1 (min settleFrames frame)))

settleInkAttr :: Int -> Brick.AttrName
settleInkAttr frame = Brick.attrName ("settleInk" ++ show (max 1 (min settleFrames frame)))

settleInk :: Int -> V.Color
settleInk frame =
  let
    toward base peak = base + ((peak - base) * frame) `div` settleFrames
  in
    V.linearColor (toward (0x84 :: Int) 0xc8) (toward 0xa7 0xf4) (toward 0x75 0xb0)

settleShade :: Int -> V.Color
settleShade frame =
  let
    scale v = (v * frame) `div` settleFrames
  in
    V.linearColor (scale (0x22 :: Int)) (scale 0x3c) (scale 0x2c)
selAttr = Brick.attrName "selection"
dimAttr = Brick.attrName "dim"
curRunAttr = Brick.attrName "currentRun"
norAttr = Brick.attrName "modeNor"
insAttr = Brick.attrName "modeIns"
msgAttr = Brick.attrName "message"
hintAttr = Brick.attrName "hint"
statusAttr = Brick.attrName "statusBar"
borderAttr = Brick.attrName "paneBorder"
lineNrAttr = Brick.attrName "lineNr"
lineNrCurAttr = lineNrAttr <> Brick.attrName "current"
lineNrDoneAttr = lineNrAttr <> Brick.attrName "reviewed"
trayFileAttr = Brick.attrName "trayFile"
trayFileCurAttr = trayFileAttr <> Brick.attrName "current"
flagMarkAttr = Brick.attrName "flagMark"
questionMarkAttr = Brick.attrName "questionMark"
doneMarkAttr = Brick.attrName "doneMark"
pendingMarkAttr = Brick.attrName "pendingMark"

-- Nudge a colour up a little, for surfaces that should read as the same
-- material under slightly more light. Only truecolor values can be
-- lifted; a palette entry has no channels to add to, so it is left be.
lighten :: Int -> V.Color -> V.Color
lighten by c =
  case c of
    V.RGBColor r g b ->
      let
        bump v = min 255 (fromIntegral v + by)
      in
        V.linearColor (bump r) (bump g) (bump b)
    _ -> c

-- A theme scope may carry a background; the border glyphs must not, or
-- the whole cell fills in and the thin bar reads as a thick one.
fgOnly :: V.Attr -> V.Attr
fgOnly a = a {V.attrBackColor = V.Default}

-- Helix-style scopes the tree-sitter path colors: top-level entries carry
-- built-in fallback colors so non-helix users get a sane palette, and
-- theme users additionally get every deeper scope their theme resolves
-- (brick's attribute hierarchy falls back through the dots on its own).
tsTopScopes :: [(T.Text, V.Color)]
tsTopScopes =
  [ ("keyword", V.magenta)
  , ("function", V.blue)
  , ("type", V.cyan)
  , ("constructor", V.yellow)
  , ("constant", V.brightCyan)
  , ("string", V.green)
  , ("comment", V.brightBlack)
  , ("operator", V.yellow)
  , ("namespace", V.brightMagenta)
  , ("label", V.yellow)
  , ("attribute", V.brightMagenta)
  , ("special", V.yellow)
  , ("tag", V.cyan)
  ]

tsDeepScopes :: [T.Text]
tsDeepScopes =
  [ "keyword.control"
  , "keyword.control.import"
  , "keyword.control.exception"
  , "keyword.operator"
  , "keyword.directive"
  , "function.method"
  , "function.macro"
  , "function.builtin"
  , "function.special"
  , "type.builtin"
  , "type.enum.variant"
  , "constant.builtin"
  , "constant.builtin.boolean"
  , "constant.character"
  , "constant.character.escape"
  , "constant.numeric"
  , "string.special"
  , "string.regexp"
  , "comment.block.documentation"
  , "variable"
  , "variable.builtin"
  , "variable.parameter"
  , "variable.other.member"
  , "punctuation"
  , "punctuation.bracket"
  , "punctuation.delimiter"
  , "punctuation.special"
  , "markup.heading"
  , "markup.bold"
  , "markup.italic"
  , "markup.list"
  , "markup.quote"
  , "markup.link.url"
  , "markup.link.text"
  , "markup.raw"
  ]

mkAttrMap :: Maybe Theme.HelixTheme -> Brick.AttrMap
mkAttrMap mth =
  let
    topPair (s, c) = (Highlight.tsScopeAttr s, Theme.scopeAttr mth s (BrickUtil.fg c))
    deepPairs = case mth of
      Nothing -> []
      Just th ->
        map
          (\s -> (Highlight.tsScopeAttr s, Theme.scopeAttr mth s V.defAttr))
          (filter (Theme.hasScope th) tsDeepScopes)
    tsPairs = map topPair tsTopScopes ++ deepPairs
    bgA = Theme.scopeAttr mth "ui.background" V.defAttr
    baseAttr =
      let
        fgA = Theme.scopeAttr mth "ui.text" V.defAttr
      in
        V.defAttr
          { V.attrForeColor = V.attrForeColor fgA
          , V.attrBackColor = V.attrBackColor bgA
          }
    -- A popup sits a hair above the body, and "a hair" has to be
    -- measured from the body rather than fixed: the body colour comes
    -- from the theme's ui.background, so a hardcoded surface drifts as
    -- far from it as the theme happens to be dark. Against a #080808
    -- background the old fixed #1e1e1e read as a different material
    -- rather than the same one lit slightly.
    floatBack = case V.attrBackColor bgA of
      V.SetTo c -> V.SetTo (lighten 0x0d c)
      _ -> V.SetTo surfFloat
    onFloat a = a {V.attrBackColor = floatBack}
    selection =
      Theme.scopeAttr mth "ui.selection.primary" (V.defAttr `V.withBackColor` surfSel)
    uiPairs =
      [ (headerAttr, BrickUtil.fg inkDim)
      , (headerRebaseAttr, BrickUtil.fg inkText)
      , (headerCountAttr, BrickUtil.fg inkText)
      , (rebaseShaAttr, BrickUtil.fg inkMidBlue)
      , (conflictAttr, BrickUtil.fg inkRose `V.withStyle` V.bold)
      , (watchPollAttr, BrickUtil.fg inkGold)
      , (trayDeclAttr, BrickUtil.fg inkMidBlue)
      , (headerFileAttr, BrickUtil.fg inkFileGold)
      , (headerOkAttr, BrickUtil.fg inkGreen)
      , (cursorMarkAttr, V.defAttr `V.withBackColor` cursorWash)
      , (addedMarkAttr, BrickUtil.fg inkGreen)
      , (removedMarkAttr, BrickUtil.fg inkRose)
      , (ctxMarkAttr, BrickUtil.fg inkFaint)
      , (removedTextAttr, BrickUtil.fg inkRose)
      , (watchingAttr, BrickUtil.fg inkGreen)
      , -- only a background, so the syntax colour underneath survives
        (wordChangeAttr, V.defAttr `V.withBackColor` wordWash)
      , (selAttr, selection)
      , (pendingWashAttr, V.defAttr `V.withBackColor` pendingWash)
      , (flaggedWashAttr, V.defAttr `V.withBackColor` flaggedWash)
      , (questionedWashAttr, V.defAttr `V.withBackColor` questionedWash)
      , (dimAttr, BrickUtil.fg inkDim)
      , (curRunAttr, BrickUtil.fg inkMidBlue)
      , (norAttr, BrickUtil.fg inkText)
      , (insAttr, BrickUtil.fg inkText)
      , (modeDotNorAttr, BrickUtil.fg inkLavender)
      , (modeDotInsAttr, BrickUtil.fg inkGreen)
      , (msgAttr, BrickUtil.fg inkText)
      , (hintAttr, BrickUtil.fg inkFaint)
      , (statusAttr, BrickUtil.fg inkText)
      , (lineNrAttr, Theme.scopeAttr mth "ui.linenr" (BrickUtil.fg V.brightBlack))
      , (lineNrCurAttr, Theme.scopeAttr mth "ui.linenr.selected" (BrickUtil.fg V.yellow))
      , (lineNrDoneAttr, Theme.scopeAttr mth "ui.linenr" (BrickUtil.fg V.brightBlack) `V.withStyle` V.dim)
      , (borderAttr, fgOnly (Theme.scopeAttr mth "ui.linenr" (BrickUtil.fg V.brightBlack)))
      , (trayFileAttr, BrickUtil.fg trayFileInk `V.withStyle` V.bold)
      , (trayFileCurAttr, BrickUtil.fg inkFileGold `V.withStyle` V.bold)
      , (flagMarkAttr, BrickUtil.fg inkFlag)
      , (questionMarkAttr, BrickUtil.fg inkGold)
      , (doneMarkAttr, BrickUtil.fg inkGreen)
      , (pendingMarkAttr, BrickUtil.fg inkBlue)
      , (noteInlineAttr, BrickUtil.fg inkLavender)
      , (replyInlineAttr, BrickUtil.fg inkMidBlue)
      , (guideAttr, BrickUtil.fg inkTeal `V.withBackColor` surfGuide)
      , (floatAttr, onFloat V.defAttr)
      , (floatSelAttr, V.defAttr `V.withBackColor` surfSel)
      , (floatSelBarAttr, BrickUtil.fg inkLavender `V.withBackColor` surfSel)
      , (floatTitleAttr, onFloat (BrickUtil.fg inkLavender))
      , -- every popup is framed in the same lavender as its title; floatFrame
        -- is the only bordered widget, so this reaches exactly the popups
        (Border.borderAttr, onFloat (BrickUtil.fg inkLavender))
      , (matchCharAttr, BrickUtil.fg matchGold)
      , (meterOnAttr, BrickUtil.fg inkGreen)
      , (meterOffAttr, BrickUtil.fg meterRail)
      , (hairAttr, BrickUtil.fg hairInk)
      , (mdHeadingAttr, BrickUtil.fg inkText `V.withStyle` V.bold `V.withStyle` V.underline)
      , (mdBoldAttr, BrickUtil.fg inkText `V.withStyle` V.bold)
      , (mdCodeAttr, BrickUtil.fg inkText `V.withBackColor` codeWash)
      , (mdLinkAttr, BrickUtil.fg inkBlue `V.withStyle` V.underline)
      , (mdTableHeaderAttr, BrickUtil.fg inkText `V.withStyle` V.underline)
      ]
    settlePairs =
      map (\n -> (settleAttr n, V.defAttr `V.withBackColor` settleShade n)) [1 .. settleFrames]
        ++ map (\n -> (settleInkAttr n, BrickUtil.fg (settleInk n))) [1 .. settleFrames]
  in
    Brick.attrMap baseAttr (tsPairs ++ uiPairs ++ settlePairs)

-- The Quiet Ink · Tinted palette: jellybeans-adjacent literals, chosen
-- in the temperaments mockups. linearColor keeps true 24-bit values;
-- vty approximates on lesser terminals.
inkRose, inkBlue, inkGold, inkGreen, inkLavender, inkMidBlue :: V.Color
inkRose = V.linearColor (0xa2 :: Int) 0x70 0x7c
inkBlue = V.linearColor (0x7f :: Int) 0x9e 0xc7
inkGold = V.linearColor (0xd3 :: Int) 0xb0 0x78
inkGreen = V.linearColor (0x84 :: Int) 0xa7 0x75
inkLavender = V.linearColor (0xa3 :: Int) 0x90 0xf0
inkMidBlue = V.linearColor (0x81 :: Int) 0x97 0xbf

inkFileGold, inkDim, inkFaint, inkText, inkFlag, trayFileInk :: V.Color
inkFileGold = V.linearColor (0xe8 :: Int) 0xd7 0xa7
inkDim = V.linearColor (0x8b :: Int) 0x8b 0x84
inkFaint = V.linearColor (0x6e :: Int) 0x6e 0x68
inkText = V.linearColor (0xb4 :: Int) 0xb4 0xaa
inkFlag = V.linearColor (0xcf :: Int) 0x8a 0x6a
trayFileInk = V.linearColor (0xd8 :: Int) 0xd3 0xc0

surfFloat, surfSel, surfGuide, inkTeal, cursorWash, wordWash, pendingWash, flaggedWash, questionedWash, hairInk, meterRail, matchGold :: V.Color
-- Only used when the theme names no background at all, so there is
-- nothing to measure a lift from.
surfFloat = V.linearColor (0x15 :: Int) 0x15 0x15
surfSel = V.linearColor (0x24 :: Int) 0x24 0x30
surfGuide = V.linearColor (0x1c :: Int) 0x2e 0x2e
inkTeal = V.linearColor (0x9f :: Int) 0xd8 0xd0
cursorWash = V.linearColor (0x3a :: Int) 0x33 0x25
wordWash = V.linearColor (0x1e :: Int) 0x2c 0x3e
pendingWash = V.linearColor (0x0e :: Int) 0x10 0x15
flaggedWash = V.linearColor (0x24 :: Int) 0x17 0x1b
questionedWash = V.linearColor (0x22 :: Int) 0x1f 0x14
hairInk = V.linearColor (0x2c :: Int) 0x2c 0x2c
codeWash :: V.Color
codeWash = V.linearColor (0x1e :: Int) 0x1e 0x24
meterRail = V.linearColor (0x3a :: Int) 0x3f 0x3a
matchGold = V.linearColor (0xfa :: Int) 0xd0 0x7a
