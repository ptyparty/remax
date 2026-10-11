{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Attrs
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
  , floatAttr
  , floatSelAttr
  , floatSelBarAttr
  , floatTitleAttr
  , matchCharAttr
  , meterOnAttr
  , meterOffAttr
  , hairAttr
  , fgOnly
  , mkAttrMap
  ) where

import Brick
  ( AttrMap
  , AttrName
  , attrMap
  , attrName
  )
import Brick.Util (fg)
import qualified Brick.Widgets.Border as B
import Data.Text (Text)
import qualified Graphics.Vty as V
import Remax.Highlight
import Remax.Theme

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
  , floatAttr
  , floatSelAttr
  , floatSelBarAttr
  , floatTitleAttr
  , matchCharAttr
  , meterOnAttr
  , meterOffAttr
  , hairAttr ::
    AttrName
headerAttr = attrName "header"
headerFileAttr = headerAttr <> attrName "file"
headerOkAttr = headerAttr <> attrName "ok"
headerRebaseAttr = headerAttr <> attrName "rebase"
trayDeclAttr = attrName "trayDecl"
headerCountAttr = headerAttr <> attrName "count"
rebaseShaAttr = attrName "rebaseSha"
conflictAttr = attrName "conflict"
watchPollAttr = attrName "watchPoll"
modeDotNorAttr = attrName "modeDotNor"
modeDotInsAttr = attrName "modeDotIns"
noteInlineAttr = attrName "noteInline"
replyInlineAttr = attrName "replyInline"
floatAttr = attrName "float"
floatSelAttr = floatAttr <> attrName "sel"
floatSelBarAttr = floatAttr <> attrName "selBar"
floatTitleAttr = floatAttr <> attrName "title"
matchCharAttr = attrName "matchChar"
meterOnAttr = attrName "meterOn"
meterOffAttr = attrName "meterOff"
hairAttr = attrName "hairline"
cursorMarkAttr = attrName "cursorMark"
addedMarkAttr = attrName "addedMark"
removedMarkAttr = attrName "removedMark"
ctxMarkAttr = attrName "ctxMark"
removedTextAttr = attrName "removedText"
watchingAttr = attrName "watching"
wordChangeAttr = attrName "wordChange"
selAttr = attrName "selection"
dimAttr = attrName "dim"
curRunAttr = attrName "currentRun"
norAttr = attrName "modeNor"
insAttr = attrName "modeIns"
msgAttr = attrName "message"
hintAttr = attrName "hint"
statusAttr = attrName "statusBar"
borderAttr = attrName "paneBorder"
lineNrAttr = attrName "lineNr"
lineNrCurAttr = lineNrAttr <> attrName "current"
lineNrDoneAttr = lineNrAttr <> attrName "reviewed"
trayFileAttr = attrName "trayFile"
trayFileCurAttr = trayFileAttr <> attrName "current"
flagMarkAttr = attrName "flagMark"
questionMarkAttr = attrName "questionMark"
doneMarkAttr = attrName "doneMark"
pendingMarkAttr = attrName "pendingMark"

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
tsTopScopes :: [(Text, V.Color)]
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

tsDeepScopes :: [Text]
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

mkAttrMap :: Maybe HelixTheme -> AttrMap
mkAttrMap mth =
  let
    topPair (s, c) = (tsScopeAttr s, scopeAttr mth s (fg c))
    deepPairs = case mth of
      Nothing -> []
      Just th ->
        map
          (\s -> (tsScopeAttr s, scopeAttr mth s V.defAttr))
          (filter (hasScope th) tsDeepScopes)
    tsPairs = map topPair tsTopScopes ++ deepPairs
    bgA = scopeAttr mth "ui.background" V.defAttr
    baseAttr =
      let
        fgA = scopeAttr mth "ui.text" V.defAttr
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
      scopeAttr mth "ui.selection.primary" (V.defAttr `V.withStyle` V.reverseVideo)
    uiPairs =
      [ (headerAttr, fg inkDim)
      , (headerRebaseAttr, fg inkText)
      , (headerCountAttr, fg inkText)
      , (rebaseShaAttr, fg inkMidBlue)
      , (conflictAttr, fg inkRose `V.withStyle` V.bold)
      , (watchPollAttr, fg inkGold)
      , (trayDeclAttr, fg inkMidBlue)
      , (headerFileAttr, fg inkFileGold)
      , (headerOkAttr, fg inkGreen)
      , (cursorMarkAttr, V.defAttr `V.withBackColor` cursorWash)
      , (addedMarkAttr, fg inkGreen)
      , (removedMarkAttr, fg inkRose)
      , (ctxMarkAttr, fg inkFaint)
      , (removedTextAttr, fg inkRose)
      , (watchingAttr, fg inkGreen)
      , -- only a background, so the syntax colour underneath survives
        (wordChangeAttr, V.defAttr `V.withBackColor` wordWash)
      , (selAttr, selection)
      , (dimAttr, fg inkDim)
      , (curRunAttr, fg inkMidBlue)
      , (norAttr, fg inkText)
      , (insAttr, fg inkText)
      , (modeDotNorAttr, fg inkLavender)
      , (modeDotInsAttr, fg inkGreen)
      , (msgAttr, fg inkText)
      , (hintAttr, fg inkFaint)
      , (statusAttr, fg inkText)
      , (lineNrAttr, scopeAttr mth "ui.linenr" (fg V.brightBlack))
      , (lineNrCurAttr, scopeAttr mth "ui.linenr.selected" (fg V.yellow))
      , (lineNrDoneAttr, scopeAttr mth "ui.linenr" (fg V.brightBlack) `V.withStyle` V.dim)
      , (borderAttr, fgOnly (scopeAttr mth "ui.linenr" (fg V.brightBlack)))
      , (trayFileAttr, fg trayFileInk `V.withStyle` V.bold)
      , (trayFileCurAttr, fg inkFileGold `V.withStyle` V.bold)
      , (flagMarkAttr, fg inkFlag)
      , (questionMarkAttr, fg inkGold)
      , (doneMarkAttr, fg inkGreen)
      , (pendingMarkAttr, fg inkBlue)
      , (noteInlineAttr, fg inkLavender)
      , (replyInlineAttr, fg inkMidBlue)
      , (floatAttr, onFloat V.defAttr)
      , (floatSelAttr, V.defAttr `V.withBackColor` surfSel)
      , (floatSelBarAttr, fg inkLavender `V.withBackColor` surfSel)
      , (floatTitleAttr, onFloat (fg inkLavender))
      , -- every popup is framed in the same lavender as its title; floatFrame
        -- is the only bordered widget, so this reaches exactly the popups
        (B.borderAttr, onFloat (fg inkLavender))
      , (matchCharAttr, fg matchGold)
      , (meterOnAttr, fg inkGreen)
      , (meterOffAttr, fg meterRail)
      , (hairAttr, fg hairInk)
      ]
  in
    attrMap baseAttr (tsPairs ++ uiPairs)

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

surfFloat, surfSel, cursorWash, wordWash, hairInk, meterRail, matchGold :: V.Color
-- Only used when the theme names no background at all, so there is
-- nothing to measure a lift from.
surfFloat = V.linearColor (0x15 :: Int) 0x15 0x15
surfSel = V.linearColor (0x24 :: Int) 0x24 0x30
cursorWash = V.linearColor (0x3a :: Int) 0x33 0x25
wordWash = V.linearColor (0x3d :: Int) 0x35 0x4a
hairInk = V.linearColor (0x2c :: Int) 0x2c 0x2c
meterRail = V.linearColor (0x3a :: Int) 0x3f 0x3a
matchGold = V.linearColor (0xfa :: Int) 0xd0 0x7a
