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
  , selAttr
  , flaggedWashAttr
  , questionedWashAttr
  , spotlightWashAttr
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
  , modePillNorAttr
  , modePillInsAttr
  , hintKeyAttr
  , statusPlaceAttr
  , statusLineAttr
  , addedWashAttr
  , removedWashAttr
  , addedWordAttr
  , removedWordAttr
  , trayAttr
  , floatStripeAttr
  , noteInlineAttr
  , replyInlineAttr
  , agentInlineAttr
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
  , borderFill
  , mkAttrMap
  ) where

import Brick qualified as Brick
import Brick.Util qualified as BrickUtil
import Data.List qualified as List
import Data.Maybe qualified as Maybe
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
  , selAttr
  , flaggedWashAttr
  , questionedWashAttr
  , spotlightWashAttr
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
  , modePillNorAttr
  , modePillInsAttr
  , hintKeyAttr
  , statusPlaceAttr
  , statusLineAttr
  , addedWashAttr
  , removedWashAttr
  , addedWordAttr
  , removedWordAttr
  , trayAttr
  , floatStripeAttr
  , noteInlineAttr
  , replyInlineAttr
  , agentInlineAttr
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
modePillNorAttr = Brick.attrName "modePillNor"
modePillInsAttr = Brick.attrName "modePillIns"
hintKeyAttr = hintAttr <> Brick.attrName "key"
statusPlaceAttr = statusLineAttr <> Brick.attrName "place"
statusLineAttr = Brick.attrName "statusLine"
addedWashAttr = Brick.attrName "addedWash"
removedWashAttr = Brick.attrName "removedWash"
addedWordAttr = addedWashAttr <> Brick.attrName "word"
removedWordAttr = removedWashAttr <> Brick.attrName "word"
trayAttr = Brick.attrName "tray"
floatStripeAttr = floatAttr <> Brick.attrName "stripe"
noteInlineAttr = Brick.attrName "noteInline"
replyInlineAttr = Brick.attrName "replyInline"
agentInlineAttr = Brick.attrName "agentInline"
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
flaggedWashAttr = Brick.attrName "flaggedWash"
questionedWashAttr = Brick.attrName "questionedWash"
spotlightWashAttr = Brick.attrName "spotlightWash"

settleFrames :: Int
settleFrames = 6

settleAttr :: Int -> Brick.AttrName
settleAttr frame = Brick.attrName ("settle" ++ show (max 1 (min settleFrames frame)))

settleInkAttr :: Int -> Brick.AttrName
settleInkAttr frame = Brick.attrName ("settleInk" ++ show (max 1 (min settleFrames frame)))

selAttr = Brick.attrName "selection"
curRunAttr = Brick.attrName "currentRun"
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

data Chrome = Chrome
  { chBg :: V.Color
  , chText :: V.Color
  , chMuted :: V.Color
  , chFaint :: V.Color
  , chSurface :: V.Color
  , chSurface2 :: V.Color
  , chRaised :: V.Color
  , chLine :: V.Color
  , chSel :: V.Color
  , chAccent :: V.Color
  , chInsert :: V.Color
  , chAdded :: V.Color
  , chRemoved :: V.Color
  , chAddedWash :: V.Color
  , chRemovedWash :: V.Color
  , chAddedWord :: V.Color
  , chRemovedWord :: V.Color
  , chWarning :: V.Color
  }

chromeOf :: Maybe Theme.HelixTheme -> Chrome
chromeOf mth =
  let
    fgOr scope fallback = Maybe.fromMaybe fallback (Theme.scopeFg mth scope)
    bgOr scope fallback = Maybe.fromMaybe fallback (Theme.scopeBg mth scope)
    exactBg scope = Theme.scopeExact mth scope >>= backOf
    bg = bgOr "ui.background" harborBg
    text = fgOr "ui.text" harborText
    surface = bgOr "ui.statusline" harborSurface
    added = fgOr "diff.plus" harborAdded
    removed = fgOr "diff.minus" harborRemoved
    normalBg = Theme.scopeBg mth "ui.statusline.normal"
    insertBg = Theme.scopeBg mth "ui.statusline.insert"
    stands c = contrast c bg >= 0.2 && contrast c surface >= 0.2
    accent = Maybe.fromMaybe harborRemax (List.find stands (Maybe.catMaybes [normalBg, insertBg]))
    washOr scope amount ink fallback = Maybe.fromMaybe (mixOr amount bg ink fallback) (exactBg scope)
  in
    Chrome
      { chBg = bg
      , chText = text
      , chMuted = fgOr "ui.text.inactive" (mixOr 0.55 bg text harborMuted)
      , chFaint = fgOr "ui.linenr" harborFaint
      , chSurface = surface
      , chSurface2 = bgOr "markup.raw.inline" (lighten 0x09 bg)
      , -- A popup sits a hair above the body, and "a hair" has to be
        -- measured from the body when the theme names no popup surface:
        -- the body colour comes from the theme's ui.background, so a
        -- hardcoded surface drifts as far from it as the theme happens
        -- to be dark.
        chRaised = bgOr "ui.popup" (lighten 0x11 bg)
      , chLine = fgOr "ui.virtual.indent-guide" (mixOr 0.12 bg text harborLine)
      , chSel = bgOr "ui.selection.primary" harborSel
      , chAccent = accent
      , chInsert = Maybe.fromMaybe accent (keepIf stands insertBg)
      , chAdded = added
      , chRemoved = removed
      , chAddedWash = washOr "diff.plus.line" 0.08 added harborAddedWash
      , chRemovedWash = washOr "diff.minus.line" 0.08 removed harborRemovedWash
      , chAddedWord = washOr "diff.plus.word" 0.2 added harborAddedWord
      , chRemovedWord = washOr "diff.minus.word" 0.2 removed harborRemovedWord
      , chWarning = fgOr "warning" harborWarning
      }

keepIf :: (a -> Bool) -> Maybe a -> Maybe a
keepIf keep m =
  case m of
    Just v | keep v -> Just v
    _ -> Nothing

contrast :: V.Color -> V.Color -> Double
contrast a b =
  case (a, b) of
    (V.RGBColor r1 g1 b1, V.RGBColor r2 g2 b2) ->
      let
        lum r g bl = (0.2126 * fromIntegral r + 0.7152 * fromIntegral g + 0.0722 * fromIntegral bl) / 255
      in
        abs (lum r1 g1 b1 - lum r2 g2 b2)
    _ -> if a == b then 0 else 1

backOf :: V.Attr -> Maybe V.Color
backOf a =
  case V.attrBackColor a of
    V.SetTo c -> Just c
    _ -> Nothing

-- A share of the way from one colour to another. Only truecolor values
-- have channels to blend; anything else takes the fallback.
mixOr :: Double -> V.Color -> V.Color -> V.Color -> V.Color
mixOr amount from to fallback =
  case (from, to) of
    (V.RGBColor r1 g1 b1, V.RGBColor r2 g2 b2) ->
      let
        step a b = round (fromIntegral a + amount * (fromIntegral b - fromIntegral a)) :: Int
      in
        V.linearColor (step r1 r2) (step g1 g2) (step b1 b2)
    _ -> fallback

-- The attr the body's raw border fills are drawn in.
borderFill :: Maybe Theme.HelixTheme -> V.Attr
borderFill mth =
  let
    ch = chromeOf mth
  in
    BrickUtil.fg (chLine ch) `V.withBackColor` chBg ch

mkAttrMap :: Maybe Theme.HelixTheme -> Brick.AttrMap
mkAttrMap mth =
  let
    ch = chromeOf mth
    topPair (s, c) = (Highlight.tsScopeAttr s, Theme.scopeAttr mth s (BrickUtil.fg c))
    deepPairs = case mth of
      Nothing -> []
      Just th ->
        map
          (\s -> (Highlight.tsScopeAttr s, Theme.scopeAttr mth s V.defAttr))
          (filter (Theme.hasScope th) tsDeepScopes)
    tsPairs = map topPair tsTopScopes ++ deepPairs
    baseAttr = V.defAttr `V.withForeColor` chText ch `V.withBackColor` chBg ch
    onRaised a = a `V.withBackColor` chRaised ch
    fgOf c = BrickUtil.fg c
    washOf c = V.defAttr `V.withBackColor` c
    pill c = (V.defAttr `V.withForeColor` chBg ch `V.withBackColor` c) `V.withStyle` V.bold
    uiPairs =
      [ (headerAttr, fgOf (chMuted ch))
      , (headerRebaseAttr, fgOf (chMuted ch))
      , (headerCountAttr, fgOf (chMuted ch))
      , (rebaseShaAttr, fgOf inkMidBlue)
      , (conflictAttr, fgOf (chRemoved ch) `V.withStyle` V.bold)
      , (watchPollAttr, fgOf (chWarning ch))
      , (trayDeclAttr, fgOf inkMidBlue)
      , (headerFileAttr, fgOf (chText ch))
      , (headerOkAttr, fgOf (chAdded ch))
      , (cursorMarkAttr, washOf (chSel ch))
      , (addedMarkAttr, fgOf (chAdded ch))
      , (removedMarkAttr, fgOf (chRemoved ch))
      , (ctxMarkAttr, fgOf (chFaint ch))
      , (removedTextAttr, fgOf (mixOr 0.35 (chText ch) (chRemoved ch) (chRemoved ch)))
      , (watchingAttr, fgOf (chAccent ch))
      , -- only a background, so the syntax colour underneath survives
        (addedWashAttr, washOf (chAddedWash ch))
      , (removedWashAttr, washOf (chRemovedWash ch))
      , (addedWordAttr, washOf (chAddedWord ch))
      , (removedWordAttr, washOf (chRemovedWord ch))
      , (selAttr, Theme.scopeAttr mth "ui.selection.primary" (washOf (chSel ch)))
      , (flaggedWashAttr, washOf (mixOr 0.12 (chBg ch) (chWarning ch) flaggedWash))
      , (questionedWashAttr, washOf (mixOr 0.1 (chBg ch) inkGold questionedWash))
      , (spotlightWashAttr, washOf (mixOr 0.12 (chBg ch) inkTeal spotlightWash))
      , (curRunAttr, fgOf (chAccent ch))
      , (modePillNorAttr, pill (chAccent ch))
      , (modePillInsAttr, pill (chInsert ch))
      , (msgAttr, fgOf (chText ch))
      , (hintAttr, fgOf (chMuted ch))
      , (hintKeyAttr, fgOf (chText ch))
      , (statusPlaceAttr, fgOf (chAccent ch))
      , (statusAttr, fgOf (chText ch))
      , (statusLineAttr, fgOf (chText ch) `V.withBackColor` chSurface ch)
      , (lineNrAttr, fgOf (chFaint ch))
      , (lineNrCurAttr, fgOf (Maybe.fromMaybe (chText ch) (Theme.scopeFg mth "ui.linenr.selected")))
      , (lineNrDoneAttr, fgOf (chFaint ch) `V.withStyle` V.dim)
      , (borderAttr, fgOf (chLine ch))
      , (trayAttr, fgOf (chText ch) `V.withBackColor` chSurface ch)
      , (trayFileAttr, fgOf (chText ch) `V.withStyle` V.bold)
      , (trayFileCurAttr, fgOf (chAccent ch) `V.withStyle` V.bold)
      , (flagMarkAttr, fgOf (chWarning ch))
      , (questionMarkAttr, fgOf inkGold)
      , (doneMarkAttr, fgOf (chAdded ch))
      , (pendingMarkAttr, fgOf (chMuted ch))
      , (noteInlineAttr, fgOf inkLavender)
      , (replyInlineAttr, fgOf inkMidBlue)
      , (agentInlineAttr, fgOf inkTeal)
      , (guideAttr, fgOf inkTeal `V.withBackColor` mixOr 0.1 (chBg ch) inkTeal surfGuide)
      , (floatAttr, onRaised (fgOf (chText ch)))
      , (floatSelAttr, washOf (chSel ch))
      , (floatSelBarAttr, fgOf (chAccent ch) `V.withBackColor` chSel ch)
      , (floatTitleAttr, onRaised (fgOf (chText ch)) `V.withStyle` V.bold)
      , (floatStripeAttr, onRaised (fgOf (chAccent ch)))
      , (matchCharAttr, fgOf matchGold)
      , (meterOnAttr, fgOf (chAccent ch))
      , (meterOffAttr, fgOf (chLine ch))
      , (hairAttr, fgOf (chLine ch))
      , (mdHeadingAttr, fgOf (chText ch) `V.withStyle` V.bold `V.withStyle` V.underline)
      , (mdBoldAttr, fgOf (chText ch) `V.withStyle` V.bold)
      , (mdCodeAttr, fgOf (chText ch) `V.withBackColor` chSurface2 ch)
      , (mdLinkAttr, fgOf inkBlue `V.withStyle` V.underline)
      , (mdTableHeaderAttr, fgOf (chText ch) `V.withStyle` V.underline)
      ]
    settleShade frame =
      mixOr (fromIntegral frame / fromIntegral settleFrames) (chBg ch) (chAddedWord ch) (chAddedWord ch)
    settleInk frame =
      mixOr (fromIntegral frame / fromIntegral settleFrames) (chAccent ch) (chText ch) (chAccent ch)
    settlePairs =
      map (\n -> (settleAttr n, washOf (settleShade n))) [1 .. settleFrames]
        ++ map (\n -> (settleInkAttr n, fgOf (settleInk n))) [1 .. settleFrames]
  in
    Brick.attrMap baseAttr (tsPairs ++ uiPairs ++ settlePairs)

harborBg, harborText, harborMuted, harborFaint, harborSurface, harborLine, harborSel, harborRemax :: V.Color
harborBg = V.linearColor (0x15 :: Int) 0x17 0x1d
harborText = V.linearColor (0xd5 :: Int) 0xd8 0xe0
harborMuted = V.linearColor (0x86 :: Int) 0x8c 0x9c
harborFaint = V.linearColor (0x4f :: Int) 0x55 0x66
harborSurface = V.linearColor (0x1b :: Int) 0x1e 0x26
harborLine = V.linearColor (0x2c :: Int) 0x31 0x3d
harborSel = V.linearColor (0x28 :: Int) 0x33 0x50
harborRemax = V.linearColor (0x9f :: Int) 0xd0 0x7e

harborAdded, harborRemoved, harborAddedWash, harborRemovedWash, harborAddedWord, harborRemovedWord, harborWarning :: V.Color
harborAdded = V.linearColor (0x9f :: Int) 0xd0 0x7e
harborRemoved = V.linearColor (0xec :: Int) 0x7b 0x7b
harborAddedWash = V.linearColor (0x1d :: Int) 0x2a 0x20
harborRemovedWash = V.linearColor (0x2d :: Int) 0x1d 0x22
harborAddedWord = V.linearColor (0x2c :: Int) 0x46 0x30
harborRemovedWord = V.linearColor (0x4b :: Int) 0x2a 0x31
harborWarning = V.linearColor (0xf0 :: Int) 0xa3 0x6a

-- The Quiet Ink · Tinted palette: jellybeans-adjacent literals, chosen
-- in the temperaments mockups. linearColor keeps true 24-bit values;
-- vty approximates on lesser terminals.
inkBlue, inkGold, inkLavender, inkMidBlue :: V.Color
inkBlue = V.linearColor (0x7f :: Int) 0x9e 0xc7
inkGold = V.linearColor (0xd3 :: Int) 0xb0 0x78
inkLavender = V.linearColor (0xa3 :: Int) 0x90 0xf0
inkMidBlue = V.linearColor (0x81 :: Int) 0x97 0xbf

surfGuide, inkTeal, flaggedWash, questionedWash, spotlightWash, matchGold :: V.Color
surfGuide = V.linearColor (0x1c :: Int) 0x2e 0x2e
inkTeal = V.linearColor (0x9f :: Int) 0xd8 0xd0
flaggedWash = V.linearColor (0x24 :: Int) 0x17 0x1b
questionedWash = V.linearColor (0x22 :: Int) 0x1f 0x14
spotlightWash = V.linearColor (0x13 :: Int) 0x2a 0x2e
matchGold = V.linearColor (0xfa :: Int) 0xd0 0x7a
