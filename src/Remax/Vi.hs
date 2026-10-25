{-# LANGUAGE OverloadedStrings #-}

-- Vi-flavored modal editing: the mirror image of the helix layer next
-- door. Where helix moves first and lets the verb consume the selection,
-- vi holds the verb and waits for the motion — so this module is a
-- grammar, not a second editor. It reads a count, an operator, and a
-- motion or text object, turns the result into exactly the selection
-- Remax.Modal's verbs already expect, and calls the same deleteSel /
-- changeSel / yankSel / indentLines the helix keys call.
--
-- Motions are its own, though: Modal's w/b/e are helix-shaped (the
-- crossed span becomes the selection, and a "word" is anything
-- non-blank), while vi needs plain positions plus the word/WORD
-- distinction and the exclusive/inclusive rules that decide whether `dw`
-- eats the character it lands on.
--
-- Not implemented, deliberately: registers, marks and the jumplist,
-- blockwise visual (the selection model is char- or line-wise only), and
-- R replace mode.
module Remax.Vi
  ( ViKeys
  , ViResult (..)
  , viIdle
  , viBusy
  , viLabel
  , handleVi
  ) where

import Data.Char qualified as Char
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Graphics.Vty qualified as V

import Remax.Modal qualified as Modal
import Remax.SoftWrap qualified as SoftWrap
import Remax.Zipper qualified as Z

-- Operators: everything that waits for a motion or a text object.
data ViOp
  = OpDelete
  | OpChange
  | OpYank
  | OpIndent
  | OpUnindent
  | OpLower
  | OpUpper
  | OpSwapCase
  deriving (Eq, Show)

data ObjKind = Inner | Around
  deriving (Eq, Show)

data FindDir = FindTo | FindBack | FindTill | FindTillBack
  deriving (Eq, Show)

-- What the next keystroke has to be. Anything other than WaitNone means
-- the vi layer owns that keystroke outright.
data ViWait
  = WaitNone
  | WaitMotion -- an operator is held, waiting for its motion
  | WaitObject ObjKind -- i or a typed inside an operator
  | WaitFind FindDir -- f F t T waiting for its target character
  | WaitReplace -- r waiting for its replacement
  | WaitG -- g typed: gg, ge, gu, gU, g~
  deriving (Eq, Show)

data ViKeys = ViKeys
  { viCount :: Maybe Int -- digits typed before the operator
  , viOp :: Maybe ViOp
  , viOpCount :: Maybe Int -- digits typed after it; vi multiplies the two
  , viWait :: ViWait
  , viFind :: Maybe (FindDir, Char) -- what ; and , repeat
  , viKeep :: [V.Event] -- the command being typed, reversed
  , viDot :: [V.Event] -- the last command that changed something
  }

-- Whether a motion includes the cell it lands on, or covers whole lines.
data Span
  = Exclusive (Int, Int)
  | Inclusive (Int, Int)
  | Linewise Int
  | AlreadySpan Modal.SelKind (Int, Int) (Int, Int)

data ViResult
  = ViPending ViKeys
  | ViDone ViKeys Modal.EdState
  | ViUnhandled

viIdle :: ViKeys
viIdle =
  ViKeys
    { viCount = Nothing
    , viOp = Nothing
    , viOpCount = Nothing
    , viWait = WaitNone
    , viFind = Nothing
    , viKeep = []
    , viDot = []
    }

-- True while a command is half-typed. The caller uses this to hand the
-- next keystroke straight to the vi layer, so a pending `f` targets a
-- `]` rather than letting the review keys steal it.
viBusy :: ViKeys -> Bool
viBusy keys =
  viWait keys /= WaitNone || viCount keys /= Nothing || viOp keys /= Nothing

-- What the status chip shows mid-command: "2d", "ci", "f", "g".
viLabel :: ViKeys -> Maybe T.Text
viLabel keys =
  let
    countText = maybe "" tshow (viCount keys)
    opText = maybe "" opChar (viOp keys)
    opCountText = maybe "" tshow (viOpCount keys)
    waitText = case viWait keys of
      WaitNone -> ""
      WaitMotion -> ""
      WaitObject Inner -> "i"
      WaitObject Around -> "a"
      WaitFind dir -> findChar dir
      WaitReplace -> "r"
      WaitG -> "g"
    label = countText <> opText <> opCountText <> waitText
  in
    if T.null label then Nothing else Just label

opChar :: ViOp -> T.Text
opChar op =
  case op of
    OpDelete -> "d"
    OpChange -> "c"
    OpYank -> "y"
    OpIndent -> ">"
    OpUnindent -> "<"
    OpLower -> "gu"
    OpUpper -> "gU"
    OpSwapCase -> "g~"

findChar :: FindDir -> T.Text
findChar dir =
  case dir of
    FindTo -> "f"
    FindBack -> "F"
    FindTill -> "t"
    FindTillBack -> "T"

tshow :: Int -> T.Text
tshow = T.pack . show

-- Dispatch ------------------------------------------------------------------------

handleVi :: ViKeys -> V.Event -> Modal.EdState -> ViResult
handleVi keys ev ed =
  let
    kept = keys {viKeep = ev : viKeep keys}
  in
    case viWait keys of
      WaitReplace -> replaceKey kept ev ed
      WaitFind dir -> findKey kept dir ev ed
      WaitObject kind -> objectKey kept kind ev ed
      WaitG -> gKey kept ev ed
      WaitMotion -> operatorKey kept ev ed
      WaitNone -> normalKey kept ev ed

-- A command is finished: reset everything transient, and remember it for
-- `.` only if it actually changed the buffer (or opened an insert
-- session, whose text `.` replays too). Bare motions leave `.` alone.
finish :: ViKeys -> Modal.EdState -> Modal.EdState -> ViResult
finish keys before after =
  let
    changed =
      Modal.bufferLines after /= Modal.bufferLines before || Modal.edMode after == Modal.Insert
    dot = if changed then reverse (viKeep keys) else viDot keys
  in
    ViDone viIdle {viFind = viFind keys, viDot = dot} after

-- Abandon a half-typed command without touching the buffer.
cancel :: ViKeys -> Modal.EdState -> ViResult
cancel keys ed =
  ViDone viIdle {viFind = viFind keys, viDot = viDot keys} (Modal.collapse ed)

resolvedCount :: ViKeys -> Int
resolvedCount keys =
  let
    countOf = maybe 1 id
  in
    countOf (viCount keys) * countOf (viOpCount keys)

hasCount :: ViKeys -> Bool
hasCount keys = viCount keys /= Nothing || viOpCount keys /= Nothing

-- Digits accumulate into whichever count is open: before the operator
-- while none is held, after it once one is.
pushDigit :: ViKeys -> Char -> ViKeys
pushDigit keys c =
  let
    grow prev = Just (maybe 0 id prev * 10 + (fromEnum c - fromEnum '0'))
  in
    case viOp keys of
      Nothing -> keys {viCount = grow (viCount keys)}
      Just _ -> keys {viOpCount = grow (viOpCount keys)}

-- Normal mode ---------------------------------------------------------------------

normalKey :: ViKeys -> V.Event -> Modal.EdState -> ViResult
normalKey keys ev ed =
  let
    count = resolvedCount keys
    withOp op = ViPending keys {viOp = Just op, viWait = WaitMotion}
    -- an operator typed while visual mode is on acts on the selection
    -- immediately; there is nothing left to wait for
    operator op =
      if Modal.edExtend ed
        then finish keys ed (applyToSelection op ed)
        else withOp op
    done = finish keys ed
  in
    case ev of
      V.EvKey (V.KChar c) []
        | Char.isDigit c
        , c /= '0' || hasCount keys ->
            ViPending (pushDigit keys c)
      V.EvKey V.KEsc [] -> cancel keys ed
      -- operators
      V.EvKey (V.KChar 'd') [] -> operator OpDelete
      V.EvKey (V.KChar 'c') [] -> operator OpChange
      V.EvKey (V.KChar 'y') [] -> operator OpYank
      V.EvKey (V.KChar '>') [] -> operator OpIndent
      V.EvKey (V.KChar '<') [] -> operator OpUnindent
      V.EvKey (V.KChar 'g') [] -> ViPending keys {viWait = WaitG}
      -- operator shorthands
      V.EvKey (V.KChar 'x') [] ->
        done (spanOp OpDelete (Exclusive (rightBy count ed)) ed)
      V.EvKey (V.KChar 'X') [] ->
        done (spanOp OpDelete (Exclusive (leftBy count ed)) ed)
      V.EvKey (V.KChar 'D') [] ->
        done (spanOp OpDelete (Inclusive (endOfLine ed)) ed)
      V.EvKey (V.KChar 'C') [] ->
        done (spanOp OpChange (Inclusive (endOfLine ed)) ed)
      V.EvKey (V.KChar 's') [] ->
        done (spanOp OpChange (Exclusive (rightBy count ed)) ed)
      V.EvKey (V.KChar 'S') [] ->
        done (spanOp OpChange (Linewise (downBy (count - 1) ed)) ed)
      V.EvKey (V.KChar 'Y') [] ->
        done (spanOp OpYank (Linewise (downBy (count - 1) ed)) ed)
      -- entering insert
      V.EvKey (V.KChar 'i') [] -> done (Modal.enterInsert Modal.EntryI id ed)
      V.EvKey (V.KChar 'a') [] -> done (Modal.enterInsert Modal.EntryAppend Z.moveRight ed)
      V.EvKey (V.KChar 'I') [] -> done (Modal.enterInsert Modal.EntryBOL firstNonBlankZ ed)
      V.EvKey (V.KChar 'A') [] -> done (Modal.enterInsert Modal.EntryEOL Z.gotoEOL ed)
      V.EvKey (V.KChar 'o') [] -> done (Modal.openLine 1 ed)
      V.EvKey (V.KChar 'O') [] -> done (Modal.openLine 0 ed)
      -- edits that need no motion
      V.EvKey (V.KChar 'r') [] -> ViPending keys {viWait = WaitReplace}
      V.EvKey (V.KChar '~') [] ->
        done (Modal.plainMove Z.moveRight (Modal.toggleCase (Modal.collapse ed)))
      V.EvKey (V.KChar 'J') [] -> done (joinBy count ed)
      V.EvKey (V.KChar 'p') [] -> done (repeatEdit count (Modal.pasteSel 1) ed)
      V.EvKey (V.KChar 'P') [] -> done (repeatEdit count (Modal.pasteSel 0) ed)
      V.EvKey (V.KChar 'u') [] -> done (repeatEdit count Modal.undo ed)
      V.EvKey (V.KChar 'r') [V.MCtrl] -> done (repeatEdit count Modal.redo ed)
      V.EvKey (V.KChar '.') [] -> replayDot keys ed
      -- visual mode
      V.EvKey (V.KChar 'v') [] -> done (enterVisual Modal.CharSel ed)
      V.EvKey (V.KChar 'V') [] -> done (enterVisual Modal.LineSel ed)
      -- find, and its repeats
      V.EvKey (V.KChar 'f') [] -> ViPending keys {viWait = WaitFind FindTo}
      V.EvKey (V.KChar 'F') [] -> ViPending keys {viWait = WaitFind FindBack}
      V.EvKey (V.KChar 't') [] -> ViPending keys {viWait = WaitFind FindTill}
      V.EvKey (V.KChar 'T') [] -> ViPending keys {viWait = WaitFind FindTillBack}
      _
        | SoftWrap.WrappedRows _ <- Modal.edRowLayout ed
        , Just dir <- rowStep ev ->
            done (Modal.plainMove (Modal.rowsBy (dir * count) ed) ed)
      _ ->
        case motionFor keys count ev ed of
          Just s -> done (moveOver s ed)
          Nothing -> ViUnhandled

rowStep :: V.Event -> Maybe Int
rowStep ev =
  case ev of
    V.EvKey (V.KChar 'j') [] -> Just 1
    V.EvKey V.KDown [] -> Just 1
    V.EvKey (V.KChar 'k') [] -> Just (-1)
    V.EvKey V.KUp [] -> Just (-1)
    _ -> Nothing

-- Waiting for an operator's motion ------------------------------------------------

operatorKey :: ViKeys -> V.Event -> Modal.EdState -> ViResult
operatorKey keys ev ed =
  let
    count = resolvedCount keys
    op = maybe OpDelete id (viOp keys)
    done = finish keys ed
  in
    case ev of
      V.EvKey (V.KChar c) []
        | Char.isDigit c
        , c /= '0' || hasCount keys ->
            ViPending (pushDigit keys c)
      V.EvKey V.KEsc [] -> cancel keys ed
      V.EvKey (V.KChar 'i') [] -> ViPending keys {viWait = WaitObject Inner}
      V.EvKey (V.KChar 'a') [] -> ViPending keys {viWait = WaitObject Around}
      V.EvKey (V.KChar 'g') [] -> ViPending keys {viWait = WaitG}
      V.EvKey (V.KChar 'f') [] -> ViPending keys {viWait = WaitFind FindTo}
      V.EvKey (V.KChar 'F') [] -> ViPending keys {viWait = WaitFind FindBack}
      V.EvKey (V.KChar 't') [] -> ViPending keys {viWait = WaitFind FindTill}
      V.EvKey (V.KChar 'T') [] -> ViPending keys {viWait = WaitFind FindTillBack}
      -- the doubled operator: dd, cc, yy, >>, <<
      V.EvKey (V.KChar c) []
        | Just c == doubleChar op ->
            done (spanOp op (Linewise (downBy (count - 1) ed)) ed)
      _ ->
        case motionFor keys count ev ed of
          Just s -> done (spanOp op (clampForOp op count ev ed s) ed)
          Nothing -> cancel keys ed

-- `dd`, `cc`, `yy`, `>>`, `<<`, `gugu`. Which key repeated means "this
-- whole line".
doubleChar :: ViOp -> Maybe Char
doubleChar op =
  case op of
    OpDelete -> Just 'd'
    OpChange -> Just 'c'
    OpYank -> Just 'y'
    OpIndent -> Just '>'
    OpUnindent -> Just '<'
    OpLower -> Just 'u'
    OpUpper -> Just 'U'
    OpSwapCase -> Just '~'

-- Two of vi's fussier rules about `w` under an operator: `cw` behaves
-- like `ce` (change the word, not the gap after it), and `dw` on the
-- last word of a line stops at the line's end instead of dragging the
-- next line up.
clampForOp :: ViOp -> Int -> V.Event -> Modal.EdState -> Span -> Span
clampForOp op count ev ed s =
  let
    (row, col) = Modal.cursorRC ed
    line = currentLineText ed
    onWord = col < T.length line && not (Char.isSpace (T.index line col))
    adjust cls to =
      if op == OpChange && onWord
        then Inclusive (wordEndFrom cls count ed)
        else
          if fst to > row
            then Exclusive (row, T.length line)
            else Exclusive to
  in
    case (ev, s) of
      (V.EvKey (V.KChar 'w') [], Exclusive to) -> adjust Modal.SmallWord to
      (V.EvKey (V.KChar 'W') [], Exclusive to) -> adjust Modal.BigWord to
      _ -> s

-- g-prefixed: gg and ge are motions, gu/gU/g~ are operators.
gKey :: ViKeys -> V.Event -> Modal.EdState -> ViResult
gKey keys ev ed =
  let
    count = resolvedCount keys
    row = fst (Modal.cursorRC ed)
    held = viOp keys
    -- gu with that same operator already held is the doubled form
    -- (`guu`, a whole line); with none held it starts one
    startOp op =
      case held of
        Just prior
          | prior == op ->
              finish keys ed (spanOp op (Linewise (downBy (count - 1) ed)) ed)
        _ -> ViPending keys {viOp = Just op, viWait = WaitMotion}
    motion s =
      case held of
        Nothing -> finish keys ed (moveOver s ed)
        Just op -> finish keys ed (spanOp op s ed)
  in
    case ev of
      V.EvKey (V.KChar 'u') [] -> startOp OpLower
      V.EvKey (V.KChar 'U') [] -> startOp OpUpper
      V.EvKey (V.KChar '~') [] -> startOp OpSwapCase
      V.EvKey (V.KChar 'g') [] ->
        motion (Linewise (maybe 0 (subtract 1) (viCount keys) - row))
      V.EvKey (V.KChar 'e') [] ->
        motion (Inclusive (wordEndBackFrom Modal.SmallWord count ed))
      V.EvKey (V.KChar 'E') [] ->
        motion (Inclusive (wordEndBackFrom Modal.BigWord count ed))
      _ -> cancel keys ed

-- f/F/t/T with their target, and the ; and , that repeat them.
findKey :: ViKeys -> FindDir -> V.Event -> Modal.EdState -> ViResult
findKey keys dir ev ed =
  case ev of
    V.EvKey (V.KChar c) [] ->
      applyFindDir keys {viFind = Just (dir, c)} dir c ed
    _ -> cancel keys ed

applyFindDir :: ViKeys -> FindDir -> Char -> Modal.EdState -> ViResult
applyFindDir keys dir c ed =
  let
    count = resolvedCount keys
  in
    case findTarget dir count c ed of
      Nothing -> cancel keys ed
      Just to ->
        let
          s = case dir of
            FindTo -> Inclusive to
            FindTill -> Inclusive to
            FindBack -> Exclusive to
            FindTillBack -> Exclusive to
        in
          case viOp keys of
            Nothing -> finish keys ed (moveOver s ed)
            Just op -> finish keys ed (spanOp op s ed)

replaceKey :: ViKeys -> V.Event -> Modal.EdState -> ViResult
replaceKey keys ev ed =
  case ev of
    V.EvKey (V.KChar c) [] -> finish keys ed (Modal.replaceSel c ed)
    _ -> cancel keys ed

-- Text objects --------------------------------------------------------------------

objectKey :: ViKeys -> ObjKind -> V.Event -> Modal.EdState -> ViResult
objectKey keys kind ev ed =
  let
    op = maybe OpDelete id (viOp keys)
    apply s = finish keys ed (spanOp op s ed)
  in
    case ev of
      V.EvKey (V.KChar c) [] ->
        case objectSpan kind c ed of
          Just s -> apply s
          Nothing -> cancel keys ed
      _ -> cancel keys ed

objectSpan :: ObjKind -> Char -> Modal.EdState -> Maybe Span
objectSpan kind c ed =
  let
    ls = Modal.bufferLines ed
    pos = Modal.cursorRC ed
    pair open close = fmap (pairSpan kind) (Modal.matchEnclosing open close ls pos)
  in
    case c of
      'w' -> Just (wordObject kind Modal.SmallWord ed)
      'W' -> Just (wordObject kind Modal.BigWord ed)
      'p' -> Just (paragraphObject kind ed)
      '(' -> pair '(' ')'
      ')' -> pair '(' ')'
      'b' -> pair '(' ')'
      '{' -> pair '{' '}'
      '}' -> pair '{' '}'
      'B' -> pair '{' '}'
      '[' -> pair '[' ']'
      ']' -> pair '[' ']'
      '<' -> pair '<' '>'
      '>' -> pair '<' '>'
      '"' -> quoteObject kind '"' ed
      '\'' -> quoteObject kind '\'' ed
      '`' -> quoteObject kind '`' ed
      _ -> Nothing

-- `a(` takes the brackets, `i(` takes what is between them; an empty
-- pair has nothing inside, so `i` collapses to no-op there.
pairSpan :: ObjKind -> ((Int, Int), (Int, Int)) -> Span
pairSpan kind (open, close) =
  case kind of
    Around -> AlreadySpan Modal.CharSel open close
    Inner -> AlreadySpan Modal.CharSel (stepForward open) (stepBackward close)

-- Positions are compared and stepped in document order; these two only
-- ever move within the enclosing span, so a row change is safe.
stepForward :: (Int, Int) -> (Int, Int)
stepForward (r, c) = (r, c + 1)

stepBackward :: (Int, Int) -> (Int, Int)
stepBackward (r, c) = (r, c - 1)

wordObject :: ObjKind -> Modal.WordClass -> Modal.EdState -> Span
wordObject kind cls ed =
  let
    ls = Modal.bufferLines ed
    (row, col) = Modal.cursorRC ed
    line = Modal.lineAt ls row
    len = T.length line
    classOf i = Modal.charClass cls (T.index line i)
    here = if col < len then classOf col else Modal.ClsSpace
    runFrom step i =
      if i >= 0 && i < len && classOf i == here then runFrom step (i + step) else i - step
    lo = if col < len then runFrom (-1) col else col
    hi = if col < len then runFrom 1 col else col
    trailing j =
      if j + 1 < len && classOf (j + 1) == Modal.ClsSpace then trailing (j + 1) else j
    leading j =
      if j - 1 >= 0 && classOf (j - 1) == Modal.ClsSpace then leading (j - 1) else j
    hiAround = trailing hi
    (loFinal, hiFinal) =
      case kind of
        Inner -> (lo, hi)
        Around ->
          if hiAround > hi || here == Modal.ClsSpace
            then (lo, hiAround)
            else (leading lo, hi)
  in
    AlreadySpan Modal.CharSel (row, loFinal) (row, max loFinal hiFinal)

-- A paragraph is a run of lines all blank or all not; `ap` takes the run
-- of the opposite kind that follows, which is what makes `dap` remove
-- the gap after the paragraph as well.
paragraphObject :: ObjKind -> Modal.EdState -> Span
paragraphObject kind ed =
  let
    ls = Modal.bufferLines ed
    lastRow = max 0 (length ls - 1)
    (row, _) = Modal.cursorRC ed
    blank r = T.null (T.strip (Modal.lineAt ls r))
    here = blank row
    runUp r = if r > 0 && blank (r - 1) == here then runUp (r - 1) else r
    runDown r = if r < lastRow && blank (r + 1) == here then runDown (r + 1) else r
    hi = runDown row
    -- the far end of the opposite-kind run just past this one
    trailing r =
      if r < lastRow && blank (r + 1) == not here then trailing (r + 1) else r
    hiFinal =
      case kind of
        Inner -> hi
        Around -> trailing hi
  in
    AlreadySpan Modal.LineSel (runUp row, 0) (hiFinal, 0)

-- Quote objects stay on one line, the way vi treats them: pair the quotes
-- off from the line's start and take the pair the cursor sits in, else
-- the next pair along.
quoteObject :: ObjKind -> Char -> Modal.EdState -> Maybe Span
quoteObject kind q ed =
  let
    ls = Modal.bufferLines ed
    (row, col) = Modal.cursorRC ed
    line = Modal.lineAt ls row
    pairUp idxs =
      case idxs of
        (a : b : rest) -> (a, b) : pairUp rest
        _ -> []
    pairs = pairUp (charCols q line)
    inside = filter (\(a, b) -> col >= a && col <= b) pairs
    ahead = filter (\(a, _) -> a > col) pairs
  in
    case inside ++ ahead of
      ((a, b) : _) ->
        case kind of
          Around -> Just (AlreadySpan Modal.CharSel (row, a) (row, b))
          Inner ->
            if b - a <= 1
              then Nothing
              else Just (AlreadySpan Modal.CharSel (row, a + 1) (row, b - 1))
      [] -> Nothing

-- The innermost open/close pair enclosing the cursor: walk back to the
-- first unmatched opener, then forward from it to its partner. Bounded
-- so a stray bracket in a huge file cannot turn into a full scan.
-- Motions -------------------------------------------------------------------------

motionFor :: ViKeys -> Int -> V.Event -> Modal.EdState -> Maybe Span
motionFor keys count ev ed =
  let
    (row, _) = Modal.cursorRC ed
  in
    case ev of
      V.EvKey (V.KChar 'h') [] -> Just (Exclusive (leftBy count ed))
      V.EvKey V.KLeft [] -> Just (Exclusive (leftBy count ed))
      V.EvKey (V.KChar 'l') [] -> Just (Exclusive (rightBy count ed))
      V.EvKey V.KRight [] -> Just (Exclusive (rightBy count ed))
      V.EvKey (V.KChar ' ') [] -> Nothing
      V.EvKey (V.KChar 'j') [] -> Just (Linewise (downBy count ed))
      V.EvKey V.KDown [] -> Just (Linewise (downBy count ed))
      V.EvKey (V.KChar 'k') [] -> Just (Linewise (downBy (negate count) ed))
      V.EvKey V.KUp [] -> Just (Linewise (downBy (negate count) ed))
      V.EvKey V.KEnter [] -> Just (Linewise (downBy count ed))
      V.EvKey (V.KChar '0') [] -> Just (Exclusive (row, 0))
      V.EvKey V.KHome [] -> Just (Exclusive (row, 0))
      V.EvKey (V.KChar '^') [] -> Just (Exclusive (row, firstNonBlankCol ed row))
      V.EvKey (V.KChar '$') [] ->
        Just (Inclusive (endOfLineAt (downBy (count - 1) ed) ed))
      V.EvKey V.KEnd [] -> Just (Inclusive (endOfLine ed))
      V.EvKey (V.KChar 'w') [] -> Just (Exclusive (wordStartFrom Modal.SmallWord count ed))
      V.EvKey (V.KChar 'W') [] -> Just (Exclusive (wordStartFrom Modal.BigWord count ed))
      V.EvKey (V.KChar 'b') [] -> Just (Exclusive (wordBackFrom Modal.SmallWord count ed))
      V.EvKey (V.KChar 'B') [] -> Just (Exclusive (wordBackFrom Modal.BigWord count ed))
      V.EvKey (V.KChar 'e') [] -> Just (Inclusive (wordEndFrom Modal.SmallWord count ed))
      V.EvKey (V.KChar 'E') [] -> Just (Inclusive (wordEndFrom Modal.BigWord count ed))
      V.EvKey (V.KChar 'G') [] ->
        Just (Linewise (maybe (lastRowOf ed) (subtract 1) (viCount keys) - row))
      V.EvKey (V.KChar '%') [] -> fmap Inclusive (Modal.matchingBracket ed)
      V.EvKey (V.KChar ';') [] -> repeatFind keys count ed id
      V.EvKey (V.KChar ',') [] -> repeatFind keys count ed flipDir
      _ -> Nothing

repeatFind ::
  ViKeys -> Int -> Modal.EdState -> (FindDir -> FindDir) -> Maybe Span
repeatFind keys count ed rewrite =
  case viFind keys of
    Nothing -> Nothing
    Just (dir0, c) ->
      let
        dir = rewrite dir0
      in
        case findTarget dir count c ed of
          Nothing -> Nothing
          Just to ->
            case dir of
              FindTo -> Just (Inclusive to)
              FindTill -> Just (Inclusive to)
              FindBack -> Just (Exclusive to)
              FindTillBack -> Just (Exclusive to)

flipDir :: FindDir -> FindDir
flipDir dir =
  case dir of
    FindTo -> FindBack
    FindBack -> FindTo
    FindTill -> FindTillBack
    FindTillBack -> FindTill

-- Positions -----------------------------------------------------------------------

currentLineText :: Modal.EdState -> T.Text
currentLineText ed = Modal.lineAt (Modal.bufferLines ed) (fst (Modal.cursorRC ed))

-- Every column holding this character, left to right.
charCols :: Char -> T.Text -> [Int]
charCols c line =
  let
    keep (i, ch) = if ch == c then Just i else Nothing
  in
    Maybe.mapMaybe keep (zip [0 ..] (T.unpack line))

lastRowOf :: Modal.EdState -> Int
lastRowOf ed = max 0 (length (Modal.bufferLines ed) - 1)

leftBy :: Int -> Modal.EdState -> (Int, Int)
leftBy n ed =
  let
    (r, c) = Modal.cursorRC ed
  in
    (r, max 0 (c - n))

rightBy :: Int -> Modal.EdState -> (Int, Int)
rightBy n ed =
  let
    (r, c) = Modal.cursorRC ed
  in
    (r, min (T.length (currentLineText ed)) (c + n))

downBy :: Int -> Modal.EdState -> Int
downBy n ed =
  let
    (r, _) = Modal.cursorRC ed
  in
    max 0 (min (lastRowOf ed) (r + n)) - r

endOfLine :: Modal.EdState -> (Int, Int)
endOfLine ed =
  let
    (r, _) = Modal.cursorRC ed
  in
    (r, max 0 (T.length (currentLineText ed) - 1))

endOfLineAt :: Int -> Modal.EdState -> (Int, Int)
endOfLineAt delta ed =
  let
    (r, _) = Modal.cursorRC ed
    row = r + delta
  in
    (row, max 0 (T.length (Modal.lineAt (Modal.bufferLines ed) row) - 1))

firstNonBlankCol :: Modal.EdState -> Int -> Int
firstNonBlankCol ed row =
  T.length (T.takeWhile Char.isSpace (Modal.lineAt (Modal.bufferLines ed) row))

firstNonBlankZ :: Z.TextZipper T.Text -> Z.TextZipper T.Text
firstNonBlankZ z =
  let
    (row, _) = Z.cursorPosition z
    col = T.length (T.takeWhile Char.isSpace (Z.currentLine z))
  in
    Z.moveCursor (row, col) z

-- Walking the buffer one cell at a time, so word motions cross lines the
-- way vi's do.

classAt :: Modal.WordClass -> [T.Text] -> (Int, Int) -> Modal.CharClass
classAt cls ls (r, c) =
  let
    l = Modal.lineAt ls r
  in
    if c < T.length l then Modal.charClass cls (T.index l c) else Modal.ClsSpace

-- w: forward to the start of the next word.
wordStartFrom :: Modal.WordClass -> Int -> Modal.EdState -> (Int, Int)
wordStartFrom cls n ed =
  let
    ls = Modal.bufferLines ed
    step p =
      let
        start = classAt cls ls p
        skipSame q =
          case Modal.nextPos ls q of
            Just q2 | classAt cls ls q2 == start && start /= Modal.ClsSpace -> skipSame q2
            _ -> q
        afterWord = if start == Modal.ClsSpace then p else skipSame p
        skipSpace q =
          case Modal.nextPos ls q of
            Just q2 | classAt cls ls q2 == Modal.ClsSpace -> skipSpace q2
            _ -> q
      in
        case Modal.nextPos ls (skipSpace afterWord) of
          Just q -> q
          Nothing -> endOfBuffer ls
  in
    iterateN n step (Modal.cursorRC ed)

-- b: back to the start of the current or previous word.
wordBackFrom :: Modal.WordClass -> Int -> Modal.EdState -> (Int, Int)
wordBackFrom cls n ed =
  let
    ls = Modal.bufferLines ed
    step p =
      let
        backOne = case Modal.prevPos ls p of
          Just q -> q
          Nothing -> p
        skipSpace q =
          case Modal.prevPos ls q of
            Just q2 | classAt cls ls q == Modal.ClsSpace -> skipSpace q2
            _ -> q
        landed = skipSpace backOne
        cls0 = classAt cls ls landed
        toStart q =
          case Modal.prevPos ls q of
            Just q2 | classAt cls ls q2 == cls0 && cls0 /= Modal.ClsSpace -> toStart q2
            _ -> q
      in
        toStart landed
  in
    iterateN n step (Modal.cursorRC ed)

-- e: forward to the last character of the current or next word.
wordEndFrom :: Modal.WordClass -> Int -> Modal.EdState -> (Int, Int)
wordEndFrom cls n ed =
  let
    ls = Modal.bufferLines ed
    step p =
      let
        fwdOne = case Modal.nextPos ls p of
          Just q -> q
          Nothing -> p
        skipSpace q =
          case Modal.nextPos ls q of
            Just q2 | classAt cls ls q == Modal.ClsSpace -> skipSpace q2
            _ -> q
        landed = skipSpace fwdOne
        cls0 = classAt cls ls landed
        toEnd q =
          case Modal.nextPos ls q of
            Just q2 | classAt cls ls q2 == cls0 && cls0 /= Modal.ClsSpace -> toEnd q2
            _ -> q
      in
        toEnd landed
  in
    iterateN n step (Modal.cursorRC ed)

-- ge: back to the last character of the previous word.
wordEndBackFrom :: Modal.WordClass -> Int -> Modal.EdState -> (Int, Int)
wordEndBackFrom cls n ed =
  let
    ls = Modal.bufferLines ed
    step p =
      let
        backOne = case Modal.prevPos ls p of
          Just q -> q
          Nothing -> p
        skipOwn q =
          case Modal.prevPos ls q of
            Just q2
              | classAt cls ls q == classAt cls ls p && classAt cls ls q /= Modal.ClsSpace ->
                  skipOwn q2
            _ -> q
        afterOwn = skipOwn backOne
        skipSpace q =
          case Modal.prevPos ls q of
            Just q2 | classAt cls ls q == Modal.ClsSpace -> skipSpace q2
            _ -> q
      in
        skipSpace afterOwn
  in
    iterateN n step (Modal.cursorRC ed)

endOfBuffer :: [T.Text] -> (Int, Int)
endOfBuffer ls =
  let
    r = max 0 (length ls - 1)
  in
    (r, max 0 (T.length (Modal.lineAt ls r) - 1))

iterateN :: Int -> (a -> a) -> a -> a
iterateN n f x = if n <= 0 then x else iterateN (n - 1) f (f x)

-- f/F/t/T's landing place, counted.
findTarget :: FindDir -> Int -> Char -> Modal.EdState -> Maybe (Int, Int)
findTarget dir n c ed =
  let
    ls = Modal.bufferLines ed
    (row, col) = Modal.cursorRC ed
    line = Modal.lineAt ls row
    forwards = filter (> col) (charCols c line)
    backwards = reverse (filter (< col) (charCols c line))
    nth xs = case drop (n - 1) xs of
      (x : _) -> Just x
      [] -> Nothing
  in
    case dir of
      FindTo -> fmap (\i -> (row, i)) (nth forwards)
      FindBack -> fmap (\i -> (row, i)) (nth backwards)
      FindTill -> fmap (\i -> (row, max col (i - 1))) (nth forwards)
      FindTillBack -> fmap (\i -> (row, min col (i + 1))) (nth backwards)

-- %: the partner of the first bracket at or after the cursor on this
-- line.
-- Applying a span -----------------------------------------------------------------

-- A bare motion: move, growing the selection when visual mode is on and
-- collapsing it when it is not.
moveOver :: Span -> Modal.EdState -> Modal.EdState
moveOver s ed =
  let
    to = case s of
      Exclusive p -> p
      Inclusive p -> p
      Linewise delta -> (fst (Modal.cursorRC ed) + delta, snd (Modal.cursorRC ed))
      AlreadySpan _ _ hi -> hi
    place = Z.moveCursor to
  in
    if Modal.edExtend ed
      then ed {Modal.edZipper = place (Modal.edZipper ed)}
      else (Modal.collapse ed) {Modal.edZipper = place (Modal.edZipper ed)}

-- Turn the span into exactly the selection Modal's verbs consume, then
-- run the verb. This is the whole trick: vi's grammar, helix's verbs.
spanOp :: ViOp -> Span -> Modal.EdState -> Modal.EdState
spanOp op s ed =
  case selectionFor s ed of
    Nothing -> Modal.collapse ed
    Just selected -> applyToSelection op selected

-- Where the cursor is now is one end of the span; the motion's target is
-- the other. Exclusive motions give back the cell they landed on.
selectionFor :: Span -> Modal.EdState -> Maybe Modal.EdState
selectionFor s ed =
  let
    ls = Modal.bufferLines ed
    from = Modal.cursorRC ed
    charSpan lo hi =
      if hi < lo
        then Nothing
        else
          Just
            ed
              { Modal.edZipper = Z.moveCursor hi (Modal.edZipper ed)
              , Modal.edSelAnchor = Just lo
              , Modal.edSelKind = Modal.CharSel
              , Modal.edExtend = False
              }
    lineSpan loRow hiRow =
      Just
        ed
          { Modal.edZipper = Z.moveCursor (hiRow, 0) (Modal.edZipper ed)
          , Modal.edSelAnchor = Just (loRow, 0)
          , Modal.edSelKind = Modal.LineSel
          , Modal.edExtend = False
          }
    -- an exclusive motion covers [lo, hi): the cell it landed on is not
    -- part of the span, so the inclusive selection stops one short
    shrink p =
      case Modal.prevPos ls p of
        Just prev -> prev
        Nothing -> p
  in
    case s of
      Linewise delta ->
        let
          row = fst from
          other = max 0 (min (lastRowOf ed) (row + delta))
        in
          lineSpan (min row other) (max row other)
      Inclusive to -> charSpan (min from to) (max from to)
      AlreadySpan Modal.CharSel lo hi -> charSpan lo hi
      AlreadySpan Modal.LineSel lo hi -> lineSpan (fst lo) (fst hi)
      Exclusive to ->
        let
          lo = min from to
          hi = max from to
        in
          if lo == hi then Nothing else charSpan lo (shrink hi)

applyToSelection :: ViOp -> Modal.EdState -> Modal.EdState
applyToSelection op ed =
  case op of
    -- vi's delete and change fill the unnamed register on their way out,
    -- which is what makes `dd`-then-`p` move a line. helix's d/c do not,
    -- so the yank has to happen here rather than in the shared verb —
    -- quietly, since removing a line is not a request to put it on the
    -- system clipboard.
    OpDelete -> Modal.deleteSel (Modal.yankInto ed)
    OpChange -> Modal.changeSel (Modal.yankInto ed)
    -- vi leaves the cursor at the start of a yank and drops the
    -- selection; helix's yankSel keeps it highlighted
    OpYank ->
      let
        anchor = maybe (Modal.cursorRC ed) id (Modal.edSelAnchor ed)
        home = min anchor (Modal.cursorRC ed)
        yanked = Modal.yankSel ed
      in
        (Modal.collapse yanked) {Modal.edZipper = Z.moveCursor home (Modal.edZipper yanked)}
    OpIndent -> Modal.collapse (Modal.indentLines 1 ed)
    OpUnindent -> Modal.collapse (Modal.indentLines (-1) ed)
    OpLower -> Modal.collapse (Modal.mapSelChars Char.toLower ed)
    OpUpper -> Modal.collapse (Modal.mapSelChars Char.toUpper ed)
    OpSwapCase -> Modal.collapse (Modal.toggleCase ed)

-- Visual, repeat, and the small edits ---------------------------------------------

enterVisual :: Modal.SelKind -> Modal.EdState -> Modal.EdState
enterVisual kind ed =
  if Modal.edExtend ed && Modal.edSelKind ed == kind
    then Modal.collapse ed
    else
      ed
        { Modal.edExtend = True
        , Modal.edSelKind = kind
        , Modal.edSelAnchor = Just (maybe (Modal.cursorRC ed) id (Modal.edSelAnchor ed))
        }

-- J with a count joins that many lines; bare J joins two, as vi does.
joinBy :: Int -> Modal.EdState -> Modal.EdState
joinBy count ed =
  let
    times = max 1 (count - 1)
  in
    iterateN times Modal.joinLines (Modal.collapse ed)

repeatEdit :: Int -> (Modal.EdState -> Modal.EdState) -> Modal.EdState -> Modal.EdState
repeatEdit count f ed = iterateN (max 1 count) f ed

-- `.` replays the last command that changed something. When that command
-- opened an insert session, the text typed into it comes back too — the
-- recording Modal already keeps for its own `.`.
replayDot :: ViKeys -> Modal.EdState -> ViResult
replayDot keys ed =
  case viDot keys of
    [] -> ViDone viIdle {viFind = viFind keys, viDot = viDot keys} ed
    evs ->
      let
        replayOne (k, acc) ev =
          case handleVi k ev acc of
            ViPending kNext -> (kNext, acc)
            ViDone kNext accNext -> (kNext, accNext)
            ViUnhandled -> (k, acc)
        start = viIdle {viFind = viFind keys}
        (_, afterCmd) = foldl replayOne (start, ed) evs
        typed = case (Modal.edMode afterCmd, Modal.edLastInsert ed) of
          (Modal.Insert, Just (_, typedEvs)) -> foldl replayInsert afterCmd typedEvs
          _ -> afterCmd
        settled =
          if Modal.edMode typed == Modal.Insert
            then typed {Modal.edMode = Modal.Normal, Modal.edInsertEntry = Nothing}
            else typed
      in
        ViDone
          viIdle {viFind = viFind keys, viDot = viDot keys}
          settled {Modal.edLastInsert = Modal.edLastInsert ed}

replayInsert :: Modal.EdState -> V.Event -> Modal.EdState
replayInsert acc ev =
  case Modal.handleInsert ev acc of
    Modal.InsertHandled next -> next
    Modal.InsertExit next -> next
    Modal.InsertUnhandled -> acc
