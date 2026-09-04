{-# LANGUAGE OverloadedStrings #-}

-- A helix-flavored modal layer over text-zipper, selection-first: word
-- motions select the span they cross (so `wc` is "change word"), `v`
-- makes every motion extend, `x` selects lines, and d/c act on whatever
-- is selected. Insert is entered by i/a/I/A/o/O/c; undo is a bounded
-- stack of zipper snapshots, one per insert session or structural edit.
--
-- It is also the shared editing engine underneath the other keymap
-- families: `handleNormal` is helix's own grammar, but the verbs below it
-- (deleteSel, changeSel, yankSel, indentLines, …) all act on whatever
-- edSelAnchor/edSelKind say, so Remax.Vi drives the same verbs by
-- building the selection operator-first and Remax.Emacs by building it
-- from the mark.
module Remax.Modal
  ( Mode (..)
  , SelKind (..)
  , EdState (..)
  , InsertEntry (..)
  , InsertResult (..)
  , newEdState
  , bufferLines
  , bufferLineCount
  , bufferUpward
  , cursorRC
  , lineAt
  , WordClass (..)
  , CharClass (..)
  , charClass
  , matchEnclosing
  , matchingBracket
  , nextPos
  , prevPos
  , plainMove
  , greedyWrap
  , selectionRows
  , charSelection
  , collapse
  , selectLine
  , hasPending
  , pendingLabel
  , handleNormal
  , handleInsert
  , deleteSel
  , changeSel
  , yankSel
  , yankInto
  , pasteSel
  , indentLines
  , joinLines
  , toggleCase
  , mapSelChars
  , replaceSel
  , openLine
  , enterInsert
  , deleteCharRange
  , replaceRows
  , undo
  , redo
  ) where

import Data.Char (isAlphaNum, isLower, isSpace, isUpper, toLower, toUpper)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as V
import qualified Remax.Zipper as Z

data Mode = Normal | Insert
  deriving (Eq, Show)

data SelKind = CharSel | LineSel
  deriving (Eq, Show)

-- A prefix key waiting for its second half: g-goto, or a find/till
-- waiting for its target character.
data Pending
  = NoPending
  | PendingG
  | PendingFind FindKind
  | PendingReplace
  | -- helix's match menu: m, then what to do with the pair
    PendingMatch
  | PendingSurround SurroundAct
  | -- mr has taken the pair to look for and wants the one to put back
    PendingSurroundTo Char
  | PendingObject ObjSide
  deriving (Eq, Show)

-- What the m menu is about to do to a surrounding pair.
data SurroundAct = SurroundAdd | SurroundDelete | SurroundReplace
  deriving (Eq, Show)

-- Whether a text object takes the delimiters with it.
data ObjSide = ObjInside | ObjAround
  deriving (Eq, Show)

data FindKind = FindFwd | FindBack | TillFwd | TillBack
  deriving (Eq, Show)

-- How the last insert session was entered, so '.' can re-enter the same
-- way before replaying the recorded keys.
data InsertEntry
  = EntryI
  | EntryAppend
  | EntryBOL
  | EntryEOL
  | EntryBelow
  | EntryAbove
  | EntryChange
  deriving (Eq, Show)

data EdState = EdState
  { edZipper :: Z.TextZipper Text
  , edMode :: Mode
  , edSelAnchor :: Maybe (Int, Int) -- selection anchor; head is the cursor
  , edSelKind :: SelKind
  , edExtend :: Bool -- v: motions extend instead of replacing
  , edPending :: Pending
  , edKill :: Text -- the kill slot (emacs C-k/C-w/M-w; future helix yank)
  , edCopied :: Bool -- the slot was just filled by a deliberate copy
  , edUndo :: [Z.TextZipper Text]
  , edRedo :: [Z.TextZipper Text] -- what undo stepped back over; edits clear it
  , edTouched :: Bool -- something was actually changed at least once
  , edInsertEntry :: Maybe InsertEntry -- how the open insert session began
  , edRecording :: [V.Event] -- keys of the open insert session, reversed
  , edLastInsert :: Maybe (InsertEntry, [V.Event]) -- what '.' replays
  }

hasPending :: EdState -> Bool
hasPending st = edPending st /= NoPending

pendingLabel :: EdState -> Maybe Text
pendingLabel st = case edPending st of
  NoPending -> Nothing
  PendingG -> Just "g"
  PendingMatch -> Just "m"
  PendingSurround SurroundAdd -> Just "ms"
  PendingSurround SurroundDelete -> Just "md"
  PendingSurround SurroundReplace -> Just "mr"
  PendingSurroundTo _ -> Just "mr"
  PendingObject ObjInside -> Just "mi"
  PendingObject ObjAround -> Just "ma"
  PendingFind FindFwd -> Just "f"
  PendingFind FindBack -> Just "F"
  PendingFind TillFwd -> Just "t"
  PendingFind TillBack -> Just "T"
  PendingReplace -> Just "r"

newEdState :: Int -> [Text] -> EdState
newEdState row ls =
  EdState
    { edZipper =
        Z.moveCursor (max 0 (min row (length ls - 1)), 0) $
          Z.textZipper (if null ls then [""] else ls) Nothing
    , edMode = Normal
    , edSelAnchor = Nothing
    , edSelKind = CharSel
    , edExtend = False
    , edPending = NoPending
    , edKill = T.empty
    , edCopied = False
    , edUndo = []
    , edRedo = []
    , edTouched = False
    , edInsertEntry = Nothing
    , edRecording = []
    , edLastInsert = Nothing
    }

bufferLines :: EdState -> [Text]
bufferLines = Z.getText . edZipper

-- length (bufferLines st), without rebuilding the list to count it.
bufferLineCount :: EdState -> Int
bufferLineCount = Z.lineCount . edZipper

-- The cursor's line and everything above it, nearest first: what a
-- search upward from the cursor wants, without building the buffer.
bufferUpward :: EdState -> [Text]
bufferUpward = Z.linesUpward . edZipper

cursorRC :: EdState -> (Int, Int)
cursorRC = Z.cursorPosition . edZipper

-- The rows covered by a line-wise selection.
selectionRows :: EdState -> Maybe (Int, Int)
selectionRows st =
  case (edSelKind st, edSelAnchor st) of
    (LineSel, Just (ar, _)) ->
      let
        r = fst (cursorRC st)
      in
        Just (min ar r, max ar r)
    _ -> Nothing

-- An inclusive character span, ordered. A char-wise selection always
-- covers at least the cursor's own cell, the way helix draws it.
charSelection :: EdState -> Maybe ((Int, Int), (Int, Int))
charSelection st =
  case (edSelKind st, edSelAnchor st) of
    (CharSel, Just a) ->
      let
        c = cursorRC st
      in
        Just (min a c, max a c)
    _ -> Nothing

undoDepth :: Int
undoDepth = 100

snapshot :: EdState -> [Z.TextZipper Text]
snapshot st = take undoDepth (edZipper st : edUndo st)

withUndo :: (Z.TextZipper Text -> Z.TextZipper Text) -> EdState -> EdState
withUndo f st =
  st
    { edZipper = f (edZipper st)
    , edUndo = snapshot st
    , edRedo = []
    , edTouched = True
    }

-- Nothing means the key is not an editing key and the caller may use it
-- for review-level commands.
handleNormal :: V.Event -> EdState -> Maybe EdState
handleNormal ev st =
  case edPending st of
    PendingG -> Just (handleG ev st {edPending = NoPending})
    PendingFind kind -> Just (handleFind kind ev st {edPending = NoPending})
    PendingReplace -> Just (handleReplace ev st {edPending = NoPending})
    PendingMatch -> Just (handleMatch ev st {edPending = NoPending})
    PendingSurround act ->
      Just (handleSurround act ev st {edPending = NoPending})
    PendingSurroundTo from ->
      Just (handleSurroundTo from ev st {edPending = NoPending})
    PendingObject side ->
      Just (handleObject side ev st {edPending = NoPending})
    NoPending -> handlePlain ev st

handlePlain :: V.Event -> EdState -> Maybe EdState
handlePlain ev st =
  let
    move f = Just (plainMove f st)
  in
    case ev of
      V.EvKey (V.KChar 'r') [] -> Just st {edPending = PendingReplace}
      V.EvKey (V.KChar 'g') [] -> Just st {edPending = PendingG}
      V.EvKey (V.KChar 'm') [] -> Just st {edPending = PendingMatch}
      V.EvKey (V.KChar 'f') [] -> Just st {edPending = PendingFind FindFwd}
      V.EvKey (V.KChar 'F') [] -> Just st {edPending = PendingFind FindBack}
      V.EvKey (V.KChar 't') [] -> Just st {edPending = PendingFind TillFwd}
      V.EvKey (V.KChar 'T') [] -> Just st {edPending = PendingFind TillBack}
      V.EvKey V.KHome [] -> move Z.gotoBOL
      V.EvKey V.KEnd [] -> move Z.gotoEOL
      V.EvKey (V.KChar 'h') [] -> move Z.moveLeft
      V.EvKey V.KLeft [] -> move Z.moveLeft
      V.EvKey (V.KChar 'l') [] -> move Z.moveRight
      V.EvKey V.KRight [] -> move Z.moveRight
      V.EvKey (V.KChar 'j') [] -> move Z.moveDown
      V.EvKey V.KDown [] -> move Z.moveDown
      V.EvKey (V.KChar 'k') [] -> move Z.moveUp
      V.EvKey V.KUp [] -> move Z.moveUp
      V.EvKey (V.KChar 'w') [] -> Just (selectMove wordForwardSel st)
      V.EvKey (V.KChar 'e') [] -> Just (selectMove wordEndSel st)
      V.EvKey (V.KChar 'b') [] -> Just (selectMove wordBackSel st)
      V.EvKey (V.KChar 'x') [] -> Just (selectLine st)
      V.EvKey (V.KChar '%') [] -> Just (selectWholeFile st)
      V.EvKey (V.KChar 'v') [] -> Just (toggleExtend st)
      V.EvKey (V.KChar ';') [] -> Just (collapse st)
      V.EvKey (V.KChar 'd') [] -> Just (deleteSel st)
      V.EvKey (V.KChar 'c') [] -> Just (changeSel st)
      V.EvKey (V.KChar 'i') [] -> Just (enterInsert EntryI id st)
      V.EvKey (V.KChar 'a') [] -> Just (enterInsert EntryAppend Z.moveRight st)
      V.EvKey (V.KChar 'I') [] -> Just (enterInsert EntryBOL Z.gotoBOL st)
      V.EvKey (V.KChar 'A') [] -> Just (enterInsert EntryEOL Z.gotoEOL st)
      V.EvKey (V.KChar 'o') [] -> Just (openLine 1 st)
      V.EvKey (V.KChar 'O') [] -> Just (openLine 0 st)
      V.EvKey (V.KChar '~') [] -> Just (toggleCase st)
      V.EvKey (V.KChar '>') [] -> Just (indentLines 1 st)
      V.EvKey (V.KChar '<') [] -> Just (indentLines (-1) st)
      V.EvKey (V.KChar 'J') [] -> Just (joinLines st)
      V.EvKey (V.KChar 'Q') [] -> Just (reflow st)
      V.EvKey (V.KChar 'y') [] -> Just (yankSel st)
      V.EvKey (V.KChar 'p') [] -> Just (pasteSel 1 st)
      V.EvKey (V.KChar 'P') [] -> Just (pasteSel 0 st)
      V.EvKey (V.KChar '.') [] -> Just (repeatInsert st)
      V.EvKey (V.KChar 'u') [] -> Just (undo st)
      V.EvKey (V.KChar 'U') [] -> Just (redo st)
      V.EvKey V.KEsc [] -> Just (collapse st)
      _ -> Nothing

-- y: the selection (or the cursor cell) into the kill slot. Line-wise
-- yanks keep a trailing newline, which is what makes p paste them as
-- whole lines. A deliberate copy, so it is also what reaches the system
-- clipboard.
yankSel :: EdState -> EdState
yankSel st = (yankInto st) {edCopied = True}

-- The same, without claiming a copy was meant. vi's d and c take a copy
-- of what they remove so that p can put it back, but pulling a line out
-- of a file is not "I want this on the system clipboard" — it must not
-- clobber whatever you copied from somewhere else.
yankInto :: EdState -> EdState
yankInto st =
  let
    ls = bufferLines st
    rowText i = case drop i ls of
      (l : _) -> l
      [] -> T.empty
  in
    case selectionRows st of
      Just (lo, hi) ->
        st {edKill = T.unlines (map rowText [lo .. hi])}
      Nothing ->
        case charSelection st of
          Just ((lr, lc), (hr, hc)) ->
            let
              extracted =
                if lr == hr
                  then T.take (hc + 1 - lc) (T.drop lc (rowText lr))
                  else
                    T.intercalate
                      "\n"
                      ( T.drop lc (rowText lr)
                          : map rowText [lr + 1 .. hr - 1]
                          ++ [T.take (hc + 1) (rowText hr)]
                      )
            in
              st {edKill = extracted}
          Nothing ->
            let
              (r, c) = cursorRC st
            in
              st {edKill = T.take 1 (T.drop c (rowText r))}

-- p / P: paste the kill slot after / before. A line-wise kill (trailing
-- newline) pastes as whole lines below / above the cursor's line and
-- selects them; a char-wise kill goes after / at the cursor cell and is
-- selected, helix-style.
pasteSel :: Int -> EdState -> EdState
pasteSel offset st =
  let
    k = edKill st
  in
    if T.null k
      then st
      else
        if "\n" `T.isSuffixOf` k
          then
            let
              ls = bufferLines st
              (row, _) = cursorRC st
              at = row + offset
              newRows = T.lines k
              ls2 = take at ls ++ newRows ++ drop at ls
            in
              st
                { edZipper =
                    Z.moveCursor
                      (at + length newRows - 1, 0)
                      (Z.textZipper ls2 Nothing)
                , edUndo = snapshot st
                , edRedo = []
                , edTouched = True
                , edSelAnchor = Just (at, 0)
                , edSelKind = LineSel
                , edExtend = False
                }
          else
            let
              z0 =
                if offset == 1
                  then Z.moveRight (edZipper st)
                  else edZipper st
              start = Z.cursorPosition z0
              z1 = Z.moveLeft (Z.insertMany k z0)
            in
              st
                { edZipper = z1
                , edUndo = snapshot st
                , edRedo = []
                , edTouched = True
                , edSelAnchor = Just start
                , edSelKind = CharSel
                , edExtend = False
                }

-- The g-goto menu: line start/end, first non-blank, file start/end.
-- Anything else cancels.
handleG :: V.Event -> EdState -> EdState
handleG ev st =
  let
    firstNonBlank z =
      let
        col = T.length (T.takeWhile isSpace (Z.currentLine z))
        row = fst (Z.cursorPosition z)
      in
        Z.moveCursor (row, col) z
    lastLineEnd z =
      Z.gotoEOL (Z.moveCursor (max 0 (length (Z.getText z) - 1), 0) z)
  in
    case ev of
      V.EvKey (V.KChar 'h') [] -> plainMove Z.gotoBOL st
      V.EvKey (V.KChar 'l') [] -> plainMove Z.gotoEOL st
      V.EvKey (V.KChar 's') [] -> plainMove firstNonBlank st
      V.EvKey (V.KChar 'g') [] -> plainMove (Z.moveCursor (0, 0)) st
      V.EvKey (V.KChar 'e') [] -> plainMove lastLineEnd st
      _ -> st

-- r with its replacement character: every character in the selection
-- (or the one under the cursor) becomes that character, newlines
-- untouched, selection kept — helix's replace.
handleReplace :: V.Event -> EdState -> EdState
handleReplace ev st =
  case ev of
    V.EvKey (V.KChar c) [] -> replaceSel c st
    _ -> st

replaceSel :: Char -> EdState -> EdState
replaceSel c st =
  let
    ls = bufferLines st
    (cr, cc) = cursorRC st
    replaceSpan s e l =
      let
        (pre, rest) = T.splitAt s l
        (mid, post) = T.splitAt (e + 1 - s) rest
      in
        pre <> T.map (const c) mid <> post
    spanFor r l = case (edSelKind st, edSelAnchor st) of
      (LineSel, Just (ar, _)) ->
        let
          lo = min ar cr
          hi = max ar cr
        in
          if r >= lo && r <= hi && not (T.null l)
            then Just (0, T.length l - 1)
            else Nothing
      (CharSel, Just a) ->
        let
          (loPos, hiPos) = (min a (cr, cc), max a (cr, cc))
          (lr, lc) = loPos
          (hr, hc) = hiPos
        in
          if r < lr || r > hr || T.null l
            then Nothing
            else
              Just
                ( if r == lr then lc else 0
                , if r == hr then min hc (T.length l - 1) else T.length l - 1
                )
      _ ->
        if r == cr && cc < T.length l
          then Just (cc, cc)
          else Nothing
    replaceLine (r, l) = case spanFor r l of
      Just (s, e) -> replaceSpan s e l
      Nothing -> l
    newLs = map replaceLine (zip [0 ..] ls)
  in
    st
      { edZipper = Z.moveCursor (cr, cc) (Z.textZipper newLs Nothing)
      , edUndo = snapshot st
      , edRedo = []
      , edTouched = True
      }

-- f/F/t/T with their target character: the crossed span becomes the
-- selection, like the word motions. Search runs across line boundaries.
handleFind :: FindKind -> V.Event -> EdState -> EdState
handleFind kind ev st =
  case ev of
    V.EvKey (V.KChar c) [] -> applyFind kind c st
    _ -> st

applyFind :: FindKind -> Char -> EdState -> EdState
applyFind kind c st =
  let
    ls = bufferLines st
    pos = cursorRC st
    found = case kind of
      FindFwd -> findForward ls pos c
      TillFwd -> fmap (stepBack ls) (findForward ls pos c)
      FindBack -> findBackward ls pos c
      TillBack -> fmap (stepFwd ls) (findBackward ls pos c)
  in
    case found of
      Nothing -> st
      Just (r, col) ->
        let
          anchor =
            if edExtend st
              then maybe pos id (edSelAnchor st)
              else pos
        in
          st
            { edZipper = Z.moveCursor (r, col) (edZipper st)
            , edSelAnchor = Just anchor
            , edSelKind = CharSel
            }

-- Word vocabularies ------------------------------------------------------------

data WordClass = SmallWord | BigWord
  deriving (Eq, Show)

data CharClass = ClsSpace | ClsWord | ClsPunct
  deriving (Eq, Show)

-- vi's two vocabularies: `w` steps between runs of identifier characters
-- and runs of punctuation, `W` only between runs of non-blank.
charClass :: WordClass -> Char -> CharClass
charClass cls c
  | isSpace c = ClsSpace
  | otherwise =
      case cls of
        BigWord -> ClsWord
        SmallWord -> if isAlphaNum c || c == '_' then ClsWord else ClsPunct

-- Matching pairs ---------------------------------------------------------------

-- Shared by both keymaps: vi reaches these through % and its text
-- objects, helix through the m menu.

nextPos :: [Text] -> (Int, Int) -> Maybe (Int, Int)
nextPos ls (r, c) =
  if c + 1 < T.length (lineAt ls r)
    then Just (r, c + 1)
    else if r + 1 < length ls then Just (r + 1, 0) else Nothing

prevPos :: [Text] -> (Int, Int) -> Maybe (Int, Int)
prevPos ls (r, c) =
  if c > 0
    then Just (r, c - 1)
    else if r > 0 then Just (r - 1, max 0 (T.length (lineAt ls (r - 1)) - 1)) else Nothing
matchEnclosing ::
  Char -> Char -> [Text] -> (Int, Int) -> Maybe ((Int, Int), (Int, Int))
matchEnclosing open close ls pos =
  let
    charOf (r, c) =
      let
        l = lineAt ls r
      in
        if c >= 0 && c < T.length l then Just (T.index l c) else Nothing
    -- walking back from the cursor, a closer met on the way means one
    -- more opener to skip; the cursor's own cell does not count
    openDelta p ch
      | ch == open = Left ()
      | ch == close && p /= pos = Right 1
      | otherwise = Right 0
    findOpen budget depth p =
      if budget <= (0 :: Int)
        then Nothing
        else
          let
            onward d =
              case prevPos ls p of
                Just prev -> findOpen (budget - 1) d prev
                Nothing -> Nothing
          in
            case fmap (openDelta p) (charOf p) of
              Just (Left ()) -> if depth == 0 then Just p else onward (depth - 1)
              Just (Right d) -> onward (depth + d)
              Nothing -> onward depth
    findClose budget depth p =
      if budget <= (0 :: Int)
        then Nothing
        else
          let
            onward d =
              case nextPos ls p of
                Just next -> findClose (budget - 1) d next
                Nothing -> Nothing
          in
            case charOf p of
              Just ch
                | ch == close && depth == 1 -> Just p
                | ch == close -> onward (depth - 1)
                | ch == open -> onward (depth + 1)
              _ -> onward depth
  in
    do
      o <- findOpen scanBudget (0 :: Int) pos
      c <- findClose scanBudget (0 :: Int) o
      pure (o, c)

scanBudget :: Int
scanBudget = 200000

matchingBracket :: EdState -> Maybe (Int, Int)
matchingBracket ed =
  let
    ls = bufferLines ed
    (row, col) = cursorRC ed
    line = lineAt ls row
    brackets = [('(', ')'), ('[', ']'), ('{', '}')]
    atCol i =
      let
        ch = T.index line i
        hit (o, cl) = o == ch || cl == ch
      in
        case filter hit brackets of
          ((o, cl) : _) -> Just (o, cl, i)
          [] -> Nothing
  in
    case mapMaybe atCol [col .. T.length line - 1] of
      [] -> Nothing
      ((o, cl, i) : _) ->
        case matchEnclosing o cl ls (row, i) of
          Just (openPos, closePos) ->
            if openPos == (row, i) then Just closePos else Just openPos
          Nothing -> Nothing

lineAt :: [Text] -> Int -> Text
lineAt ls r =
  if r >= 0 && r < length ls then ls !! r else ""

findForward :: [Text] -> (Int, Int) -> Char -> Maybe (Int, Int)
findForward ls (cr, cc) c =
  case T.findIndex (== c) (T.drop (cc + 1) (lineAt ls cr)) of
    Just i -> Just (cr, cc + 1 + i)
    Nothing ->
      let
        hitIn r = fmap (\i -> (r, i)) (T.findIndex (== c) (lineAt ls r))
      in
        case mapMaybe hitIn [cr + 1 .. length ls - 1] of
          (hit : _) -> Just hit
          [] -> Nothing

findBackward :: [Text] -> (Int, Int) -> Char -> Maybe (Int, Int)
findBackward ls (cr, cc) c =
  case lastIndexOf c (T.take cc (lineAt ls cr)) of
    Just i -> Just (cr, i)
    Nothing ->
      let
        hitIn r = fmap (\i -> (r, i)) (lastIndexOf c (lineAt ls r))
      in
        case mapMaybe hitIn [cr - 1, cr - 2 .. 0] of
          (hit : _) -> Just hit
          [] -> Nothing

lastIndexOf :: Char -> Text -> Maybe Int
lastIndexOf c t =
  fmap (\i -> T.length t - 1 - i) (T.findIndex (== c) (T.reverse t))

-- One position earlier / later in document order, for till motions.
stepBack :: [Text] -> (Int, Int) -> (Int, Int)
stepBack ls (r, c)
  | c > 0 = (r, c - 1)
  | r > 0 = (r - 1, max 0 (T.length (lineAt ls (r - 1)) - 1))
  | otherwise = (r, c)

stepFwd :: [Text] -> (Int, Int) -> (Int, Int)
stepFwd ls (r, c)
  | c + 1 < T.length (lineAt ls r) = (r, c + 1)
  | r + 1 < length ls = (r + 1, 0)
  | otherwise = (r, c)

collapse :: EdState -> EdState
collapse st =
  st {edSelAnchor = Nothing, edExtend = False, edSelKind = CharSel}

-- hjkl and arrows: collapse and move, unless v-extend is on.
plainMove :: (Z.TextZipper Text -> Z.TextZipper Text) -> EdState -> EdState
plainMove f st
  | edExtend st = st {edZipper = f (edZipper st)}
  | otherwise = (collapse st) {edZipper = f (edZipper st)}

-- Word motions: the traversed span becomes the selection (anchor at the
-- old cursor), matching helix's selection-first model.
selectMove :: (Z.TextZipper Text -> Z.TextZipper Text) -> EdState -> EdState
selectMove f st =
  let
    oldPos = cursorRC st
    anchor =
      if edExtend st
        then maybe oldPos id (edSelAnchor st)
        else oldPos
  in
    st
      { edZipper = f (edZipper st)
      , edSelAnchor = Just anchor
      , edSelKind = CharSel
      }

toggleExtend :: EdState -> EdState
toggleExtend st
  | edExtend st = st {edExtend = False}
  | otherwise =
      st
        { edExtend = True
        , edSelAnchor =
            case edSelAnchor st of
              Just a -> Just a
              Nothing -> Just (cursorRC st)
        }

-- The whole buffer as a line-wise selection, helix's %.
selectWholeFile :: EdState -> EdState
selectWholeFile st =
  let
    lastRow = max 0 (length (bufferLines st) - 1)
  in
    st
      { edZipper = Z.moveCursor (lastRow, 0) (edZipper st)
      , edSelAnchor = Just (0, 0)
      , edSelKind = LineSel
      }

-- First x selects the current line; each further x extends downward.
selectLine :: EdState -> EdState
selectLine st =
  case (edSelKind st, edSelAnchor st) of
    (LineSel, Just _) -> st {edZipper = Z.moveDown (edZipper st)}
    _ ->
      st
        { edSelAnchor = Just (fst (cursorRC st), 0)
        , edSelKind = LineSel
        }

-- Open an insert session: mode, undo snapshot, and a fresh recording so
-- '.' can replay it later.
beginInsert :: InsertEntry -> EdState -> EdState
beginInsert entry st =
  st
    { edMode = Insert
    , edInsertEntry = Just entry
    , edRecording = []
    , edUndo = snapshot st
    , edRedo = []
    , edTouched = True
    }

enterInsert ::
  InsertEntry -> (Z.TextZipper Text -> Z.TextZipper Text) -> EdState -> EdState
enterInsert entry place st =
  beginInsert entry (collapse st) {edZipper = place (edZipper st)}

-- Replay the last insert session where the cursor stands now: re-enter
-- the same way, re-type the same keys, land back in normal mode.
repeatInsert :: EdState -> EdState
repeatInsert st =
  case edLastInsert st of
    Nothing -> st
    Just (entry, evs) ->
      let
        entered = case entry of
          EntryI -> enterInsert EntryI id st
          EntryAppend -> enterInsert EntryAppend Z.moveRight st
          EntryBOL -> enterInsert EntryBOL Z.gotoBOL st
          EntryEOL -> enterInsert EntryEOL Z.gotoEOL st
          EntryBelow -> openLine 1 st
          EntryAbove -> openLine 0 st
          EntryChange -> changeSel st
        replayOne acc ev = case handleInsert ev acc of
          InsertHandled next -> next
          InsertExit next -> next
          InsertUnhandled -> acc
        typed = foldl replayOne entered evs
      in
        typed
          { edMode = Normal
          , edInsertEntry = Nothing
          , edLastInsert = edLastInsert st
          }

deleteSel :: EdState -> EdState
deleteSel st =
  case selectionRows st of
    Just (lo, hi) -> replaceRows lo hi [] st
    Nothing ->
      case charSpanToAct st of
        Just (lo, hi) -> deleteCharRange lo hi st
        Nothing -> withUndo Z.deleteChar st

changeSel :: EdState -> EdState
changeSel st =
  case selectionRows st of
    Just (lo, hi) -> beginInsert EntryChange (replaceRows lo hi [""] st)
    Nothing ->
      case charSpanToAct st of
        Just (lo, hi) -> beginInsert EntryChange (deleteCharRange lo hi st)
        Nothing -> beginInsert EntryChange (withUndo Z.deleteChar st)

-- The span d/c should act on: a char selection wider than the bare
-- cursor. A collapsed selection acts like plain d/c on one character.
charSpanToAct :: EdState -> Maybe ((Int, Int), (Int, Int))
charSpanToAct st =
  case charSelection st of
    Just (lo, hi) | lo /= hi -> Just (lo, hi)
    _ -> Nothing

-- Remove the inclusive character span, joining lines when it crosses
-- newlines; the cursor lands where the span began.
deleteCharRange :: (Int, Int) -> (Int, Int) -> EdState -> EdState
deleteCharRange (lr, lc) (hr, hc) st =
  let
    ls = bufferLines st
    joined = T.take lc (lineAt ls lr) <> T.drop (hc + 1) (lineAt ls hr)
    kept0 = take lr ls ++ [joined] ++ drop (hr + 1) ls
    kept = if null kept0 then [""] else kept0
    row = min lr (length kept - 1)
    col = min lc (T.length (lineAt kept row))
  in
    (collapse st)
      { edZipper = Z.moveCursor (row, col) (Z.textZipper kept Nothing)
      , edUndo = snapshot st
      , edRedo = []
      , edTouched = True
      }

-- Swap rows [a..b] for the given replacement, landing the cursor at the
-- start of the replaced region.
replaceRows :: Int -> Int -> [Text] -> EdState -> EdState
replaceRows a b repl st =
  let
    ls = bufferLines st
    kept0 = take a ls ++ repl ++ drop (b + 1) ls
    kept = if null kept0 then [""] else kept0
    row = min a (length kept - 1)
  in
    (collapse st)
      { edZipper = Z.moveCursor (row, 0) (Z.textZipper kept Nothing)
      , edUndo = snapshot st
      , edRedo = []
      , edTouched = True
      }

openLine :: Int -> EdState -> EdState
openLine offset st =
  let
    ls = bufferLines st
    row = fst (cursorRC st) + offset
    opened = take row ls ++ [""] ++ drop row ls
    entry = if offset == 1 then EntryBelow else EntryAbove
  in
    beginInsert
      entry
      (collapse st) {edZipper = Z.moveCursor (row, 0) (Z.textZipper opened Nothing)}

-- The rows a selection covers, else just the cursor's row.
selRowSpan :: EdState -> (Int, Int)
selRowSpan st =
  let
    cr = fst (cursorRC st)
  in
    case edSelAnchor st of
      Just (ar, _) -> (min ar cr, max ar cr)
      Nothing -> (cr, cr)

-- Join the selected lines into one (or the current line with the next),
-- one space at each seam, leading whitespace of continuation lines
-- dropped — and when the first line is a comment, the joined lines shed
-- their own comment leaders instead of dragging them mid-sentence. The
-- cursor lands on the first seam.
joinLines :: EdState -> EdState
joinLines st =
  let
    ls = bufferLines st
    (lo, hi0) = selRowSpan st
    hi = if hi0 > lo then hi0 else min (lo + 1) (length ls - 1)
    mLeader = lineLeader (lineAt ls lo)
    stripNext nxt =
      let
        b = T.stripStart nxt
      in
        case mLeader of
          Just leader
            | leader `T.isPrefixOf` b ->
                T.stripStart (T.drop (T.length leader) b)
          _ -> b
    joinTwo acc nxt =
      let
        a = T.stripEnd acc
        b = stripNext nxt
      in
        if T.null b then a else if T.null a then b else a <> " " <> b
    joined = foldl joinTwo (lineAt ls lo) (map (lineAt ls) [lo + 1 .. hi])
    kept = take lo ls ++ [joined] ++ drop (hi + 1) ls
    col = min (T.length (T.stripEnd (lineAt ls lo))) (T.length joined)
  in
    if hi <= lo
      then st
      else
        (collapse st)
          { edZipper = Z.moveCursor (lo, col) (Z.textZipper kept Nothing)
          , edUndo = snapshot st
          , edRedo = []
          , edTouched = True
          }

indentWidth :: Int
indentWidth = 2

-- Indent or unindent the selected lines (or the cursor's line): > adds
-- the indent width, < removes up to that much leading space. Blank lines
-- are left alone, and the selection survives so it can be repeated.
indentLines :: Int -> EdState -> EdState
indentLines dir st =
  let
    ls = bufferLines st
    (lo, hi) = selRowSpan st
    pad = T.replicate indentWidth " "
    adjust l
      | T.null (T.strip l) = l
      | dir > 0 = pad <> l
      | otherwise =
          T.drop (min indentWidth (T.length (T.takeWhile (== ' ') l))) l
    keepOrAdjust (r, l) = if r >= lo && r <= hi then adjust l else l
    kept = map keepOrAdjust (zip [0 ..] ls)
    (cr, cc) = cursorRC st
    newCol = min cc (T.length (lineAt kept cr))
  in
    st
      { edZipper = Z.moveCursor (cr, newCol) (Z.textZipper kept Nothing)
      , edUndo = snapshot st
      , edRedo = []
      , edTouched = True
      }

reflowWidth :: Int
reflowWidth = 80

commentLeaders :: [Text]
commentLeaders = ["-- |", "///", "--", "//", ";;", "#", ";"]

lineLeader :: Text -> Maybe Text
lineLeader l =
  find
    (`T.isPrefixOf` T.dropWhile (\c -> c == ' ' || c == '\t') l)
    commentLeaders

-- The indentation-plus-comment-leader prefix a reflowed line should keep.
linePrefix :: Text -> Text
linePrefix l =
  let
    ws = T.takeWhile (\c -> c == ' ' || c == '\t') l
  in
    case lineLeader l of
      Just leader -> ws <> leader <> " "
      Nothing -> ws

-- The line's text with indentation and any comment leader stripped.
lineContent :: Text -> Text
lineContent l =
  let
    rest = T.dropWhile (\c -> c == ' ' || c == '\t') l
  in
    case lineLeader l of
      Just leader -> T.stripStart (T.drop (T.length leader) rest)
      Nothing -> rest

-- Rewrap the selected lines — or, with no selection, the contiguous run
-- of same-prefixed lines around the cursor — to reflowWidth columns,
-- keeping the first line's indentation and comment leader on every
-- produced line.
reflow :: EdState -> EdState
reflow st =
  let
    ls = bufferLines st
    cr = fst (cursorRC st)
    (lo, hi) = case edSelAnchor st of
      Just _ -> selRowSpan st
      Nothing -> paragraphAround ls cr
    prefix = linePrefix (lineAt ls lo)
    allWords = concatMap (T.words . lineContent . lineAt ls) [lo .. hi]
    target = max 20 (reflowWidth - T.length prefix)
    newRows = case greedyWrap target allWords of
      [] -> [T.stripEnd prefix]
      packed -> map (\ws -> T.stripEnd (prefix <> T.unwords ws)) packed
    kept = take lo ls ++ newRows ++ drop (hi + 1) ls
  in
    (collapse st)
      { edZipper = Z.moveCursor (lo, 0) (Z.textZipper kept Nothing)
      , edUndo = snapshot st
      , edRedo = []
      , edTouched = True
      }

-- Contiguous non-blank neighbours sharing the cursor line's exact prefix.
paragraphAround :: [Text] -> Int -> (Int, Int)
paragraphAround ls cr =
  let
    p = linePrefix (lineAt ls cr)
    ok r =
      r >= 0
        && r < length ls
        && linePrefix (lineAt ls r) == p
        && not (T.null (T.strip (lineAt ls r)))
    up r = if ok (r - 1) then up (r - 1) else r
    down r = if ok (r + 1) then down (r + 1) else r
  in
    if ok cr then (up cr, down cr) else (cr, cr)

greedyWrap :: Int -> [Text] -> [[Text]]
greedyWrap w =
  let
    grab _ acc [] = (acc, [])
    grab n acc (y : ys)
      | n + 1 + T.length y <= w = grab (n + 1 + T.length y) (y : acc) ys
      | otherwise = (acc, y : ys)
    go [] = []
    go (x : xs) =
      let
        (line, rest) = grab (T.length x) [x] xs
      in
        reverse line : go rest
  in
    go

-- Swap the case of every letter in the selection (char- or line-wise), or
-- of the char under the cursor.
toggleCase :: EdState -> EdState
toggleCase =
  let
    flipChar c
      | isUpper c = toLower c
      | isLower c = toUpper c
      | otherwise = c
  in
    mapSelChars flipChar

-- Rewrite every character of the selection (char- or line-wise), or the
-- char under the cursor, through the given function. Length never
-- changes, so the cursor and the selection both survive in place. vi's
-- gu/gU ride on this alongside helix's ~.
mapSelChars :: (Char -> Char) -> EdState -> EdState
mapSelChars f st =
  let
    (cr, cc) = cursorRC st
    ls = bufferLines st
    ((lr, lc), (hr, hc)) =
      case (edSelKind st, edSelAnchor st) of
        (LineSel, Just (ar, _)) ->
          let
            lo = min ar cr
            hi = max ar cr
          in
            ((lo, 0), (hi, max 0 (T.length (lineAt ls hi) - 1)))
        _ ->
          case charSelection st of
            Just spanPair -> spanPair
            Nothing -> ((cr, cc), (cr, cc))
    flipSpan s e l =
      let
        (pre, rest) = T.splitAt s l
        (mid, post) = T.splitAt (e + 1 - s) rest
      in
        pre <> T.map f mid <> post
    flipLine (r, l)
      | r < lr || r > hr = l
      | otherwise =
          flipSpan
            (if r == lr then lc else 0)
            (if r == hr then hc else max 0 (T.length l - 1))
            l
    newLs = map flipLine (zip [0 ..] ls)
  in
    st
      { edZipper = Z.moveCursor (cr, cc) (Z.textZipper newLs Nothing)
      , edUndo = snapshot st
      , edRedo = []
      , edTouched = True
      }

undo :: EdState -> EdState
undo st =
  case edUndo st of
    [] -> st
    z : zs ->
      (collapse st)
        { edZipper = z
        , edUndo = zs
        , edRedo = take undoDepth (edZipper st : edRedo st)
        }

redo :: EdState -> EdState
redo st =
  case edRedo st of
    [] -> st
    z : zs ->
      (collapse st)
        { edZipper = z
        , edRedo = zs
        , edUndo = take undoDepth (edZipper st : edUndo st)
        }

data InsertResult
  = InsertHandled EdState
  | InsertExit EdState
  | InsertUnhandled

handleInsert :: V.Event -> EdState -> InsertResult
handleInsert ev st =
  let
    edit f =
      InsertHandled
        st {edZipper = f (edZipper st), edRecording = ev : edRecording st}
  in
    case ev of
      V.EvKey V.KEsc [] ->
        InsertExit
          st
            { edMode = Normal
            , edLastInsert =
                case edInsertEntry st of
                  Just entry -> Just (entry, reverse (edRecording st))
                  Nothing -> edLastInsert st
            , edInsertEntry = Nothing
            }
      V.EvKey (V.KChar '\t') [] -> edit (Z.insertMany "  ")
      V.EvKey (V.KChar c) [] -> edit (Z.insertChar c)
      V.EvKey V.KEnter [] -> edit Z.breakLine
      V.EvKey V.KBS [] -> edit Z.deletePrevChar
      V.EvKey V.KDel [] -> edit Z.deleteChar
      V.EvKey V.KLeft [] -> edit Z.moveLeft
      V.EvKey V.KRight [] -> edit Z.moveRight
      V.EvKey V.KUp [] -> edit Z.moveUp
      V.EvKey V.KDown [] -> edit Z.moveDown
      V.EvPaste bytes -> edit (Z.insertMany (TE.decodeUtf8Lenient bytes))
      _ -> InsertUnhandled

-- Word motions. Forward motions land the cursor on the last character of
-- the crossed span (so `w` selects through the whitespace before the next
-- word), hopping to an adjacent line's edge when the line runs out.
wordStartsIn :: Text -> [Int]
wordStartsIn t =
  let
    isStart i =
      wordChar (T.index t i)
        && (i == 0 || not (wordChar (T.index t (i - 1))))
  in
    filter isStart [0 .. T.length t - 1]

wordForwardSel :: Z.TextZipper Text -> Z.TextZipper Text
wordForwardSel z =
  let
    t = Z.currentLine z
    (row, col) = Z.cursorPosition z
    -- land on the char before a word start, but only if that is real
    -- forward progress — from the space right before a word the first
    -- candidate is the cursor itself, and the next word is the target
    targets = filter (> col) (map (subtract 1) (wordStartsIn t))
  in
    case targets of
      (i : _) -> Z.moveCursor (row, i) z
      [] ->
        if col < T.length t - 1
          then Z.gotoEOL z
          else Z.moveCursor (row + 1, 0) z

wordEndSel :: Z.TextZipper Text -> Z.TextZipper Text
wordEndSel z =
  let
    t = Z.currentLine z
    (row, col) = Z.cursorPosition z
    len = T.length t
    isEnd i =
      wordChar (T.index t i)
        && (i == len - 1 || not (wordChar (T.index t (i + 1))))
    ends = filter isEnd [col + 1 .. len - 1]
  in
    case ends of
      (i : _) -> Z.moveCursor (row, i) z
      [] -> Z.moveCursor (row + 1, 0) z

wordBackSel :: Z.TextZipper Text -> Z.TextZipper Text
wordBackSel z =
  let
    t = Z.currentLine z
    (row, col) = Z.cursorPosition z
    starts = filter (< col) (wordStartsIn t)
  in
    case reverse starts of
      (i : _) -> Z.moveCursor (row, i) z
      [] -> if row > 0 then Z.gotoEOL (Z.moveUp z) else z

wordChar :: Char -> Bool
wordChar = not . isSpace

-- The m menu ----------------------------------------------------------------------

-- helix's match mode. mm jumps to the other half of the pair the cursor
-- is on; ms, md and mr add, remove and swap the pair around it; mi and
-- ma select what is inside or around it.
handleMatch :: V.Event -> EdState -> EdState
handleMatch ev st =
  case ev of
    V.EvKey (V.KChar 'm') [] ->
      case matchingBracket st of
        Just pos -> plainMove (Z.moveCursor pos) st
        Nothing -> st
    V.EvKey (V.KChar 's') [] -> st {edPending = PendingSurround SurroundAdd}
    V.EvKey (V.KChar 'd') [] -> st {edPending = PendingSurround SurroundDelete}
    V.EvKey (V.KChar 'r') [] -> st {edPending = PendingSurround SurroundReplace}
    V.EvKey (V.KChar 'i') [] -> st {edPending = PendingObject ObjInside}
    V.EvKey (V.KChar 'a') [] -> st {edPending = PendingObject ObjAround}
    _ -> st

handleSurround :: SurroundAct -> V.Event -> EdState -> EdState
handleSurround act ev st =
  case ev of
    V.EvKey (V.KChar c) [] ->
      case act of
        SurroundAdd -> surroundAdd c st
        SurroundDelete -> surroundDelete c st
        -- mr needs both halves of the question before it can answer
        SurroundReplace -> st {edPending = PendingSurroundTo c}
    _ -> st

handleSurroundTo :: Char -> V.Event -> EdState -> EdState
handleSurroundTo from ev st =
  case ev of
    V.EvKey (V.KChar to) [] -> surroundReplace from to st
    _ -> st

handleObject :: ObjSide -> V.Event -> EdState -> EdState
handleObject side ev st =
  case ev of
    V.EvKey (V.KChar 'w') [] -> selectRun side SmallWord st
    V.EvKey (V.KChar 'W') [] -> selectRun side BigWord st
    V.EvKey (V.KChar c) [] ->
      case enclosingFor c st of
        Just (open, close) -> selectPair side open close st
        Nothing -> st
    _ -> st

-- miw takes the run of like characters under the cursor; maw takes the
-- whitespace after it as well, or before it when the run ends the line.
selectRun :: ObjSide -> WordClass -> EdState -> EdState
selectRun side cls st =
  let
    (row, col) = cursorRC st
    line = lineAt (bufferLines st) row
    len = T.length line
    classAt i = charClass cls (T.index line i)
    here = if col < len then classAt col else ClsSpace
    runTo step i =
      if i >= 0 && i < len && classAt i == here
        then runTo step (i + step)
        else i - step
    lo = if col < len then runTo (-1) col else col
    hi = if col < len then runTo 1 col else col
    after j = if j + 1 < len && classAt (j + 1) == ClsSpace then after (j + 1) else j
    before j = if j - 1 >= 0 && classAt (j - 1) == ClsSpace then before (j - 1) else j
    (a, b) = case side of
      ObjInside -> (lo, hi)
      ObjAround ->
        let
          grown = after hi
        in
          if grown /= hi then (lo, grown) else (before lo, hi)
  in
    if len == 0
      then st
      else
        st
          { edZipper = Z.moveCursor (row, b) (edZipper st)
          , edSelAnchor = Just (row, a)
          , edSelKind = CharSel
          , edExtend = False
          }

-- The two halves a character asks for. Either side names the pair, and
-- anything that is not a bracket stands for itself, which is how the
-- quotes come out right.
pairChars :: Char -> (Char, Char)
pairChars c =
  case c of
    '(' -> ('(', ')')
    ')' -> ('(', ')')
    'b' -> ('(', ')')
    '[' -> ('[', ']')
    ']' -> ('[', ']')
    '{' -> ('{', '}')
    '}' -> ('{', '}')
    'B' -> ('{', '}')
    '<' -> ('<', '>')
    '>' -> ('<', '>')
    _ -> (c, c)

-- Where the pair around the cursor sits.
--
-- Brackets nest, so finding one means counting; a quote does not nest
-- and cannot be told apart from its own partner, so its pairs are taken
-- in order along the line and the cursor picks the one it is in.
enclosingFor :: Char -> EdState -> Maybe ((Int, Int), (Int, Int))
enclosingFor c st =
  let
    ls = bufferLines st
    pos = cursorRC st
    (open, close) = pairChars c
  in
    if open /= close
      then matchEnclosing open close ls pos
      else
        -- only the three that actually delimit anything. Falling through
        -- to "it stands for itself" would make miw hunt for the letter w
        -- and hand back whatever it found between two of them.
        if c `elem` quoteChars
          then enclosingQuote open ls pos
          else Nothing

quoteChars :: String
quoteChars = "\"'`"

enclosingQuote :: Char -> [Text] -> (Int, Int) -> Maybe ((Int, Int), (Int, Int))
enclosingQuote q ls (r, c) =
  let
    line = lineAt ls r
    at = filter (\i -> T.index line i == q) [0 .. T.length line - 1]
    couples xs =
      case xs of
        (a : b : rest) -> (a, b) : couples rest
        _ -> []
    holding (a, b) = c >= a && c <= b
  in
    case filter holding (couples at) of
      ((a, b) : _) -> Just ((r, a), (r, b))
      [] -> Nothing

-- Rewrite the buffer and put the cursor down, keeping undo.
reshapeTo :: [Text] -> (Int, Int) -> EdState -> EdState
reshapeTo ls pos st =
  st
    { edZipper =
        Z.moveCursor pos (Z.textZipper (if null ls then [""] else ls) Nothing)
    , edUndo = snapshot st
    , edRedo = []
    , edTouched = True
    }

withLine :: Int -> (Text -> Text) -> [Text] -> [Text]
withLine r f ls = take r ls ++ [f (lineAt ls r)] ++ drop (r + 1) ls

insertAt :: Int -> Int -> Char -> [Text] -> [Text]
insertAt r c ch =
  withLine r (\l -> T.take c l <> T.singleton ch <> T.drop c l)

replaceAt :: Int -> Int -> Char -> [Text] -> [Text]
replaceAt r c ch =
  withLine r (\l -> T.take c l <> T.singleton ch <> T.drop (c + 1) l)

dropAt :: Int -> Int -> [Text] -> [Text]
dropAt r c = withLine r (\l -> T.take c l <> T.drop (c + 1) l)

surroundAdd :: Char -> EdState -> EdState
surroundAdd c st =
  case charSelection st of
    Nothing -> st
    Just ((lr, lc), (hr, hc)) ->
      let
        (open, close) = pairChars c
        -- the closer goes in first: putting the opener in ahead of it
        -- would shift where it belongs and the pair would land crooked
        ls =
          insertAt lr lc open (insertAt hr (hc + 1) close (bufferLines st))
        -- and on one line the opener has pushed the closer along again
        closeCol = if lr == hr then hc + 2 else hc + 1
        moved = reshapeTo ls (hr, closeCol) st
      in
        moved {edSelAnchor = Just (lr, lc), edSelKind = CharSel}

surroundDelete :: Char -> EdState -> EdState
surroundDelete c st =
  case enclosingFor c st of
    Nothing -> st
    Just ((orow, ocol), (crow, ccol)) ->
      let
        ls = dropAt orow ocol (dropAt crow ccol (bufferLines st))
        (r, cc) = cursorRC st
        -- losing the opener slides everything after it on that line left
        col = if r == orow && cc > ocol then cc - 1 else cc
      in
        collapse (reshapeTo ls (r, max 0 col) st)

surroundReplace :: Char -> Char -> EdState -> EdState
surroundReplace from to st =
  case enclosingFor from st of
    Nothing -> st
    Just ((orow, ocol), (crow, ccol)) ->
      let
        (open, close) = pairChars to
        ls = replaceAt orow ocol open (replaceAt crow ccol close (bufferLines st))
      in
        collapse (reshapeTo ls (cursorRC st) st)

-- mi takes what is between the delimiters, ma takes them with it. An
-- empty pair has nothing inside, so mi there only moves the cursor.
selectPair :: ObjSide -> (Int, Int) -> (Int, Int) -> EdState -> EdState
selectPair side (orow, ocol) (crow, ccol) st =
  let
    inner = ((orow, ocol + 1), (crow, ccol - 1))
    span' = case side of
      ObjAround -> Just ((orow, ocol), (crow, ccol))
      ObjInside ->
        if orow == crow && ocol + 1 > ccol - 1
          then Nothing
          else Just inner
  in
    case span' of
      Nothing ->
        collapse st {edZipper = Z.moveCursor (orow, ocol + 1) (edZipper st)}
      Just (anchor, headPos) ->
        st
          { edZipper = Z.moveCursor headPos (edZipper st)
          , edSelAnchor = Just anchor
          , edSelKind = CharSel
          , edExtend = False
          }
