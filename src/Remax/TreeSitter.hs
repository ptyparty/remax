{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Tree-sitter highlighting with everything compiled into the binary: the
-- MIT-licensed runtime and each vendored grammar ride along as C sources,
-- and each grammar's own MIT highlights.scm is embedded at build time.
-- Nothing here needs helix, a system libtree-sitter, or network access.
--
-- Matches are filtered through their query predicates (#eq?, #any-of?,
-- and negations; a pattern using a predicate we don't evaluate is
-- dropped rather than over-applied). Upstream queries follow the nvim
-- capture conventions, so names are normalized to helix-style scopes and
-- non-highlight captures (@spell, @_helpers…) are ignored.
module Remax.TreeSitter
  ( Highlighter
  , GrammarKeys (..)
  , HighlighterTable
  , initHighlighters
  , highlighterFor
  , tsHighlight
  ) where

import Control.Monad qualified as Monad
import Data.Array.IO qualified as IOArray
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.FileEmbed qualified as FileEmbed
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Ord qualified as Ord
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word qualified as DataWord
import Foreign.C.String qualified as CString
import Foreign.C.Types qualified as CTypes
import Foreign.Marshal.Alloc qualified as Alloc
import Foreign.Marshal.Array qualified as MArray
import Foreign.Ptr qualified as Ptr
import Foreign.Storable qualified as Storable
import System.FilePath qualified as FilePath
import System.Timeout qualified as Timeout

data CParser
data CTree
data CQuery
data CQueryCursor
data CLanguage
data CNode

foreign import ccall unsafe "ts_parser_new"
  c_parser_new :: IO (Ptr.Ptr CParser)

foreign import ccall unsafe "ts_parser_delete"
  c_parser_delete :: Ptr.Ptr CParser -> IO ()

foreign import ccall unsafe "ts_parser_set_language"
  c_parser_set_language :: Ptr.Ptr CParser -> Ptr.Ptr CLanguage -> IO CTypes.CBool

foreign import ccall unsafe "ts_parser_set_timeout_micros"
  c_parser_set_timeout :: Ptr.Ptr CParser -> DataWord.Word64 -> IO ()

foreign import ccall safe "ts_parser_parse_string"
  c_parser_parse_string ::
    Ptr.Ptr CParser -> Ptr.Ptr CTree -> Ptr.Ptr CTypes.CChar -> DataWord.Word32 -> IO (Ptr.Ptr CTree)

foreign import ccall unsafe "ts_tree_delete"
  c_tree_delete :: Ptr.Ptr CTree -> IO ()

foreign import ccall unsafe "ts_query_new"
  c_query_new ::
    Ptr.Ptr CLanguage -> Ptr.Ptr CTypes.CChar -> DataWord.Word32 -> Ptr.Ptr DataWord.Word32 -> Ptr.Ptr DataWord.Word32 -> IO (Ptr.Ptr CQuery)

foreign import ccall unsafe "ts_query_capture_count"
  c_query_capture_count :: Ptr.Ptr CQuery -> IO DataWord.Word32

foreign import ccall unsafe "ts_query_capture_name_for_id"
  c_query_capture_name :: Ptr.Ptr CQuery -> DataWord.Word32 -> Ptr.Ptr DataWord.Word32 -> IO (Ptr.Ptr CTypes.CChar)

foreign import ccall unsafe "ts_query_pattern_count"
  c_query_pattern_count :: Ptr.Ptr CQuery -> IO DataWord.Word32

foreign import ccall unsafe "ts_query_predicates_for_pattern"
  c_query_predicates ::
    Ptr.Ptr CQuery -> DataWord.Word32 -> Ptr.Ptr DataWord.Word32 -> IO (Ptr.Ptr DataWord.Word32)

foreign import ccall unsafe "ts_query_string_value_for_id"
  c_query_string_value :: Ptr.Ptr CQuery -> DataWord.Word32 -> Ptr.Ptr DataWord.Word32 -> IO (Ptr.Ptr CTypes.CChar)

foreign import ccall unsafe "ts_query_cursor_new"
  c_query_cursor_new :: IO (Ptr.Ptr CQueryCursor)

foreign import ccall unsafe "ts_query_cursor_delete"
  c_query_cursor_delete :: Ptr.Ptr CQueryCursor -> IO ()

foreign import ccall unsafe "remax_tree_root_node"
  c_root_node :: Ptr.Ptr CTree -> IO (Ptr.Ptr CNode)

foreign import ccall unsafe "remax_node_free"
  c_node_free :: Ptr.Ptr CNode -> IO ()

foreign import ccall unsafe "remax_query_cursor_exec"
  c_cursor_exec :: Ptr.Ptr CQueryCursor -> Ptr.Ptr CQuery -> Ptr.Ptr CNode -> IO ()

foreign import ccall unsafe "remax_query_cursor_next_match"
  c_next_match ::
    Ptr.Ptr CQueryCursor ->
    Ptr.Ptr DataWord.Word32 -> -- pattern index
    Ptr.Ptr DataWord.Word32 -> -- starts
    Ptr.Ptr DataWord.Word32 -> -- ends
    Ptr.Ptr DataWord.Word32 -> -- capture ids
    DataWord.Word32 -> -- max captures
    Ptr.Ptr DataWord.Word32 -> -- count
    IO CTypes.CInt

foreign import ccall unsafe "tree_sitter_haskell"
  c_lang_haskell :: IO (Ptr.Ptr CLanguage)

foreign import ccall unsafe "tree_sitter_purescript"
  c_lang_purescript :: IO (Ptr.Ptr CLanguage)

foreign import ccall unsafe "tree_sitter_python"
  c_lang_python :: IO (Ptr.Ptr CLanguage)

foreign import ccall unsafe "tree_sitter_yaml"
  c_lang_yaml :: IO (Ptr.Ptr CLanguage)

foreign import ccall unsafe "tree_sitter_json"
  c_lang_json :: IO (Ptr.Ptr CLanguage)

foreign import ccall unsafe "tree_sitter_toml"
  c_lang_toml :: IO (Ptr.Ptr CLanguage)

foreign import ccall unsafe "tree_sitter_hcl"
  c_lang_hcl :: IO (Ptr.Ptr CLanguage)

foreign import ccall unsafe "tree_sitter_sql"
  c_lang_sql :: IO (Ptr.Ptr CLanguage)

foreign import ccall unsafe "tree_sitter_bash"
  c_lang_bash :: IO (Ptr.Ptr CLanguage)

foreign import ccall unsafe "tree_sitter_nu"
  c_lang_nu :: IO (Ptr.Ptr CLanguage)

foreign import ccall unsafe "tree_sitter_markdown"
  c_lang_markdown :: IO (Ptr.Ptr CLanguage)

foreign import ccall unsafe "tree_sitter_typescript"
  c_lang_typescript :: IO (Ptr.Ptr CLanguage)

foreign import ccall unsafe "tree_sitter_tsx"
  c_lang_tsx :: IO (Ptr.Ptr CLanguage)

foreign import ccall unsafe "tree_sitter_javascript"
  c_lang_javascript :: IO (Ptr.Ptr CLanguage)

-- One vendored grammar: extensions it claims, its language function, and
-- its embedded highlight query.
-- How a grammar claims a file. Both ways of claiming live on the same
-- record, so a language cannot be reachable by extension while its
-- shebangs quietly point somewhere else.
data GrammarKeys = GrammarKeys
  { gkExtensions :: [String]
  , gkShebangs :: [T.Text] -- bare interpreter names, e.g. "bash"
  }

type HighlighterTable = [(GrammarKeys, Highlighter)]

vendored :: [(GrammarKeys, IO (Ptr.Ptr CLanguage), BS.ByteString)]
vendored =
  [
    ( GrammarKeys
        [".hs", ".hs-boot", ".hsc"]
        -- a stack script is Haskell: #!/usr/bin/env stack
        ["runhaskell", "runghc", "stack"]
    , c_lang_haskell
    , $(FileEmbed.embedFile "vendor/grammars/haskell/highlights.scm")
    )
  ,
    ( GrammarKeys [".purs"] []
    , c_lang_purescript
    , $(FileEmbed.embedFile "vendor/grammars/purescript/highlights.scm")
    )
  ,
    ( GrammarKeys
        [".py", ".pyi", ".pyw"]
        ["python", "python3", "python2"]
    , c_lang_python
    , $(FileEmbed.embedFile "vendor/grammars/python/highlights.scm")
    )
  ,
    ( GrammarKeys [".yaml", ".yml"] []
    , c_lang_yaml
    , $(FileEmbed.embedFile "vendor/grammars/yaml/highlights.scm")
    )
  ,
    ( GrammarKeys [".json"] []
    , c_lang_json
    , $(FileEmbed.embedFile "vendor/grammars/json/highlights.scm")
    )
  ,
    ( GrammarKeys [".toml"] []
    , c_lang_toml
    , $(FileEmbed.embedFile "vendor/grammars/toml/highlights.scm")
    )
  ,
    ( GrammarKeys [".tf", ".tfvars", ".hcl", ".nomad"] []
    , c_lang_hcl
    , $(FileEmbed.embedFile "vendor/grammars/hcl/highlights.scm")
    )
  ,
    ( GrammarKeys [".sql"] []
    , c_lang_sql
    , $(FileEmbed.embedFile "vendor/grammars/sql/highlights.scm")
    )
  ,
    ( GrammarKeys
        [".sh", ".bash", ".zsh"]
        ["sh", "bash", "zsh", "dash", "ash", "ksh"]
    , c_lang_bash
    , $(FileEmbed.embedFile "vendor/grammars/bash/highlights.scm")
    )
  ,
    ( GrammarKeys [".nu"] ["nu", "nushell"]
    , c_lang_nu
    , $(FileEmbed.embedFile "vendor/grammars/nu/highlights.scm")
    )
  ,
    ( GrammarKeys [".md", ".markdown"] []
    , c_lang_markdown
    , $(FileEmbed.embedFile "vendor/grammars/markdown/highlights.scm")
    )
  ,
    ( GrammarKeys [".ts", ".mts", ".cts"] ["ts-node", "tsx", "deno"]
    , c_lang_typescript
    , $(FileEmbed.embedFile "vendor/grammars/typescript/highlights.scm")
    )
  ,
    ( GrammarKeys [".tsx"] []
    , c_lang_tsx
    , $(FileEmbed.embedFile "vendor/grammars/tsx/highlights.scm")
    )
  ,
    ( GrammarKeys [".js", ".mjs", ".cjs", ".jsx"] ["node", "nodejs", "bun"]
    , c_lang_javascript
    , $(FileEmbed.embedFile "vendor/grammars/javascript/highlights.scm")
    )
  ]

-- One predicate test attached to a pattern. Candidate strings stay as
-- UTF-8 bytes so match text never needs decoding to compare.
data PredTest
  = TestStrings Bool DataWord.Word32 [BS.ByteString] -- negated? capture-id candidates
  | TestCaptures Bool DataWord.Word32 DataWord.Word32
  | TestAlwaysPass -- directives like #set!
  | TestAlwaysFail -- predicates we don't evaluate

data Highlighter = Highlighter
  { hlLang :: Ptr.Ptr CLanguage
  , hlQuery :: Ptr.Ptr CQuery
  , hlNames :: [T.Text] -- capture id -> normalized scope ("" = don't paint)
  , hlPreds :: [[PredTest]] -- pattern index -> tests
  }

-- Build every vendored grammar's query once at startup; a grammar whose
-- query fails to compile is dropped rather than crashing the tool.
initHighlighters :: IO HighlighterTable
initHighlighters =
  let
    build (keys, getLang, queryBytes) = do
      lang <- getLang
      query <-
        BS.useAsCStringLen queryBytes $ \(p, len) ->
          Alloc.alloca $ \errOffset ->
            Alloc.alloca $ \errType ->
              c_query_new lang p (fromIntegral len) errOffset errType
      if query == Ptr.nullPtr
        then pure []
        else do
          names <- captureNames query
          preds <- patternPredicates query
          pure [(keys, Highlighter lang query names preds)]
  in
    fmap concat (mapM build vendored)

captureNames :: Ptr.Ptr CQuery -> IO [T.Text]
captureNames query = do
  count <- c_query_capture_count query
  Monad.forM (indices count) $ \i ->
    Alloc.alloca $ \lenP -> do
      namePtr <- c_query_capture_name query i lenP
      len <- Storable.peek lenP
      raw <- T.pack <$> CString.peekCStringLen (namePtr, fromIntegral len)
      pure (normalizeScope raw)

indices :: DataWord.Word32 -> [DataWord.Word32]
indices n = if n == 0 then [] else [0 .. n - 1]

-- Upstream queries use nvim capture conventions; map them onto the helix
-- scope names the theme understands, and blank out the captures that are
-- not highlights at all.
normalizeScope :: T.Text -> T.Text
normalizeScope name
  | "_" `T.isPrefixOf` name = ""
  | name `elem` ["spell", "nospell", "conceal", "none"] = ""
  | "injection." `T.isPrefixOf` name = ""
  | "local." `T.isPrefixOf` name = ""
  | otherwise = Map.findWithDefault name name renameTable

renameTable :: Map.Map T.Text T.Text
renameTable =
  Map.fromList
    [ ("function.call", "function")
    , ("variable.member", "variable.other.member")
    , ("property", "variable.other.member")
    , ("field", "variable.other.member")
    , ("parameter", "variable.parameter")
    , ("module", "namespace")
    , ("boolean", "constant.builtin.boolean")
    , ("number", "constant.numeric")
    , ("number.float", "constant.numeric.float")
    , ("character", "constant.character")
    , ("keyword.exception", "keyword.control.exception")
    , ("keyword.repeat", "keyword.control.repeat")
    , ("keyword.conditional", "keyword.control.conditional")
    , ("keyword.import", "keyword.control.import")
    , ("keyword.return", "keyword.control.return")
    , ("keyword.debug", "function.builtin")
    , ("comment.documentation", "comment.block.documentation")
    , ("string.documentation", "comment.block.documentation")
    , ("constant.macro", "constant.builtin")
    ]

-- The predicate steps for each pattern: triples of (type, value id) laid
-- out flat, groups terminated by a Done step. Type 1 is a capture
-- reference, type 2 a string, 0 the terminator.
patternPredicates :: Ptr.Ptr CQuery -> IO [[PredTest]]
patternPredicates query = do
  count <- c_query_pattern_count query
  Monad.forM (indices count) $ \pix ->
    Alloc.alloca $ \lenP -> do
      stepsPtr <- c_query_predicates query pix lenP
      len <- Storable.peek lenP
      steps <- Monad.forM (indices len) $ \i -> do
        let
          base = stepsPtr `Ptr.plusPtr` (fromIntegral i * 8)
        stepType <- Storable.peekByteOff base 0 :: IO DataWord.Word32
        valueId <- Storable.peekByteOff base 4 :: IO DataWord.Word32
        pure (stepType, valueId)
      mapM (parsePred query) (splitOnDone steps)

splitOnDone :: [(DataWord.Word32, DataWord.Word32)] -> [[(DataWord.Word32, DataWord.Word32)]]
splitOnDone steps =
  case break ((== 0) . fst) steps of
    ([], []) -> []
    ([], rest) -> splitOnDone (drop 1 rest)
    (grp, rest) -> grp : splitOnDone (drop 1 rest)

parsePred :: Ptr.Ptr CQuery -> [(DataWord.Word32, DataWord.Word32)] -> IO PredTest
parsePred query steps =
  let
    stepValue (kind, value) =
      case kind of
        1 -> pure (Left value)
        _ -> fmap Right (stringValue query value)
    buildPred name args =
      case (name, args) of
        ("eq?", [Left c, Right s]) -> TestStrings False c [TE.encodeUtf8 s]
        ("not-eq?", [Left c, Right s]) -> TestStrings True c [TE.encodeUtf8 s]
        ("eq?", [Left a, Left b]) -> TestCaptures False a b
        ("not-eq?", [Left a, Left b]) -> TestCaptures True a b
        ("any-of?", Left c : rest)
          | all isRight rest ->
              TestStrings False c (Maybe.mapMaybe rightBytes rest)
        ("not-any-of?", Left c : rest)
          | all isRight rest ->
              TestStrings True c (Maybe.mapMaybe rightBytes rest)
        -- the common "^(a|b|c)$" form of #match? is just an any-of
        ("match?", [Left c, Right rx])
          | Just alts <- simpleAlternation rx ->
              TestStrings False c (map TE.encodeUtf8 alts)
        ("not-match?", [Left c, Right rx])
          | Just alts <- simpleAlternation rx ->
              TestStrings True c (map TE.encodeUtf8 alts)
        _
          | "!" `T.isSuffixOf` name -> TestAlwaysPass
          | otherwise -> TestAlwaysFail
    isRight e = case e of
      Right _ -> True
      Left _ -> False
    rightBytes e = case e of
      Right str -> Just (TE.encodeUtf8 str)
      Left _ -> Nothing
  in
    case steps of
      ((2, nameId) : rest) -> do
        name <- stringValue query nameId
        args <- mapM stepValue rest
        pure (buildPred name args)
      _ -> pure TestAlwaysFail

simpleAlternation :: T.Text -> Maybe [T.Text]
simpleAlternation rx = do
  inner <- T.stripSuffix ")$" =<< T.stripPrefix "^(" rx
  let
    alts = T.splitOn "|" inner
    literal t =
      not (T.null t)
        && T.all (\c -> Char.isAlphaNum c || c == '_' || c == '\'') t
  if not (null alts) && all literal alts
    then Just alts
    else Nothing

stringValue :: Ptr.Ptr CQuery -> DataWord.Word32 -> IO T.Text
stringValue query sid =
  Alloc.alloca $ \lenP -> do
    p <- c_query_string_value query sid lenP
    len <- Storable.peek lenP
    T.pack <$> CString.peekCStringLen (p, fromIntegral len)

-- Extension first. A file without a usable one falls back to its
-- shebang, which is all scripts/build has to go on — no extension, but
-- "#!/bin/bash" on line one.
highlighterFor :: HighlighterTable -> FilePath -> Maybe T.Text -> Maybe Highlighter
highlighterFor table path mFirstLine =
  let
    byExtension (keys, _) = FilePath.takeExtension path `elem` gkExtensions keys
    byShebang name (keys, _) = name `elem` gkShebangs keys
  in
    case List.find byExtension table of
      Just (_, hl) -> Just hl
      Nothing -> do
        name <- mFirstLine >>= shebangInterpreter
        fmap snd (List.find (byShebang name) table)

-- The interpreter a shebang names, bare: "#!/bin/sh" is sh.
shebangInterpreter :: T.Text -> Maybe T.Text
shebangInterpreter line =
  case T.stripPrefix "#!" (T.stripStart line) of
    Nothing -> Nothing
    Just rest -> interpreterName (T.words rest)

-- env is a launcher rather than a language, so "#!/usr/bin/env -S bash
-- --norc" names bash: step over env and any flags to reach what it is
-- actually starting.
interpreterName :: [T.Text] -> Maybe T.Text
interpreterName ws =
  case ws of
    [] -> Nothing
    (w : rest) ->
      let
        name = T.pack (FilePath.takeFileName (T.unpack w))
      in
        if name == "env"
          then interpreterName (dropWhile (T.isPrefixOf "-") rest)
          else Just name

parseBudgetMicros :: DataWord.Word64
parseBudgetMicros = 300000

-- Painting and rendering walk every capture the query produced; on a
-- huge generated file that is hundreds of thousands of captures and
-- seconds of work, so it runs under its own wall-clock budget.
tokenizeBudgetMicros :: Int
tokenizeBudgetMicros = 800000

-- Files past this size render plain without trying: they are almost
-- always generated, and even a successful highlight of one costs more
-- than it is worth.
maxHighlightLines :: Int
maxHighlightLines = 10000

maxCapturesPerMatch :: Int
maxCapturesPerMatch = 32

-- Highlight whole lines: each returned line is a list of (scope, text)
-- runs, "" for unstyled stretches. Nothing when the parse failed or blew
-- its budget, so the caller can fall back.
tsHighlight :: Highlighter -> [T.Text] -> IO (Maybe [[(T.Text, T.Text)]])
tsHighlight hl ls
  | length ls > maxHighlightLines = pure Nothing
  | otherwise = do
      parser <- c_parser_new
      ok <- c_parser_set_language parser (hlLang hl)
      if ok == CTypes.CBool 0
        then do
          c_parser_delete parser
          pure Nothing
        else do
          c_parser_set_timeout parser parseBudgetMicros
          let
            bs = TE.encodeUtf8 (T.unlines ls)
          tree <-
            BS.useAsCStringLen bs $ \(p, len) ->
              c_parser_parse_string parser Ptr.nullPtr p (fromIntegral len)
          if tree == Ptr.nullPtr
            then do
              c_parser_delete parser
              pure Nothing
            else do
              root <- c_root_node tree
              cursor <- c_query_cursor_new
              c_cursor_exec cursor (hlQuery hl) root
              -- capture id + 1 per byte; later matches overwrite earlier,
              -- which is how query files layer general-to-specific
              arr <-
                IOArray.newArray (0, max 0 (BS.length bs - 1)) 0 :: IO (IOArray.IOUArray Int DataWord.Word16)
              -- the paint and render walk runs under the tokenize
              -- budget; the result is forced inside it so no work
              -- escapes as thunks to be paid for later
              rendered <- Timeout.timeout tokenizeBudgetMicros $ do
                paintMatches hl bs cursor arr
                out <- renderLines arr (hlNames hl) ls
                let
                  forceSeg (a, b) r = T.length a `seq` T.length b `seq` r
                  forceLine line r = foldr forceSeg () line `seq` r
                foldr forceLine () out `seq` pure out
              c_query_cursor_delete cursor
              c_node_free root
              c_tree_delete tree
              c_parser_delete parser
              pure rendered

-- Collect every predicate-passing capture, then paint widest spans first
-- so narrower (deeper) captures overwrite them — the innermost-node-wins
-- rule real highlight engines follow. Equal spans are settled by pattern
-- index: query files put general patterns first and specific ones later,
-- so the later (higher-index) pattern paints last and wins.
paintMatches ::
  Highlighter -> BS.ByteString -> Ptr.Ptr CQueryCursor -> IOArray.IOUArray Int DataWord.Word16 -> IO ()
paintMatches hl source cursor arr = do
  caps <- collectMatches hl source cursor
  let
    total = BS.length source
    ordered =
      List.sortBy (Ord.comparing (\(s, e, _, pix) -> (negate (e - s), pix))) caps
    fillRange :: DataWord.Word16 -> Int -> Int -> IO ()
    fillRange code b limit =
      Monad.unless (b >= limit || b >= total) $ do
        IOArray.writeArray arr b code
        fillRange code (b + 1) limit
  mapM_
    (\(s, e, cid, _) -> fillRange (fromIntegral cid + 1) (max 0 s) (min e total))
    ordered

collectMatches ::
  Highlighter -> BS.ByteString -> Ptr.Ptr CQueryCursor -> IO [(Int, Int, DataWord.Word32, Int)]
collectMatches hl source cursor =
  Alloc.alloca $ \pixP ->
    Alloc.alloca $ \countP ->
      MArray.allocaArray maxCapturesPerMatch $ \startsP ->
        MArray.allocaArray maxCapturesPerMatch $ \endsP ->
          MArray.allocaArray maxCapturesPerMatch $ \idsP -> do
            let
              scopeAt cid = case drop (fromIntegral cid) (hlNames hl) of
                (nm : _) -> nm
                [] -> ""
              loop acc = do
                more <-
                  c_next_match
                    cursor
                    pixP
                    startsP
                    endsP
                    idsP
                    (fromIntegral maxCapturesPerMatch)
                    countP
                if more == 0
                  then pure (reverse acc)
                  else do
                    pix <- fromIntegral <$> Storable.peek pixP
                    n <- fromIntegral <$> Storable.peek countP
                    caps <-
                      Monad.forM [0 .. n - 1] $ \i -> do
                        s <- Storable.peekElemOff startsP i
                        e <- Storable.peekElemOff endsP i
                        cid <- Storable.peekElemOff idsP i
                        pure (fromIntegral s, fromIntegral e, cid)
                    let
                      preds = case drop pix (hlPreds hl) of
                        (p : _) -> p
                        [] -> []
                      withPattern (s, e, cid) = (s, e, cid, pix)
                      paintable (_, _, cid) = scopeAt cid /= ""
                      kept =
                        if all (evalPred source caps) preds
                          then map withPattern (filter paintable caps)
                          else []
                    loop (reverse kept ++ acc)
            loop []

evalPred :: BS.ByteString -> [(Int, Int, DataWord.Word32)] -> PredTest -> Bool
evalPred source caps predTest =
  let
    captureText cid =
      let
        isCapture (_, _, c) = c == cid
        slice (s, e, _) = BS.take (e - s) (BS.drop s source)
      in
        fmap slice (List.find isCapture caps)
  in
    case predTest of
      TestAlwaysPass -> True
      TestAlwaysFail -> False
      TestStrings negated cid candidates ->
        case captureText cid of
          Nothing -> False
          Just t -> (t `elem` candidates) /= negated
      TestCaptures negated a b ->
        case (captureText a, captureText b) of
          (Just ta, Just tb) -> (ta == tb) /= negated
          _ -> False

-- Walk each line char by char (tree-sitter speaks UTF-8 bytes, the buffer
-- speaks Text chars), grouping consecutive chars with the same capture.
renderLines :: IOArray.IOUArray Int DataWord.Word16 -> [T.Text] -> [T.Text] -> IO [[(T.Text, T.Text)]]
renderLines arr names ls =
  let
    scopeOf code =
      if code == 0
        then ""
        else case drop (fromIntegral code - 1) names of
          (n : _) -> n
          [] -> ""
    go _ [] = pure []
    go offset (l : rest) = do
      segs <- lineSegs offset l
      later <- go (offset + BS.length (TE.encodeUtf8 l) + 1) rest
      pure (segs : later)
    lineSegs offset l = do
      let
        chars = T.unpack l
        step ::
          (Int, String, T.Text, [(T.Text, String)]) ->
          Char ->
          IO (Int, String, T.Text, [(T.Text, String)])
        step (byte, cur, curScope, acc) ch = do
          code <- IOArray.readArray arr byte
          let
            scope = scopeOf code
            width = utf8Width ch
          if scope == curScope
            then pure (byte + width, ch : cur, curScope, acc)
            else
              pure
                ( byte + width
                , [ch]
                , scope
                , if null cur then acc else (curScope, reverse cur) : acc
                )
      (_, cur, curScope, acc) <-
        Monad.foldM step (offset, [], "", []) chars
      let
        final = if null cur then acc else (curScope, reverse cur) : acc
        toSeg (sc, cs) = (sc, T.pack cs)
      pure (map toSeg (reverse final))
  in
    go 0 ls

utf8Width :: Char -> Int
utf8Width c
  | fromEnum c < 0x80 = 1
  | fromEnum c < 0x800 = 2
  | fromEnum c < 0x10000 = 3
  | otherwise = 4
