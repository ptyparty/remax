{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_, unless)
import Data.Char (isControl, ord)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Numeric (showHex)
import System.Directory
  ( doesDirectoryExist
  , getCurrentDirectory
  , setCurrentDirectory
  )
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (isAbsolute, makeRelative, normalise, (</>))
import System.IO (hFlush, stdout)

import Paths_remax (version)
import Remax.Git (ReviewMode (..), branchRoot, refExists, repoRoot)
import Remax.Store
import Remax.UI (runReview)

data ModeArg = ArgHead | ArgBase Text | ArgBranch | ArgUnstaged | ArgUntracked

data Cmd
  = Review ModeArg
  | -- Nothing scopes to the current directory; a path need not still
    -- exist, which is the point — you forget a directory *because* it
    -- is gone.
    Forget (Maybe FilePath)
  | List Bool
  | Reply Text Text
  | ShowVersion

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> die err
    Right (mDir, cmd) -> do
      forM_ mDir $ \dir -> do
        there <- doesDirectoryExist dir
        unless there $
          die
            ( "No such directory: "
                ++ dir
                ++ "\nTo forget verdicts for a directory that is gone, run"
                ++ " remax forget "
                ++ dir
                ++ " from somewhere that still exists."
            )
        setCurrentDirectory dir
      case cmd of
        Review marg -> review marg
        Forget mPath -> forget mPath
        List asJson -> list asJson
        Reply key text -> reply key text
        ShowVersion -> putStrLn ("remax " ++ showVersion version)

usage :: String
usage =
  "usage: remax [-C DIR] [--base REF | --branch | --unstaged | --untracked]\n\
  \       remax [-C DIR] forget [PATH]  forget verdicts under PATH (default: the\n\
  \                                     current directory); PATH need not still exist\n\
  \       remax [-C DIR] list [--json]  show flagged and questioned lines\n\
  \       remax [-C DIR] reply KEY TEXT  answer a note; shows in the review session\n\
  \\n\
  \  -C DIR       run as if started in DIR\n\
  \  --base REF   review the working tree against REF (default: HEAD)\n\
  \  --branch     review against the merge-base with the mainline\n\
  \  --unstaged   review the working tree against the index\n\
  \  --untracked  review new files as pure additions"

parseArgs :: [String] -> Either String (Maybe FilePath, Cmd)
parseArgs args0 =
  let
    go mDir args =
      case args of
        ("-C" : dir : rest) -> go (Just dir) rest
        ["-C"] -> Left usage
        [] -> Right (mDir, Review ArgHead)
        ["--unstaged"] -> Right (mDir, Review ArgUnstaged)
        ["--untracked"] -> Right (mDir, Review ArgUntracked)
        ["--branch"] -> Right (mDir, Review ArgBranch)
        ["--base", ref] -> Right (mDir, Review (ArgBase (T.pack ref)))
        ["forget"] -> Right (mDir, Forget Nothing)
        ["forget", path] -> Right (mDir, Forget (Just path))
        ["list"] -> Right (mDir, List False)
        ["list", "--json"] -> Right (mDir, List True)
        ["--version"] -> Right (mDir, ShowVersion)
        ("reply" : key : rest)
          | not (null rest) ->
              Right (mDir, Reply (T.pack key) (T.pack (unwords rest)))
        _ -> Left usage
  in
    go Nothing args0

review :: ModeArg -> IO ()
review marg = do
  root <- requireRoot
  mode <- case marg of
    ArgHead -> pure (VsRef "HEAD")
    ArgUnstaged -> pure VsIndex
    ArgUntracked -> pure Untracked
    ArgBase ref -> do
      ok <- refExists root ref
      unless ok (die ("No such ref: " ++ T.unpack ref))
      pure (VsRef ref)
    ArgBranch -> do
      got <- branchRoot root
      case got of
        Left e -> die (T.unpack e)
        Right sha -> pure (VsRef (T.take 10 sha))
  runReview mode root

requireRoot :: IO FilePath
requireRoot = do
  got <- repoRoot
  case got of
    Just root -> pure root
    Nothing -> die "Not inside a git repository."

-- Forget the verdicts recorded for files under the current directory, so
-- those changes come up for review from scratch. At the top of the
-- repository that is everything recorded for it.
-- Verdicts outlive the files they were recorded against — that is the
-- whole design, since they are keyed by line content — so forgetting has
-- to be able to name a directory that is no longer there. The scope is
-- resolved by path arithmetic alone, never touching the filesystem, so a
-- deleted path scopes exactly as well as a live one.
forget :: Maybe FilePath -> IO ()
forget mPath = do
  root <- requireRoot
  here <- getCurrentDirectory
  conn <- openStore
  rows <- repoRows conn (T.pack root)
  let
    target = case mPath of
      Nothing -> here
      Just path
        | isAbsolute path -> normalise path
        | otherwise -> normalise (here </> path)
    relative = makeRelative root target
    scope = if relative == "." || relative == root then "" else T.pack relative
    -- by path component, as :accept matches: "gen" covers gen/a.txt but
    -- not generated.hs, and naming one file exactly forgets just it
    underScope (_, f) =
      T.null scope || f == scope || (scope <> "/") `T.isPrefixOf` f
    doomed = filter underScope rows
    files = nub (map snd doomed)
    described = if T.null scope then "the repository" else T.unpack scope
  if null doomed
    then putStrLn ("Nothing is recorded for the files under " ++ described ++ ".")
    else do
      putStrLn
        ( show (length doomed)
            ++ " line verdict(s) across "
            ++ show (length files)
            ++ " file(s) under "
            ++ described
            ++ "."
        )
      putStrLn "  Forgetting them means reviewing those changes again from scratch."
      putStr "  Forget them? [y/n]: "
      hFlush stdout
      answer <- getLine
      if answer == "y"
        then do
          deleteFiles conn (T.pack root) files
          putStrLn ("  \10003 forgot " ++ show (length doomed) ++ " line verdict(s)")
        else putStrLn "  left alone"

list :: Bool -> IO ()
list asJson = do
  root <- requireRoot
  conn <- openStore
  rows <- punchRows conn (T.pack root)
  if asJson
    then TIO.putStrLn (punchJson (T.pack root) rows)
    else
      if null rows
        then putStrLn "No flagged or questioned lines recorded for this repository."
        else forM_ rows $ \row -> do
          let
            position = case punchLineNo row of
              Just n -> ":" <> T.pack (show n)
              Nothing -> ""
          TIO.putStrLn
            ( punchVerdict row
                <> "  "
                <> punchFile row
                <> position
                <> "  ("
                <> punchAt row
                <> ")"
            )
          TIO.putStrLn ("    " <> punchLine row)
          forM_ (punchThread row) $ \post ->
            case T.lines (postBody post) of
              [] -> pure ()
              (firstLine : more) -> do
                let
                  label = postAuthor post <> ": "
                  indent = T.replicate (T.length label) " "
                TIO.putStrLn ("    " <> label <> firstLine)
                forM_ more (\l -> TIO.putStrLn ("    " <> indent <> l))
          forM_ (punchCommitSha row) $ \sha ->
            TIO.putStrLn
              ( "    commit: "
                  <> sha
                  <> " "
                  <> fromMaybe "" (punchCommitSubject row)
              )

-- An agent's post in a note conversation: KEY is the item's key from
-- `list --json`; the post appends to the whole thread's group and
-- renders in order, with its author, in a running session.
reply :: Text -> Text -> IO ()
reply key text = do
  root <- requireRoot
  conn <- openStore
  n <- appendAgentPost conn (T.pack root) "agent" key text
  if n == 0
    then die "No such key recorded for this repository."
    else putStrLn ("reply attached to " ++ show n ++ " line(s)")

-- JSON output ------------------------------------------------------------------

-- The punch list for machines (agents acting on review feedback). The
-- line text carries its +/- marker; line numbers are advisory positions
-- from when the verdict was recorded.
punchJson :: Text -> [PunchRow] -> Text
punchJson repo rows =
  let
    item row =
      jsonObject
        [ ("key", jsonString (punchKey row))
        , ("file", jsonString (punchFile row))
        , ("verdict", jsonString (punchVerdict row))
        , ("line", jsonString (punchLine row))
        , ("line_no", maybe "null" (T.pack . show) (punchLineNo row))
        , ("note", maybe "null" jsonString (openingNote row))
        , ("reply", maybe "null" jsonString (latestAnswer row))
        , ("thread", threadJson (punchThread row))
        , ("commit_sha", maybe "null" jsonString (punchCommitSha row))
        , ("commit_subject", maybe "null" jsonString (punchCommitSubject row))
        , ("updated_at", jsonString (punchAt row))
        ]
  in
    jsonObject
      [ ("repo", jsonString repo)
      , ("items", "[" <> T.intercalate ", " (map item rows) <> "]")
      ]

-- Compatibility views of the thread: the opening reviewer post is the
-- "note", the newest non-reviewer post the "reply".
openingNote :: PunchRow -> Maybe Text
openingNote row =
  case punchThread row of
    (post : _)
      | postAuthor post == "reviewer" -> Just (postBody post)
    _ -> Nothing

latestAnswer :: PunchRow -> Maybe Text
latestAnswer row =
  case filter (\p -> postAuthor p /= "reviewer") (reverse (punchThread row)) of
    (post : _) -> Just (postBody post)
    [] -> Nothing

threadJson :: [Post] -> Text
threadJson posts =
  let
    one post =
      jsonObject
        [ ("author", jsonString (postAuthor post))
        , ("body", jsonString (postBody post))
        ]
  in
    "[" <> T.intercalate ", " (map one posts) <> "]"

jsonObject :: [(Text, Text)] -> Text
jsonObject fields =
  let
    field (k, v) = jsonString k <> ": " <> v
  in
    "{" <> T.intercalate ", " (map field fields) <> "}"

jsonString :: Text -> Text
jsonString t =
  let
    escape c = case c of
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\t' -> "\\t"
      '\r' -> "\\r"
      _
        | isControl c ->
            let
              hex = showHex (ord c) ""
            in
              T.pack ("\\u" ++ replicate (4 - length hex) '0' ++ hex)
        | otherwise -> T.singleton c
  in
    "\"" <> T.concatMap escape t <> "\""
