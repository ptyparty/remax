{-# LANGUAGE OverloadedStrings #-}

module Remax.Git
  ( ReviewMode (..)
  , modeLabel
  , repoRoot
  , changedFiles
  , baseLines
  , looksBinary
  , refExists
  , mainlineRef
  , branchRoot
  , trackedChanges
  , hasStagedChanges
  , listFiles
  , listRefs
  , RebaseStatus (..)
  , rebaseStatus
  , commitMessage
  , gitStateWatchDirs
  , headCommit
  , runGit
  ) where

import qualified Data.ByteString as BS
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist)
import qualified System.Environment as Env
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, (</>))
import System.IO.Error (catchIOError)
import System.Process
  ( CreateProcess (env)
  , proc
  , readCreateProcessWithExitCode
  , readProcessWithExitCode
  )

-- What the working tree is reviewed against. The buffer on screen is
-- always the working file (that is what editing edits), so the mode picks
-- the base side of the diff:
--   VsRef ref — everything different from ref in tracked files
--               ("HEAD" is the classic uncommitted review; a merge-base
--               gives the whole-branch review)
--   VsIndex   — only what is not staged yet
--   Untracked — new files, reviewed as pure additions
data ReviewMode = VsRef Text | VsIndex | Untracked
  deriving (Eq, Show)

modeLabel :: ReviewMode -> Text
modeLabel mode =
  case mode of
    VsRef "HEAD" -> "uncommitted"
    VsRef ref -> "vs " <> ref
    VsIndex -> "unstaged"
    Untracked -> "untracked"

repoRoot :: IO (Maybe FilePath)
repoRoot = do
  (code, out, _) <-
    readProcessWithExitCode "git" ["rev-parse", "--show-toplevel"] ""
  pure $ case code of
    ExitSuccess -> Just (T.unpack (T.strip (T.pack out)))
    ExitFailure _ -> Nothing

changedFiles :: ReviewMode -> FilePath -> IO [FilePath]
changedFiles mode root = do
  let
    args = case mode of
      VsRef ref ->
        ["-C", root, "diff", "--name-only", "--diff-filter=d", "--no-color", T.unpack ref]
      VsIndex ->
        ["-C", root, "diff", "--name-only", "--diff-filter=d", "--no-color"]
      Untracked ->
        ["-C", root, "ls-files", "--others", "--exclude-standard"]
  (code, out, _) <- readProcessWithExitCode "git" args ""
  pure $ case code of
    ExitSuccess -> filter (not . null) (lines out)
    ExitFailure _ -> []

-- The base copy of one file. A file with no base copy (new file, or the
-- ref side is missing) reviews against nothing.
baseLines :: ReviewMode -> FilePath -> FilePath -> IO [Text]
baseLines mode root path =
  case mode of
    Untracked -> pure []
    _ -> do
      let
        ref = case mode of
          VsRef r -> T.unpack r ++ ":"
          _ -> ":"
      (code, out, _) <-
        readProcessWithExitCode "git" ["-C", root, "show", ref ++ path] ""
      pure $ case code of
        ExitSuccess -> T.lines (T.pack out)
        ExitFailure _ -> []

-- NUL in the first 8k is the same heuristic git uses.
looksBinary :: FilePath -> FilePath -> IO Bool
looksBinary root path = do
  bytes <- BS.readFile (root </> path)
  pure (BS.elem 0 (BS.take 8000 bytes))

-- Everything the repository considers a file worth seeing: tracked plus
-- untracked-unignored — the fuzzy finder's haystack.
listFiles :: FilePath -> IO [FilePath]
listFiles root = do
  (code, out, _) <-
    readProcessWithExitCode
      "git"
      ["-C", root, "ls-files", "--cached", "--others", "--exclude-standard"]
      ""
  pure $ case code of
    ExitSuccess -> filter (not . null) (lines out)
    ExitFailure _ -> []

-- Short ref names for completion: local branches first, then remotes.
listRefs :: FilePath -> IO [Text]
listRefs root = do
  (code, out, _) <-
    readProcessWithExitCode
      "git"
      [ "-C"
      , root
      , "for-each-ref"
      , "--format=%(refname:short)"
      , "refs/heads"
      , "refs/remotes"
      ]
      ""
  pure $ case code of
    ExitSuccess -> "HEAD" : filter (not . T.null) (T.lines (T.pack out))
    ExitFailure _ -> ["HEAD"]

-- Ref probing ---------------------------------------------------------------

refExists :: FilePath -> Text -> IO Bool
refExists root ref = do
  (code, _, _) <-
    readProcessWithExitCode
      "git"
      ["-C", root, "rev-parse", "--verify", "--quiet", T.unpack ref ++ "^{commit}"]
      ""
  pure (code == ExitSuccess)

-- The ref the repository treats as its mainline, for merge-base reviews.
mainlineRef :: FilePath -> IO (Maybe Text)
mainlineRef root =
  let
    go candidates =
      case candidates of
        [] -> pure Nothing
        (c : rest) -> do
          ok <- refExists root c
          if ok then pure (Just c) else go rest
  in
    go ["origin/main", "origin/master", "main", "master"]

-- Where the current branch forked from the mainline: the whole-branch
-- review base.
branchRoot :: FilePath -> IO (Either Text Text)
branchRoot root = do
  mMain <- mainlineRef root
  case mMain of
    Nothing -> pure (Left "no mainline ref found (origin/main, origin/master, main, master)")
    Just mainline -> do
      (code, out, err) <-
        readProcessWithExitCode
          "git"
          ["-C", root, "merge-base", "HEAD", T.unpack mainline]
          ""
      pure $ case code of
        ExitSuccess -> Right (T.strip (T.pack out))
        ExitFailure _ -> Left (T.strip (T.pack err))

-- Whether any tracked file differs from HEAD or the index — the guard
-- before starting a rebase.
trackedChanges :: FilePath -> IO Bool
trackedChanges root = do
  (code, out, _) <-
    readProcessWithExitCode
      "git"
      ["-C", root, "status", "--porcelain", "--untracked-files=no"]
      ""
  pure $ case code of
    ExitSuccess -> not (null (filter (not . null) (lines out)))
    ExitFailure _ -> True

-- Where git actually writes its state, for the filesystem watcher: in a
-- linked worktree the root's .git is a file and index/HEAD live under
-- the main repository's .git/worktrees/<name>/, with shared refs in the
-- common dir. Watching the literal root/.git there watches nothing.
gitStateWatchDirs :: FilePath -> IO [FilePath]
gitStateWatchDirs root = do
  let
    resolve name = do
      (code, out, _) <-
        readProcessWithExitCode
          "git"
          ["-C", root, "rev-parse", "--path-format=absolute", name]
          ""
      pure $ case code of
        ExitSuccess ->
          let
            path = T.unpack (T.strip (T.pack out))
          in
            if null path then [] else [path]
        ExitFailure _ -> []
  gitDir <- resolve "--git-dir"
  commonDir <- resolve "--git-common-dir"
  let
    dirs = gitDir ++ filter (`notElem` gitDir) commonDir
  pure $ case dirs of
    [] -> [root </> ".git"]
    _ -> dirs

-- Whether anything is staged — the guard before an --amend that would
-- otherwise be a pointless no-op (or an error on an empty index).
hasStagedChanges :: FilePath -> IO Bool
hasStagedChanges root = do
  (code, _, _) <-
    readProcessWithExitCode
      "git"
      ["-C", root, "diff", "--cached", "--quiet"]
      ""
  pure (code /= ExitSuccess)

-- Rebase awareness ------------------------------------------------------------

-- A rebase paused mid-flight: which commit it stopped at (counting only
-- the picks, not interleaved exec lines), its subject, and whether the
-- stop is a real conflict rather than a deliberate pause.
data RebaseStatus = RebaseStatus
  { rebaseStep :: Int
  , rebaseTotal :: Int
  , rebaseSha :: Text -- short sha of the stopped commit ("" when unknown)
  , rebaseSubject :: Text
  , rebaseConflicted :: Bool
  }
  deriving (Eq, Show)

-- Where git actually keeps a state directory, resolved by git itself: in
-- a linked worktree .git is a file and the state lives under the main
-- repository's .git/worktrees/<name>/, so a literal .git/<name> path
-- misses it.
gitStateDir :: FilePath -> String -> IO (Maybe FilePath)
gitStateDir root name = do
  (code, out, _) <-
    readProcessWithExitCode "git" ["-C", root, "rev-parse", "--git-path", name] ""
  case code of
    ExitFailure _ -> pure Nothing
    ExitSuccess -> do
      let
        raw = T.unpack (T.strip (T.pack out))
        path = if isAbsolute raw then raw else root </> raw
      present <- doesDirectoryExist path
      pure (if present then Just path else Nothing)

hasUnmerged :: FilePath -> IO Bool
hasUnmerged root = do
  (code, out, _) <-
    readProcessWithExitCode "git" ["-C", root, "ls-files", "--unmerged"] ""
  pure (code == ExitSuccess && not (null (filter (not . null) (lines out))))

rebaseStatus :: FilePath -> IO (Maybe RebaseStatus)
rebaseStatus root = do
  mMerge <- gitStateDir root "rebase-merge"
  case mMerge of
    Just dir -> do
      done <- readFileOr "" (dir </> "done")
      todo <- readFileOr "" (dir </> "git-rebase-todo")
      let
        pickWords = ["pick", "p", "edit", "e", "reword", "r", "fixup", "f", "squash", "s"]
        pickOf l = case T.words l of
          (verb : pickSha : rest)
            | verb `elem` pickWords -> Just (pickSha, T.unwords rest)
          _ -> Nothing
        donePicks = mapMaybe pickOf (T.lines done)
        todoPicks = mapMaybe pickOf (T.lines todo)
        (sha, subject) = case reverse donePicks of
          (p1 : _) -> p1
          [] -> ("", "")
      conflicted <- hasUnmerged root
      pure . Just $
        RebaseStatus
          { rebaseStep = length donePicks
          , rebaseTotal = length donePicks + length todoPicks
          , rebaseSha = sha
          , rebaseSubject = subject
          , rebaseConflicted = conflicted
          }
    Nothing -> do
      -- the am-based backend (git rebase --apply, git am): no todo, just
      -- patch counters
      mApply <- gitStateDir root "rebase-apply"
      case mApply of
        Nothing -> pure Nothing
        Just dir -> do
          nextT <- readFileOr "0" (dir </> "next")
          lastT <- readFileOr "0" (dir </> "last")
          let
            num t = case reads (T.unpack (T.strip t)) of
              [(n, "")] -> n
              _ -> 0
          conflicted <- hasUnmerged root
          pure . Just $
            RebaseStatus
              { rebaseStep = num nextT
              , rebaseTotal = num lastT
              , rebaseSha = ""
              , rebaseSubject = ""
              , rebaseConflicted = conflicted
              }

readFileOr :: Text -> FilePath -> IO Text
readFileOr fallback path =
  catchIOError (fmap T.pack (readFile path)) (const (pure fallback))

-- The full message (with author and date) of one commit, for the
-- commit-under-review popup.
commitMessage :: FilePath -> Text -> IO (Either Text Text)
commitMessage root ref = do
  (code, out, err) <-
    readProcessWithExitCode
      "git"
      [ "-C"
      , root
      , "show"
      , "-s"
      , "--format=commit %H%nAuthor: %an <%ae>%nDate:   %ad%n%n%B"
      , T.unpack ref
      ]
      ""
  pure $ case code of
    ExitSuccess -> Right (T.strip (T.pack out))
    ExitFailure _ -> Left (T.strip (T.pack err))

-- The short sha and subject of HEAD — during a rebase stop, that is the
-- commit under review, so verdicts can carry their commit.
headCommit :: FilePath -> IO (Maybe (Text, Text))
headCommit root = do
  (code, out, _) <-
    readProcessWithExitCode
      "git"
      ["-C", root, "log", "-1", "--format=%h%x09%s"]
      ""
  pure $ case code of
    ExitFailure _ -> Nothing
    ExitSuccess ->
      case T.breakOn "\t" (T.strip (T.pack out)) of
        (sha, rest)
          | T.null sha -> Nothing
          | otherwise -> Just (sha, T.drop 1 rest)

-- Git actions -------------------------------------------------------------------

-- Run a git command for its effect, reporting failure as the (trimmed)
-- stderr so the UI can show it. GIT_EDITOR is forced non-interactive:
-- these run underneath a live TUI, where an editor popping up would
-- wedge the terminal.
runGit :: FilePath -> [String] -> IO (Either Text ())
runGit root args = do
  environment <- Env.getEnvironment
  let
    p =
      (proc "git" (["-C", root] ++ args))
        { env = Just (("GIT_EDITOR", "true") : filter ((/= "GIT_EDITOR") . fst) environment)
        }
  (code, _, err) <- readCreateProcessWithExitCode p ""
  pure $ case code of
    ExitSuccess -> Right ()
    ExitFailure _ ->
      let
        msg = T.strip (T.pack err)
      in
        Left (if T.null msg then "git " <> T.pack (unwords args) <> " failed" else msg)
