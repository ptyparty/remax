{-# LANGUAGE OverloadedStrings #-}

module Remax.Git
  ( ReviewMode (..)
  , RefReview (..)
  , Subject (..)
  , subjectOf
  , Writable
  , writable
  , subjectPlace
  , subjectText
  , sameCommitSubjects
  , modeLabel
  , modeToText
  , modeFromText
  , repoRoot
  , changedFiles
  , reviewFiles
  , baseLines
  , RefContent (..)
  , refContent
  , looksBinary
  , refExists
  , readFileOr
  , mainlineRef
  , branchRoot
  , branchRootOf
  , trackedChanges
  , hasStagedChanges
  , FileScope (..)
  , buildOutputDirs
  , listFiles
  , listFilesUnder
  , isIgnored
  , isTracked
  , listRefs
  , childRepos
  , PathChange (..)
  , PathStat (..)
  , pathChangeLetter
  , pathStats
  , RebaseStatus (..)
  , rebaseStatus
  , commitBody
  , CommitUnderReview (..)
  , commitUnderReview
  , setStoppedCommitMessage
  , commitMessage
  , gitStateWatchDirs
  , headCommit
  , commitAt
  , commitParents
  , resolveShort
  , isSymbolicRef
  , emptyTreeRef
  , pickableRefs
  , recentCommits
  , branchCommits
  , currentBranch
  , localBranches
  , runGit
  ) where

import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Text.Read qualified as TR
import System.Directory qualified as Directory
import System.Environment qualified as Env
import System.Exit qualified as Exit
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import System.IO qualified as SysIO
import System.IO.Error qualified as IOError
import System.Process qualified as Process

-- What is reviewed against what. The buffer on screen is the working
-- file (that is what editing edits) in every mode but AtRef, where it is
-- the file as the ref holds it and nothing can be edited; the mode picks
-- the base side of the diff:
--   VsRef ref — everything different from ref in tracked files
--               ("HEAD" is the classic uncommitted review; a merge-base
--               gives the whole-branch review)
--   VsIndex   — only what is not staged yet
--   Untracked — new files, reviewed as pure additions
data ReviewMode = VsRef T.Text | VsIndex | Untracked | AtRef RefReview
  deriving (Eq, Show)

data RefReview = RefReview
  { rrTip :: T.Text
  , rrBase :: T.Text
  }
  deriving (Eq, Show)

data Subject = WorkingTree | Ref T.Text
  deriving (Eq, Show)

subjectOf :: ReviewMode -> Subject
subjectOf mode =
  case mode of
    AtRef rr -> Ref (rrTip rr)
    _ -> WorkingTree

data Writable = Writable

writable :: ReviewMode -> Either T.Text Writable
writable mode =
  case subjectOf mode of
    WorkingTree -> Right Writable
    Ref tip -> Left tip

subjectPlace :: ReviewMode -> FilePath -> IO (Maybe T.Text, Maybe (T.Text, T.Text))
subjectPlace mode root =
  case subjectOf mode of
    WorkingTree -> do
      branch <- currentBranch root
      tip <- headCommit root
      pure (branch, tip)
    Ref tip -> do
      at <- commitAt root tip
      pure (Just tip, at)

subjectText :: ReviewMode -> T.Text
subjectText mode =
  case subjectOf mode of
    WorkingTree -> "worktree"
    Ref tip -> tip

modeToText :: ReviewMode -> T.Text
modeToText mode =
  case mode of
    VsRef ref -> "vs " <> ref
    VsIndex -> "unstaged"
    Untracked -> "untracked"
    AtRef rr -> "at " <> rrTip rr <> " vs " <> rrBase rr

modeFromText :: T.Text -> Maybe ReviewMode
modeFromText t =
  case T.words t of
    ["vs", ref] -> Just (VsRef ref)
    ["unstaged"] -> Just VsIndex
    ["untracked"] -> Just Untracked
    ["at", tip, "vs", base] -> Just (AtRef (RefReview {rrTip = tip, rrBase = base}))
    _ -> Nothing

modeLabel :: ReviewMode -> T.Text
modeLabel mode =
  case mode of
    VsRef "HEAD" -> "uncommitted"
    VsRef ref -> "vs " <> ref
    VsIndex -> "unstaged"
    Untracked -> "untracked"
    AtRef rr
      | rrBase rr == emptyTreeRef -> rrTip rr <> " vs nothing"
      | otherwise -> rrTip rr <> " vs " <> rrBase rr

repoRoot :: IO (Maybe FilePath)
repoRoot = do
  (code, out, _) <-
    Process.readProcessWithExitCode "git" ["rev-parse", "--show-toplevel"] ""
  pure $ case code of
    Exit.ExitSuccess -> Just (T.unpack (T.strip (T.pack out)))
    Exit.ExitFailure _ -> Nothing

-- Every path the change set covers, deletions included: a file that was
-- removed is a change like any other and has to be reviewable, so no
-- --diff-filter culls it here. Its working copy is gone, which the
-- loader recognizes and reviews as an all-removed file.
changedFiles :: ReviewMode -> FilePath -> IO [FilePath]
changedFiles mode root = do
  let
    args = case mode of
      VsRef ref ->
        ["-C", root, "diff", "--name-only", "--no-color", T.unpack ref]
      VsIndex ->
        ["-C", root, "diff", "--name-only", "--no-color"]
      Untracked ->
        ["-C", root, "ls-files", "--others", "--exclude-standard"]
      AtRef rr ->
        ["-C", root, "diff", "--name-only", "--no-color", T.unpack (rrBase rr), T.unpack (rrTip rr)]
  (code, out, _) <- Process.readProcessWithExitCode "git" args ""
  pure $ case code of
    Exit.ExitSuccess -> filter (not . null) (lines out)
    Exit.ExitFailure _ -> []

-- The files a review puts on screen: what git reports as changed, plus
-- the ones that are new.
--
-- Deliberately not folded into changedFiles, because one caller must not
-- have it. amendHead folds that list into a commit, and sweeping a
-- brand-new scratch file into an amend is not something a review tool
-- should do quietly.
--
-- New files need nothing else to render: baseLines answers [] when git
-- cannot find the path, so one reviews as a pure addition, which is what
-- Untracked mode has always shown.
reviewFiles :: ReviewMode -> Bool -> FilePath -> IO [FilePath]
reviewFiles mode withNew root = do
  changed <- changedFiles mode root
  let
    -- Untracked mode is already nothing but new files
    wanted = withNew && mode /= Untracked && subjectOf mode == WorkingTree
  if not wanted
    then pure changed
    else do
      new <- untrackedFiles root
      let
        known = Set.fromList changed
      -- changed first, so a review opens where it did before and the new
      -- ones arrive after rather than shuffling the file order
      pure (changed ++ filter (`Set.notMember` known) new)

-- The base copy of one file. A file with no base copy (new file, or the
-- ref side is missing) reviews against nothing.
baseLines :: ReviewMode -> FilePath -> FilePath -> IO [T.Text]
baseLines mode root path =
  case mode of
    Untracked -> pure []
    _ -> do
      let
        ref = case mode of
          VsRef r -> T.unpack r ++ ":"
          AtRef rr -> T.unpack (rrBase rr) ++ ":"
          _ -> ":"
      (code, out, _) <-
        Process.readProcessWithExitCode "git" ["-C", root, "show", ref ++ path] ""
      pure $ case code of
        Exit.ExitSuccess -> T.lines (T.pack out)
        Exit.ExitFailure _ -> []

data RefContent = RefBinary | RefText [T.Text]
  deriving (Eq, Show)

refContent :: FilePath -> T.Text -> FilePath -> IO (Maybe RefContent)
refContent root ref path = do
  let
    spec = Process.proc "git" ["-C", root, "show", T.unpack ref ++ ":" ++ path]
  (bytes, code) <-
    SysIO.withFile "/dev/null" SysIO.WriteMode $ \devNull -> do
      (_, mOut, _, ph) <-
        Process.createProcess
          spec {Process.std_out = Process.CreatePipe, Process.std_err = Process.UseHandle devNull}
      out <- maybe (pure BS.empty) BS.hGetContents mOut
      done <- Process.waitForProcess ph
      pure (out, done)
  pure $ case code of
    Exit.ExitFailure _ -> Nothing
    Exit.ExitSuccess
      | BS.elem 0 (BS.take 8000 bytes) -> Just RefBinary
      | otherwise -> Just (RefText (T.lines (TE.decodeUtf8With TEE.lenientDecode bytes)))

-- NUL in the first 8k is the same heuristic git uses. Only those 8k are
-- read: the caller reads the whole file straight afterwards, and pulling
-- forty megabytes off disk to look at the first page of it is a cost the
-- largest files paid twice.
looksBinary :: FilePath -> FilePath -> IO Bool
looksBinary root path =
  SysIO.withFile (root </> path) SysIO.ReadMode $ \h -> do
    bytes <- BS.hGet h 8000
    pure (BS.elem 0 bytes)

-- Whether a listing stops at the files git is willing to talk about, or
-- takes in the ignored ones too. Browsing is not reviewing: a build
-- directory has no business in the change set, and every so often it is
-- exactly the file you need to open.
data FileScope = VisibleFiles | AllFiles
  deriving (Eq, Show)

-- Everything the repository considers a file worth seeing: tracked plus
-- untracked-unignored — the fuzzy finder's haystack. Under AllFiles the
-- exclusions come off and the ignored files come with it.
-- Directories that hold build output rather than anything anybody wrote.
--
-- Only ever applied to untracked files, where gitignore rules apply, so
-- a project that genuinely keeps source in one of these still has it
-- listed — git reports tracked files whatever the exclusions say.
buildOutputDirs :: [String]
buildOutputDirs =
  [ ".git/"
  , ".stack-work/"
  , "dist-newstyle/"
  , "node_modules/"
  , ".spago/"
  , "target/"
  , ".venv/"
  , "__pycache__/"
  ]

listFiles :: FileScope -> FilePath -> IO [FilePath]
listFiles scope root = listFilesUnder scope root []

listFilesUnder :: FileScope -> FilePath -> [FilePath] -> IO [FilePath]
listFilesUnder scope root paths = do
  let
    within = case paths of
      [] -> []
      _ -> "--" : paths
    excluding = case scope of
      VisibleFiles -> ["--exclude-standard"]
      -- Showing the ignored files is for finding the ones somebody might
      -- open — a .env, a local config, something generated worth a look —
      -- and not for enumerating a build directory. In a real checkout
      -- that is the difference between twenty-six thousand paths and
      -- three hundred and forty thousand, and git never descends into
      -- them rather than listing them for us to throw away.
      AllFiles -> concatMap (\d -> ["-x", d]) buildOutputDirs
  (code, out, _) <-
    Process.readProcessWithExitCode
      "git"
      (["-C", root, "ls-files", "--cached", "--others"] ++ excluding ++ within)
      ""
  pure $ case code of
    Exit.ExitSuccess -> filter (not . null) (lines out)
    Exit.ExitFailure _ -> []

-- Whether git has this path in the index — for a directory, whether it
-- holds anything git knows about. It decides whether a move or a removal
-- goes through git or straight to the filesystem: a tracked rename told
-- to git stays a rename, while the same move done behind its back reads
-- as an unrelated delete and add until someone stages it.
isTracked :: FilePath -> FilePath -> IO Bool
isTracked root path = do
  (code, out, _) <-
    Process.readProcessWithExitCode "git" ["-C", root, "ls-files", "--", path] ""
  pure $ case code of
    Exit.ExitSuccess -> any (not . null) (lines out)
    Exit.ExitFailure _ -> False

isIgnored :: FilePath -> FilePath -> IO Bool
isIgnored root path = do
  (code, _, _) <-
    Process.readProcessWithExitCode
      "git"
      ["-C", root, "check-ignore", "-q", "--", path]
      ""
  pure (code == Exit.ExitSuccess)

-- Short ref names for completion: local branches first, then remotes.
listRefs :: FilePath -> IO [T.Text]
listRefs root = do
  (code, out, _) <-
    Process.readProcessWithExitCode
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
    Exit.ExitSuccess -> "HEAD" : filter (not . T.null) (T.lines (T.pack out))
    Exit.ExitFailure _ -> ["HEAD"]

-- Sibling repositories --------------------------------------------------------

-- Git checkouts directly beneath a directory, sorted. A story directory
-- is not itself a repository — it is a folder of them, one per repo the
-- story touches — so a session started there has to find its subjects
-- rather than refuse.
--
-- Depth one only: deeper would wander into vendored trees and build
-- directories, and the layouts this serves put the checkouts side by
-- side anyway.
childRepos :: FilePath -> IO [FilePath]
childRepos dir = do
  entries <- listDirectoryOr dir
  unreachable <- blockingMountPoints
  let
    reachable = filter (`Set.notMember` unreachable) (List.sort entries)
  found <- mapM asRepo reachable
  pure (concat found)

blockingMountPoints :: IO (Set.Set FilePath)
blockingMountPoints = do
  table <- readFileOr "" "/proc/self/mountinfo"
  let
    entry line =
      case break (== "-") (words (T.unpack line)) of
        (before, "-" : fstype : _)
          | length before >= 5 && blockingFilesystem fstype -> Just (before !! 4)
        _ -> Nothing
  pure (Set.fromList (Maybe.mapMaybe entry (T.lines table)))

blockingFilesystem :: String -> Bool
blockingFilesystem fstype =
  any (`List.isPrefixOf` fstype) ["fuse", "nfs", "cifs", "smb", "9p", "autofs", "sshfs"]

asRepo :: FilePath -> IO [FilePath]
asRepo path = do
  -- .git is a directory in a normal clone and a file in a linked
  -- worktree, which is most of what these directories hold
  isDir <- Directory.doesDirectoryExist (path </> ".git")
  isFile <- Directory.doesFileExist (path </> ".git")
  pure [path | isDir || isFile]

listDirectoryOr :: FilePath -> IO [FilePath]
listDirectoryOr dir =
  IOError.catchIOError
    (fmap (map (dir </>)) (Directory.listDirectory dir))
    (const (pure []))

-- Per-path change shape ------------------------------------------------------

-- What happened to a path, as git classifies it. A rename and a copy
-- carry where they came from, which is the whole point of asking git
-- with -M: without it a move is an unrelated delete and add.
data PathChange
  = PathAdded
  | PathModified
  | PathDeleted
  | PathRenamed FilePath
  | PathCopied FilePath
  | PathUntracked
  | PathOtherChange
  deriving (Eq, Show)

-- Counts are Nothing for a binary file, where git reports "-" rather
-- than a number, and for an untracked file, where getting a real count
-- would mean a diff against /dev/null per file.
data PathStat = PathStat
  { psChange :: PathChange
  , psCounts :: Maybe (Int, Int)
  }
  deriving (Eq, Show)

pathChangeLetter :: PathChange -> T.Text
pathChangeLetter change =
  case change of
    PathAdded -> "A"
    PathModified -> "M"
    PathDeleted -> "D"
    PathRenamed _ -> "R"
    PathCopied _ -> "C"
    PathUntracked -> "?"
    PathOtherChange -> "\183"

-- Status and line counts for every path the mode considers changed, plus
-- the untracked files, which no diff reports.
--
-- Two calls, both -z: without it a rename comes back brace-compressed as
-- "src/{Old.hs => New.hs}", which is miserable to parse. With -z a
-- rename is instead a token followed by two NUL-separated paths, in both
-- formats:
--
--   --name-status  "R100\0src/Old.hs\0src/New.hs\0"
--   --numstat      "4\t2\t\0src/Old.hs\0src/New.hs\0"   (empty path field)
pathStats :: ReviewMode -> FilePath -> IO (Map.Map FilePath PathStat)
pathStats mode root = do
  others <- untrackedFiles root
  let
    untracked =
      Map.fromList (map (\p -> (p, PathStat PathUntracked Nothing)) others)
  case mode of
    Untracked -> pure untracked
    _ -> do
      let
        refArgs = case mode of
          VsRef ref -> [T.unpack ref]
          AtRef rr -> [T.unpack (rrBase rr), T.unpack (rrTip rr)]
          _ -> []
        newFiles = case subjectOf mode of
          WorkingTree -> untracked
          Ref _ -> Map.empty
        diffFields kind = do
          (code, out, _) <-
            Process.readProcessWithExitCode
              "git"
              (["-C", root, "diff", kind, "-M", "-z"] ++ refArgs)
              ""
          pure $ case code of
            Exit.ExitSuccess -> nulFields (T.pack out)
            Exit.ExitFailure _ -> []
      changes <- fmap parseNameStatus (diffFields "--name-status")
      counts <- fmap parseNumstat (diffFields "--numstat")
      let
        stat path change =
          PathStat change (Map.findWithDefault Nothing path counts)
      pure (Map.union (Map.mapWithKey stat changes) newFiles)

untrackedFiles :: FilePath -> IO [FilePath]
untrackedFiles root = do
  (code, out, _) <-
    Process.readProcessWithExitCode
      "git"
      ["-C", root, "ls-files", "--others", "--exclude-standard"]
      ""
  pure $ case code of
    Exit.ExitSuccess -> filter (not . null) (lines out)
    Exit.ExitFailure _ -> []

nulFields :: T.Text -> [T.Text]
nulFields = filter (not . T.null) . T.splitOn "\NUL"

parseNameStatus :: [T.Text] -> Map.Map FilePath PathChange
parseNameStatus fields =
  case fields of
    [] -> Map.empty
    (token : rest)
      | pairedToken token ->
          case rest of
            (from : to : more) ->
              Map.insert
                (T.unpack to)
                (pairedChange token (T.unpack from))
                (parseNameStatus more)
            _ -> Map.empty
      | otherwise ->
          case rest of
            (path : more) ->
              Map.insert (T.unpack path) (letterChange token) (parseNameStatus more)
            _ -> Map.empty

-- R and C are the two statuses that name a source as well as a target.
pairedToken :: T.Text -> Bool
pairedToken token = T.isPrefixOf "R" token || T.isPrefixOf "C" token

pairedChange :: T.Text -> FilePath -> PathChange
pairedChange token from =
  case T.take 1 token of
    "C" -> PathCopied from
    _ -> PathRenamed from

letterChange :: T.Text -> PathChange
letterChange token =
  case T.take 1 token of
    "A" -> PathAdded
    "M" -> PathModified
    "D" -> PathDeleted
    _ -> PathOtherChange

parseNumstat :: [T.Text] -> Map.Map FilePath (Maybe (Int, Int))
parseNumstat fields =
  case fields of
    [] -> Map.empty
    (record : rest) ->
      case T.splitOn "\t" record of
        [added, removed, path]
          | T.null path ->
              -- a rename: the paths follow as their own NUL fields
              case rest of
                (_from : to : more) ->
                  Map.insert
                    (T.unpack to)
                    (numstatCounts added removed)
                    (parseNumstat more)
                _ -> Map.empty
          | otherwise ->
              Map.insert
                (T.unpack path)
                (numstatCounts added removed)
                (parseNumstat rest)
        _ -> parseNumstat rest

-- git writes "-" for both sides of a binary file.
numstatCounts :: T.Text -> T.Text -> Maybe (Int, Int)
numstatCounts added removed = do
  a <- readCount added
  r <- readCount removed
  pure (a, r)

readCount :: T.Text -> Maybe Int
readCount t =
  case TR.decimal t of
    Right (n, rest)
      | T.null rest -> Just n
    _ -> Nothing

-- Ref probing ---------------------------------------------------------------

refExists :: FilePath -> T.Text -> IO Bool
refExists root ref = do
  (code, _, _) <-
    Process.readProcessWithExitCode
      "git"
      ["-C", root, "rev-parse", "--verify", "--quiet", T.unpack ref ++ "^{commit}"]
      ""
  pure (code == Exit.ExitSuccess)

-- The ref the repository treats as its mainline, for merge-base reviews.
mainlineRef :: FilePath -> IO (Maybe T.Text)
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
branchRoot :: FilePath -> IO (Either T.Text T.Text)
branchRoot root = branchRootOf root "HEAD"

branchRootOf :: FilePath -> T.Text -> IO (Either T.Text T.Text)
branchRootOf root tip = do
  mMain <- mainlineRef root
  case mMain of
    Nothing -> pure (Left "no mainline ref found (origin/main, origin/master, main, master)")
    Just mainline -> do
      (code, out, err) <-
        Process.readProcessWithExitCode
          "git"
          ["-C", root, "merge-base", T.unpack tip, T.unpack mainline]
          ""
      pure $ case code of
        Exit.ExitSuccess -> Right (T.strip (T.pack out))
        Exit.ExitFailure _ -> Left (T.strip (T.pack err))

-- Whether any tracked file differs from HEAD or the index — the guard
-- before starting a rebase.
trackedChanges :: FilePath -> IO Bool
trackedChanges root = do
  (code, out, _) <-
    Process.readProcessWithExitCode
      "git"
      ["-C", root, "status", "--porcelain", "--untracked-files=no"]
      ""
  pure $ case code of
    Exit.ExitSuccess -> not (null (filter (not . null) (lines out)))
    Exit.ExitFailure _ -> True

-- Where git actually writes its state, for the filesystem watcher: in a
-- linked worktree the root's .git is a file and index/HEAD live under
-- the main repository's .git/worktrees/<name>/, with shared refs in the
-- common dir. Watching the literal root/.git there watches nothing.
gitStateWatchDirs :: FilePath -> IO [FilePath]
gitStateWatchDirs root = do
  let
    resolve name = do
      (code, out, _) <-
        Process.readProcessWithExitCode
          "git"
          ["-C", root, "rev-parse", "--path-format=absolute", name]
          ""
      pure $ case code of
        Exit.ExitSuccess ->
          let
            path = T.unpack (T.strip (T.pack out))
          in
            if null path then [] else [path]
        Exit.ExitFailure _ -> []
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
    Process.readProcessWithExitCode
      "git"
      ["-C", root, "diff", "--cached", "--quiet"]
      ""
  pure (code /= Exit.ExitSuccess)

-- Rebase awareness ------------------------------------------------------------

-- A rebase paused mid-flight: which commit it stopped at (counting only
-- the picks, not interleaved exec lines), its subject, and whether the
-- stop is a real conflict rather than a deliberate pause.
data RebaseStatus = RebaseStatus
  { rebaseStep :: Int
  , rebaseTotal :: Int
  , rebaseSha :: T.Text -- short sha of the stopped commit ("" when unknown)
  , rebaseSubject :: T.Text
  , rebaseConflicted :: Bool
  }
  deriving (Eq, Show)

-- Where git actually keeps a state directory, resolved by git itself: in
-- a linked worktree .git is a file and the state lives under the main
-- repository's .git/worktrees/<name>/, so a literal .git/<name> path
-- misses it.
gitStateDir :: FilePath -> String -> IO (Maybe FilePath)
gitStateDir = gitStatePath Directory.doesDirectoryExist

gitStateFile :: FilePath -> String -> IO (Maybe FilePath)
gitStateFile = gitStatePath Directory.doesFileExist

gitStatePath :: (FilePath -> IO Bool) -> FilePath -> String -> IO (Maybe FilePath)
gitStatePath exists root name = do
  (code, out, _) <-
    Process.readProcessWithExitCode "git" ["-C", root, "rev-parse", "--git-path", name] ""
  case code of
    Exit.ExitFailure _ -> pure Nothing
    Exit.ExitSuccess -> do
      let
        raw = T.unpack (T.strip (T.pack out))
        path = if FilePath.isAbsolute raw then raw else root </> raw
      present <- exists path
      pure (if present then Just path else Nothing)

hasUnmerged :: FilePath -> IO Bool
hasUnmerged root = do
  (code, out, _) <-
    Process.readProcessWithExitCode "git" ["-C", root, "ls-files", "--unmerged"] ""
  pure (code == Exit.ExitSuccess && not (null (filter (not . null) (lines out))))

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
        donePicks = Maybe.mapMaybe pickOf (T.lines done)
        todoPicks = Maybe.mapMaybe pickOf (T.lines todo)
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

readFileOr :: T.Text -> FilePath -> IO T.Text
readFileOr fallback path =
  IOError.catchIOError
    (fmap (TE.decodeUtf8With TEE.lenientDecode) (BS.readFile path))
    (const (pure fallback))

-- The full message (with author and date) of one commit, for the
-- commit-under-review popup.
-- Just the message, for the buffer that edits it. commitMessage's
-- decorated form belongs in the read-only popup; amending with it writes
-- the sha, author and date into the new message.
commitBody :: FilePath -> T.Text -> IO (Either T.Text T.Text)
commitBody root ref = do
  (code, out, err) <-
    Process.readProcessWithExitCode
      "git"
      ["-C", root, "show", "-s", "--format=%B", T.unpack ref]
      ""
  pure $ case code of
    Exit.ExitSuccess -> Right (T.stripEnd (T.pack out))
    Exit.ExitFailure _ -> Left (T.strip (T.pack err))

data CommitUnderReview
  = CommitAtHead
  | CommitPending (Maybe T.Text)

commitUnderReview :: FilePath -> IO CommitUnderReview
commitUnderReview root = do
  stop <- rebaseStop root
  case stop of
    StopCommitted -> pure CommitAtHead
    StopPending mFile -> CommitPending <$> traverse (fmap T.stripEnd . readFileOr "") mFile

data RebaseStop
  = StopCommitted
  | StopPending (Maybe FilePath)

rebaseStop :: FilePath -> IO RebaseStop
rebaseStop root = do
  mApply <- gitStateDir root "rebase-apply"
  mMerge <- gitStateDir root "rebase-merge"
  mMergeMsg <- gitStateFile root "MERGE_MSG"
  case (mApply, mMerge, mMergeMsg) of
    (Just _, _, _) -> pure (StopPending Nothing)
    (Nothing, Just dir, Just _) -> do
      let
        file = dir </> "message"
      present <- Directory.doesFileExist file
      pure (StopPending (if present then Just file else Nothing))
    _ -> pure StopCommitted

setStoppedCommitMessage :: Writable -> FilePath -> T.Text -> IO (Either T.Text ())
setStoppedCommitMessage _ root text = do
  mFile <- stoppedMessageFile root
  case mFile of
    Nothing -> pure (Left "the stopped commit is no longer pending — amend cancelled")
    Just file -> do
      res <- IOError.tryIOError (BS.writeFile file (TE.encodeUtf8 (text <> "\n")))
      pure $ case res of
        Left e -> Left (T.pack (show e))
        Right () -> Right ()

stoppedMessageFile :: FilePath -> IO (Maybe FilePath)
stoppedMessageFile root = do
  stop <- rebaseStop root
  pure $ case stop of
    StopPending mFile -> mFile
    StopCommitted -> Nothing

commitMessage :: FilePath -> T.Text -> IO (Either T.Text T.Text)
commitMessage root ref = do
  (code, out, err) <-
    Process.readProcessWithExitCode
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
    Exit.ExitSuccess -> Right (T.strip (T.pack out))
    Exit.ExitFailure _ -> Left (T.strip (T.pack err))

-- The short sha and subject of HEAD — during a rebase stop, that is the
-- commit under review, so verdicts can carry their commit.
-- The branch HEAD points at, or Nothing when it is detached.
currentBranch :: FilePath -> IO (Maybe T.Text)
currentBranch root = do
  (code, out, _) <-
    Process.readProcessWithExitCode
      "git"
      ["-C", root, "symbolic-ref", "--short", "-q", "HEAD"]
      ""
  pure $ case code of
    Exit.ExitFailure _ -> Nothing
    Exit.ExitSuccess ->
      case T.strip (T.pack out) of
        b
          | T.null b -> Nothing
          | otherwise -> Just b

-- Local branches, for the checkout picker: most recently committed
-- first, which is the order you tend to want them in. Carries each
-- branch's tip so the picker can say what you would be moving to.
--   (branch, how long ago, tip sha, subject)
localBranches :: FilePath -> IO [(T.Text, T.Text, T.Text, T.Text)]
localBranches root = do
  (code, out, _) <-
    Process.readProcessWithExitCode
      "git"
      [ "-C"
      , root
      , "for-each-ref"
      , "--sort=-committerdate"
      , "--format=%(refname:short)%09%(committerdate:relative)%09%(objectname:short)%09%(contents:subject)"
      , "refs/heads"
      ]
      ""
  pure $ case code of
    Exit.ExitFailure _ -> []
    Exit.ExitSuccess -> Maybe.mapMaybe fourFields (T.lines (T.pack out))

commitParents :: FilePath -> T.Text -> IO [(T.Text, T.Text)]
commitParents root ref = do
  (code, out, _) <-
    Process.readProcessWithExitCode
      "git"
      ["-C", root, "log", "-1", "--format=%P", T.unpack ref, "--"]
      ""
  case code of
    Exit.ExitFailure _ -> pure []
    Exit.ExitSuccess -> fmap Maybe.catMaybes (mapM (commitAt root) (T.words (T.pack out)))

resolveShort :: FilePath -> T.Text -> IO (Maybe T.Text)
resolveShort root ref = do
  (code, out, _) <-
    Process.readProcessWithExitCode
      "git"
      ["-C", root, "rev-parse", "--short=10", "--verify", "--quiet", T.unpack ref ++ "^{commit}"]
      ""
  pure $ case (code, T.strip (T.pack out)) of
    (Exit.ExitSuccess, sha) | not (T.null sha) -> Just sha
    _ -> Nothing

-- The spellings among the given subjects that name the view's commit:
-- the exact spelling first, then any other that resolves to the same
-- commit, so a tour posted for a sha is found by a session on the
-- branch, and the other way round. A working-tree view only matches
-- itself.
sameCommitSubjects :: FilePath -> ReviewMode -> [T.Text] -> IO [T.Text]
sameCommitSubjects root mode subjects = do
  let
    mine = subjectText mode
    others = filter (/= mine) subjects
  case subjectOf mode of
    WorkingTree -> pure [mine]
    Ref tip -> do
      target <- resolveFull root tip
      resolved <- mapM (\s -> fmap ((,) s) (resolveFull root s)) others
      let
        same (s, r) = if Maybe.isJust r && r == target then Just s else Nothing
      pure (mine : Maybe.mapMaybe same resolved)

resolveFull :: FilePath -> T.Text -> IO (Maybe T.Text)
resolveFull root ref = do
  (code, out, _) <-
    Process.readProcessWithExitCode
      "git"
      ["-C", root, "rev-parse", "--verify", "--quiet", T.unpack ref ++ "^{commit}"]
      ""
  pure $ case (code, T.strip (T.pack out)) of
    (Exit.ExitSuccess, sha) | not (T.null sha) -> Just sha
    _ -> Nothing

isSymbolicRef :: FilePath -> T.Text -> IO Bool
isSymbolicRef root ref = do
  (code, out, _) <-
    Process.readProcessWithExitCode
      "git"
      ["-C", root, "rev-parse", "--symbolic-full-name", T.unpack ref]
      ""
  pure (code == Exit.ExitSuccess && not (T.null (T.strip (T.pack out))))

emptyTreeRef :: T.Text
emptyTreeRef = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

pickableRefs :: FilePath -> IO [(T.Text, T.Text, T.Text, T.Text)]
pickableRefs root = do
  (code, out, _) <-
    Process.readProcessWithExitCode
      "git"
      [ "-C"
      , root
      , "for-each-ref"
      , "--sort=-creatordate"
      , "--format=%(refname:short)%09%(creatordate:relative)%09%(objectname:short)%09%(contents:subject)"
      , "refs/heads"
      , "refs/remotes"
      , "refs/tags"
      ]
      ""
  pure $ case code of
    Exit.ExitFailure _ -> []
    Exit.ExitSuccess -> Maybe.mapMaybe fourFields (T.lines (T.pack out))

-- Recent history for the base picker: short sha, how long ago, subject.
-- Newest first, which is the order the picker keeps when nothing is
-- typed.
--   (short sha, how long ago, ref names, subject)
-- --first-parent, so this is the branch's own line of development going
-- back: a merge is one commit, and what it brought in stays out of the
-- list. Without it git walks every parent and sorts by date, so commits
-- authored on merged-in branches turn up interleaved with yours, reading
-- as history that isn't yours. They stay legal bases -- everything here
-- is reachable from HEAD either way -- so :base <sha> can still name one.
-- %D is git's own decoration -- "HEAD -> main, origin/main", empty when
-- nothing points here -- which is how the picker answers "where is HEAD"
-- and "which of these is a branch tip".
recentCommits :: Int -> T.Text -> FilePath -> IO [(T.Text, T.Text, T.Text, T.Text)]
recentCommits n from root = logCommits n [T.unpack from] root

branchCommits :: Int -> T.Text -> FilePath -> IO [(T.Text, T.Text, T.Text, T.Text)]
branchCommits n tip root = do
  forked <- branchRootOf root tip
  own <- case forked of
    Left _ -> pure []
    Right base -> logCommits n [T.unpack base ++ ".." ++ T.unpack tip] root
  if null own then recentCommits n tip root else pure own

logCommits :: Int -> [String] -> FilePath -> IO [(T.Text, T.Text, T.Text, T.Text)]
logCommits n revs root = do
  (code, out, _) <-
    Process.readProcessWithExitCode
      "git"
      ( [ "-C"
        , root
        , "log"
        , "--first-parent"
        , "--format=%h%x09%ar%x09%D%x09%s"
        , "-n"
        , show n
        ]
          ++ revs
          ++ ["--"]
      )
      ""
  pure $ case code of
    Exit.ExitFailure _ -> []
    Exit.ExitSuccess -> Maybe.mapMaybe fourFields (T.lines (T.pack out))

-- Four tab-separated fields, the last keeping any tabs of its own --
-- subjects are free text.
fourFields :: T.Text -> Maybe (T.Text, T.Text, T.Text, T.Text)
fourFields line =
  case T.splitOn "\t" line of
    (a : b : c : rest)
      | not (T.null a) -> Just (a, b, c, T.intercalate "\t" rest)
    _ -> Nothing

headCommit :: FilePath -> IO (Maybe (T.Text, T.Text))
headCommit root = commitAt root "HEAD"

commitAt :: FilePath -> T.Text -> IO (Maybe (T.Text, T.Text))
commitAt root ref = do
  (code, out, _) <-
    Process.readProcessWithExitCode
      "git"
      ["-C", root, "log", "-1", "--format=%h%x09%s", T.unpack ref, "--"]
      ""
  pure $ case code of
    Exit.ExitFailure _ -> Nothing
    Exit.ExitSuccess ->
      case T.breakOn "\t" (T.strip (T.pack out)) of
        (sha, rest)
          | T.null sha -> Nothing
          | otherwise -> Just (sha, T.drop 1 rest)

-- Git actions -------------------------------------------------------------------

-- Run a git command for its effect, reporting failure as the (trimmed)
-- stderr so the UI can show it. GIT_EDITOR is forced non-interactive:
-- these run underneath a live TUI, where an editor popping up would
-- wedge the terminal.
runGit :: Writable -> FilePath -> [String] -> IO (Either T.Text ())
runGit _ root args = do
  environment <- Env.getEnvironment
  let
    p =
      (Process.proc "git" (["-C", root] ++ args))
        { Process.env = Just (("GIT_EDITOR", "true") : filter ((/= "GIT_EDITOR") . fst) environment)
        }
  (code, _, err) <- Process.readCreateProcessWithExitCode p ""
  pure $ case code of
    Exit.ExitSuccess -> Right ()
    Exit.ExitFailure _ ->
      let
        msg = T.strip (T.pack err)
      in
        Left (if T.null msg then "git " <> T.pack (unwords args) <> " failed" else msg)
