{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Remax.UI.Action.Picker
  ( reopenLastSelector
  , openBufferPicker
  , openBasePicker
  , openRepoPicker
  , openBranchPicker
  , openRefPicker
  , openCommitPicker
  , openPullPicker
  , reviewCommit
  , applyCheckout
  , openGrepPicker
  , openGrepHit
  , handlePickerKey
  ) where

import Brick qualified as Brick
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Graphics.Vty qualified as V
import Remax.Git qualified as Git
import Remax.GitHub qualified as GitHub
import Remax.Modal qualified as Modal
import Remax.UI.Action.Fuzzy qualified as Fuzzy
import Remax.UI.Action.Open qualified as Open
import Remax.UI.Action.Pull qualified as PullAction
import Remax.UI.Action.Repo qualified as Repo
import Remax.UI.Action.Settings qualified as Settings
import Remax.UI.Core.Jobs qualified as Jobs
import Remax.UI.Core.Types qualified as Types
import Remax.UI.Core.Widgets qualified as Widgets
import Remax.UI.Model.Load qualified as Load
import Remax.UI.Model.Nav qualified as Nav
import Remax.Zipper qualified as Z
import System.FilePath qualified as FilePath
import System.Process qualified as Process

-- Reopen it. Helix spells this ␣' and means the same thing by it: not
-- a fresh dialog of the same kind, but the one you had, with what you
-- had typed into it and where you had got to in the results.
reopenLastSelector :: Brick.EventM Types.Name Types.St ()
reopenLastSelector = do
  st <- State.get
  case Types.stLastSelector st of
    Nothing -> Types.setMsg "nothing selected yet this session"
    Just (Types.LastTree tv) ->
      State.modify (\s -> s {Types.stTree = Just tv, Types.stPicker = Nothing})
    Just (Types.LastPicker pk) ->
      State.modify (\s -> s {Types.stPicker = Just pk, Types.stTree = Nothing})

-- The picker narrowed to what the session already has open.
openBufferPicker :: Brick.EventM Types.Name Types.St ()
openBufferPicker = do
  st <- State.get
  State.modify $ \s ->
    s
      { Types.stPicker =
          Just (refreshPicker (Types.Picker Types.PickFiles "" (map Types.fvPath (Types.stFiles st)) [] [] 0 Set.empty))
      }

-- A picker row carries three separate things, because they are not the
-- same string: the ref to act on, the columns to draw, and the text to
-- filter against. Filtering has to see the unclipped cells -- otherwise
-- the tail of a long branch name is both invisible and unsearchable --
-- and acting has to see the unclipped ref.
pickerRow :: T.Text -> T.Text -> T.Text -> FilePath
pickerRow ref display match =
  T.unpack (T.intercalate "\t" [ref, display, match])

splitPickerRow :: FilePath -> (T.Text, T.Text, T.Text)
splitPickerRow row =
  case T.splitOn "\t" (T.pack row) of
    (ref : display : match : _) -> (ref, display, match)
    [ref, display] -> (ref, display, display)
    _ -> (T.pack row, T.pack row, T.pack row)

-- Pick a base out of the history instead of spelling one: bare :base.
openBasePicker :: Brick.EventM Types.Name Types.St ()
openBasePicker = do
  st <- State.get
  let
    tip = case Git.sourceOf (Types.stMode st) of
      Git.FromRef ref -> ref
      Git.FromWorkingTree -> "HEAD"
  commits <- MonadIO.liftIO (Git.recentCommits 300 tip (Types.stRoot st))
  let
    here ref = case Types.stMode st of
      Git.VsRef cur | cur == ref -> "  \8592 current"
      Git.AtRef rr | Git.rrBase rr == ref -> "  \8592 current"
      Git.AtPull pr _ | Git.prBase pr == ref -> "  \8592 current"
      _ -> ""
    -- sha · how long ago · what points here · subject
    cellsFor (sha, ago, refs, subject) = [sha, ago, refs, subject <> here sha]
    topRow = case Git.sourceOf (Types.stMode st) of
      Git.FromWorkingTree -> ["HEAD", "", "", "uncommitted changes" <> here "HEAD"]
      Git.FromRef ref -> [ref <> "^", "", "", "the tip commit alone" <> here (ref <> "^")]
    cells =
      [ topRow
      , ["branch", "", "", "the whole branch \8212 merge-base with the mainline"]
      ]
        ++ map cellsFor commits
    widths = Widgets.columnWidths [12, 15, 28] cells
    refOf cs = Maybe.fromMaybe "" (Types.safeIdx cs 0)
    rows =
      map
        (\cs -> pickerRow (refOf cs) (Widgets.columnRow widths cs) (T.unwords cs))
        cells
  State.modify $ \s ->
    s {Types.stPicker = Just (refreshRowPicker (Types.Picker Types.PickBase "" rows [] [] 0 Set.empty))}

-- For the base and branch pickers, where the incoming order carries
-- meaning: rank on match quality alone and let the sort's stability keep
-- it within a tier. (refreshPicker's third component is path length,
-- which would scramble it.)
refreshRowPicker :: Types.Picker -> Types.Picker
refreshRowPicker pk =
  let
    q = Types.pkInput pk
    quality t = fmap (\(a, b, _) -> (a, b)) (Fuzzy.fuzzyScore q (T.unpack t))
    score row =
      let
        (ref, disp, match) = splitPickerRow row
      in
        fmap (\sc -> (sc, (ref, disp))) (quality match)
    ranked = map snd (List.sortOn fst (Maybe.mapMaybe score (Types.pkAll pk)))
  in
    pk
      { Types.pkMatches = map snd ranked
      , Types.pkTargets = map (\(ref, _) -> (T.unpack ref, 0)) ranked
      , Types.pkSel = 0
      }

-- 'branch' means the merge-base, the same word :base already takes.
applyBaseChoice :: T.Text -> Brick.EventM Types.Name Types.St ()
applyBaseChoice ref
  | T.null ref = pure ()
  | ref == "branch" = Settings.setBaseBranch
  | otherwise = Settings.setBaseRef ref

openRepoPicker :: Brick.EventM Types.Name Types.St ()
openRepoPicker = do
  Repo.rescanRepos
  st <- State.get
  case Types.stRoots st of
    [] -> Types.setMsg "no other repositories here"
    [_] -> Types.setMsg "this is the only repository here"
    roots -> do
      let
        remembered root =
          Map.findWithDefault
            [T.pack (FilePath.takeFileName root), "", "\8230"]
            root
            (Types.stRepoInfo st)
        rows = repoPickerRows st (map (\root -> (root, remembered root)) roots)
        -- opening on the next one round makes Enter by itself "move on",
        -- while the list is still there to choose from deliberately
        startAt = case List.findIndex (== Types.stRoot st) roots of
          Just i -> (i + 1) `mod` length roots
          Nothing -> 0
      -- refreshRowPicker ranks the rows and lands on the first, so the
      -- starting row is chosen after it has done that
      State.modify $ \s ->
        s
          { Types.stPicker =
              Just
                ( (refreshRowPicker (Types.Picker Types.PickRepo "" rows [] [] 0 Set.empty))
                    { Types.pkSel = startAt
                    }
                )
          }
      Jobs.startJob "describing repositories" (Jobs.forBounded 4 (Repo.describeRepo st) roots) $
        \outcome -> case outcome of
          Left err -> Jobs.jobFailed "describing repositories" err
          Right described -> do
            State.modify $ \s ->
              s {Types.stRepoInfo = Map.union (Map.fromList described) (Types.stRepoInfo s)}
            latest <- State.get
            case Types.stPicker latest of
              Just pk -> case Types.pkMode pk of
                Types.PickRepo ->
                  State.modify $ \s ->
                    s {Types.stPicker = Just (replaceRepoRows (repoPickerRows latest described) pk)}
                _ -> pure ()
              Nothing -> pure ()

repoPickerRows :: Types.St -> [(FilePath, [T.Text])] -> [FilePath]
repoPickerRows st described =
  let
    markCurrent (root, cells) =
      case (root == Types.stRoot st, reverse cells) of
        (True, lastCell : before) -> (root, reverse ((lastCell <> "  \8592 current") : before))
        _ -> (root, cells)
    marked = map markCurrent described
    widths = Widgets.columnWidths [24, 28, 16] (map snd marked)
    -- the row shows the basename; the ref carries the whole path, so
    -- a clipped name can never become the thing we switch to
    row (root, cells) =
      pickerRow
        (T.pack root)
        (Widgets.columnRow widths cells)
        (T.pack root <> " " <> T.unwords cells)
  in
    map row marked

replaceRepoRows :: [FilePath] -> Types.Picker -> Types.Picker
replaceRepoRows rows pk =
  let
    chosen = fmap fst (Types.safeIdx (Types.pkTargets pk) (Types.pkSel pk))
    refreshed = refreshRowPicker pk {Types.pkAll = rows}
    kept = do
      ref <- chosen
      List.findIndex ((== ref) . fst) (Types.pkTargets refreshed)
  in
    refreshed {Types.pkSel = Maybe.fromMaybe (Types.pkSel pk) kept}

-- Check out a branch from the same overlay. A sixth explicit mutation
-- point, and like the others it only ever runs because you asked: the
-- picker is the confirmation, and git's own refusal is the guardrail
-- when the tree would be clobbered.
openBranchPicker :: Brick.EventM Types.Name Types.St ()
openBranchPicker = do
  st <- State.get
  case Types.stRebase st of
    Just _ -> Types.setMsg "mid-rebase — :continue or :abort! before switching branches"
    Nothing -> do
      branches <- MonadIO.liftIO (Git.localBranches (Types.stRoot st))
      let
        here b =
          case Git.subjectOf (Types.stMode st) of
            Git.WorkingTree | Just b == Types.stBranch st -> "  \8592 current"
            _ -> ""
        -- branch · how long ago · the tip you would land on · subject
        cellsFor (b, ago, sha, subject) = [b, ago, sha, subject <> here b]
        cells = map cellsFor branches
        widths = Widgets.columnWidths [28, 15, 12] cells
        refOf cs = Maybe.fromMaybe "" (Types.safeIdx cs 0)
        rows =
          map
            (\cs -> pickerRow (refOf cs) (Widgets.columnRow widths cs) (T.unwords cs))
            cells
      if null rows
        then Types.setMsg "no local branches"
        else State.modify $ \s ->
          s
            { Types.stPicker =
                Just
                  (refreshRowPicker (Types.Picker Types.PickBranch "" rows [] [] 0 Set.empty))
            }

openRefPicker :: Brick.EventM Types.Name Types.St ()
openRefPicker = do
  st <- State.get
  branches <- MonadIO.liftIO (Git.localBranches (Types.stRoot st))
  let
    here b = case Git.subjectOf (Types.stMode st) of
      Git.Ref tip | tip == b -> "  \8592 current"
      _ -> ""
    treeNote = case Git.subjectOf (Types.stMode st) of
      Git.WorkingTree -> "  \8592 current"
      _ -> ""
    cellsFor (b, ago, sha, subject) = [b, ago, sha, subject <> here b]
    cells = ["working tree", "", "", "the working tree, editable" <> treeNote] : map cellsFor branches
    widths = Widgets.columnWidths [28, 15, 12] cells
    refOf cs = Maybe.fromMaybe "" (Types.safeIdx cs 0)
    rows =
      map
        (\cs -> pickerRow (refOf cs) (Widgets.columnRow widths cs) (T.unwords cs))
        cells
  State.modify $ \s ->
    s {Types.stPicker = Just (refreshRowPicker (Types.Picker Types.PickRef "" rows [] [] 0 Set.empty))}

openPullPicker :: Brick.EventM Types.Name Types.St ()
openPullPicker = do
  st <- State.get
  known <- MonadIO.liftIO (GitHub.cachedRepoName (Types.stConn st) (Types.stRoot st))
  State.modify $ \s ->
    s {Types.stPicker = Just (pullPickerWith ["loading open pull requests from gh\8230"] (Types.Picker Types.PickPull "" [] [] [] 0 Set.empty))}
  Jobs.startJob "listing pull requests" (listOpenPulls (Types.stRoot st) (GitHub.repoCached known)) $ \outcome -> do
    latest <- State.get
    Monad.when (Types.stEpoch latest == Types.stEpoch st) $ do
      let
        rows = case outcome of
          Left err -> Left ["\10007 " <> err]
          Right (Left failure) -> Left (pullFailureRows failure)
          Right (Right (repo, [])) -> Left ["no open pull requests on " <> repo]
          Right (Right (_, pulls)) -> Right (pullPickerRows latest pulls)
      case outcome of
        Right (Right (repo, _)) -> MonadIO.liftIO (GitHub.rememberRepoName (Types.stConn latest) known repo)
        _ -> pure ()
      case Types.stPicker latest of
        Just pk | Types.PickPull <- Types.pkMode pk ->
          State.modify $ \s ->
            s
              { Types.stPicker = Just $ case rows of
                  Left notes -> pullPickerWith notes pk
                  Right found -> onFirstTarget (replaceRepoRows found pk)
              }
        _ -> pure ()

listOpenPulls :: FilePath -> Maybe T.Text -> IO (Either GitHub.GhFailure (T.Text, [GitHub.PullRow]))
listOpenPulls root cached = do
  resolved <- maybe (GitHub.resolveRepo root) (pure . Right) cached
  viewer <- GitHub.viewerLogin root
  case (resolved, viewer) of
    (Left failure, _) -> pure (Left failure)
    (_, Left failure) -> pure (Left failure)
    (Right repo, Right login) -> fmap (fmap (\pulls -> (repo, pulls))) (GitHub.listPulls root repo login)

pullFailureRows :: GitHub.GhFailure -> [T.Text]
pullFailureRows failure =
  case failure of
    GitHub.GhLoggedOut -> ["\10007 gh is not logged in", "run gh auth login, then :pr again"]
    GitHub.GhOffline why -> ["\10007 cannot reach GitHub: " <> why, "check the network, then :pr again"]
    GitHub.GhMissing -> ["\10007 gh is not installed", "install the GitHub CLI and run gh auth login, then :pr again"]
    _ -> ["\10007 " <> GitHub.failureText failure]

onFirstTarget :: Types.Picker -> Types.Picker
onFirstTarget pk =
  case Types.safeIdx (Types.pkTargets pk) (Types.pkSel pk) of
    Just (target, _)
      | null target ->
          pk {Types.pkSel = Maybe.fromMaybe (Types.pkSel pk) (List.findIndex (not . null . fst) (Types.pkTargets pk))}
    _ -> pk

pullPickerWith :: [T.Text] -> Types.Picker -> Types.Picker
pullPickerWith notes pk =
  refreshRowPicker pk {Types.pkAll = map (\note -> pickerRow "" note "") notes}

pullPickerRows :: Types.St -> [GitHub.PullRow] -> [FilePath]
pullPickerRows st pulls =
  let
    openNumber = case Git.subjectOf (Types.stMode st) of
      Git.Pull pr _ -> Just (Git.prNumber pr)
      _ -> Nothing
    cellsFor p =
      [ Types.tshow (GitHub.rowNumber p)
      , GitHub.rowAuthor p
      , (if GitHub.rowDraft p then "draft " else "") <> GitHub.rowTitle p
      , PullAction.shortAge (GitHub.rowAgeSeconds p)
      , checksGlyph (GitHub.rowChecks p)
      , reviewWord (GitHub.rowReview p) <> (if openNumber == Just (GitHub.rowNumber p) then "  \8592 open" else "")
      ]
    header = ["#", "author", "title", "age", "ci", "review"]
    cells = map cellsFor pulls
    widths = Widgets.columnWidths [6, 14, 48, 5, 3] (header : cells)
    row p cs = pickerRow (Types.tshow (GitHub.rowNumber p)) (Widgets.columnRow widths cs) (T.unwords cs)
  in
    pickerRow "" (Widgets.columnRow widths header) "" : zipWith row pulls cells

checksGlyph :: GitHub.Checks -> T.Text
checksGlyph checks =
  case checks of
    GitHub.ChecksPass -> "\10003"
    GitHub.ChecksFail -> "\10007"
    GitHub.ChecksRunning -> "\9675"
    GitHub.ChecksNone -> "\8211"

reviewWord :: GitHub.ReviewState -> T.Text
reviewWord review =
  case review of
    GitHub.NotReviewed -> "\8211"
    GitHub.Commented -> "commented"
    GitHub.Approved -> "\10003 approved"
    GitHub.ChangesRequested -> "changes requested"
    GitHub.ReviewRequested -> "requested"

openCommitPicker :: Brick.EventM Types.Name Types.St ()
openCommitPicker = do
  st <- State.get
  rows <- MonadIO.liftIO (commitPickerRows (Types.stRoot st) Set.empty)
  if null rows
    then Types.setMsg "no refs to pick from"
    else State.modify $ \s ->
      s {Types.stPicker = Just (refreshRowPicker (Types.Picker Types.PickCommit "" rows [] [] 0 Set.empty))}

commitPickerRows :: FilePath -> Set.Set T.Text -> IO [FilePath]
commitPickerRows root expanded = do
  refs <- Git.pickableRefs root
  groups <- Monad.forM refs $ \(name, ago, tipSha, subject) -> do
    let
      open = Set.member name expanded
      marker = if open then "\9662 " else "\9656 "
    commits <- if open then Git.branchCommits 100 name root else pure []
    let
      commitRow (csha, cago, _, csubject) =
        ("commit:" <> (if csha == tipSha then name else csha), ["    " <> csha, cago, csubject])
    pure (("ref:" <> name, [marker <> name, ago, subject]) : map commitRow commits)
  let
    entries = concat groups
    widths = Widgets.columnWidths [34, 15] (map snd entries)
  pure (map (\(target, cells) -> pickerRow target (Widgets.columnRow widths cells) (T.unwords cells)) entries)

commitChoice :: Types.Picker -> T.Text -> Brick.EventM Types.Name Types.St ()
commitChoice pk target =
  case (T.stripPrefix "ref:" target, T.stripPrefix "commit:" target) of
    (Just name, _) -> do
      st <- State.get
      let
        expanded =
          if Set.member name (Types.pkExpanded pk)
            then Set.delete name (Types.pkExpanded pk)
            else Set.insert name (Types.pkExpanded pk)
      rows <- MonadIO.liftIO (commitPickerRows (Types.stRoot st) expanded)
      State.modify $ \s ->
        s {Types.stPicker = Just (replaceRepoRows rows pk {Types.pkExpanded = expanded})}
    (_, Just sha) -> reviewCommit sha
    _ -> pure ()

reviewCommit :: T.Text -> Brick.EventM Types.Name Types.St ()
reviewCommit ref = do
  st <- State.get
  let
    root = Types.stRoot st
  resolved <- MonadIO.liftIO (Git.resolveShort root ref)
  case resolved of
    Nothing -> Types.setMsg ("no such commit: " <> ref)
    Just sha -> do
      symbolic <- MonadIO.liftIO (Git.isSymbolicRef root ref)
      parents <- MonadIO.liftIO (Git.commitParents root sha)
      let
        tip = if symbolic then ref else sha
      case parents of
        [] -> Settings.setMode (Git.AtRef (Git.RefReview {Git.rrTip = tip, Git.rrBase = Git.emptyTreeRef}))
        [_] -> Settings.setMode (Git.AtRef (Git.RefReview {Git.rrTip = tip, Git.rrBase = tip <> "^"}))
        many -> openParentPicker tip many

openParentPicker :: T.Text -> [(T.Text, T.Text)] -> Brick.EventM Types.Name Types.St ()
openParentPicker tip parents = do
  let
    cells = zipWith (\i (sha, subject) -> [sha, "parent " <> Types.tshow (i :: Int), subject]) [1 :: Int ..] parents
    widths = Widgets.columnWidths [12, 10] cells
    rows =
      zipWith
        (\i cs -> pickerRow (tip <> "^" <> Types.tshow i) (Widgets.columnRow widths cs) (T.unwords cs))
        [1 :: Int ..]
        cells
  State.modify $ \s ->
    s {Types.stPicker = Just (refreshRowPicker (Types.Picker (Types.PickParent tip) "" rows [] [] 0 Set.empty))}

applyRefChoice :: T.Text -> Brick.EventM Types.Name Types.St ()
applyRefChoice choice
  | T.null choice = pure ()
  | choice == "working tree" = Settings.returnToTree
  | otherwise = Settings.setRef choice

applyCheckout :: T.Text -> Brick.EventM Types.Name Types.St ()
applyCheckout branch
  | T.null branch = pure ()
  | otherwise = Types.whenWritable $ \grant -> do
      st <- State.get
      if Just branch == Types.stBranch st
        then Types.setMsg ("already on " <> branch)
        else do
          res <- MonadIO.liftIO (Git.runGit grant (Types.stRoot st) ["checkout", T.unpack branch])
          case res of
            Left e -> Types.setMsg e
            Right () -> do
              Load.forceGitRefresh
              Types.setMsg ("checked out " <> branch)

-- Project-wide search: git grep behind the same overlay, Enter jumping
-- straight to the hit.
openGrepPicker :: T.Text -> Brick.EventM Types.Name Types.St ()
openGrepPicker query = do
  st <- State.get
  (matches, targets) <- MonadIO.liftIO (grepRepo (Types.effectiveMode st) (Types.stRoot st) query)
  State.modify $ \s ->
    s
      { Types.stPicker =
          Just (Types.Picker Types.PickGrep query [] matches targets 0 Set.empty)
      }

-- Land on a grep hit: open the file, select the match on its line, and
-- prime n/N so the in-file search continues from there.
openGrepHit :: FilePath -> Int -> T.Text -> Brick.EventM Types.Name Types.St ()
openGrepHit path line query = do
  Open.openFilePath (T.pack path)
  st <- State.get
  case Types.currentFileMay st of
    Just fv
      | Types.fvPath fv == path -> do
          let
            ed = Types.fvEd fv
            ls = Modal.bufferLines ed
            row = max 0 (min (line - 1) (length ls - 1))
            lineText = case drop row ls of
              (l : _) -> l
              [] -> T.empty
            col = case T.breakOn query lineText of
              (pre, rest)
                | not (T.null rest) -> T.length pre
              _ -> 0
            width = max 1 (T.length query)
            placed =
              ed
                { Modal.edZipper = Z.moveCursor (row, col + width - 1) (Modal.edZipper ed)
                , Modal.edSelAnchor = Just (row, col)
                , Modal.edSelKind = Modal.CharSel
                }
          Nav.setEd (Types.stCurF st) placed
          State.modify (\s -> s {Types.stLastSearch = query})
    _ -> pure ()

-- Literal, case-sensitive, binary-skipping; capped so a short query
-- cannot flood the overlay.
grepRepo :: Git.ReviewMode -> FilePath -> T.Text -> IO ([T.Text], [(FilePath, Int)])
grepRepo mode root query =
  if T.length query < 2
    then pure ([], [])
    else do
      let
        treeArgs = case Git.sourceOf mode of
          Git.FromWorkingTree -> []
          Git.FromRef tip -> [T.unpack tip]
        prefix = case Git.sourceOf mode of
          Git.FromWorkingTree -> ""
          Git.FromRef tip -> tip <> ":"
      (_, out, _) <-
        Process.readProcessWithExitCode
          "git"
          ( [ "-C"
            , root
            , "grep"
            , "-nF"
            , "-I"
            , "--no-color"
            , "-e"
            , T.unpack query
            ]
              ++ treeArgs
              ++ ["--"]
          )
          ""
      let
        hits = take 200 (T.lines (T.pack out))
        parse hit0 = do
          hit <- T.stripPrefix prefix hit0
          let
            (path, rest1) = T.breakOn ":" hit
          _ <- if T.null rest1 then Nothing else Just ()
          let
            (lineT, rest2) = T.breakOn ":" (T.drop 1 rest1)
          n <- case TR.decimal lineT of
            Right (v, leftover) | T.null leftover -> Just v
            _ -> Nothing
          let
            snippet = T.strip (T.drop 1 rest2)
            display =
              Widgets.shortPath 36 path
                <> ":"
                <> lineT
                <> "  "
                <> T.take 160 snippet
          pure (display, (T.unpack path, n))
        parsed = Maybe.mapMaybe parse hits
      pure (map fst parsed, map snd parsed)

refreshPicker :: Types.Picker -> Types.Picker
refreshPicker pk =
  let
    q = Types.pkInput pk
    scored =
      Maybe.mapMaybe (\path -> fmap (\sc -> (sc, path)) (Fuzzy.fuzzyScore q path)) (Types.pkAll pk)
    ranked = take 200 (map snd (List.sortOn fst scored))
  in
    pk
      { Types.pkMatches = map (Widgets.shortPath 120 . T.pack) ranked
      , Types.pkTargets = map (\path -> (path, 0)) ranked
      , Types.pkSel = 0
      }

handlePickerKey :: V.Event -> Types.Picker -> Brick.EventM Types.Name Types.St ()
handlePickerKey ev pk =
  case ev of
    V.EvKey V.KEsc [] -> State.modify (\s -> s {Types.stPicker = Nothing})
    V.EvKey V.KEnter [] ->
      case Types.safeIdx (Types.pkTargets pk) (Types.pkSel pk) of
        Just (target, _) | null target -> pure ()
        chosen -> do
          State.modify (\s -> s {Types.stPicker = Nothing})
          Monad.forM_ chosen (pickerChoose pk)
    V.EvKey V.KUp [] -> pickerMove pk (-1)
    V.EvKey (V.KChar 'p') [V.MCtrl] -> pickerMove pk (-1)
    V.EvKey V.KDown [] -> pickerMove pk 1
    V.EvKey (V.KChar 'n') [V.MCtrl] -> pickerMove pk 1
    V.EvKey V.KBS [] -> pickerRetype pk (T.dropEnd 1 (Types.pkInput pk))
    V.EvKey (V.KChar c) [] -> pickerRetype pk (T.snoc (Types.pkInput pk) c)
    _ -> pure ()

pickerMove :: Types.Picker -> Int -> Brick.EventM Types.Name Types.St ()
pickerMove pk d =
  let
    bound = max 0 (length (Types.pkMatches pk) - 1)
    next = max 0 (min bound (Types.pkSel pk + d))
  in
    State.modify (\s -> s {Types.stPicker = Just pk {Types.pkSel = next}})

pickerRetype :: Types.Picker -> T.Text -> Brick.EventM Types.Name Types.St ()
pickerRetype pk q =
  case Types.pkMode pk of
    Types.PickFiles -> State.modify (\s -> s {Types.stPicker = Just (refreshPicker pk {Types.pkInput = q})})
    Types.PickGrep -> do
      st <- State.get
      (matches, targets) <- MonadIO.liftIO (grepRepo (Types.effectiveMode st) (Types.stRoot st) q)
      State.modify $ \s ->
        s {Types.stPicker = Just pk {Types.pkInput = q, Types.pkMatches = matches, Types.pkTargets = targets, Types.pkSel = 0}}
    Types.PickPull -> State.modify (\s -> s {Types.stPicker = Just (onFirstTarget (refreshRowPicker pk {Types.pkInput = q}))})
    _ -> State.modify (\s -> s {Types.stPicker = Just (refreshRowPicker pk {Types.pkInput = q})})

pickerChoose :: Types.Picker -> (FilePath, Int) -> Brick.EventM Types.Name Types.St ()
pickerChoose pk (target, line) =
  case Types.pkMode pk of
    Types.PickBase -> applyBaseChoice (T.pack target)
    Types.PickBranch -> applyCheckout (T.pack target)
    Types.PickRef -> applyRefChoice (T.pack target)
    Types.PickCommit -> commitChoice pk (T.pack target)
    Types.PickParent tip -> Settings.setMode (Git.AtRef (Git.RefReview {Git.rrTip = tip, Git.rrBase = T.pack target}))
    Types.PickRepo -> Repo.switchRepo target
    Types.PickPull -> Monad.forM_ (GitHub.pullNumber (T.pack target)) PullAction.openPull
    _
      | line == 0 -> Open.openFilePath (T.pack target)
      | otherwise -> do
          State.modify (\s -> s {Types.stGrepNav = Just (Types.pkInput pk, Types.pkTargets pk, Types.pkSel pk)})
          openGrepHit target line (Types.pkInput pk)
