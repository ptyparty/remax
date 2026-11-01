{-# LANGUAGE OverloadedStrings #-}

module Remax.Setup
  ( SetupAction (..)
  , runSetup
  ) where

import Control.Monad qualified as Monad
import Data.Either qualified as Either
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory qualified as Directory
import System.Environment qualified as Env
import System.Exit qualified as Exit
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import System.IO.Error qualified as IOError

data SetupAction = SetupList | SetupApply | SetupRemove

data LinkState = Installed | Missing | Outdated | Different
  deriving (Eq)

data Link = Link
  { linkLabel :: T.Text
  , linkSource :: FilePath
  , linkTarget :: FilePath
  , linkTail :: FilePath
  }

data Removal = Removed | Restored | Skipped

data Applied = Linked | LinkedAside FilePath

data SetupError
  = BackupTaken FilePath FilePath
  | LinkFailed IOError.IOError

renderSetupError :: SetupError -> T.Text
renderSetupError e =
  case e of
    BackupTaken backup target ->
      T.pack backup <> " is already there from an earlier setup; move it before setting up " <> T.pack target
    LinkFailed ioe -> T.pack (show ioe)

runSetup :: SetupAction -> IO ()
runSetup action = do
  found <- findCheckout
  root <- maybe (Exit.die noCheckout) pure found
  configDir <- claudeConfigDir
  links <- setupLinks configDir root
  case action of
    SetupList -> listLinks configDir links
    SetupApply -> applyLinks links
    SetupRemove -> Monad.forM_ links (removeAndReport (labelWidth links))

noCheckout :: String
noCheckout =
  "remax setup: can't find the remax checkout this binary was built in; \
  \run the remax built by cabal build inside the checkout"

setupLinks :: FilePath -> FilePath -> IO [Link]
setupLinks configDir root = do
  names <- Directory.listDirectory (root </> "skills")
  pure (map (skillLink configDir root) (List.sort names))

skillLink :: FilePath -> FilePath -> FilePath -> Link
skillLink configDir root name =
  Link
    { linkLabel = "skill " <> T.pack name
    , linkSource = root </> "skills" </> name
    , linkTarget = configDir </> "skills" </> name
    , linkTail = "skills" </> name
    }

listLinks :: FilePath -> [Link] -> IO ()
listLinks configDir links = do
  TIO.putStrLn ("Claude Code setup for remax, in " <> T.pack configDir <> ":\n")
  states <- mapM linkState links
  Monad.forM_ (zip links states) $ \(link, state) ->
    TIO.putStrLn (line (labelWidth links) (mark state) link (describe state link))
  Monad.when (any (/= Installed) states) $
    TIO.putStrLn "\nRun `remax setup --yes` to set it up."

applyLinks :: [Link] -> IO ()
applyLinks links = do
  needed <- Monad.filterM (fmap (/= Installed) . linkState) links
  Monad.when (null needed) (TIO.putStrLn "Nothing to set up.")
  outcomes <- mapM applyLink needed
  Monad.forM_ (zip needed outcomes) $ \(link, outcome) ->
    TIO.putStrLn (applied (labelWidth links) link outcome)
  Monad.when (any Either.isLeft outcomes) (Exit.exitWith (Exit.ExitFailure 1))

applied :: Int -> Link -> Either SetupError Applied -> T.Text
applied width link outcome =
  case outcome of
    Left problem -> line width (mark Missing) link (renderSetupError problem)
    Right Linked -> line width (mark Installed) link "linked"
    Right (LinkedAside backup) -> line width (mark Installed) link ("linked, keeping the old one as " <> T.pack backup)

removeAndReport :: Int -> Link -> IO ()
removeAndReport width link = do
  removal <- removeLink link
  case removal of
    Removed -> TIO.putStrLn ("  " <> padded width link <> "  removed")
    Restored -> TIO.putStrLn ("  " <> padded width link <> "  removed, and put back what it replaced")
    Skipped -> pure ()

line :: Int -> T.Text -> Link -> T.Text -> T.Text
line width sign link text = "  " <> sign <> " " <> padded width link <> "  " <> text

padded :: Int -> Link -> T.Text
padded width link = T.justifyLeft width ' ' (linkLabel link)

labelWidth :: [Link] -> Int
labelWidth links = maximum (0 : map (T.length . linkLabel) links)

mark :: LinkState -> T.Text
mark state =
  case state of
    Installed -> "✓"
    Missing -> "✗"
    Outdated -> "↻"
    Different -> "!"

describe :: LinkState -> Link -> T.Text
describe state link =
  case state of
    Installed -> "linked"
    Missing -> "missing"
    Outdated -> "links to another remax checkout"
    Different -> "something else is there; it would be kept as " <> T.pack (backupPath link)

linkState :: Link -> IO LinkState
linkState link = do
  there <- present (linkTarget link)
  isLink <- if there then Directory.pathIsSymbolicLink (linkTarget link) else pure False
  case (there, isLink) of
    (False, _) -> pure Missing
    (True, False) -> pure Different
    (True, True) -> pointedState link

pointedState :: Link -> IO LinkState
pointedState link = do
  raw <- Directory.getSymbolicLinkTarget (linkTarget link)
  let
    pointed = FilePath.normalise (FilePath.takeDirectory (linkTarget link) </> raw)
    suffix = FilePath.pathSeparator : linkTail link
    owner = take (length pointed - length suffix) pointed
  same <- samePath pointed (linkSource link)
  exists <- Directory.doesPathExist pointed
  owned <- isCheckout owner
  pure $
    if same
      then Installed
      else
        if suffix `List.isSuffixOf` pointed && (not exists || owned)
          then Outdated
          else Different

applyLink :: Link -> IO (Either SetupError Applied)
applyLink link = do
  state <- linkState link
  tried <- IOError.tryIOError $
    case state of
      Installed -> pure (Right Linked)
      Missing -> Right Linked <$ makeLink link
      Outdated -> do
        Directory.removeDirectoryLink (linkTarget link)
        Right Linked <$ makeLink link
      Different -> moveAside link
  pure (either (Left . LinkFailed) id tried)

moveAside :: Link -> IO (Either SetupError Applied)
moveAside link = do
  let
    backup = backupPath link
  taken <- present backup
  if taken
    then pure (Left (BackupTaken backup (linkTarget link)))
    else do
      Directory.createDirectoryIfMissing True (FilePath.takeDirectory backup)
      Directory.renamePath (linkTarget link) backup
      makeLink link
      pure (Right (LinkedAside backup))

makeLink :: Link -> IO ()
makeLink link = do
  Directory.createDirectoryIfMissing True (FilePath.takeDirectory (linkTarget link))
  Directory.createDirectoryLink (linkSource link) (linkTarget link)

removeLink :: Link -> IO Removal
removeLink link = do
  state <- linkState link
  if state /= Installed
    then pure Skipped
    else do
      Directory.removeDirectoryLink (linkTarget link)
      let
        backup = backupPath link
      kept <- present backup
      if kept
        then Restored <$ Directory.renamePath backup (linkTarget link)
        else pure Removed

backupPath :: Link -> FilePath
backupPath link =
  let
    folder = FilePath.takeDirectory (linkTarget link)
  in
    FilePath.takeDirectory folder </> "remax-backup" </> FilePath.takeFileName folder </> FilePath.takeFileName (linkTarget link)

present :: FilePath -> IO Bool
present path = do
  exists <- Directory.doesPathExist path
  if exists
    then pure True
    else IOError.catchIOError (Directory.pathIsSymbolicLink path) (const (pure False))

samePath :: FilePath -> FilePath -> IO Bool
samePath a b = do
  aExists <- Directory.doesPathExist a
  bExists <- Directory.doesPathExist b
  if aExists && bExists
    then (==) <$> Directory.canonicalizePath a <*> Directory.canonicalizePath b
    else pure (FilePath.normalise a == FilePath.normalise b)

findCheckout :: IO (Maybe FilePath)
findCheckout = do
  exe <- Env.getExecutablePath
  real <- Directory.canonicalizePath exe
  found <- Monad.filterM isCheckout (ancestors (FilePath.takeDirectory real))
  pure (Maybe.listToMaybe found)

ancestors :: FilePath -> [FilePath]
ancestors dir =
  let
    parent = FilePath.takeDirectory dir
  in
    if parent == dir then [dir] else dir : ancestors parent

isCheckout :: FilePath -> IO Bool
isCheckout dir = do
  cabal <- Directory.doesFileExist (dir </> "remax.cabal")
  skills <- Directory.doesDirectoryExist (dir </> "skills")
  pure (cabal && skills)

claudeConfigDir :: IO FilePath
claudeConfigDir = do
  configured <- Env.lookupEnv "CLAUDE_CONFIG_DIR"
  case configured of
    Just dir | not (null dir) -> pure dir
    _ -> do
      home <- Directory.getHomeDirectory
      pure (home </> ".claude")
