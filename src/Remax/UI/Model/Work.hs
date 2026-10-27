module Remax.UI.Model.Work
  ( scopeOf
  , scopeFor
  , loadFor
  , reloadWork
  , withConversations
  , remoteOf
  ) where

import Brick qualified as Brick
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.State qualified as State
import Data.Text qualified as T
import Database.SQLite.Simple qualified as SQLite
import Remax.Conversation qualified as Conv
import Remax.Git qualified as Git
import Remax.PullPoll qualified as PullPoll
import Remax.UI.Core.Types qualified as Types
import Remax.WorkStore qualified as WorkStore

scopeFor :: FilePath -> Git.ReviewMode -> WorkStore.Scope
scopeFor root mode =
  WorkStore.Scope
    { WorkStore.scopeRepo = T.pack root
    , WorkStore.scopePull = case Git.subjectOf mode of
        Git.Pull pr _ -> Just (Git.prNumber pr)
        _ -> Nothing
    }

scopeOf :: Types.St -> WorkStore.Scope
scopeOf st = scopeFor (Types.stRoot st) (Types.stMode st)

loadFor :: SQLite.Connection -> FilePath -> Git.ReviewMode -> IO Conv.OwnWork
loadFor conn root mode = WorkStore.loadWork conn (scopeFor root mode)

reloadWork :: Brick.EventM Types.Name Types.St ()
reloadWork = do
  st <- State.get
  own <- MonadIO.liftIO (WorkStore.loadWork (Types.stConn st) (scopeOf st))
  State.modify (\s -> withConversations s {Types.stWork = own})

remoteOf :: Types.St -> Conv.Remote
remoteOf st =
  case Types.ppSnapshot (Types.stPullPoll st) of
    Just snap -> PullPoll.snapshotRemote snap
    Nothing -> Conv.emptyRemote

withConversations :: Types.St -> Types.St
withConversations st =
  st {Types.stConversations = Conv.conversations (remoteOf st) (Types.stWork st)}
