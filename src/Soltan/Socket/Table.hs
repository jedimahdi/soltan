module Soltan.Socket.Table where

import Control.Lens (ix, (^?))
import qualified Data.Map as Map
import Pipes (Consumer, Effect, Pipe, await, runEffect, yield, (>->))
import Pipes.Concurrent (fromInput, toOutput)
import Soltan.Data.Has (grab)
import Soltan.Data.Username (Username)
import Soltan.Hokm.Types (Game)
import Soltan.Logger (WithLog)
import qualified Soltan.Logger as Logger
import Soltan.Socket.Types (Client (..), MsgOut (..), ServerState (..), Table (..), TableName, WithServerState)
import UnliftIO (MonadUnliftIO, async)

setupTablePipeline :: forall m env. (WithServerState env m, MonadUnliftIO m, WithLog env m) => TableName -> Table -> m ()
setupTablePipeline tableName Table{..} = void <| async <| infinitely <| runEffect <| pipeline
 where
  pipeline :: Effect m ()
  pipeline =
    fromInput gameOutMailbox
      >-> broadcast tableName
      >-> updateTable tableName
      >-> Logger.pipe
      >-> gameDone

gameDone :: MonadIO m => Consumer Game m ()
gameDone = do
  game <- await
  pass

broadcast :: (WithServerState env m, MonadUnliftIO m) => TableName -> Pipe Game Game m ()
broadcast tableName = do
  g <- await
  ServerState{..} <- grab @(TVar ServerState) >>= readTVarIO
  case lobby ^? ix tableName of
    Nothing -> pass
    Just Table{..} -> do
      let cs = mapMaybe (\username -> clients ^? ix username) subscribers
      void . lift . async <| mapM_ (informSubscriber tableName g) cs
      yield game

informSubscriber :: MonadIO m => TableName -> Game -> Client -> m ()
informSubscriber tableName game Client{..} = do
  runEffect <| yield (NewGameState tableName game) >-> toOutput outgoingMailbox

updateTable :: WithServerState env m => TableName -> Pipe Game Game m ()
updateTable tableName = do
  game <- await
  serverStateTVar <- grab @(TVar ServerState)
  atomically <| updateTableSTM serverStateTVar tableName game
  yield game

updateTableSTM :: TVar ServerState -> TableName -> Game -> STM ()
updateTableSTM serverStateTVar tableName game = modifyTVar' serverStateTVar (#lobby . ix tableName . #game .~ game)

withTable :: WithServerState env m => TableName -> m r -> (Table -> m r) -> m r
withTable tableName onFail app = do
  serverState <- grab @(TVar ServerState) >>= readTVarIO
  maybe onFail app (serverState ^? #lobby . ix tableName)
