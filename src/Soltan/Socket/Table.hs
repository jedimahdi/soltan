{-# LANGUAGE NumericUnderscores #-}
module Soltan.Socket.Table where

import Control.Lens (ix, (^?))
import qualified Data.Map as Map
import Pipes (Consumer, Effect, Pipe, await, runEffect, yield, (>->))
import Pipes.Concurrent (fromInput, toOutput)
import Soltan.Data.Has (grab)
import Soltan.Data.Username (Username)
import Soltan.Effects.LogMessages (LogMessages)
import qualified Soltan.Effects.LogMessages as Logger
import Soltan.Hokm (Action (..), runAction)
import Soltan.Hokm.Types (Game)
import Soltan.Hokm.Utils (isEndOfRound, mkGameSummary)
import Soltan.Socket.Types (Client (..), MsgOut (..), ServerState (..), Table (..), TableName, WithServerState)
import UnliftIO (MonadUnliftIO, async)
import UnliftIO.Concurrent (threadDelay)

setupTablePipeline :: forall m. (MonadUnliftIO m) => TVar ServerState -> TableName -> Table -> m ()
setupTablePipeline s tableName Table{..} = do
  print <| "Game pipeline running for " <> tableName
  void <| async <| infinitely <| runEffect <| pipeline
 where
  pipeline :: Effect m ()
  pipeline =
    fromInput gameOutMailbox
      >-> logPipe
      >-> broadcast s tableName
      >-> updateTable s tableName
      >-> logPipe
      >-> nextRound s tableName

nextRound :: (MonadIO m) => TVar ServerState -> TableName -> Consumer Game m ()
nextRound s tableName = do
  game <- await
  when (isEndOfRound game) do
    threadDelay 1_000_000
    withTable s tableName pass \table -> do
      let eNewGame = runAction NextRound game
      case eNewGame of
        Left e -> pass
        Right newGame ->
          runEffect $ yield newGame >-> toOutput (table ^. #gameInMailbox)

logPipe :: MonadIO m => Show a => Pipe a a m ()
logPipe = do
  a <- await
  liftIO <| putStrLn <| "Logging from logPipe " <> show a
  yield a

broadcast :: (MonadUnliftIO m) => TVar ServerState -> TableName -> Pipe Game Game m ()
broadcast s tableName = do
  g <- await
  ServerState{..} <- readTVarIO s
  case lobby ^? ix tableName of
    Nothing -> pass
    Just Table{..} -> do
      let cs = mapMaybe (\username -> clients ^? ix username) subscribers
      void . lift . async <| mapM_ (informSubscriber tableName g) cs
      yield g

informSubscriber :: MonadIO m => TableName -> Game -> Client -> m ()
informSubscriber tableName game Client{..} = do
  case mkGameSummary username game of
    Nothing -> pass
    Just gameSummary ->
      runEffect <| yield (NewGameStateSummary tableName gameSummary) >-> toOutput outgoingMailbox

updateTable :: MonadIO m => TVar ServerState -> TableName -> Pipe Game Game m ()
updateTable s tableName = do
  game <- await
  atomically <| updateTableSTM s tableName game
  yield game

updateTableSTM :: TVar ServerState -> TableName -> Game -> STM ()
updateTableSTM serverStateTVar tableName game = modifyTVar' serverStateTVar (#lobby . ix tableName . #game .~ game)

withTable :: MonadIO m => TVar ServerState -> TableName -> m r -> (Table -> m r) -> m r
withTable s tableName onFail app = do
  serverState <- readTVarIO s
  maybe onFail app (serverState ^? #lobby . ix tableName)
