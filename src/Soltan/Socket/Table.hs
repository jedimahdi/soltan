{-# LANGUAGE NumericUnderscores #-}

module Soltan.Socket.Table where

import Control.Lens (ix, (^?))
import qualified Data.Map as Map
import Pipes (Consumer, Effect, Pipe, await, runEffect, yield, (>->))
import Pipes.Concurrent (fromInput, toOutput)
import Soltan.Data.Has (grab)
import Soltan.Data.Username (Username)
import Soltan.Effects.Clients (AcquireClients, ManageClients)
import qualified Soltan.Effects.Clients as Clients
import Soltan.Effects.Concurrent (Concurrent)
import qualified Soltan.Effects.Concurrent as Concurrent
import Soltan.Effects.Lobby (AcquireLobby, ManageLobby)
import qualified Soltan.Effects.Lobby as Lobby
import Soltan.Effects.LogMessages (HasLog, LogMessages)
import qualified Soltan.Effects.LogMessages as Logger
import Soltan.Effects.Random (MonadRandom)
import qualified Soltan.Effects.Random as Random
import Soltan.Hokm (Action (..), Game, getPlayerIndexWithUsername, isEndOfTrick, mkGameSummary, nextStage, runAction)
import Soltan.Socket.Types (Client (..), MsgOut (..), ServerState (..), Table (..), TableName)
import System.Random (StdGen, getStdGen)
import UnliftIO (MonadUnliftIO, async)

setupTablePipeline ::
  forall m.
  (Concurrent m, ManageLobby m, AcquireClients m, MonadRandom m, HasLog m) =>
  TableName ->
  Table ->
  m ()
setupTablePipeline tableName Table{..} = do
  Concurrent.forkProcess <| infinitely <| runEffect <| pipeline
 where
  pipeline :: Effect m ()
  pipeline =
    Concurrent.fromInput gameOutMailbox
      >-> logPipe
      >-> broadcast tableName
      >-> updateTable tableName
      >-> nextRound tableName

nextRound :: (Concurrent m, AcquireLobby m, MonadRandom m, HasLog m) => TableName -> Consumer Game m ()
nextRound tableName = do
  game <- await
  when (isEndOfTrick game) do
    Logger.info "Is end of trick ======"
    lift <| Concurrent.threadDelay 1_000_000
    Logger.info "Is end of trick ====== After delay"
    lift <| Lobby.withTable tableName (Logger.warning "Did not find the table") \table -> do
      Logger.info <| "Found the table" <> show table
      gen <- Random.generateStdGen
      Logger.info <| "Random " <> show gen
      let newGame = nextStage gen game
      Logger.info <| "New Stage Game: " <> show newGame
      runEffect $ yield newGame >-> Concurrent.toOutput (table ^. #gameInMailbox)

logPipe :: HasLog m => Show a => Pipe a a m ()
logPipe = do
  a <- await
  Logger.debug <| "Logging from logPipe " <> show a
  yield a

broadcast :: (Concurrent m, AcquireLobby m, AcquireClients m) => TableName -> Pipe Game Game m ()
broadcast tableName = do
  g <- await
  lift <| Lobby.withTable tableName pass \table -> do
    clients <- Clients.getClients
    let cs = mapMaybe (\username -> clients ^? ix username) (table ^. #subscribers)
    Concurrent.forkProcess <| mapM_ (informSubscriber tableName g) cs
  yield g

informSubscriber :: Concurrent m => TableName -> Game -> Client -> m ()
informSubscriber tableName game Client{..} = do
  case getPlayerIndexWithUsername username game of
    Nothing -> pass
    Just playerIndex ->
      runEffect <| yield (NewGameStateSummary tableName (mkGameSummary playerIndex game)) >-> Concurrent.toOutput outgoingMailbox

updateTable :: ManageLobby m => TableName -> Pipe Game Game m ()
updateTable tableName = do
  game <- await
  Lobby.updateGame tableName game
  yield game

withTable :: MonadIO m => TVar ServerState -> TableName -> m r -> (Table -> m r) -> m r
withTable s tableName onFail app = do
  serverState <- readTVarIO s
  maybe onFail app (serverState ^? #lobby . ix tableName)