module Soltan.Socket.Msg where

import Control.Lens (elemOf, ix, (<>~), (^?))
import Pipes (Pipe, await, yield)
import Pipes.Concurrent (Output, send)
import Soltan.Data.Has (grab)
import Soltan.Data.Username (Username)
import Soltan.Hokm (ActionStatus (..), Game, runAction, validateAction)
import Soltan.Hokm.Types (Action (..), PlayerIndex)
import Soltan.Hokm.Utils (getPlayerIndexWithUsername)
import Soltan.Socket.Lobby (summariseTables)
import Soltan.Socket.Table (withTable)
import Soltan.Socket.Types (
  Client,
  Err (..),
  GameMsgIn (..),
  MsgHandlerConfig (..),
  MsgIn (..),
  MsgOut (..),
  ServerState (ServerState, clients, lobby),
  Table (..),
  TableDoesNotExistInLobby (..),
  TableName,
  WithClient,
  WithServerState,
 )

msgHandler :: WithServerState env m => Client -> MsgIn -> m (Either Err MsgOut)
msgHandler _ GetTables = getTablesHandler
msgHandler client (SubscribeToTable tableName) = subscribeToTableHandler tableName client
msgHandler client (GameMsgIn msg) = gameMsgHandler msg client

withTable_ :: WithServerState env m => TableName -> (Table -> m (Either Err r)) -> m (Either Err r)
withTable_ tableName = withTable tableName (pure . Left <| TableDoesNotExist tableName)

runPlayerAction :: WithServerState env m => TableName -> Game -> Action 'Unknown -> m (Either Err MsgOut)
runPlayerAction tableName game action = do
  serverStateTVar <- grab @(TVar ServerState)
  let eValidAction = validateAction game action
  case eValidAction of
    Left e -> pure . Left <| GameErr e
    Right validAction -> do
      let newGame = runAction validAction game
      atomically
        <| modifyTVar' serverStateTVar (#lobby . ix tableName . #game .~ newGame)
      pure . Right <| NewGameState tableName newGame

withPlayerIndex :: Applicative m => Username -> Game -> (PlayerIndex -> m (Either Err r)) -> m (Either Err r)
withPlayerIndex username game f = maybe (pure . Left <| PlayerNotInTheGame) f (getPlayerIndexWithUsername username game)

gameMsgHandler :: WithServerState env m => GameMsgIn -> Client -> m (Either Err MsgOut)
gameMsgHandler msg client = case msg of
  PlayCardMsg tableName card ->
    withTable_ tableName \table -> do
      let username = client ^. #username
      let game = table ^. #game
      withPlayerIndex username game \playerIndex ->
        runPlayerAction tableName game (PlayCard playerIndex card)
  ChooseHokmMsg tableName suit ->
    withTable_ tableName \table -> do
      let username = client ^. #username
      let game = table ^. #game
      withPlayerIndex username game \playerIndex ->
        runPlayerAction tableName game (ChooseHokm playerIndex suit)

subscribeToTableHandler :: WithServerState env m => TableName -> Client -> m (Either Err MsgOut)
subscribeToTableHandler tableName client = do
  serverStateTVar <- grab @(TVar ServerState)
  withTable_ tableName \table -> do
    let username = client ^. #username
    let isAlreadySubscribed = elemOf (#subscribers . traverse) username table
    unless isAlreadySubscribed
      <| atomically
      <| modifyTVar' serverStateTVar (#lobby . ix tableName . #subscribers <>~ [username])
    pure <| Right <| SuccessfullySubscribedToTable tableName (table ^. #game)

getTablesHandler :: WithServerState env m => m (Either Err MsgOut)
getTablesHandler = do
  ServerState{..} <- grab @(TVar ServerState) >>= readTVarIO
  pure <| Right <| TableList (summariseTables lobby)
