{-# OPTIONS_GHC -Wno-redundant-constraints #-}
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

-- import Soltan.Socket.Table (withTable)

import Soltan.Effects.Lobby (ManageLobby)
import qualified Soltan.Effects.Lobby as Lobby
import Soltan.Socket.Prelude
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

msgHandler :: Client -> MsgIn -> LangL (Either Err MsgOut)
msgHandler _ GetTables = getTablesHandler
msgHandler client (SubscribeToTable tableName) = subscribeToTableHandler tableName client
msgHandler client (GameMsgIn msg) = gameMsgHandler msg client

withTable :: ManageLobby m => TableName -> (Table -> m (Either Err r)) -> m (Either Err r)
withTable tableName = Lobby.withTable tableName (pure . Left <| TableDoesNotExist tableName)

runPlayerAction :: TableName -> Game -> Action 'Unknown -> Either Err MsgOut
runPlayerAction tableName game action =
  validateAction game action |> bimap GameErr (NewGameState tableName . flip runAction game)

withPlayerIndex :: Applicative m => Username -> Game -> (PlayerIndex -> m (Either Err r)) -> m (Either Err r)
withPlayerIndex username game f = maybe (pure . Left <| PlayerNotInTheGame) f (getPlayerIndexWithUsername username game)

gameMsgHandler :: ManageLobby m => GameMsgIn -> Client -> m (Either Err MsgOut)
gameMsgHandler msg client = case msg of
  PlayCardMsg tableName card ->
    withTable tableName \table -> do
      let game = table ^. #game
      withPlayerIndex username game \playerIndex ->
        pure <| runPlayerAction tableName game (PlayCard playerIndex card)
  ChooseHokmMsg tableName suit ->
    withTable tableName \table -> do
      let game = table ^. #game
      withPlayerIndex username game \playerIndex ->
        pure <| runPlayerAction tableName game (ChooseHokm playerIndex suit)
 where
  username = client ^. #username

subscribeToTableHandler :: ManageLobby m => TableName -> Client -> m (Either Err MsgOut)
subscribeToTableHandler tableName client = do
  withTable tableName \table -> do
    let username = client ^. #username
    let isAlreadySubscribed = elemOf (#subscribers . traverse) username table
    unless isAlreadySubscribed <| Lobby.addSubscriber tableName username
    pure . Right <| SuccessfullySubscribedToTable tableName (table ^. #game)

getTablesHandler :: ManageLobby m => m (Either Err MsgOut)
getTablesHandler = do
  lobby <- Lobby.getLobby
  pure . Right <| TableList (summariseTables lobby)
