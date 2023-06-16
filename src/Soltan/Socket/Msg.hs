{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Soltan.Socket.Msg where

import Control.Lens (elemOf, ix, (<>~), (^?))
import Pipes (Pipe, await, runEffect, yield, (>->))
import Pipes.Concurrent (Output, send)
import Soltan.Data.Has (grab)
import Soltan.Data.Username (Username)
import Soltan.Effects.Concurrent (Concurrent)
import qualified Soltan.Effects.Concurrent as Concurrent
import Soltan.Effects.Lobby (ManageLobby)
import qualified Soltan.Effects.Lobby as Lobby
import Soltan.Effects.LogMessages (LogMessages)
import qualified Soltan.Effects.LogMessages as Logger
import Soltan.Effects.Now (Now)
import Soltan.Hokm (Game, initialDeck, runAction, validateAction)
import Soltan.Hokm.Types (Action (..), PlayerIndex)
import Soltan.Hokm.Utils (getPlayerIndexWithUsername)
import Soltan.Socket.Lobby (summariseTables)
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

runPlayerAction :: TableName -> Game -> Action -> Either Err MsgOut
runPlayerAction tableName game action =
  runAction action game |> bimap GameErr (NewGameState tableName)

withPlayerIndex :: Applicative m => Username -> Game -> (PlayerIndex -> m (Either Err r)) -> m (Either Err r)
withPlayerIndex username game f = maybe (pure . Left <| PlayerNotInTheGame) f (getPlayerIndexWithUsername username game)

gameMsgHandler :: (ManageLobby m, LogMessages m, Now m) => GameMsgIn -> Client -> m (Either Err MsgOut)
gameMsgHandler msg client = do
  Logger.debug <| "Game Msg In handler for " <> show msg
  case msg of
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

subscribeToTableHandler :: (ManageLobby m, Concurrent m, LogMessages m, Now m) => TableName -> Client -> m (Either Err MsgOut)
subscribeToTableHandler tableName client = do
  Lobby.withTable tableName pass \table -> do
    let username = client ^. #username
    let subscribers = table ^. #subscribers
    let isAlreadySubscribed = elemOf traverse username subscribers
    unless isAlreadySubscribed <| Lobby.addSubscriber tableName username
    pass

  withTable tableName \table -> do
    let subscribers = table ^. #subscribers
    when (length subscribers == 4) do
      let deck = initialDeck
      let eNewGame = runAction (StartGame deck subscribers) (table ^. #game)
      case eNewGame of
        Left e -> pass
        Right newGame -> do
          Logger.info <| "Starting game with users " <> show subscribers <> " and game is " <> show newGame
          runEffect <| yield newGame >-> Concurrent.toOutput (table ^. #gameInMailbox)
    pure . Right <| SuccessfullySubscribedToTable tableName (table ^. #game)

getTablesHandler :: ManageLobby m => m (Either Err MsgOut)
getTablesHandler = do
  lobby <- Lobby.getLobby
  pure . Right <| TableList (summariseTables lobby)
