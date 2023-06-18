{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Soltan.Socket.Msg where

import Control.Lens (elemOf, ix, (<>~), (^?))
import qualified Data.List as List
import qualified Soltan.Data.Four as Four
import Soltan.Data.Username (Username)
import Soltan.Effects.Lobby (AcquireLobby)
import qualified Soltan.Effects.Lobby as Lobby
import Soltan.Effects.LogMessages (HasLog)
import qualified Soltan.Effects.LogMessages as Logger
import Soltan.Effects.Random (MonadRandom)
import qualified Soltan.Effects.Random as Random
import Soltan.Hokm (Game, runAction, validateAction)
import Soltan.Hokm.Hokm (startGame)
import Soltan.Hokm.Types (Action (..), PlayerIndex)
import Soltan.Hokm.Utils (getPlayerIndexWithUsername)
import Soltan.Socket.Lobby (summariseTables)
import Soltan.Socket.Types (
  Client,
  Command (..),
  Err (..),
  GameMsgIn (..),
  MsgIn (..),
  MsgOut (..),
  Table (..),
  TableDoesNotExistInLobby (..),
  TableName,
 )

msgHandler :: (AcquireLobby m, HasLog m, MonadRandom m) => Client -> MsgIn -> m (Either Err [Command])
msgHandler _ GetTables = getTablesHandler
msgHandler client (SubscribeToTable tableName) = subscribeToTableHandler tableName client
msgHandler client (GameMsgIn msg) = gameMsgHandler msg client

withTable :: AcquireLobby m => TableName -> (Table -> m (Either Err r)) -> m (Either Err r)
withTable tableName = Lobby.withTable tableName (pure . Left <| TableDoesNotExist tableName)

withPlayerIndex :: Applicative m => Username -> Game -> (PlayerIndex -> m (Either Err r)) -> m (Either Err r)
withPlayerIndex username game f = maybe (pure . Left <| PlayerNotInTheGame) f (getPlayerIndexWithUsername username game)

runPlayerAction :: AcquireLobby m => TableName -> Username -> (PlayerIndex -> Action) -> m (Either Err [Command])
runPlayerAction tableName username mkAction =
  withTable tableName \table -> do
    let game = table ^. #game
    withPlayerIndex username game \playerIndex ->
      runAction (mkAction playerIndex) game |> bimap GameErr (NewGameState tableName) |> fmap List.singleton |> pure

gameMsgHandler :: (AcquireLobby m, HasLog m) => GameMsgIn -> Client -> m (Either Err [Command])
gameMsgHandler msg client = do
  Logger.debug <| "Game Msg In handler for " <> show msg
  case msg of
    PlayCardMsg tableName card ->
      runPlayerAction tableName username (`PlayCard` card)
    ChooseHokmMsg tableName suit ->
      runPlayerAction tableName username (`ChooseHokm` suit)
 where
  username = client ^. #username

subscribeToTableHandler :: (AcquireLobby m, HasLog m, MonadRandom m) => TableName -> Client -> m (Either Err [Command])
subscribeToTableHandler tableName client = do
  withTable tableName \table -> do
    let username = client ^. #username
    let subscribers = table ^. #subscribers
    let isAlreadySubscribed = elemOf traverse username subscribers
    let newSubscribers = if isAlreadySubscribed then subscribers else username : subscribers
    let joinCommand = [JoinLobby tableName username | not isAlreadySubscribed]
    let msg = SuccessfullySubscribedToTable tableName (table ^. #game)
    case Four.mkFromList newSubscribers of
      Nothing -> pure <| Right <| joinCommand <> [SendMsg msg]
      Just users -> do
        gen <- Random.generateStdGen
        let newGame = startGame gen users (table ^. #game)
        Logger.info <| "Starting game with users " <> show subscribers <> " and game is " <> show newGame
        pure <| Right <| joinCommand <> [SendMsg msg, NewGameState tableName newGame]

getTablesHandler :: AcquireLobby m => m (Either Err [Command])
getTablesHandler = do
  lobby <- Lobby.getLobby
  let msgOut = TableList (summariseTables lobby)
  pure . Right <| [SendMsg msgOut]
