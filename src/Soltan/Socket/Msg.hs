{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Soltan.Socket.Msg where

import Control.Lens (elemOf, ix, lengthOf, (<>~), (^?))
import Control.Monad.Trans.Except (except, throwE)
import qualified Data.List as List
import qualified Soltan.Data.Four as Four
import Soltan.Data.Username (Username)
import Soltan.Effects.Lobby (AcquireLobby)
import qualified Soltan.Effects.Lobby as Lobby
import Soltan.Effects.LogMessages (HasLog)
import qualified Soltan.Effects.LogMessages as Logger
import Soltan.Effects.Random (MonadRandom)
import qualified Soltan.Effects.Random as Random
import Soltan.Hokm (Action (..), Game, PlayerIndex, getPlayerIndexWithUsername, mkGameSummary, runAction, startGame, validateAction)
import Soltan.Socket.Lobby (summariseTable, summariseTables)
import Soltan.Socket.Types (
  Client,
  Task (..),
  Err (..),
  GameMsgIn (..),
  MsgIn (..),
  MsgOut (..),
  Table (..),
  TableDoesNotExistInLobby (..),
  TableName,
 )

msgHandler :: (AcquireLobby m, HasLog m, MonadRandom m) => Username -> MsgIn -> m [Task]
msgHandler username msg =
  case msg of
    Login _ -> pure []
    GetTables -> getTablesHandler
    SubscribeToTable tableName -> subscribeToTableHandler tableName username
    GameMsgIn gameMsg -> gameMsgHandler gameMsg username
    |> runExceptT
    |> fmap (either (List.singleton . SendMsg . ErrMsg) identity)

getTable :: AcquireLobby m => TableName -> ExceptT Err m Table
getTable tableName = ExceptT <| fmap (maybeToRight (TableDoesNotExist tableName)) <| Lobby.getTable tableName

getPlayerIndex :: Monad m => Username -> Game -> ExceptT Err m PlayerIndex
getPlayerIndex username game = maybe (throwE PlayerNotInTheGame) pure (getPlayerIndexWithUsername username game)

runPlayerAction :: AcquireLobby m => TableName -> Username -> (PlayerIndex -> Action) -> ExceptT Err m [Task]
runPlayerAction tableName username mkAction = do
  table <- getTable tableName
  let game = table ^. #game
  playerIndex <- getPlayerIndex username game
  hoistEither
    <| bimap GameErr (List.singleton . NewGameState tableName)
    <| runAction (mkAction playerIndex) game

gameMsgHandler :: (AcquireLobby m, HasLog m) => GameMsgIn -> Username -> ExceptT Err m [Task]
gameMsgHandler msg username = do
  Logger.debug <| "Game Msg In handler for " <> show msg
  case msg of
    PlayCardMsg tableName card ->
      runPlayerAction tableName username (`PlayCard` card)
    ChooseHokmMsg tableName suit ->
      runPlayerAction tableName username (`ChooseHokm` suit)

subscribeToTableHandler :: (AcquireLobby m, HasLog m, MonadRandom m) => TableName -> Username -> ExceptT Err m [Task]
subscribeToTableHandler tableName username = do
  table <- getTable tableName
  let subscribers = table ^. #subscribers
      isAlreadySubscribed = elemOf traverse username subscribers
      isAlreadyFull = lengthOf traverse subscribers >= 4
      newSubscribers =
        if isAlreadySubscribed
          then subscribers
          else username : subscribers
      tableSummary = summariseTable tableName table
      joinCommand = [JoinLobby tableName username | not isAlreadySubscribed]
      msg = SuccessfullySubscribedToTable tableName tableSummary
  when isAlreadyFull <| throwE (TableIsFull tableName)
  case Four.mkFromList newSubscribers of
    Nothing -> pure <| joinCommand <> [SendMsg msg]
    Just users -> do
      gen <- Random.generateStdGen
      let newGame = startGame gen users (table ^. #game)
      pure <| joinCommand <> [SendMsg msg, NewGameState tableName newGame]

getTablesHandler :: AcquireLobby m => ExceptT Err m [Task]
getTablesHandler = do
  lobby <- Lobby.getLobby
  let msgOut = TableList (summariseTables lobby)
  pure [SendMsg msgOut]
