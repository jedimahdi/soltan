module Soltan.Effect.GameState where

import qualified Soltan.Data.Game        as Game
import           Soltan.Data.Game        (Game)
import           Soltan.Data.Game.Action
import           Soltan.Data.Username    (Username)
import           Soltan.Game.Action
import Soltan.App.Monad (App)
import           Soltan.App.Env         (Has (..), grab, Games)
import Control.Lens (at)
import qualified Data.Map as Map

class Monad m => GameState m where
  dispatchAction :: Game.Id -> Action 'Valid -> m ()
  addGame :: Game -> m ()
  findGameById :: Game.Id -> m (Maybe Game)

instance GameState App where
  dispatchAction = dispatchActionImpl
  addGame = addGameImpl
  findGameById = findGameByIdImpl

type WithGames r m = (MonadReader r m, Has Games r, MonadIO m)

withGame :: WithGames r m => Game.Id -> (Game -> m ()) -> m ()
withGame gameId action = do
  gamesVar <- grab @Games
  games <- readTVarIO gamesVar
  maybe pass action (games ^. at gameId)

dispatchActionImpl :: WithGames r m => Game.Id -> Action 'Valid -> m ()
dispatchActionImpl gameId action = do
  gamesVar <- grab @Games
  atomically $ modifyTVar' gamesVar (Map.adjust (gameReducer action) gameId)

dispatchAction' :: GameState m => Game.Id -> Action 'Unknown -> m ()
dispatchAction' gameId unknownAction = do
  maybeGame <- findGameById gameId
  case maybeGame of
    Nothing -> pass
    Just game -> do
      case validateAction unknownAction game of
        Left _ -> pass
        Right action -> do
          dispatchAction gameId action
    

-- dispatchActionImpl :: WithGames r m => Game.Id -> Action 'Unknown -> m ()
-- dispatchActionImpl gameId unknownAction =
--   withGame gameId \game -> do
--     case canPerformAction unknownAction game of
--       Left _ -> pass
--       Right action -> do
--         let newGame = gameReducer action game
--         pass

addGameImpl :: WithGames r m => Game -> m ()
addGameImpl game = do
  gamesVar <- grab @Games
  atomically <| modifyTVar' gamesVar (Map.insert (game ^. #id) game)
  
findGameByIdImpl :: WithGames r m => Game.Id -> m (Maybe Game)
findGameByIdImpl gameId = do
  gamesVar <- grab @Games
  games <- readTVarIO gamesVar
  pure <| games ^. at gameId

