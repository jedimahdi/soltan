module Hokm.Api.Effect.GameState
    where

import           Control.Lens         ( view, (^.) )
import           Data.List            ( (!!) )
import qualified Data.List            as List
import           Data.List.Split      ( chunksOf )
import qualified Data.Map             as Map
import           Hokm.Api.Data.Card   ( Card )
import qualified Hokm.Api.Data.Card   as Card
import           Hokm.Api.Data.Game   ( Game )
import qualified Hokm.Api.Data.Game   as Game
import qualified Hokm.Api.Data.User   as User
import           Polysemy             ( Embed, Member, Members, Sem, embed, interpret, makeSem )
import           Polysemy.AtomicState ( AtomicState, atomicGet, atomicModify' )

type Games = Map Game.Id (TVar Game)

data GameStateL m a where
  GetGame :: Game.Id -> GameStateL m (Maybe Game)
  GetGameList :: GameStateL m [Game]
  AddGame :: Game -> GameStateL m ()
  DeleteGame :: Game.Id -> GameStateL m ()
  ModifyGame :: Game.Id -> (Game -> Game) -> GameStateL m (Maybe Game)

makeSem ''GameStateL

run :: Members [AtomicState Games, Embed IO] r => Sem (GameStateL ': r) a -> Sem r a
run = interpret \case
          GetGame gameId -> do
            gamesMap <- atomicGet
            Map.lookup gameId gamesMap |> traverse readTVarIO

          GetGameList -> do
            gamesMap <- atomicGet
            Map.elems gamesMap |> traverse readTVarIO

          AddGame game -> do
            newGame <- newTVarIO game
            atomicModify' <| Map.insert (game ^. #id) newGame

          DeleteGame gameId ->
            atomicModify' <| Map.delete gameId

          ModifyGame gameId f -> do
            gamesMap <- atomicGet
            let maybeGame = Map.lookup gameId gamesMap

            case maybeGame of
              Nothing -> pure Nothing
              Just gameVar -> do
                atomically <| do
                  game <- readTVar gameVar
                  let newGame = f game
                  writeTVar gameVar newGame
                  pure <| Just newGame
