module Hokm.Api.Effect.GameState
    where

import           Control.Lens
    ( at, index, itraversed, ix, sans, traverseOf, traversed, view, (.~), (?~), (^.) )
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
import           Polysemy.AtomicState ( AtomicState, atomicGet, atomicModify', atomicState' )

type Games = Map Game.Id (TVar Game)

data GameStateL m a where
  GetGame :: Game.Id -> GameStateL m (Maybe Game)
  AddGame :: Game -> GameStateL m ()
  DeleteGame :: Game.Id -> GameStateL m ()
  ModifyGame :: Game.Id -> (Game -> Game) -> GameStateL m (Maybe Game)
  GameAtomicState :: Game.Id -> (Game -> Either Game.Error Game) -> GameStateL m (Maybe Game)

makeSem ''GameStateL

run :: Members [AtomicState Games, Embed IO] r => Sem (GameStateL ': r) a -> Sem r a
run = interpret \case
          GetGame gameId -> do
            gamesMap <- atomicGet
            gamesMap ^. at gameId |> traverse readTVarIO

          AddGame game -> do
            newGame <- newTVarIO game
            atomicModify' <| at (game ^. #id) ?~ newGame

          DeleteGame gameId ->
            atomicModify' <| sans gameId

          ModifyGame gameId f -> do
            gamesMap <- atomicGet
            let maybeGame = gamesMap ^. at gameId

            case maybeGame of
              Nothing -> pure Nothing
              Just gameVar -> do
                atomically <| do
                  newGame <- f <$> readTVar gameVar
                  writeTVar gameVar newGame
                  pure <| Just newGame


          GameAtomicState gameId f -> do
            gamesMap <- atomicGet
            let maybeGame = gamesMap ^. at gameId

            case maybeGame of
              Nothing -> pure Nothing
              Just gameVar -> do
                atomically <| do
                  e <- f <$> readTVar gameVar
                  case e of
                    Left _ -> pure Nothing
                    Right g -> do
                      writeTVar gameVar g
                      pure <| Just g
