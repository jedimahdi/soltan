module Hokm.Api.Effect.GamesState where

import           Control.Lens
    ( at, index, itraversed, ix, sans, to, traverseOf, traversed, view, (.~), (?~), (^.) )
import           Data.List          ( (!!) )
import qualified Data.List          as List
import           Data.List.Split    ( chunksOf )
import qualified Data.Map           as Map
import           Hokm.Api.Data.Card ( Card )
import qualified Hokm.Api.Data.Card as Card
import           Hokm.Api.Data.Game ( Game )
import qualified Hokm.Api.Data.Game as Game
import qualified Hokm.Api.Data.User as User
import           Polysemy           ( Embed, Member, Members, Sem, embed, interpret, makeSem )
import           Prelude            hiding ( state )

data Games = Games { lobby :: TVar (Map Game.Id Game.NotFull)
                   , games :: TVar (Map Game.Id (TVar Game))
                   }
  deriving stock (Generic)

data GamesStateL m a where
  JoinGame :: Game.Id -> (Game.NotFull -> Either Game.NotFull Game) -> GamesStateL m (Maybe (Either Game.NotFull Game))
  FindNotFullGame :: GamesStateL m (Maybe Game.NotFull)
  AddGameToLobby :: Game.NotFull -> GamesStateL m ()
  ModifyGame :: Game.Id -> (Game -> Either Game.Error Game) -> GamesStateL m (Either Game.Error Game)
  ModifyGameWithoutError :: Game.Id -> (Game -> Game) -> GamesStateL m (Maybe Game)

makeSem ''GamesStateL

run :: Members '[Embed IO] r => Games -> Sem (GamesStateL ': r) a -> Sem r a
run state = interpret \case
          JoinGame gameId joinFunc -> atomically do
            lobby <- readTVar (state ^. #lobby)
            case lobby ^. at gameId of
              Nothing -> pure Nothing
              Just pgame -> case joinFunc pgame of
                 Left notFull -> do
                   modifyTVar' (state ^. #lobby) (ix gameId .~ notFull)
                   pure . Just . Left <| notFull
                 Right game   -> do
                   modifyTVar' (state ^. #lobby) (sans gameId)
                   newGameVar <- newTVar game
                   modifyTVar' (state ^. #games) (at (game ^. #id) ?~ newGameVar)
                   pure . Just . Right <| game

          FindNotFullGame -> do
            gamesMap <- readTVarIO (state ^. #lobby)
            pure . viaNonEmpty head <| Map.elems gamesMap

          AddGameToLobby game -> atomically <| modifyTVar' (state ^. #lobby) (Map.insert (game ^. #id) game)

          ModifyGame gameId f -> do
            gamesMap <- readTVarIO (state ^. #games)
            let maybeGame = gamesMap ^. at gameId

            case maybeGame of
              Nothing -> pure <| Left Game.GameNotFound
              Just gameVar -> do
                atomically <| do
                  e <- f <$> readTVar gameVar
                  case e of
                    Left err -> pure <| Left err
                    Right g -> do
                      writeTVar gameVar g
                      pure <| Right g

          ModifyGameWithoutError gameId f -> do
            gamesMap <- readTVarIO (state ^. #games)
            let maybeGame = gamesMap ^. at gameId

            case maybeGame of
              Nothing -> pure Nothing
              Just gameVar -> do
                atomically <| do
                  newGame <- f <$> readTVar gameVar
                  writeTVar gameVar newGame
                  pure <| Just newGame
