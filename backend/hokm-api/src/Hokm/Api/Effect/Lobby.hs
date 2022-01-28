module Hokm.Api.Effect.Lobby
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
import qualified Polysemy.AtomicState as AtomicState
    ( AtomicState, atomicGet, atomicModify', atomicState' )

type LobbyGames = Map Game.Id Game.NotFull

data LobbyL m a where
  ModifyGame :: Game.Id -> (Game.NotFull -> Game.NotFull) -> LobbyL m (Maybe Game.NotFull)
  AtomicState :: (LobbyGames -> (LobbyGames, a)) -> LobbyL m a
  GetAll :: LobbyL m [Game.NotFull]
  AddGame :: Game.NotFull -> LobbyL m ()

makeSem ''LobbyL

run :: Members [AtomicState.AtomicState (Map Game.Id Game.NotFull), Embed IO] r => Sem (LobbyL ': r) a -> Sem r a
run = interpret \case
          GetAll       -> do
            Map.elems <$> AtomicState.atomicGet

          ModifyGame gameId f -> do
            AtomicState.atomicState' (\m -> case m ^. at gameId of
                                  Just g  -> (m |> ix gameId .~ f g, Just <| f g)
                                  Nothing -> (m, Nothing))
          AtomicState f -> do
            AtomicState.atomicState' f

          AddGame game -> do
            AtomicState.atomicModify' (Map.insert (game ^. #id) game)
