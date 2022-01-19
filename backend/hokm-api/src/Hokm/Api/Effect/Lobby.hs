module Hokm.Api.Effect.Lobby
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

data LobbyL m a where
  AddGame :: Game.NotFull -> LobbyL m ()
  GetAll :: LobbyL m [Game.NotFull]
  -- DeleteGame :: Game.Id -> LobbyL m ()

makeSem ''LobbyL

run :: Members [AtomicState [Game.NotFull], Embed IO] r => Sem (LobbyL ': r) a -> Sem r a
run = interpret \case
          AddGame game -> atomicModify' (game :)
          GetAll       -> atomicGet


