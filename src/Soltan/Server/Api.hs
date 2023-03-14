module Soltan.Server.Api where

import qualified Soltan.Server.Lobby   as Lobby
import           Soltan.Server.Prelude

data Routes route
  = Routes
      { lobby :: route :- "lobby" :> Lobby.Api
      , game :: route :- "game" :> Game.Api
      }
  deriving stock (Generic)

type Api = ToApi Routes

server :: Routes AppServer
server = Routes
  { lobby = toServant Lobby.server
  }
