module Hokm.Api.Network.Anatomy.Socket
    ( Routes (..)
    ) where

import           Data.Generics.Labels                 ()
import           Hokm.Api.Data.User
import           Hokm.Api.Network.Anatomy.Prelude
import qualified Hokm.Api.Network.Anatomy.Socket.Game as Game
import qualified Hokm.Api.Servant.Response            as Response
import           Servant.API.WebSocket

data Routes route = Routes { game :: route :- "game" :> ToServantApi Game.Routes
                           }
  deriving stock (Generic)
