module Hokm.Api.Network.Anatomy
    ( Routes (..)
    ) where


import           Data.Generics.Labels        ()
import qualified Hokm.Api.Network.Anatomy.Api    as Api
import qualified Hokm.Api.Network.Anatomy.Socket as Socket
import           Servant
import           Servant.API.Generic

data Routes route = Routes { api    :: route :- "api" :> ToServantApi Api.Routes
                           , socket :: route :- "socket" :> ToServantApi Socket.Routes
                           }
  deriving stock (Generic)
