module Hokm.Api.Network.Anatomy.Socket.Game
    ( Routes (..)
    ) where

import           Data.Generics.Labels             ()
import           Hokm.Api.Data.Game               ( Game )
import qualified Hokm.Api.Data.Game               as Game
import           Hokm.Api.Network.Anatomy.Prelude
import qualified Hokm.Api.Servant.Response        as Response
import qualified Hokm.Data.Validation             as Validation
import           Servant.API.WebSocket

data Routes route = Routes { joinGame :: route :- Capture "id" Game.Id :> WebSocket
                           }
  deriving stock (Generic)
