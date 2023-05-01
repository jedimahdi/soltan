module Soltan.App.Client where

import           Data.Generics.Labels ()
import qualified Network.WebSockets   as WS
import           Soltan.Data.Username (Username)

type Clients = TVar (Map Username Client)

data Client
  = Client
      { username   :: Username
      , connection :: WS.Connection
      }
  deriving stock (Generic)

mk :: MonadIO m => Username -> WS.Connection -> m Client
mk username conn = pure <| Client username conn
