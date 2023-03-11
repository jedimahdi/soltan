module Soltan.App.Client where

import qualified Network.WebSockets as WS
import Data.Generics.Labels ()
import Soltan.Data.Username (Username)

type Clients = TVar (Map Username Client)

data Client = Client
  { username   :: Username
  , connection :: WS.Connection
  }
  deriving stock (Generic)
