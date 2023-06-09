module Soltan.Effects.Clients where

import Soltan.Socket.Types (Client)

class Monad m => ManageClients m where
  addClient :: Client -> m ()
