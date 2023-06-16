module Soltan.Socket.Prelude where

import Soltan.Effects.Clients (ManageClients)
import Soltan.Effects.Concurrent (Concurrent)
import Soltan.Effects.Lobby (ManageLobby)
import Soltan.Effects.LogMessages (LogMessages)
import Soltan.Effects.Now (Now)
import Soltan.Effects.WebSocket (WebSocket)

type AppL a = forall m. (WebSocket m, LogMessages m, Now m, ManageLobby m, ManageClients m, Concurrent m) => m a

type LangL a = forall m. (LogMessages m, Now m, ManageLobby m, Concurrent m) => m a
