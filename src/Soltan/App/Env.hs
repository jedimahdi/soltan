module Soltan.App.Env where

import Colog (HasLog (..), LogAction, Message)
import Soltan.App.Client (Clients)
import Soltan.App.Lobby (Lobby)

data Env (m :: Type -> Type) = Env
    {  envLogAction :: !(LogAction m Message)
    ,  envHub       :: Clients
    ,  envLobby     :: Lobby
    }

instance HasLog (Env m) Message m where
    getLogAction :: Env m -> LogAction m Message
    getLogAction = envLogAction
    {-# INLINE getLogAction #-}

    setLogAction :: LogAction m Message -> Env m -> Env m
    setLogAction newAction env = env { envLogAction = newAction }
    {-# INLINE setLogAction #-}

class Has field env where
    obtain :: env -> field

instance Has (LogAction m Message) (Env m) where obtain = envLogAction
instance Has Clients (Env m) where obtain = envHub
instance Has Lobby (Env m) where obtain = envLobby

grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}
