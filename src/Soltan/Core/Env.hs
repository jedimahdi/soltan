module Soltan.Core.Env where

import Colog (HasLog (..), LogAction, Message)

data Env (m :: Type -> Type) = Env
    {  envLogAction     :: !(LogAction m Message)
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

grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}
