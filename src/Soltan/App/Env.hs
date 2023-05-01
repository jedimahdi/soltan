module Soltan.App.Env where

import Colog (HasLog (..), LogAction, Message)
import Soltan.App.Client (Clients)
import Soltan.App.Lobby (Lobby)
import Soltan.Data.Game (Game)
import qualified Soltan.Data.Game as Game
import qualified Soltan.Logger.Message as Logger.Message

type Games = TVar (Map Game.Id Game)

data Env (m :: Type -> Type) = Env
  { envLogAction :: !(LogAction m Logger.Message.Minimal)
  , envHub :: Clients
  , envLobby :: Lobby
  , envGames :: Games
  }

instance HasLog (Env m) Logger.Message.Minimal m where
  getLogAction :: Env m -> LogAction m Logger.Message.Minimal
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Logger.Message.Minimal -> Env m -> Env m
  setLogAction newAction env = env{envLogAction = newAction}
  {-# INLINE setLogAction #-}

class Has field env where
  obtain :: env -> field

instance Has (LogAction m Logger.Message.Minimal) (Env m) where obtain = envLogAction
instance Has Clients (Env m) where obtain = envHub
instance Has Lobby (Env m) where obtain = envLobby
instance Has Games (Env m) where obtain = envGames

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}
