{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Soltan.SocketApp where

import Colog (
  HasLog (..),
  LogAction (..),
  logMsg,
  logTextStderr,
  logTextStdout,
  (&>),
 )
import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Random.Class (MonadRandom)
import qualified Control.Monad.Trans.Reader as ReaderT
import Data.Generics.Labels ()
import qualified Network.WebSockets as WS
import Relude.Extra.Bifunctor (firstF)
import Soltan.Data.Has (Has (..))
import Soltan.Logger.Message (Scope (..))
import qualified Soltan.Logger.Message as Logger.Message
import Soltan.Logger.Severity (Severity (..))
import Soltan.Socket.Types (Client, ServerState)
import UnliftIO (MonadUnliftIO)

data SocketEnv (m :: Type -> Type) = SocketEnv
  { conn :: WS.Connection
  , serverState :: TVar ServerState
  , logAction :: !(LogAction m Logger.Message.Minimal)
  }
  deriving stock (Generic)

instance Has (TVar ServerState) (SocketEnv m) where obtain = view #serverState
instance Has WS.Connection (SocketEnv m) where obtain = view #conn
instance HasLog (SocketEnv m) Logger.Message.Minimal m where
  getLogAction :: SocketEnv m -> LogAction m Logger.Message.Minimal
  getLogAction = logAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Logger.Message.Minimal -> SocketEnv m -> SocketEnv m
  setLogAction newAction env = env{logAction = newAction}
  {-# INLINE setLogAction #-}

mkEnv :: MonadIO m => WS.Connection -> TVar ServerState -> SocketEnv m
mkEnv conn s = SocketEnv conn s (Logger.Message.Scoped WebSocket >$< logScopedMessageToStdStreams)

newtype SocketApp a = SocketApp
  { unApp :: ReaderT (SocketEnv SocketApp) IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (SocketEnv SocketApp), MonadRandom, MonadMask, MonadCatch, MonadThrow, MonadUnliftIO)

runSocketApp :: SocketEnv SocketApp -> SocketApp a -> IO a
runSocketApp env app = runReaderT (unApp app) env

newtype WithClientM m a = WithClientM
  {unWithClient :: ReaderT Client m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadRandom, MonadReader Client, MonadMask, MonadCatch, MonadThrow, MonadUnliftIO, MonadTrans)

-- instance MonadReader (SocketEnv SocketApp) ()

runWithClient :: Client -> WithClientM m a -> m a
runWithClient client = usingReaderT client . unWithClient

-------------------------------------------------------
-- Log Actions

logScopedMessageToStdStreams :: MonadIO m => LogAction m Logger.Message.Scoped
logScopedMessageToStdStreams = LogAction \message@(Logger.Message.Scoped _ Logger.Message.Minimal{severity}) ->
  let action = if severity == Error || severity == Panic then logTextStderr else logTextStdout in Logger.Message.prettyPrintScoped message &> action
