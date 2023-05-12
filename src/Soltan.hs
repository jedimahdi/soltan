module Soltan where

import Colog
import qualified Control.Concurrent.Async as Async
import qualified Data.Map as Map
import qualified Network.Wai.Handler.Warp as Warp
import Soltan.App.Env
import Soltan.App.Monad
import qualified Soltan.Logger as Logger
import Soltan.Logger.Message (Scope (..))
import qualified Soltan.Logger.Message as Logger.Message
import Soltan.Network.Wai.Cors (corsMiddleware)
import Soltan.Network.Wai.Log (logMiddleware)
import Soltan.Server (application)
import Soltan.Socket (runSocketServer)
import Prelude hiding ((>$<))

data Config = Config
  { apiPort :: Int
  , socketPort :: Int
  , logAction :: forall m. MonadIO m => LogAction m Logger.Message.Minimal
  }

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config{..} = do
  envLobby <- newTVarIO Map.empty
  envHub <- newTVarIO Map.empty
  envGames <- newTVarIO Map.empty
  let envLogAction = logAction
  pure Env{..}

main :: IO ()
main = do
  let config@Config{..} = Config 8000 5000 (Logger.Message.Scoped Api >$< Logger.logScopedMessageToStdStreams)
  env <- mkAppEnv config
  let settings =
        Warp.defaultSettings
          |> Warp.setPort apiPort
          |> Warp.setBeforeMainLoop (beforeMainLoopHook config)
  let serverApp = Warp.runSettings settings . corsMiddleware . logMiddleware logAction <| application env
  let socketApp = runSocketServer socketPort
  Async.concurrently_ serverApp socketApp

beforeMainLoopHook :: MonadIO m => Config -> m ()
beforeMainLoopHook Config{..} =
  usingLoggerT logAction
    <| Logger.info ("Started listening on 127.0.0.1:" <> show apiPort <> ". Websocket server on ws://localhost:" <> show socketPort)
