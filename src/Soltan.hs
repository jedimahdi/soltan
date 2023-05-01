module Soltan where

import Colog
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
import Prelude hiding ((>$<))

data Config = Config
  { port :: Word16
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
  let config@Config{..} = Config 8080 (Logger.Message.Scoped Api >$< Logger.logScopedMessageToStdStreams)
  env <- mkAppEnv config
  let settings =
        Warp.defaultSettings
          |> Warp.setPort (fromIntegral port)
          |> Warp.setBeforeMainLoop (beforeMainLoopHook config)
  Warp.runSettings settings . corsMiddleware . logMiddleware logAction <| application env

beforeMainLoopHook :: MonadIO m => Config -> m ()
beforeMainLoopHook Config{logAction, port} =
  usingLoggerT logAction
    <| Logger.info ("Started listening on 127.0.0.1:" <> show port <> ".")
