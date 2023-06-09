module Soltan where

import Colog
import qualified Control.Concurrent.Async as Async
import qualified Data.Map as Map
import qualified Network.Wai.Handler.Warp as Warp
import qualified Soltan.Effects.LogMessages as Logger
import Soltan.Logger.Message (Scope (..))
import qualified Soltan.Logger.Message as Logger.Message
import Soltan.Socket (runSocketServer)
import Prelude hiding ((>$<))

data Config = Config
  { apiPort :: Int
  , socketPort :: Int
  , logAction :: forall m. MonadIO m => LogAction m Logger.Message.Minimal
  }

main :: IO ()
main = do
  -- let config@Config{..} = Config 8000 5000 (Logger.Message.Scoped Api >$< Logger.logScopedMessageToStdStreams)
  runSocketServer 5000

-- beforeMainLoopHook :: MonadIO m => Config -> m ()
-- beforeMainLoopHook Config{..} =
--   usingLoggerT logAction
--     <| Logger.info ("Started listening on 127.0.0.1:" <> show apiPort <> ". Websocket server on ws://localhost:" <> show socketPort)
