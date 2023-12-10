module Soltan where

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (writeTChan)
import Control.Concurrent.STM.TChan (newTChanIO)
import qualified Data.Map as Map
import qualified Network.Wai.Handler.Warp as Warp
import Soltan.Data.Username (Username)
import Soltan.Game.Manager
import Soltan.Game.Types
import Soltan.Logger.Message (Scope (..))
import qualified Soltan.Logger.Message as Logger.Message
import Soltan.Socket
import Soltan.Socket.Types
import Prelude hiding ((>$<))

main :: IO ()
main = do
  gameActionsChan <- newTChanIO
  clientsMap <- newTVarIO mempty
  tablesMap <- newTVarIO mempty

  let
    sendMessage :: Username -> Message -> IO ()
    sendMessage username msg = do
      clients <- readTVarIO clientsMap
      whenJust
        (clients ^? ix username)
        \client -> atomically $ writeTChan (client ^. #sendChan) msg

    sendGameCommand :: TableId -> GameCommand -> IO ()
    sendGameCommand id command = do
      games <- readTVarIO tablesMap
      whenJust
        (games ^? ix id)
        \table -> atomically $ writeTChan (table ^. #chan) command

  startGameManagerThread gameActionsChan tablesMap sendMessage
  run' 5000 clientsMap tablesMap (atomically . writeTChan gameActionsChan) sendGameCommand

-- let config@Config{..} = Config 8000 5000 (Logger.Message.Scoped Api >$< Logger.logScopedMessageToStdStreams)
-- data Config = Config
--   { apiPort :: Int
--   , socketPort :: Int
--   , logAction :: forall m. (MonadIO m) => LogAction m Logger.Message.Minimal
--   }

-- runSocketServer 5000

-- beforeMainLoopHook :: MonadIO m => Config -> m ()
-- beforeMainLoopHook Config{..} =
--   usingLoggerT logAction
--     <| Logger.info ("Started listening on 127.0.0.1:" <> show apiPort <> ". Websocket server on ws://localhost:" <> show socketPort)
