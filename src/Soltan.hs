module Soltan where

import qualified Chronos
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (writeTChan)
import Control.Concurrent.STM.TChan (newTChanIO)
import qualified Data.Map as Map
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS
import Soltan.Data.Username (Username)
import Soltan.Game.Manager
import Soltan.Game.Types
import Soltan.Logger
import Soltan.Logger.Message (LogMessage (..))
import Soltan.Logger.Severity (Severity)
import Soltan.Socket
import Soltan.Socket.Types
import Prelude hiding ((>$<))

import Soltan.App
import UnliftIO.Concurrent (forkIO)

type Clients = TVar (Map Username WS.Connection)

main :: IO ()
main = do
  commandChannel <- newChannel
  outgoingChannels :: TVar (Map Username (Channel Outgoing)) <- newTVarIO mempty
  logChan <- newTChanIO
  let
    log :: Severity -> Text -> IO ()
    log severity message = do
      time <- liftIO Chronos.now
      withFrozenCallStack (logMsg logChan LogMessage{time, cs = callStack, ..})
  app <- initialApp log outgoingChannels commandChannel
  startLoggerThread logChan
  _ <- forkIO $ runSocketServer 5000 outgoingChannels commandChannel log
  runApp commandChannel app

-- main :: IO ()
-- main = do
--   gameActionsChan <- newTChanIO
--   clientsMap <- newTVarIO mempty
--   tablesMap <- newTVarIO mempty
--   logChan <- newTChanIO
--
--   let
--     sendMessage :: Username -> Message -> IO ()
--     sendMessage username msg = do
--       clients <- readTVarIO clientsMap
--       whenJust
--         (clients ^? ix username)
--         \client -> atomically $ writeTChan (client ^. #sendChan) msg
--
--     sendGameCommand :: TableId -> GameCommand -> IO ()
--     sendGameCommand id command = do
--       games <- readTVarIO tablesMap
--       whenJust
--         (games ^? ix id)
--         \table -> atomically $ writeTChan (table ^. #chan) command
--
--     log :: Severity -> Text -> IO ()
--     log severity message = do
--       time <- liftIO Chronos.now
--       withFrozenCallStack (logMsg logChan LogMessage{time, cs = callStack, ..})
--
--   startGameManagerThread gameActionsChan tablesMap sendMessage log
--   startLoggerThread logChan
--   run' 5000 clientsMap tablesMap (atomically . writeTChan gameActionsChan) sendGameCommand log
