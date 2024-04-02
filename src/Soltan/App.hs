module Soltan.App (App, initialApp, runApp, Channel, newChannel, Command, Outgoing) where

import Data.Generics.Labels ()
import Soltan.App.Auth
import Soltan.App.Channel
import Soltan.App.Game
import Soltan.App.Room
import Soltan.App.Types
import Soltan.Data.Username (Username)
import Soltan.Logger.Severity (Severity)

data App = App
  { sendCommand :: SendCommand
  , outgoingChannel :: TVar (Map Username (Channel Outgoing))
  , roomsState :: RoomsState
  , log :: Severity -> Text -> IO ()
  }
  deriving (Generic)

instance HasRoomsState App where
  roomsStateL = #roomsState

instance HasSendCommand App where
  sendCommandL = #sendCommand

runCommand :: App -> Command -> IO ()
runCommand app = \case
  RoomCommand c -> handleRoomCommand app c
  AuthCommand c -> handleAuthCommand app c

initialApp :: (Severity -> Text -> IO ()) -> TVar (Map Username (Channel Outgoing)) -> Channel Command -> IO App
initialApp log outgoingChannel commandChannel = do
  let sendCommand = writeChannel commandChannel
  roomsState <- initialRoomState
  pure App{..}

runApp :: Channel Command -> App -> IO ()
runApp commandChannel app = forever do
  command <- readChannel commandChannel
  runCommand app command
