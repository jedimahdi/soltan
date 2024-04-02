module Soltan.App.Room where

import Control.Lens (Lens')
import Data.Generics.Labels ()
import qualified Data.Map as Map
import Soltan.App.Types
import Soltan.Data.Username (Username)

data RoomStatus = Full | NotFull

data Room = Room
  { users :: [Username]
  , creator :: Username
  , status :: RoomStatus
  }
  deriving (Generic)

roomCapacity :: Int
roomCapacity = 4

type RoomsState = TVar (Map RoomId Room)

initialRoomState :: IO RoomsState
initialRoomState =
  newTVarIO Map.empty

class HasRoomsState s where
  roomsStateL :: Lens' s RoomsState

handleRoomCommand :: (HasRoomsState s, HasSendCommand s) => s -> RoomCommand -> IO ()
handleRoomCommand s = \case
  Join username roomId -> joinRoom state sendCommand username roomId
  Leave _ -> pass
 where
  state = s ^. roomsStateL
  sendCommand = s ^. sendCommandL

joinRoom :: RoomsState -> SendCommand -> Username -> RoomId -> IO ()
joinRoom state sendCommand username id = do
  withRoom id state \room -> do
    atomically $ modifyTVar' state (ix id . #users %~ (username :))

withRoom :: (MonadIO m) => RoomId -> RoomsState -> (Room -> m ()) -> m ()
withRoom id state f = do
  rooms <- readTVarIO state
  case rooms ^? ix id of
    Nothing -> pass
    Just room -> do
      f room
