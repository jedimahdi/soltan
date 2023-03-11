module Soltan.App.Lobby where

import qualified Soltan.Data.Game as Game
import Soltan.Data.Username (Username)
import Data.Generics.Labels ()

data RoomStatus = Open | Full
  deriving stock (Eq, Show, Generic)

data SRoomStatus (s :: RoomStatus) where
  SOpen :: SRoomStatus 'Open
  SFull :: SRoomStatus 'Full

fromSRoomStatus :: SRoomStatus s -> RoomStatus
fromSRoomStatus SOpen = Open
fromSRoomStatus SFull = Full

data Room (s :: RoomStatus) = Room
  { id     :: Game.Id
  , users  :: [Username]
  , status :: RoomStatus
  }
  deriving stock (Generic)

data SomeRoom where
  SomeRoom :: Room s -> SomeRoom

withSomeRoom :: SomeRoom -> (forall (s :: RoomStatus). Room s -> r) -> r
withSomeRoom (SomeRoom r) f = f r

type Lobby = TVar (Map Game.Id SomeRoom)
