{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Soltan.App.Lobby where

import           Data.Generics.Labels ()
import qualified Soltan.Data.Game     as Game
import           Soltan.Data.Username (Username)

data RoomStatus = Open | Full deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

data SRoomStatus (s :: RoomStatus) where
  SOpen :: SRoomStatus 'Open
  SFull :: SRoomStatus 'Full

fromSRoomStatus :: SRoomStatus s -> RoomStatus
fromSRoomStatus SOpen = Open
fromSRoomStatus SFull = Full

data Room (s :: RoomStatus)
  = Room
      { id     :: Game.Id
      , name   :: Text
      , users  :: [Username]
      , status :: RoomStatus
      }
  deriving stock (Generic, Show)
  deriving (ToJSON)

data SomeRoom where
  SomeRoom :: Room s -> SomeRoom

instance ToJSON SomeRoom where
  toJSON (SomeRoom room) = toJSON room

withSomeRoom :: SomeRoom -> (forall (s :: RoomStatus). Room s -> r) -> r
withSomeRoom (SomeRoom r) f = f r

type Lobby = TVar (Map Game.Id SomeRoom)
