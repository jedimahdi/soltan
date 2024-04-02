module Soltan.App.Types where

import Control.Lens (Lens')
import Soltan.Data.Username (Username)

data Outgoing = ErrorMsg Text
  deriving
    ( Show
    , Eq
    , Generic
    , FromJSON
    , ToJSON
    )

data Command
  = RoomCommand RoomCommand
  | AuthCommand AuthCommand

type SendCommand = Command -> IO ()

class HasSendCommand s where
  sendCommandL :: Lens' s SendCommand

type RoomId = Int

data RoomCommand
  = Join Username RoomId
  | Leave RoomId

data AuthCommand = Login Username | Logout Username
