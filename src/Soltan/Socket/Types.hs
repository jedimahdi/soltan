{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Soltan.Socket.Types where

import Control.Concurrent.STM (TChan)
import Data.Generics.Labels ()
import qualified Network.WebSockets as WS
import Pipes.Concurrent (Input, Output)
import Soltan.Data.Username (Username)
import Soltan.Game.Types (GameCommand, Table, TableCommand, TableId, TableInfo)
import Soltan.Hokm (Card, Game, GameErr, GameSummary, Suit)
import Text.Show
import Prelude hiding (Show, show)

type LogManager = TChan Text

data Server = Server
  { clients :: !(TVar (Map Username Client))
  , tables :: !(TVar (Map TableId Table))
  , logManager :: !LogManager
  , sendTableCommand :: TableCommand -> IO ()
  , sendGameCommand :: TableId -> GameCommand -> IO ()
  }
  deriving stock (Generic)

data ClientStatus = Idle | InGame TableId

data Client = Client
  { username :: Username
  , connection :: WS.Connection
  , sendChan :: TChan Message
  , status :: TVar ClientStatus
  }
  deriving stock (Generic)

data Message
  = Notice Text
  | Send MsgOut
  | GameInfo Game
  | Command MsgIn
  deriving stock (Generic, Show)

data MsgOut
  = TableList [TableInfo]
  | ErrMsg Err
  | AuthSuccess Username
  | NewGameStateSummary GameSummary
  | Noti Text
  | SuccessfullySubscribedToTable TableId TableInfo
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON)

data Err
  = AuthFailed
  | TableDoesNotExist TableId
  | GameErr GameErr
  | PlayerNotInTheGame
  | TableIsFull TableId
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON)

data MsgIn
  = GetTables
  | Login Username
  | JoinTable TableId
  | LeaveTable TableId
  | NewTable
  deriving
    ( -- | GameMsgIn GameMsgIn
      Show
    , Eq
    , Generic
    , FromJSON
    , ToJSON
    )

-- data GameMsgIn
--   = PlayCardMsg TableName Card
--   | ChooseHokmMsg TableName Suit
--   deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype TableDoesNotExistInLobby
  = TableDoesNotExistInLobby Text
  deriving stock (Show)

instance Exception TableDoesNotExistInLobby
