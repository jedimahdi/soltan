module Soltan.Socket.Types where

import Data.Generics.Labels ()
import Pipes.Concurrent (Input, Output)
import Soltan.Data.Username (Username)
import Soltan.Hokm (Card, Game, GameErr, GameSummary, Suit)
import Text.Show
import Prelude hiding (Show, show)

type TableName = Text

data Table = Table
  { subscribers :: [Username]
  , gameOutMailbox :: Input Game
  , gameInMailbox :: Output Game
  , game :: Game
  }
  deriving stock (Generic)

instance Show Table where
  show Table{..} =
    show subscribers <> "\n" <> show game

instance Eq Table where
  Table{game = game1} == Table{game = game2} = game1 == game2

data TableSummary = TableSummary
  { tableName :: TableName
  , playerCount :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Client = Client
  { username :: Username
  , outgoingMailbox :: Output MsgOut
  }
  deriving stock (Generic)

instance Show Client where
  show Client{..} = show username

instance Eq Client where
  Client{username = username1} == Client{username = username2} =
    username1 == username2

type Lobby = Map TableName Table

data ServerState = ServerState
  { clients :: Map Username Client
  , lobby :: Lobby
  }
  deriving stock (Generic)

data MsgOut
  = TableList [TableSummary]
  | ErrMsg Err
  | AuthSuccess Username
  | NewGameStateSummary TableName GameSummary
  | SuccessfullySubscribedToTable TableName TableSummary
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON)

data Err
  = AuthFailed
  | TableDoesNotExist TableName
  | GameErr GameErr
  | PlayerNotInTheGame
  | TableIsFull TableName
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON)

data MsgIn
  = GetTables
  | Login Username
  | SubscribeToTable TableName
  | GameMsgIn GameMsgIn
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data GameMsgIn
  = PlayCardMsg TableName Card
  | ChooseHokmMsg TableName Suit
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype TableDoesNotExistInLobby
  = TableDoesNotExistInLobby Text
  deriving stock (Show)

instance Exception TableDoesNotExistInLobby

data Command
  = SendMsg MsgOut
  | NewGameState TableName Game
  | JoinLobby TableName Username
  deriving stock (Show, Eq)
