module Soltan.Game.Types where

import Soltan.Data.Username (Username)
import qualified Soltan.Effects.Stream as Stream
import Soltan.Hokm (Card, Game, GameErr, GameSummary, Suit)

type TableName = Text

data Table = Table
  { subscribers :: [Username]
  , name :: TableName
  , game :: !Game
  }
  deriving stock (Generic)

data TableSummary = TableSummary
  { tableName :: TableName
  , playerCount :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

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

data Client m = Client
  { username :: Username
  , outMailbox :: Stream.Stream m MsgOut
  }
