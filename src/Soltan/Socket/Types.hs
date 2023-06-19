module Soltan.Socket.Types where

import Control.Concurrent.STM (TChan, TVar)
import Data.Generics.Labels ()
import qualified Network.WebSockets as WS
import Pipes (
  Consumer,
  Effect,
  Pipe,
  await,
  runEffect,
  yield,
  (>->),
 )
import Pipes.Concurrent (Input, Output)
import Soltan.Data.Has (Has (..))
import Soltan.Data.Username (Username)
import Soltan.Hokm (Card, Game, GameErr, Suit)
import Soltan.Hokm.Types (GameSummary)
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

-- instance Ord Table where
--   Table{game = game1} `compare` Table{game = game2} =
--     game1 `compare` game2

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

instance Has Client Client where
  obtain = identity

instance Show Client where
  show Client{..} = show username

instance Eq Client where
  Client{username = username1} == Client{username = username2} =
    username1 == username2

-- newtype Lobby
--   = Lobby (Map TableName Table)

type Lobby = Map TableName Table

-- deriving (Ord, Eq, Show)

data ServerState = ServerState
  { clients :: Map Username Client
  , lobby :: Lobby
  }
  deriving stock (Generic)

data MsgOut
  = TableList [TableSummary]
  | ErrMsg Err
  | AuthSuccess
  | NewGameStateSummary TableName GameSummary
  | SuccessfullySubscribedToTable TableName Game
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON)

data Err
  = AuthFailed
  | TableDoesNotExist TableName
  | GameErr GameErr
  | PlayerNotInTheGame
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON)

data MsgIn
  = GetTables
  | SubscribeToTable TableName
  | GameMsgIn GameMsgIn
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data GameMsgIn
  = PlayCardMsg TableName Card
  | ChooseHokmMsg TableName Suit
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data MsgHandlerConfig = MsgHandlerConfig
  { serverStateTVar :: TVar ServerState
  , username :: Username
  , clientConn :: WS.Connection
  }

newtype TableDoesNotExistInLobby
  = TableDoesNotExistInLobby Text
  deriving stock (Show)

instance Exception TableDoesNotExistInLobby

type WithServerState env m = (MonadReader env m, Has (TVar ServerState) env, MonadIO m)
type WithClient env m = (MonadReader env m, Has Client env)

data Command
  = SendMsg MsgOut
  | NewGameState TableName Game
  | JoinLobby TableName Username
  deriving stock (Show, Eq)
