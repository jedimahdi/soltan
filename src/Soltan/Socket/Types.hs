module Soltan.Socket.Types where

import Control.Concurrent.STM (TChan, TVar)
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
import Soltan.Data.Game (Game)
import Soltan.Data.Username (Username)
import Text.Show
import Prelude hiding (Show, show)

type TableName = Text

data Table = Table
  { subscribers :: [Username]
  , gameOutMailbox :: Input Game
  , gameInMailbox :: Output Game
  , channel :: TChan MsgOut
  -- , waitlist :: [Username]
  -- , game :: Game
  }

-- instance Show Table where
--   show Table{..} =
--     show subscribers <> "\n" <> show waitlist <> "\n" <> show game

-- instance Eq Table where
--   Table{game = game1} == Table{game = game2} = game1 == game2
--
-- instance Ord Table where
--   Table{game = game1} `compare` Table{game = game2} =
--     game1 `compare` game2

data Client = Client
  { username :: Username
  , conn :: WS.Connection
  , outgoingMailbox :: Output MsgOut
  }

instance Show Client where
  show Client{..} = show username

instance Eq Client where
  Client{username = username1} == Client{username = username2} =
    username1 == username2

newtype Lobby
  = Lobby (Map TableName Table)

-- deriving (Ord, Eq, Show)

data ServerState = ServerState
  { clients :: Map Username Client
  , lobby :: Lobby
  }

data MsgOut
  = ErrMsg Err
  | AuthSuccess
  -- | NewGameState TableName Game
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data Err = AuthFailed
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data MsgIn
  = GetTables
  | SubscribeToTable TableName
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data MsgHandlerConfig = MsgHandlerConfig
  { serverStateTVar :: TVar ServerState
  , username :: Username
  , clientConn :: WS.Connection
  }

newtype TableDoesNotExistInLobby
  = TableDoesNotExistInLobby Text
  deriving (Show)

instance Exception TableDoesNotExistInLobby
