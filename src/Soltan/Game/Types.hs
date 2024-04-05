{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Soltan.Game.Types where

import Control.Concurrent.STM (TChan)
import Control.Concurrent.STM.TChan (readTChan)
import Control.Lens (at, folded, lengthOf, sans, use, (%=), (.=), (<<+=), (<<.=), (?~))
import Control.Lens.TH (makePrisms)
import Data.Generics.Labels ()
import qualified Data.Map as Map
import qualified Data.Set as Set
import Soltan.Data.Four (Four)
import Soltan.Data.Username (Username)
import Soltan.Hokm (Game)
import qualified Soltan.Hokm as Game
import Text.Show (show)
import UnliftIO.Concurrent (forkFinally, forkIO)
import UnliftIO.STM (newTChanIO)
import Prelude hiding (show)

type TableId = Int

data TableStatus = Open | Started

data Table = Table
  { id :: TableId
  , chan :: TChan GameCommand
  , users :: [Username]
  , disconnectedUsers :: [Username]
  , status :: TableStatus
  }
  deriving stock (Generic)

instance Show Table where
  show = show . mkTableInfo

mkTableInfo :: Table -> TableInfo
mkTableInfo Table{..} = TableInfo{..}

data TableInfo = TableInfo
  { id :: TableId
  , users :: [Username]
  , disconnectedUsers :: [Username]
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

data GameCommand
  = ChooseHokm Username Game.Suit
  | StartGame (Four Username)
  | PlayCard Username Game.Card
  | NextRound
  deriving (Show)

data TableCommand
  = NewGame Username
  deriving (Show)

data GamesState = GameState
  { gamesMapVar :: TVar (Map TableId Table)
  , nextTableId :: Int
  }
  deriving stock (Generic)

-- data GameLoopEnv = GameLoopEnv
--   { log :: Severity -> Text -> IO ()
--   }
