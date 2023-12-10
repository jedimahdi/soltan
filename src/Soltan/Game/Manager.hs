{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infinitely" #-}
module Soltan.Game.Manager where

import Control.Concurrent.STM (TChan)
import Control.Concurrent.STM.TChan (readTChan)
import Control.Lens (at, folded, lengthOf, sans, use, (%=), (.=), (<<+=), (<<.=), (?~))
import Control.Lens.TH (makePrisms)
import Data.Generics.Labels ()
import qualified Data.Map as Map
import qualified Data.Set as Set
import Soltan.Data.Username (Username)
import Soltan.Game.Types
import Soltan.Hokm (Game)
import qualified Soltan.Hokm as Game
import Soltan.Socket.Types (Message (..), MsgIn (..))
import System.Random (newStdGen)
import UnliftIO.Concurrent (forkFinally, forkIO)
import UnliftIO.STM (newTChanIO)
import Prelude hiding (state)

startGameManagerThread :: TChan TableCommand -> TVar (Map TableId Table) -> (Username -> Message -> IO ()) -> IO ()
startGameManagerThread tableActionsChan gamesMapVar sendMessage = do
  let nextTableId = 1
  let state = GameState{..}
  void $ forkIO $ flip evalStateT state $ forever do
    command <- atomically $ readTChan tableActionsChan
    case command of
      NewGame creator -> do
        tableChan <- newTChanIO
        id <- #nextTableId <<+= 1
        let newTable = Table id tableChan [creator] [] Open
        atomically $ modifyTVar' gamesMapVar (at id ?~ newTable)
        liftIO $ sendMessage creator (Command GetTables)
        liftIO $ sendMessage creator (Command (JoinTable id))
        void $ liftIO $ forkFinally (gameLoop id tableChan sendMessage) (\_ -> removeTable id)
 where
  removeTable id =
    atomically $ modifyTVar' gamesMapVar (sans id)

gameLoop :: TableId -> TChan GameCommand -> (Username -> Message -> IO ()) -> IO ()
gameLoop id tableChan sendMessage = do
  let initGame = Game.initialGame
  void $ usingStateT initGame loop
 where
  loop :: StateT Game IO ()
  loop = forever do
    cmd <- atomically $ readTChan tableChan
    case cmd of
      StartGame users -> do
        gen <- newStdGen
        game <- get
        let startedGame = Game.startGame gen users game
        put startedGame
        traverse_ (\u -> liftIO $ sendMessage u (GameInfo startedGame)) users
      ChooseHokm hokm -> do
        pass
