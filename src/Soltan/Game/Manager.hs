{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infinitely" #-}
module Soltan.Game.Manager where

import Control.Concurrent.STM (TChan)
import Control.Concurrent.STM.TChan (readTChan)
import Control.Lens (
  at,
  folded,
  lengthOf,
  sans,
  use,
  (%=),
  (.=),
  (<<+=),
  (<<.=),
  (?~),
 )
import Control.Lens.TH (makePrisms)
import Data.Generics.Labels ()
import qualified Data.Map as Map
import qualified Data.Set as Set
import Soltan.Data.Username (Username)
import Soltan.Game.Types
import Soltan.Hokm (Game)
import qualified Soltan.Hokm as Game
import Soltan.Logger.Message (LogMessage)
import Soltan.Logger.Severity (Severity (..))
import Soltan.Socket.Types (Message (..), MsgIn (..))
import System.Random (newStdGen)
import UnliftIO.Concurrent (forkFinally, forkIO)
import UnliftIO.STM (newTChanIO)
import Prelude hiding (state)

startGameManagerThread :: TChan TableCommand -> TVar (Map TableId Table) -> (Username -> Message -> IO ()) -> (Severity -> Text -> IO ()) -> IO ()
startGameManagerThread tableActionsChan gamesMapVar sendMessage log = do
  let nextTableId = 1
  let state = GameState{..}
  void $ forkIO $ flip evalStateT state $ forever do
    command <- atomically $ readTChan tableActionsChan
    liftIO $ log Debug $ "[GameManager] Received: " <> show command
    case command of
      NewGame creator -> do
        tableChan <- newTChanIO
        id <- #nextTableId <<+= 1
        let newTable = Table id tableChan [creator] [] Open
        atomically $ modifyTVar' gamesMapVar (at id ?~ newTable)
        liftIO $ sendMessage creator (Command GetTables)
        liftIO $ sendMessage creator (Command (JoinTable id))
        void $ liftIO $ forkFinally (gameLoop id tableChan sendMessage log) (\_ -> removeTable id)
 where
  removeTable id =
    atomically $ modifyTVar' gamesMapVar (sans id)

gameLoop :: TableId -> TChan GameCommand -> (Username -> Message -> IO ()) -> (Severity -> Text -> IO ()) -> IO ()
gameLoop id tableChan sendMessage log = do
  let initGame = Game.initialGame
  void $ usingStateT initGame loop
 where
  broadcast :: Message -> StateT Game IO ()
  broadcast msg = do
    game <- get
    case Game.getPlayers game of
      Nothing -> pass
      Just players ->
        traverse_ ((liftIO . flip sendMessage msg) . view #playerName) (Game.playersToList players)

  performGameAction :: Username -> (Game.PlayerIndex -> Game.Action) -> StateT Game IO ()
  performGameAction username mkAction = do
    game <- get
    let maybePlayerIdx = Game.getPlayerIndexWithUsername username game
    case maybePlayerIdx of
      Nothing -> pass
      Just playerIdx -> do
        let eGame = Game.runAction (mkAction playerIdx) game
        case eGame of
          Left e -> pass
          Right newGame -> do
            put newGame
            broadcast (GameInfo id newGame)

  loop :: StateT Game IO ()
  loop = forever do
    cmd <- atomically $ readTChan tableChan
    liftIO $ log Debug $ "[Game] " <> show id <> " <- " <> show cmd
    case cmd of
      StartGame users -> do
        gen <- newStdGen
        game <- get
        let startedGame = Game.startGame gen users game
        put startedGame
        broadcast (GameInfo id startedGame)
      ChooseHokm username hokm -> do
        performGameAction username (`Game.ChooseHokm` hokm)
      PlayCard username card -> do
        performGameAction username (`Game.PlayCard` card)

    game <- get
    when (Game.isEndOfTrick game) do
      -- put (Game.nextStage)
      pass
