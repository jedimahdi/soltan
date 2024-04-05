{-# HLINT ignore "Use infinitely" #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Soltan.Game.Manager where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TChan)
import Control.Concurrent.STM.TChan (readTChan, writeTChan)
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
        let newTable = Table id tableChan [] [] Open
        atomically $ modifyTVar' gamesMapVar (at id ?~ newTable)
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
  broadcast :: (Game -> Message) -> StateT Game IO ()
  broadcast mkMsg = do
    game <- get
    case Game.getPlayers game of
      Nothing -> pass
      Just players ->
        traverse_ ((liftIO . flip sendMessage (mkMsg game)) . view #playerName) (Game.playersToList players)

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
            broadcast (GameInfo id)

  runCommand :: GameCommand -> StateT Game IO ()
  runCommand (StartGame users) = do
    gen <- newStdGen
    game <- get
    let startedGame = Game.startGame gen users game
    put startedGame
    broadcast (GameInfo id)
  runCommand (PlayCard username card) = do
    get
      |> fmap (Game.performGameAction username (`Game.PlayCard` card))
      |> chainedTo (either (const pass) (\game -> put game >> broadcast (GameInfo id)))
  runCommand (ChooseHokm username suit) = do
    get
      |> fmap (Game.performGameAction username (`Game.ChooseHokm` suit))
      |> chainedTo (either (const pass) (\game -> put game >> broadcast (GameInfo id)))
  runCommand NextRound = do
    gen <- newStdGen
    modify' (Game.nextStage gen)
    broadcast (GameInfo id)

  loop :: StateT Game IO ()
  loop = forever do
    cmd <- atomically $ readTChan tableChan
    liftIO $ log Debug $ "[Game] " <> show id <> " <- " <> show cmd
    runCommand cmd
    game <- get
    when (Game.isEndOfTrick game) do
      void $ liftIO $ forkIO do
        threadDelay 1_000_000
        atomically $ writeTChan tableChan NextRound
