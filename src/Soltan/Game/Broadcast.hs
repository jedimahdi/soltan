{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Soltan.Game.Broadcast where

import Control.Lens (ix, (^?))
import qualified Data.Map as Map
import Soltan.Data.Has (grab)
import Soltan.Data.Username (Username)
import Soltan.Effects.Clients (AcquireClients, ManageClients)
import qualified Soltan.Effects.Clients as Clients
import Soltan.Effects.Concurrent (Concurrent)
import qualified Soltan.Effects.Concurrent as Concurrent
import Soltan.Effects.Lobby (AcquireLobby, ManageLobby)
import qualified Soltan.Effects.Lobby as Lobby
import Soltan.Effects.LogMessages (HasLog, LogMessages)
import qualified Soltan.Effects.LogMessages as Logger
import Soltan.Effects.Random (MonadRandom)
import qualified Soltan.Effects.Random as Random
import Soltan.Effects.Stream
import qualified Soltan.Effects.Stream as Stream
import Soltan.Game.Types (MsgOut (..), Table, TableName, User (..))
import Soltan.Hokm (Action (..), Game, canNextStage, getPlayerIndexWithUsername, isEndOfTrick, mkGameSummary, nextStage, runAction)
import System.Random (StdGen, getStdGen)
import UnliftIO (MonadUnliftIO, async)

setupTablePipeline ::
  forall m.
  (Concurrent m, ManageLobby m, AcquireClients m, MonadRandom m, HasLog m) =>
  TableName ->
  Table ->
  m ()
setupTablePipeline tableName _ = do
  Concurrent.forkProcess <| infinitely <| runStreaming <| pipeline
 where
  pipeline :: Streaming m () Void ()
  pipeline =
    Lobby.inputFromGameStream
      >-> Stream.iterM (broadcast tableName)
      >-> Stream.iterM (Lobby.updateGame tableName)
      >-> Stream.filter canNextStage
      >-> Stream.iterM (\_ -> Concurrent.threadDelay 1_000_000)
      >-> Stream.mapM (\game -> flip nextStage game <$> Random.generateStdGen)
      >-> Lobby.outputToGameStream

broadcast :: (AcquireLobby m, AcquireClients m, MonadStream m) => TableName -> Game -> m ()
broadcast tableName g = do
  Lobby.withTable tableName pass \table -> do
    clients <- Clients.getClients
    let cs = mapMaybe (\username -> clients ^? ix username) (table ^. #subscribers)
    mapM_ (informSubscriber tableName g) cs

informSubscriber :: (MonadStream m) => TableName -> Game -> User m -> m ()
informSubscriber tableName game User{..} = do
  case getPlayerIndexWithUsername username game of
    Nothing -> pass
    Just playerIndex ->
      runStreaming <| Stream.yield (NewGameStateSummary tableName (mkGameSummary playerIndex game)) >-> Stream.toStream outMailbox
