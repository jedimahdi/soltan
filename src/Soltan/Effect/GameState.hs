module Soltan.Effect.GameState where

import Soltan.Data.Game.Action
import Soltan.Game.Action
import Soltan.Data.Game (Game)
import Soltan.Data.Username (Username)
import qualified Soltan.Data.Game as Game

class Monad m => GameState m where
  dispatchAction :: Game.Id -> Action 'Unknown -> m ()
  addGame :: Game -> m ()
  findGameById :: Game.Id -> m (Maybe Game)
