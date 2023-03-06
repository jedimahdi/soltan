module Soltan.Effect.GameState where

import Soltan.Data.Game.Action
import Soltan.Game.Action
import Soltan.Data.Game (Game)
import qualified Soltan.Data.Game as Game

class Monad m => GameState m where
  dispatchAction :: Game.Id -> Action 'Unknown -> m ()
  findGameById :: Game.Id -> m (Maybe Game)
