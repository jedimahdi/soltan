module Soltan.Effect.GameState where

import qualified Soltan.Data.Game        as Game
import           Soltan.Data.Game        (Game)
import           Soltan.Data.Game.Action
import           Soltan.Data.Username    (Username)
import           Soltan.Game.Action

class Monad m => GameState m where
  dispatchAction :: Game.Id -> Action 'Unknown -> m ()
  addGame :: Game -> m ()
  findGameById :: Game.Id -> m (Maybe Game)
