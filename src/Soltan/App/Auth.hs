module Soltan.App.Auth where

import Control.Lens (Lens')
import Data.Generics.Labels ()
import Soltan.App.Types
import Soltan.Data.Username (Username)

handleAuthCommand :: s -> AuthCommand -> IO ()
handleAuthCommand s _ = pass
