module Soltan.App.Game where

import Control.Lens (Lens')
import Data.Generics.Labels ()
import Soltan.App.Types
import Soltan.Data.Username (Username)

handleGameCommand :: AuthCommand -> IO ()
handleGameCommand _ = pass
