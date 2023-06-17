module Hokm.Utils where

import Soltan.Hokm.Types (Card, Players)
import Soltan.Hokm.Utils (mkPlayers)

initialTestPlayers :: [Card] -> [Card] -> [Card] -> [Card] -> Players
initialTestPlayers c1 c2 c3 c4 =
  mkPlayers ("p1", c1) ("p2", c2) ("p3", c3) ("p4", c4)

-- mkChooseHokmGame :: 
