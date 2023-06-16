module Hokm.Utils where

import Soltan.Hokm.Types (Card, Players)
import Soltan.Hokm.Utils (initialPlayers)

initialTestPlayers :: [Card] -> [Card] -> [Card] -> [Card] -> Players
initialTestPlayers c1 c2 c3 c4 =
  initialPlayers "p1" "p2" "p3" "p4"
    |> #player1 . #cards
    .~ c1
      |> #player2 . #cards
    .~ c2
      |> #player3 . #cards
    .~ c3
      |> #player4 . #cards
    .~ c4
