module Soltan.Hokm.Scoring where

import Soltan.Hokm.Types (Point, Team (..), Trick)

isRoundEnded :: Trick -> Trick -> Bool
isRoundEnded teamATricks teamBTricks = teamATricks >= 7 || teamBTricks >= 7

isGameEnded :: Point -> Point -> Bool
isGameEnded teamAPoints teamBPoints = teamAPoints >= 7 || teamBPoints >= 7

calculateNewTricks :: Team -> Trick -> Trick -> (Trick, Trick)
calculateNewTricks trickWinnerTeam teamATricks teamBTricks
  | trickWinnerTeam == A = (teamATricks + 1, teamBTricks)
  | otherwise = (teamATricks, teamBTricks + 1)

findWinnerTeam :: Point -> Team
findWinnerTeam teamAPoints = if teamAPoints >= 7 then A else B

calculateNewPoints :: Team -> Trick -> Trick -> Point -> Point -> (Point, Point)
calculateNewPoints hakemTeam teamATricks teamBTricks teamAPoints teamBPoints
  | teamATricks == 7 && teamBTricks == 0 && hakemTeam == B = (teamAPoints + 3, teamBPoints)
  | teamATricks == 7 && teamBTricks == 0 = (teamAPoints + 2, teamBPoints)
  | teamATricks == 7 = (teamAPoints + 1, teamBPoints)
  | teamBTricks == 7 && teamATricks == 0 && hakemTeam == A = (teamAPoints, teamBPoints + 3)
  | teamBTricks == 7 && teamATricks == 0 = (teamAPoints, teamBPoints + 2)
  | teamBTricks == 7 = (teamAPoints, teamBPoints + 1)
  | otherwise = (teamAPoints, teamBPoints)
