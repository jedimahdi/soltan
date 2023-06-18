module Soltan.Hokm.Hokm where

import qualified Soltan.Data.AtMostThree as AtMostThree
import Soltan.Data.Four (Four (..))
import qualified Soltan.Data.Four as Four
import Soltan.Data.Username (Username)
import Soltan.Hokm.Types hiding (Three, Two)
import Soltan.Hokm.Utils (findWinnerOfTrick, mkPlayers, nextPlayerIndexTurn, shuffledDeck)
import System.Random (RandomGen)
import Prelude hiding (state)

startGame :: RandomGen g => g -> Four Username -> Game -> Game
startGame gen (Four.Four u1 u2 u3 u4) GameBeforeStart =
  let deck = shuffledDeck gen
   in GameChoosingHokm
        <| ChoosingHokmState
          { hakem = Player1
          , remainingDeck = drop 5 deck
          , players =
              mkPlayers (u1, take 5 deck) (u2, []) (u3, []) (u4, [])
          , teamAPoints = 0
          , teamBPoints = 0
          }
startGame _ _ g = g

-- score newTeamATricks newTeamBTricks teamAPoints hakemTeam = if newTeamATricks == 7
--   then
--     if newTeamBTricks == 0
--       then if hakemTeam == B then teamAPoints + 3 else teamAPoints + 2
--       else teamAPoints + 1
--   else teamAPoints

nextStage :: RandomGen g => g -> Game -> Game
nextStage gen (GameEndOfTrick state@(GameEndOfTrickState{..})) =
  let winner = findWinnerOfTrick state
      trickWinnerTeam = state ^. #players . playerL winner . #team
      newTeamATricks = if trickWinnerTeam == A then teamATricks + 1 else teamATricks
      newTeamBTricks = if trickWinnerTeam == B then teamBTricks + 1 else teamBTricks
   in if newTeamATricks == 7 || newTeamBTricks == 7
        then
          let newTeamAPoints =
                if newTeamATricks == 7
                  then
                    if newTeamBTricks == 0
                      then if hakemTeam == B then teamAPoints + 3 else teamAPoints + 2
                      else teamAPoints + 1
                  else teamAPoints
              newTeamBPoints =
                if newTeamBTricks == 7
                  then
                    if newTeamATricks == 0
                      then if hakemTeam == B then teamBPoints + 3 else teamBPoints + 2
                      else teamBPoints + 1
                  else teamBPoints
              hakemTeam = state ^. #players . playerL hakem . #team
              winnerTeam = if newTeamAPoints >= 7 then A else B
              u1 = state ^. #players . #player1 . #playerName
              u2 = state ^. #players . #player2 . #playerName
              u3 = state ^. #players . #player3 . #playerName
              u4 = state ^. #players . #player4 . #playerName
           in if newTeamAPoints >= 7 || newTeamBPoints >= 7
                then
                  GameEnd
                    <| GameEndState
                      { winnerTeam
                      , players =
                          mkPlayers (u1, []) (u2, []) (u3, []) (u4, [])
                      , teamAPoints = newTeamAPoints
                      , teamBPoints = newTeamBPoints
                      }
                else
                  let deck = shuffledDeck gen
                      newHakem = if hakemTeam == trickWinnerTeam then hakem else nextPlayerIndexTurn hakem
                      hakemCards = take 5 deck
                      newPlayers = case newHakem of
                        Player1 ->
                          mkPlayers (u1, hakemCards) (u2, []) (u3, []) (u4, [])
                        Player2 ->
                          mkPlayers (u1, []) (u2, hakemCards) (u3, []) (u4, [])
                        Player3 ->
                          mkPlayers (u1, []) (u2, []) (u3, hakemCards) (u4, [])
                        Player4 ->
                          mkPlayers (u1, []) (u2, []) (u3, []) (u4, hakemCards)
                   in GameChoosingHokm
                        <| ChoosingHokmState
                          { hakem = newHakem
                          , remainingDeck = drop 5 deck
                          , players = newPlayers
                          , teamAPoints = newTeamAPoints
                          , teamBPoints = newTeamBPoints
                          }
        else
          GameInProgress
            <| GameInProgressState{board = AtMostThree.Zero, turn = winner, teamATricks = newTeamATricks, teamBTricks = newTeamBTricks, ..}
nextStage _ g = g
