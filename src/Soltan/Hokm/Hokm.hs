module Soltan.Hokm.Hokm where

import qualified Soltan.Data.AtMostThree as AtMostThree
import Soltan.Data.Four (Four (..))
import qualified Soltan.Data.Four as Four
import Soltan.Data.Username (Username)
import Soltan.Hokm.Scoring (calculateNewPoints, calculateNewTricks, findWinnerTeam, isGameEnded, isRoundEnded)
import Soltan.Hokm.Types hiding (Three, Two)
import Soltan.Hokm.Utils (findWinnerOfTrick, mkChooseHokmState, mkPlayers, nextPlayerIndexTurn, shuffledDeck)
import System.Random (RandomGen)
import Prelude hiding (state)

startGame :: RandomGen g => g -> Four Username -> Game -> Game
startGame gen (Four.Four u1 u2 u3 u4) GameBeforeStart =
  GameChoosingHokm <| mkChooseHokmState gen Player1 0 0 u1 u2 u3 u4
startGame _ _ g = g

nextStage :: RandomGen g => g -> Game -> Game
nextStage gen (GameEndOfTrick state@(GameEndOfTrickState{..}))
  | isGameEnded newTeamAPoints newTeamBPoints =
      GameEnd
        <| GameEndState
          { winnerTeam
          , players =
              mkPlayers (u1, []) (u2, []) (u3, []) (u4, [])
          , teamAPoints = newTeamAPoints
          , teamBPoints = newTeamBPoints
          }
  | isRoundEnded newTeamATricks newTeamBTricks =
      GameChoosingHokm <| mkChooseHokmState gen newHakem newTeamAPoints newTeamBPoints u1 u2 u3 u4
  | otherwise =
      GameInProgress
        <| GameInProgressState{board = AtMostThree.Zero, turn = trickWinner, teamATricks = newTeamATricks, teamBTricks = newTeamBTricks, ..}
 where
  trickWinner = findWinnerOfTrick state
  trickWinnerTeam = state ^. #players . playerL trickWinner . #team
  (newTeamATricks, newTeamBTricks) = calculateNewTricks trickWinnerTeam teamATricks teamBTricks
  (newTeamAPoints, newTeamBPoints) = calculateNewPoints hakemTeam newTeamATricks newTeamBTricks teamAPoints teamBPoints
  hakemTeam = state ^. #players . playerL hakem . #team
  winnerTeam = findWinnerTeam newTeamAPoints
  u1 = state ^. #players . #player1 . #playerName
  u2 = state ^. #players . #player2 . #playerName
  u3 = state ^. #players . #player3 . #playerName
  u4 = state ^. #players . #player4 . #playerName
  newHakem = if hakemTeam == trickWinnerTeam then hakem else nextPlayerIndexTurn hakem
nextStage _ g = g

-- let trickWinner = findWinnerOfTrick state
--     trickWinnerTeam = state ^. #players . playerL trickWinner . #team
--     (newTeamATricks, newTeamBTricks) = calculateNewTricks trickWinnerTeam teamATricks teamBTricks
--  in if isTrickEnded newTeamATricks newTeamBTricks
--       then
--         let (newTeamAPoints, newTeamBPoints) = calculateNewPoints winnerTeam hakemTeam teamATricks teamBTricks teamAPoints teamBPoints
--             hakemTeam = state ^. #players . playerL hakem . #team
--             winnerTeam = findWinnerTeam newTeamAPoints
--             u1 = state ^. #players . #player1 . #playerName
--             u2 = state ^. #players . #player2 . #playerName
--             u3 = state ^. #players . #player3 . #playerName
--             u4 = state ^. #players . #player4 . #playerName
--          in if isGameEnded newTeamAPoints newTeamBPoints
--               then
--                 GameEnd
--                   <| GameEndState
--                     { winnerTeam
--                     , players =
--                         mkPlayers (u1, []) (u2, []) (u3, []) (u4, [])
--                     , teamAPoints = newTeamAPoints
--                     , teamBPoints = newTeamBPoints
--                     }
--               else
--                 let deck = shuffledDeck gen
--                     newHakem = if hakemTeam == trickWinnerTeam then hakem else nextPlayerIndexTurn hakem
--                     hakemCards = take 5 deck
--                     newPlayers = case newHakem of
--                       Player1 ->
--                         mkPlayers (u1, hakemCards) (u2, []) (u3, []) (u4, [])
--                       Player2 ->
--                         mkPlayers (u1, []) (u2, hakemCards) (u3, []) (u4, [])
--                       Player3 ->
--                         mkPlayers (u1, []) (u2, []) (u3, hakemCards) (u4, [])
--                       Player4 ->
--                         mkPlayers (u1, []) (u2, []) (u3, []) (u4, hakemCards)
--                  in GameChoosingHokm
--                       <| ChoosingHokmState
--                         { hakem = newHakem
--                         , remainingDeck = drop 5 deck
--                         , players = newPlayers
--                         , teamAPoints = newTeamAPoints
--                         , teamBPoints = newTeamBPoints
--                         }
--       else
--         GameInProgress
--           <| GameInProgressState{board = AtMostThree.Zero, turn = trickWinner, teamATricks = newTeamATricks, teamBTricks = newTeamBTricks, ..}
