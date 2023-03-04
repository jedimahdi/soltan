module Soltan.Game.Action where

import Control.Lens (elemOf, ix, to, traversed)
import Data.List.PointedList (focus)
import Refined (unrefine)
import Soltan.Data.Game (Game, Hokm (..))
import Soltan.Data.Game.Action
import Soltan.Data.Game.Card (Card, Suit)
import Soltan.Data.Username (Username)

data Error
  = WrongTurn
  | AlreadyStarted
  | NotInGame
  | RoundNotEnded
  | UsernameNotFound
  | EndOfRound
  | WrongSuit
  | GameNotFound
  | HokmNotChoosen
  | HokmIsChoosen
  | CardNotFound

canPerformAction :: Action 'Unknown -> Game -> Either Error (Action 'Valid)
canPerformAction action game =
  case action of
    PlayCard username card   -> canPlayCard username card game
    ChooseHokm username suit -> canChooseHokm username suit game
    NextRound                -> canNextRound game
  |> fmap (const (unsafeStatusCoerce action))

canNextRound :: Game -> Either Error ()
canNextRound game = Right ()

canChooseHokm :: Username -> Suit -> Game -> Either Error ()
canChooseHokm username suit game = Right ()

canPlayCard :: Username -> Card -> Game -> Either Error ()
canPlayCard username card game
  =  [ checkHokmIsChoosen game
     , checkIsRightTurn username game
     , checkHaveCard username card game
     ]
  |> sequence
  |> void

checkHokmIsChoosen :: Game -> Either Error ()
checkHokmIsChoosen game
  | game ^. #hokm == NotChoosed = Left HokmNotChoosen
  | otherwise = Right ()

checkHokmIsNotChoosen :: Game -> Either Error ()
checkHokmIsNotChoosen game
  | game ^. #hokm == NotChoosed = Right ()
  | otherwise = Left HokmIsChoosen

checkIsRightTurn :: Username -> Game -> Either Error ()
checkIsRightTurn username game
  | game ^. #players . focus == username = Right ()
  | otherwise = Left WrongTurn

checkHaveCard :: Username -> Card -> Game -> Either Error ()
checkHaveCard username card game
  | elemOf (#hands .  ix username . traversed) card game = Right ()
  | otherwise = Left CardNotFound

performAction :: Action 'Valid -> Game -> Game
performAction action game
  = undefined
