{-# LANGUAGE TemplateHaskell #-}

module Soltan.Hokm.Types where

import Control.Lens (Lens', lens)
import Control.Lens.TH (makePrisms)
import Data.Generics.Labels ()
import Soltan.Data.Username (Username)
import qualified Text.Show
import Soltan.Data.AtMostThree (AtMostThree)
import Soltan.Data.Four (Four)

data Rank
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving stock (Eq, Read, Ord, Bounded, Enum, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Show Rank where
  show x = case x of
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    Ten -> "T"
    Jack -> "J"
    Queen -> "Q"
    King -> "K"
    Ace -> "A"

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving stock (Eq, Read, Ord, Bounded, Enum, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Show Suit where
  show x = case x of
    Clubs -> "♧ "
    Diamonds -> "♢ "
    Hearts -> "♡ "
    Spades -> "♤ "

data Card = Card
  { rank :: Rank
  , suit :: Suit
  }
  deriving stock (Eq, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Ord Card where
  compare = compare `on` rank

instance Show Card where
  show (Card r s) = show r ++ show s

data Team = A | B
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data Player = Player
  { playerName :: Username
  , team :: Team
  , cards :: [Card]
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ActionStatus = Unknown | Valid | Invalid

data Action (status :: ActionStatus) where
  PlayCard :: PlayerIndex -> Card -> Action 'Unknown
  ChooseHokm :: PlayerIndex -> Suit -> Action 'Unknown
  NextRound :: Action 'Unknown

deriving stock instance Eq (Action s)
deriving stock instance Show (Action s)

data GameErr
  = NoPlayerCanPlay
  | OutOfTurn PlayerIndex
  | CardNotFound
  | EndOfRound
  | GameNotStarted
  | GameHasEnded
  | InvalidAction
  | WrongSuit
  | NoChoosingHokmPhase
  | NotHakem
  | NotEndOfRound
  deriving (Show, Eq, Generic, ToJSON, FromJSON, Exception)

data Game
  = GameBeforeStart
  | GameChoosingHokm ChoosingHokmState
  | GameInProgress GameInProgressState
  | GameEndOfRound GameEndOfRoundState
  | GameEnd GameEndState
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data ChoosingHokmState = ChoosingHokmState
  { remainingDeck :: [Card]
  , hakem :: PlayerIndex
  , players :: Players
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data GameInProgressState = GameInProgressState
  { turn :: PlayerIndex
  , hakem :: PlayerIndex
  , trumpSuit :: Suit
  , players :: Players
  , board :: AtMostThree PlayedCard
  , teamAPoints :: Point
  , teamBPoints :: Point
  , teamARounds :: Round
  , teamBRounds :: Round
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data GameEndOfRoundState = GameEndOfRoundState
  { hakem :: PlayerIndex
  , trumpSuit :: Suit
  , players :: Players
  , board :: Four PlayedCard
  , teamAPoints :: Point
  , teamBPoints :: Point
  , teamARounds :: Round
  , teamBRounds :: Round
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data GameEndState = GameEndState
  { winnerTeam :: Team
  , players :: Players
  , prevHakem :: PlayerIndex
  , teamAPoints :: Point
  , teamBPoints :: Point
  , teamARounds :: Round
  , teamBRounds :: Round
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

newtype Round = Round Natural
  deriving stock (Show, Generic)
  deriving newtype (Num, Eq)
  deriving anyclass (ToJSON)

newtype Point = Point Natural
  deriving stock (Show, Generic)
  deriving newtype (Num, Eq)
  deriving anyclass (ToJSON)

data PlayedCard = PlayedCard
  { card :: Card
  , playerIndex :: PlayerIndex
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

data Players = Players
  { player1 :: Player
  , player2 :: Player
  , player3 :: Player
  , player4 :: Player
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data Turn = NoOne | TurnIndex PlayerIndex
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data PlayerIndex = Player1 | Player2 | Player3 | Player4
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)


getPlayer :: PlayerIndex -> Players -> Player
getPlayer idx (Players player1 player2 player3 player4) =
  case idx of
    Player1 -> player1
    Player2 -> player2
    Player3 -> player3
    Player4 -> player4

playerL :: PlayerIndex -> Lens' Players Player
playerL idx = lens getter setter
 where
  getter = getPlayer idx
  setter (Players player1 player2 player3 player4) updatedPlayer =
    case idx of
      Player1 -> Players updatedPlayer player2 player3 player4
      Player2 -> Players player1 updatedPlayer player3 player4
      Player3 -> Players player1 player2 updatedPlayer player4
      Player4 -> Players player1 player2 player3 updatedPlayer

makePrisms ''Game
