{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Soltan.Hokm.Types where

import Control.Lens (Lens', lens)
import Control.Lens.TH (makePrisms)
import Data.Generics.Labels ()
import Soltan.Data.AtMostThree (AtMostThree)
import Soltan.Data.Four (Four)
import Soltan.Data.Username (Username)
import qualified Text.Show

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

data Action
  = PlayCard PlayerIndex Card
  | ChooseHokm PlayerIndex Suit

deriving stock instance Eq Action
deriving stock instance Show Action

data GameErr
  = NoPlayerCanPlay
  | OutOfTurn PlayerIndex
  | CardNotFound
  | EndOfTrick
  | GameNotStarted
  | GameAlreadyStarted
  | GameHasEnded
  | InvalidAction
  | WrongSuit
  | NoChoosingHokmPhase
  | NotHakem
  | NotEndOfTrick
  deriving (Show, Eq, Generic, ToJSON, FromJSON, Exception)

data Game
  = GameBeforeStart
  | GameChoosingHokm ChoosingHokmState
  | GameInProgress GameInProgressState
  | GameEndOfTrick GameEndOfTrickState
  | GameEnd GameEndState
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data ChoosingHokmState = ChoosingHokmState
  { remainingDeck :: [Card]
  , hakem :: PlayerIndex
  , players :: Players
  , teamAPoints :: Point
  , teamBPoints :: Point
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
  , teamATricks :: Trick
  , teamBTricks :: Trick
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data GameEndOfTrickState = GameEndOfTrickState
  { hakem :: PlayerIndex
  , trumpSuit :: Suit
  , players :: Players
  , board :: Four PlayedCard
  , teamAPoints :: Point
  , teamBPoints :: Point
  , teamATricks :: Trick
  , teamBTricks :: Trick
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data GameEndState = GameEndState
  { winnerTeam :: Team
  , players :: Players
  , teamAPoints :: Point
  , teamBPoints :: Point
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

newtype Trick = Trick Natural
  deriving stock (Show, Generic)
  deriving newtype (Num, Eq, Ord)
  deriving anyclass (ToJSON)

newtype Point = Point Natural
  deriving stock (Show, Generic)
  deriving newtype (Num, Eq, Ord)
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
  deriving stock (Show, Eq, Generic, Enum, Bounded)
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

data PlayerSummary = PlayerSummary
  { username :: Username
  , team :: Team
  , idx :: PlayerIndex
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data GameSummaryStatus
  = SummaryNotStarted
  | SummaryChoosingHokm
  | SummaryInProgress
  | SummaryEndOfTrick
  | SummaryEndOfGame
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data GameSummary
  = GameSummaryBeforeStart
  | GameSummaryChoosingHokm
      { cards :: [Card]
      , hakem :: PlayerIndex
      , playerIndex :: PlayerIndex
      , players :: [PlayerSummary]
      , teamAPoints :: Point
      , teamBPoints :: Point
      }
  | GameSummaryInProgress
      { cards :: [Card]
      , hakem :: PlayerIndex
      , playerIndex :: PlayerIndex
      , turn :: PlayerIndex
      , trumpSuit :: Suit
      , board :: [PlayedCard]
      , players :: [PlayerSummary]
      , teamATricks :: Trick
      , teamBTricks :: Trick
      , teamAPoints :: Point
      , teamBPoints :: Point
      }
  | GameSummaryEndOfTrick
      { cards :: [Card]
      , hakem :: PlayerIndex
      , playerIndex :: PlayerIndex
      , trumpSuit :: Suit
      , board :: [PlayedCard]
      , players :: [PlayerSummary]
      , teamATricks :: Trick
      , teamBTricks :: Trick
      , teamAPoints :: Point
      , teamBPoints :: Point
      }
  | GameSummaryEnd
      { playerIndex :: PlayerIndex
      , winnerTeam :: Team
      , players :: [PlayerSummary]
      , teamAPoints :: Point
      , teamBPoints :: Point
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

makePrisms ''Game
