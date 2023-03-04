module Soltan.Data.Game
  ( Game
  , Hokm(..)
  , mk
  , Error(..)
  )
  where

import Data.Generics.Labels ()
import Data.List.PointedList (PointedList)
import qualified Data.List.PointedList as PointedList
import qualified Data.Map as Map
import Refined (Refined, SizeEqualTo, refine)
import Soltan.Data.Game.Card (Card, Suit)
import Soltan.Data.Username (Username)

data Hokm
  = NotChoosed
  | Choosed Suit
  deriving stock (Eq, Generic, Show)

data PlayedCard = PlayedCard
  { card     :: Card
  , username :: Username
  }
  deriving stock (Eq, Generic, Show)

data Game = Game
  { players :: PointedList Username
  , hands   :: Map Username [Card]
  , middle  :: [PlayedCard]
  , hokm    :: Hokm
  , hakem   :: Username
  }
  deriving stock (Eq, Generic, Show)

data Error
  = IncorrectUserCount
  | WrongUsers
  deriving stock (Eq, Generic, Show)

mk :: [Username] -> Map Username [Card] -> [PlayedCard] -> Username -> Hokm -> Either Error Game
mk users hands middle hakem hokm = do
  players <- maybeToRight IncorrectUserCount <| PointedList.fromList users
  -- unless (users == Map.keys hands) $ Left WrongUsers
  unless (hakem `elem` users) $ Left WrongUsers
  unless (length users == 4) $ Left IncorrectUserCount
  pure $ Game {..}
