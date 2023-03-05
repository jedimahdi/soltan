module Soltan.Data.Game
  ( Game
  , Hokm(..)
  , mk
  , Error(..)
  , baseSuit
  , baseSuitL
  , haveSuit
  , haveBaseSuit
  , PlayedCard(..)
  )
  where

import Data.Generics.Labels ()
import Data.List.PointedList (PointedList, focus)
import qualified Data.List.PointedList as PointedList
import qualified Data.Map as Map
import Refined (Refined, SizeEqualTo, refine)
import Soltan.Data.Game.Card (Card, Suit)
import Soltan.Data.Username (Username)
import Control.Lens (view, set, Getter, to)

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

baseSuit :: Game -> Maybe Suit
baseSuit game = game ^. #middle |> viaNonEmpty last |> fmap (view (#card . #suit))

baseSuitL :: Getter Game (Maybe Suit)
baseSuitL = to baseSuit

haveSuit :: Username -> Suit -> Game -> Bool
haveSuit username suit game = game ^. #middle |> fmap (view (#card . #suit)) |> elem suit

haveBaseSuit :: Username -> Game -> Bool
haveBaseSuit username game = case game ^. baseSuitL of
  Nothing -> False
  Just suit -> haveSuit username suit game

data Error
  = IncorrectUserCount
  | WrongUsers
  deriving stock (Eq, Generic, Show)

mk :: [Username] -> Map Username [Card] -> [PlayedCard] -> Username -> Hokm -> Either Error Game
mk users hands middle hakem hokm = do
  players <- PointedList.fromList users |> fmap (set focus hakem) |> maybeToRight IncorrectUserCount
  -- unless (users == Map.keys hands) $ Left WrongUsers
  unless (hakem `elem` users) $ Left WrongUsers
  unless (length users == 4) $ Left IncorrectUserCount
  pure $ Game {..}
