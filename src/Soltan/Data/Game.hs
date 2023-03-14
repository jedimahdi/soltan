module Soltan.Data.Game
    ( Error (..)
    , Game
    , Hokm (..)
    , Id
    , PlayedCard (..)
    , baseSuit
    , baseSuitL
    , haveBaseSuit
    , haveSuit
    , mk
    , mk'
    , playersL
    , turnL
    ) where

import           Control.Lens          (Getter, Lens', Traversal', set, to,
                                        view)
import           Data.Generics.Labels  ()
import           Data.List             ((!!))
import           Data.List.PointedList (PointedList, focus)
import qualified Data.List.PointedList as PointedList
import           Data.List.Split       (chunksOf)
import qualified Data.Map              as Map
import           Refined               (Refined, SizeEqualTo, refine)
import           Soltan.Data.Game.Card (Card, Deck, Suit)
import           Soltan.Data.Username  (Username)

type Id = UUID

data Hokm
  = NotChoosed
  | Choosed Suit
  deriving stock (Eq, Generic, Show)

data PlayedCard
  = PlayedCard
      { card     :: Card
      , username :: Username
      }
  deriving stock (Eq, Generic, Show)

data Game
  = Game
      { id      :: Id
      , players :: PointedList Username
      , hands   :: Map Username [Card]
      , middle  :: [PlayedCard]
      , hokm    :: Hokm
      , hakem   :: Username
      }
  deriving stock (Eq, Generic, Show)

playersL :: Getter Game [Username]
playersL = #players . to toList

turnL :: Lens' Game Username
turnL = #players . focus

baseSuit :: Game -> Maybe Suit
baseSuit game = game ^. #middle |> viaNonEmpty last |> fmap (view (#card . #suit))

baseSuitL :: Getter Game (Maybe Suit)
baseSuitL = to baseSuit

haveSuit :: Username -> Suit -> Game -> Bool
haveSuit username suit game = game ^. #middle |> fmap (view (#card . #suit)) |> elem suit

haveBaseSuit :: Username -> Game -> Bool
haveBaseSuit username game = case game ^. baseSuitL of
  Nothing   -> False
  Just suit -> haveSuit username suit game

data Error = IncorrectUserCount | WrongUsers deriving stock (Eq, Generic, Show)

mk :: [Username] -> Deck -> Either Error Game
mk usernames deck = do
  let hands = deck |> chunksOf 13 |> zip usernames |> Map.fromList
  mk' usernames hands [] (usernames !! 0) NotChoosed

mk' :: [Username] -> Map Username [Card] -> [PlayedCard] -> Username -> Hokm -> Either Error Game
mk' users hands middle hakem hokm = do
  players <- PointedList.fromList users |> fmap (set focus hakem) |> maybeToRight IncorrectUserCount
  -- unless (users == Map.keys hands) $ Left WrongUsers
  unless (hakem `elem` users) $ Left WrongUsers
  unless (length users == 4) $ Left IncorrectUserCount
  -- let id = 1
  pure $ Game {..}

