module Hokm.Api.Data.Game
    where

import           Control.Lens         ( view, (^.) )
import           Data.Aeson           ( FromJSON, ToJSON )
import           Data.Generics.Labels ()
import           Data.List            ( (!!) )
import qualified Data.List            as List
import           Data.List.Split      ( chunksOf )
import qualified Data.Map             as Map
import           Hokm.Api.Data.Card   ( Card )
import qualified Hokm.Api.Data.Card   as Card
import qualified Hokm.Api.Data.User   as User

type Id = UUID

data Player = Player { username   :: User.Username
                     , playedCard :: Maybe Card
                     }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

mkPlayer :: User.Username -> Player
mkPlayer username = let playedCard = Nothing in Player {..}

type Hands = Map User.Username [Card]

nextTurn :: [Player] -> User.Username -> Maybe User.Username
nextTurn players turn = fmap (view #username) . fmap (\i -> players !! ((i+1) `mod` 4)) . List.elemIndex turn . fmap (view #username) <| players

playerPlayCard :: User.Username -> Card -> [Player] -> [Player]
playerPlayCard username card
  = fmap (\p -> if p ^. #username == username then p {playedCard = Just card} else p)

emptyMiddleCards :: [Player] -> [Player]
emptyMiddleCards = fmap (\p -> p {playedCard = Nothing})

getMiddleCards :: [Player] -> [Card]
getMiddleCards = mapMaybe (view #playedCard)

getUsers :: [Player] -> [User.Username]
getUsers = map (view #username)

data Game
  = NotFull { id            :: Id
            , joinedPlayers :: [User.Username]
            }
  | ChooseHokm { id      :: Id
               , players :: [Player]
               , king    :: User.Username
               , hands   :: Hands
               }
  | Started { id        :: Id
            , players   :: [Player]
            , king      :: User.Username
            , trumpSuit :: Card.Suit
            , baseSuit  :: Maybe Card.Suit
            , hands     :: Hands
            , turn      :: Maybe User.Username
            }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)


getId :: Game -> Id
getId NotFull {..}    = id
getId ChooseHokm {..} = id
getId Started {..}    = id

isNotFull :: Game -> Bool
isNotFull NotFull {} = True
isNotFull _          = False

maxCardInSuit :: Card.Suit -> [Card] -> Maybe Card
maxCardInSuit suit = viaNonEmpty head . sortOn (Down . view #value) . filter ((==suit) . view #suit)

highestPointInSuit :: Card.Suit -> [Player] -> Maybe User.Username
highestPointInSuit suit players
  = fmap fst . viaNonEmpty head . sortOn (Down . view #value . snd) . filter ((==suit) . view #suit . snd) <| zip (getUsers players) (getMiddleCards players)

mk :: Id -> Game
mk id = NotFull {id = id, joinedPlayers = []}

joinedGame :: [Card] -> User.Username -> Game -> Game
joinedGame newDeck username NotFull {..}
  | length joinedPlayers == 3 = ChooseHokm { id = id
                                           , players = fmap mkPlayer joinedPlayers
                                           , king = username
                                           , hands = chunksOf 13 newDeck |> zip (username : joinedPlayers) |> foldr (\(name, cs) m -> Map.insert name cs m) Map.empty
                                           }
  | otherwise = NotFull {id = id, joinedPlayers = username : joinedPlayers}
joinedGame _ _ game                 = game

startGame :: Card.Suit -> Game -> Game
startGame trumpSuit ChooseHokm {..} = Started {id = id, players = players, king = king, trumpSuit = trumpSuit, baseSuit = Nothing, hands = hands, turn = Just king}
startGame _ game = game


playCard :: Card -> User.Username -> Game -> Game
playCard card username game@Started {..} = let middleCards = getMiddleCards players
                                               newBaseSuit = if null middleCards then Just <| card ^. #suit else baseSuit
                                               newTurn = turn >>= nextTurn players
                                               newHands = Map.adjust (filter (/= card)) username hands
                                               newPlayers = playerPlayCard username card players
                                            in game {baseSuit = newBaseSuit, players = newPlayers, turn = newTurn, hands = newHands}
playCard _ _ game = game

endRound :: Game -> Game
endRound game@Started {..} = let middleCards = getMiddleCards players
                                 nextTurn = highestPointInSuit trumpSuit players <|> (join <| flip highestPointInSuit players <$> baseSuit)
                                 newPlayers = emptyMiddleCards players
                              in game {baseSuit = Nothing, players = newPlayers, turn = nextTurn}
endRound game = game
