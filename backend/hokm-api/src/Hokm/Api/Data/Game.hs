module Hokm.Api.Data.Game
    where

import           Control.Lens         ( view, (^.) )
import           Data.Aeson           ( FromJSON, ToJSON )
import           Data.Generics.Labels ()
import           Data.List            ( (!!) )
import           Data.List.Split      ( chunksOf )
import qualified Data.Map             as Map
import           Hokm.Api.Data.Card   ( Card )
import qualified Hokm.Api.Data.Card   as Card
import qualified Hokm.Api.Data.User   as User

data Player = Player { username   :: User.Username
                     , playedCard :: Maybe Card
                     }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Id = UUID

type Hands = Map User.Username [Card]

data Players = Players { p1 :: Player
                       , p2 :: Player
                       , p3 :: Player
                       , p4 :: Player
                       }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

nextTurn :: Players -> User.Username -> Maybe User.Username
nextTurn (Players p1 p2 p3 p4) turn | turn == p1 ^. #username = Just <| p2 ^. #username
                                    | turn == p2 ^. #username = Just <| p3 ^. #username
                                    | turn == p3 ^. #username = Just <| p4 ^. #username
                                    | turn == p4 ^. #username = Just <| p1 ^. #username
                                    | otherwise = Nothing

playerPlayCard :: User.Username -> Card -> Players -> Players
playerPlayCard username card players@(Players p1 p2 p3 p4)
  | p1 ^. #username == username = Players (p1 {playedCard = Just card}) p2 p3 p4
  | p2 ^. #username == username = Players p1 (p2 {playedCard = Just card}) p3 p4
  | p3 ^. #username == username = Players p1 p2 (p3 {playedCard = Just card}) p4
  | p4 ^. #username == username = Players p1 p2 p3 (p4 {playedCard = Just card})
  | otherwise = players

emptyMiddleCards :: Players -> Players
emptyMiddleCards (Players p1 p2 p3 p4) = Players (p1 {playedCard = Nothing}) (p2 {playedCard = Nothing}) (p3 {playedCard = Nothing}) (p4 {playedCard = Nothing})

getMiddleCards :: Players -> [Card]
getMiddleCards (Players p1 p2 p3 p4) = catMaybes [p1 ^. #playedCard, p2 ^. #playedCard, p3 ^. #playedCard, p4 ^. #playedCard]

getUsers :: Players -> [User.Username]
getUsers (Players p1 p2 p3 p4) = [p1 ^. #username, p2 ^. #username, p3 ^. #username, p4 ^. #username]

data Game
  = NotFull { id            :: Id
            , joinedPlayers :: [User.Username]
            }
  | ChooseHokm { id      :: Id
               , players :: Players
               , king    :: User.Username
               , hands   :: Hands
               }
  | Started { id        :: Id
            , players   :: Players
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

highestPointInSuit :: Card.Suit -> Players -> Maybe User.Username
highestPointInSuit suit players
  = fmap fst . viaNonEmpty head . sortOn (Down . view #value . snd) . filter ((==suit) . view #suit . snd) <| zip (getUsers players) (getMiddleCards players)


mkPlayer :: User.Username -> Player
mkPlayer username = Player username Nothing

mk :: Id -> Game
mk id = NotFull {id = id, joinedPlayers = []}


joinedGame :: [Card] -> User.Username -> Game -> Game
joinedGame newDeck username NotFull {..}
  | length joinedPlayers == 3 = ChooseHokm { id = id
                                           , players = Players (mkPlayer <| joinedPlayers !! 0) (mkPlayer <| joinedPlayers !! 1) (mkPlayer <| joinedPlayers !! 2) (mkPlayer username)
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
endRound  game = game
