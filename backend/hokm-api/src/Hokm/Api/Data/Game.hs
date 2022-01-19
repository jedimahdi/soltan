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

mkHands :: [Card] -> [User.Username] -> Hands
mkHands deck usernames = chunksOf 13 deck |> zip usernames |> foldr (\(name, cs) m -> Map.insert name cs m) Map.empty

getMiddleCards :: [Player] -> [Card]
getMiddleCards = mapMaybe (view #playedCard)

getUsers :: [Player] -> [User.Username]
getUsers = map (view #username)

maxCardInSuit :: Card.Suit -> [Card] -> Maybe Card
maxCardInSuit suit = viaNonEmpty head . sortOn (Down . view #value) . filter ((==suit) . view #suit)

highestPointInSuit :: Card.Suit -> [Player] -> Maybe User.Username
highestPointInSuit suit players
  = fmap fst . viaNonEmpty head . sortOn (Down . view #value . snd) . filter ((==suit) . view #suit . snd) <| zip (getUsers players) (getMiddleCards players)

data NotFull = NotFull { id            :: Id
                       , joinedPlayers :: [User.Username]
                       }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Status
  = ChooseHokm
  | InGame Card.Suit
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Game = Game { id       :: Id
                 , status   :: Status
                 , players  :: [Player]
                 , hands    :: Hands
                 , king     :: User.Username
                 , baseSuit :: Maybe Card.Suit
                 , turn     :: Maybe User.Username
                 }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Error = WrongTurn | AlreadyStarted | NotInGame | RoundNotEnded | UsernameNotFound | EndOfRound | AlreadyPlayedCard

updateInGame :: (Card.Suit -> Game -> Either Error Game) -> Game -> Either Error Game
updateInGame f game@Game {..}
  = case status of
      ChooseHokm       -> Left NotInGame
      InGame trumpSuit -> f trumpSuit game

updateWhenRoundEnd :: (Card.Suit -> Game -> Either Error Game) -> Game -> Either Error Game
updateWhenRoundEnd f game@Game {..}
  = case status of
      ChooseHokm       -> Left NotInGame
      InGame trumpSuit -> case (baseSuit, length <| getMiddleCards players) of
            (Just _, 4) -> f trumpSuit game
            _           -> Left RoundNotEnded

removeCardFromHand :: Card -> User.Username -> Game -> Either Error Game
removeCardFromHand card username game@Game {..}
  = case Map.lookup username hands of
      Nothing -> Left UsernameNotFound
      Just _  -> Right <| game { hands = Map.adjust (filter (/= card)) username hands }

nextBaseSuit :: Card -> Game -> Either Error Game
nextBaseSuit card game@Game {..}
  = let suit = if null (getMiddleCards players) then Just <| card ^. #suit else baseSuit
    in Right <| game {baseSuit = suit}

nextTurnInRound :: Game -> Either Error Game
nextTurnInRound game@Game {..}
  = case (turn, length <| getMiddleCards players) of
      (Nothing, _) -> Left EndOfRound
      (Just _, 4) -> pure game { turn = Nothing }
      (Just t, _) -> let newTurn = fmap (view #username) . fmap (\i -> players !! ((i+1) `mod` 4)) . List.elemIndex t . fmap (view #username) <| players
        in pure game {turn = newTurn}

nextTurnStartOfRound :: Card.Suit -> Game -> Either Error Game
nextTurnStartOfRound trumpSuit game@Game {..}
  = let newTurn = highestPointInSuit trumpSuit players <|> (join <| flip highestPointInSuit players <$> baseSuit)
      in pure game {turn = newTurn}

playerPlayCard :: User.Username -> Card -> Game -> Either Error Game
playerPlayCard username card game@Game{..}
  = let newPlayers = fmap (\p -> if p ^. #username == username then p {playedCard = Just card} else p) players
        in pure game {players = newPlayers}

emptyMiddleCards :: Game -> Either Error Game
emptyMiddleCards game@Game{..} =
  let newPlayers = fmap (\p -> p {playedCard = Nothing}) players
      in pure game {players = newPlayers}

emptyBaseSuit :: Game -> Either Error Game
emptyBaseSuit game =
  pure game {baseSuit = Nothing}

joinedGame :: [Card] -> User.Username -> NotFull -> Either NotFull Game
joinedGame newDeck username notFull@NotFull {..}
  | length joinedPlayers == 3 = Right <| Game { id = id
                                              , status = ChooseHokm
                                              , players = fmap mkPlayer joinedPlayers
                                              , hands = mkHands newDeck (username : joinedPlayers)
                                              , king = username
                                              , baseSuit = Nothing
                                              , turn = Just username
                                              }
  | otherwise = Left <| notFull { joinedPlayers = username : joinedPlayers }

startGame :: Card.Suit -> Game -> Either Error Game
startGame trumpSuit game@Game {..} = case status of
                                       InGame _   -> Left AlreadyStarted
                                       ChooseHokm -> Right <| game { status = InGame trumpSuit }

playCard :: Card -> User.Username -> Game -> Either Error Game
playCard card username = updateInGame \_ -> removeCardFromHand card username <=< nextTurnInRound <=< nextBaseSuit card <=< playerPlayCard username card

endRound :: Game -> Either Error Game
endRound = updateWhenRoundEnd \trumpSuit -> emptyBaseSuit <=< emptyMiddleCards <=< nextTurnStartOfRound trumpSuit
