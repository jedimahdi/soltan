module Hokm.Api.Data.Game
    where

import           Control.Lens
    ( at, cons, elemOf, filtered, folded, has, hasn't, index, indices, itraversed, ix, traversed,
    view, (%~), (.~), (?~), (^.) )
import           Data.Aeson                  ( FromJSON, ToJSON )
import           Data.Generics.Labels        ()
import           Data.List                   ( (!!) )
import qualified Data.List                   as List
import           Data.List.Split             ( chunksOf )
import qualified Data.Map                    as Map
import qualified Data.UUID                   as UUID
import           Hokm.Api.Data.Card          ( Card )
import qualified Hokm.Api.Data.Card          as Card
import qualified Hokm.Api.Data.Text.Username as Username
import qualified Hokm.Api.Data.User          as User

type Id = UUID

data Player = Player { cards      :: [Card]
                     , playedCard :: Maybe Card
                     }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Players = Map User.Username Player

mkPlayers :: [Card] -> [User.Username] -> Players
mkPlayers deck usernames = chunksOf 13 deck |> zip usernames |> foldr (\(name, cs) m -> Map.insert name (Player cs Nothing) m) Map.empty

getMiddleCards :: Players -> [Card]
getMiddleCards = mapMaybe (view #playedCard) . Map.elems

getUsers :: Players -> [User.Username]
getUsers = Map.keys

maxCardInSuit :: Card.Suit -> [Card] -> Maybe Card
maxCardInSuit suit = viaNonEmpty head . sortOn (Down . view #value) . filter ((==suit) . view #suit)

highestPointInSuit :: Card.Suit -> Players -> Maybe User.Username
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
                 , players  :: Players
                 , king     :: User.Username
                 , baseSuit :: Maybe Card.Suit
                 , turn     :: Maybe User.Username
                 }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Error = WrongTurn | AlreadyStarted | NotInGame | RoundNotEnded | UsernameNotFound | EndOfRound | CardNotFound | WrongSuit deriving stock
  ( Eq
  , Generic
  , Show
  )
  deriving anyclass (FromJSON, ToJSON)

card1 :: Card
card1 = Card.Card {suit = Card.Club, value = Card.Two}

card2 :: Card
card2 = Card.Card {suit = Card.Diamond, value = Card.Two}

card3 :: Card
card3 = Card.Card {suit = Card.Heart, value = Card.Two}

card4 :: Card
card4 = Card.Card {suit = Card.Spade, value = Card.Two}

card5 :: Card
card5 = Card.Card {suit = Card.Club, value = Card.Three}

suit1 :: Card.Suit
suit1 = Card.Heart

p1 :: User.Username
p1 = Username.Mk "p1"

p2 :: User.Username
p2 = Username.Mk "p2"

p3 :: User.Username
p3 = Username.Mk "p3"

p4 :: User.Username
p4 = Username.Mk "p4"

p5 :: User.Username
p5 = Username.Mk "p5"

game1 :: Game
game1 = Game { id = UUID.nil
             , status = InGame suit1
             , players = mkPlayers Card.makeDeck [p1, p2, p3, p4]
             , king = p1
             , baseSuit = Nothing
             , turn = Just p1
             }

updateInGame :: (Card.Suit -> Game -> Either Error Game) -> Game -> Either Error Game
updateInGame f game@Game {..}
  = case status of
      ChooseHokm       -> Left NotInGame
      InGame trumpSuit -> f trumpSuit game

nextBaseSuit :: Card -> Game -> Either Error Game
nextBaseSuit card game@Game {..}
  = let suit = if null (getMiddleCards players) then Just <| card ^. #suit else baseSuit
    in game |> #baseSuit .~ suit |> pure

nextTurnInRound :: Game -> Either Error Game
nextTurnInRound game@Game {..}
  = case (turn, length <| getMiddleCards players) of
      (Nothing, _) -> Left EndOfRound
      (Just _, 4) -> pure game { turn = Nothing }
      (Just t, _) -> let users = getUsers players
                         newTurn = fmap (\i -> users !! ((i+1) `mod` 4)) . List.elemIndex t <| users
        in pure game {turn = newTurn}

playerPlayCard :: User.Username -> Card -> Game -> Either Error Game
playerPlayCard username card game
  = case (username `elem` Map.keys (game ^. #players), game ^. #turn == Just username) of
      (_, False)   -> Left WrongTurn
      (False, _)   -> Left UsernameNotFound
      (True, True) -> game |> #players . ix username %~ (\p -> p { playedCard = Just card, cards = filter (/= card) (cards p) }) |> pure

emptyMiddleCards :: Game -> Either Error Game
emptyMiddleCards game =
  game |> #players . traversed . #playedCard .~ Nothing |> pure

emptyBaseSuit :: Game -> Either Error Game
emptyBaseSuit game =
  game |> #baseSuit .~ Nothing |> pure

joinGame :: [Card] -> User.Username -> NotFull -> Either NotFull Game
joinGame newDeck username notFull@NotFull {..}
  | length joinedPlayers == 3 = pure <| Game { id = id
                                             , status = ChooseHokm
                                             , players = mkPlayers newDeck (username : joinedPlayers)
                                             , king = username
                                             , baseSuit = Nothing
                                             , turn = Just username
                                             }
  | otherwise = notFull |> #joinedPlayers %~ cons username |> Left

startGame :: Card.Suit -> Game -> Either Error Game
startGame trumpSuit game = case game ^. #status of
   InGame _   -> Left AlreadyStarted
   ChooseHokm -> game |> #status .~ InGame trumpSuit |> pure

playCard :: Card -> User.Username -> Game -> Either Error Game
playCard card username game =
  case ( game ^. #status
       , game ^. #turn == Just username
       , game ^. #players . at username
       , has (#players . ix username . #cards . folded . filtered (==card)) game
       , isNothing (game ^. #baseSuit)
         || game ^. #baseSuit == Just (card ^. #suit)
         || hasn't (#players . ix username . #cards . folded . filtered (\c -> Just (c ^. #suit) == game ^. #baseSuit)) game
       ) of
    (ChooseHokm, _, _, _, _)                            -> Left NotInGame
    (_, False, _, _, _)                                 -> Left WrongTurn
    (_, _, Nothing, _, _)                               -> Left UsernameNotFound
    (_, _, _, False, _)                                 -> Left CardNotFound
    (_, _, _, _, False)                                 -> Left WrongSuit
    (InGame _trumpSuit, True, Just _player, True, True) ->
      let newSuit = if null (getMiddleCards (game ^. #players))
                       then Just <| card ^. #suit
                       else game ^. #baseSuit
          users = getUsers <| game ^. #players
          newTurn = if (length <| getMiddleCards <| game ^. #players) < 3
                       then fmap (\i -> users !! ((i+1) `mod` 4)) . List.elemIndex username <| users
                       else Nothing
       in game
          |> #players . ix username . #playedCard .~ Just card
          |> #players . ix username . #cards %~ filter (/= card)
          |> #baseSuit .~ newSuit
          |> #turn .~ newTurn
          |> pure

-- updateWhenRoundEnd :: (Card.Suit -> Game -> Either Error Game) -> Game -> Either Error Game
-- updateWhenRoundEnd f game@Game {..}
--   = case status of
--       ChooseHokm       -> Left NotInGame
--       InGame trumpSuit -> case (baseSuit, length <| getMiddleCards players) of
--             (Just _, 4) -> f trumpSuit game
--             _           -> Left RoundNotEnded
--
-- removeCardFromHand :: Card -> User.Username -> Game -> Either Error Game
-- removeCardFromHand card username game@Game {..}
--   = case Map.lookup username hands of
--       Nothing -> Left UsernameNotFound
--       Just cards  ->
--         if card `elem` cards
--            then Right <| game { hands = Map.adjust (filter (/= card)) username hands }
--            else Left CardNotFound

-- playCard :: Card -> User.Username -> Game -> Either Error Game
-- playCard card username = updateInGame \_ -> nextTurnInRound <=< nextBaseSuit card <=< playerPlayCard username card

-- endRound :: Game -> Either Error Game
-- endRound = updateWhenRoundEnd \trumpSuit -> emptyBaseSuit <=< emptyMiddleCards <=< nextTurnStartOfRound trumpSuit


-- nextTurnStartOfRound :: Card.Suit -> Game -> Either Error Game
-- nextTurnStartOfRound trumpSuit game@Game {..}
--   = let newTurn = highestPointInSuit trumpSuit players <|> (join <| flip highestPointInSuit players <$> baseSuit)
--       in pure game {turn = newTurn}
