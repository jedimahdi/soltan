module Hokm.Api.Data.GameResponse where

import           Control.Lens
    ( _1, _2, at, cons, elemOf, filtered, folded, has, hasn't, index, indices, itraversed, ix,
    traversed, view, (%~), (.~), (?~), (^.), (^@..) )
import           Data.Aeson           ( FromJSON, ToJSON, (.=) )
import qualified Data.Aeson           as Aeson
import           Data.Generics.Labels ()
import           Data.List            ( (!!) )
import qualified Data.List            as List
import           Data.List.Split      ( chunksOf )
import qualified Data.Map             as Map
import           Hokm.Api.Data.Card   ( Card )
import qualified Hokm.Api.Data.Card   as Card
import qualified Hokm.Api.Data.Game   as Game
import qualified Hokm.Api.Data.User   as User

data NotFullResponse = NotFullResponse { id            :: Game.Id
                                       , joinedPlayers :: [User.Username]
                                       }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

instance Aeson.ToJSON NotFullResponse where
  toJSON NotFullResponse {..} = Aeson.object [
       "tag" .= Aeson.String "NotFull"
     , "id" .= id
     , "joinedPlayers" .= joinedPlayers
     ]

mkNotFullResponse :: Game.NotFull -> NotFullResponse
mkNotFullResponse Game.NotFull {..} = NotFullResponse {..}

data Player = Player { username   :: User.Username
                     , playedCard :: Maybe Card
                     }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data GameResponse
  = NotFull { id            :: Game.Id
            , joinedPlayers :: [User.Username]
            }
  | ChooseHokm { id      :: Game.Id
               , players :: [Player]
               , king    :: User.Username
               , cards   :: [Card]
               }
  | Started { id        :: Game.Id
            , players   :: [Player]
            , king      :: User.Username
            , trumpSuit :: Card.Suit
            , baseSuit  :: Maybe Card.Suit
            , cards     :: [Card]
            , turn      :: Maybe User.Username
            }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

normalizePlayers :: User.Username -> [Player] -> [Player]
normalizePlayers username players = let (f, s) = List.span ((/=username) . view #username) players
                                      in s ++ f
mkNotFullResp :: Game.NotFull -> GameResponse
mkNotFullResp Game.NotFull {..} = NotFull {..}

mk :: Game.Game -> User.Username -> Maybe GameResponse
mk game@Game.Game {..} username = case game ^. #players . at username of
    Nothing -> Nothing
    Just p ->
      let players = game ^@.. #players . itraversed |> fmap (\x -> Player (x ^. _1) (x ^. _2 . #playedCard)) |> normalizePlayers username
                in  case status of
                       Game.InGame trumpSuit ->
                          let cards = p ^. #cards
                          in
                           pure Started {..}
                       Game.ChooseHokm ->
                          let cards = take 5 (p ^. #cards)
                          in
                         pure ChooseHokm {..}
