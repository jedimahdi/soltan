module Hokm.Api.Data.GameResponse
    where

import           Control.Lens         ( view, (^.) )
import           Data.Aeson           ( FromJSON, ToJSON )
import           Data.Generics.Labels ()
import           Data.List            ( (!!) )
import           Data.List.Split      ( chunksOf )
import qualified Data.Map             as Map
import           Hokm.Api.Data.Card   ( Card )
import qualified Hokm.Api.Data.Card   as Card
import qualified Hokm.Api.Data.Game   as Game
import qualified Hokm.Api.Data.User   as User

data GameResponse
  = NotFull { id            :: Game.Id
            , joinedPlayers :: [User.Username]
            }
  | ChooseHokm { id      :: Game.Id
               , players :: Game.Players
               , king    :: User.Username
               , cards   :: [Card]
               }
  | Started { id        :: Game.Id
            , players   :: Game.Players
            , king      :: User.Username
            , trumpSuit :: Card.Suit
            , baseSuit  :: Maybe Card.Suit
            , cards     :: [Card]
            , turn      :: Maybe User.Username
            }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

mk :: User.Username -> Game.Game -> Maybe GameResponse
mk _ Game.NotFull {..}           = Just NotFull {..}
mk username Game.ChooseHokm {..} = Map.lookup username hands |> fmap (\allCards -> let cards = take 5 allCards in ChooseHokm {..})
mk username Game.Started {..}    = Map.lookup username hands |> fmap (\cards -> Started {..})
