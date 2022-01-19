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
  = ChooseHokm { id      :: Game.Id
               , players :: [Game.Player]
               , king    :: User.Username
               , cards   :: [Card]
               }
  | Started { id        :: Game.Id
            , players   :: [Game.Player]
            , king      :: User.Username
            , trumpSuit :: Card.Suit
            , baseSuit  :: Maybe Card.Suit
            , cards     :: [Card]
            , turn      :: Maybe User.Username
            }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

mk :: User.Username -> Game.Game -> Maybe GameResponse
mk username Game.Game{..} = case status of
                                   Game.InGame trumpSuit ->
                                     Map.lookup username hands |> fmap (\allCards -> let cards = take 5 allCards in Started {..})
                                   Game.ChooseHokm ->
                                     Map.lookup username hands |> fmap (\allCards -> let cards = take 5 allCards in ChooseHokm {..})
