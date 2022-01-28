module Hokm.Api.Network.Anatomy.Api.Game
    where

import           Data.Aeson                       ( FromJSON, ToJSON )
import           Data.Generics.Labels             ()
import           Hokm.Api.Data.Card               ( Card )
import qualified Hokm.Api.Data.Card               as Card
import           Hokm.Api.Data.Game               ( Game )
import qualified Hokm.Api.Data.Game               as Game
import           Hokm.Api.Data.GameResponse       ( GameResponse, NotFullResponse )
import           Hokm.Api.Network.Anatomy.Prelude
import qualified Hokm.Api.Servant.Response        as Response
import qualified Hokm.Data.Validation             as Validation

type FindGameResponse = '[Response.Ok NotFullResponse , Response.Unauthorized]


data ChooseHokmRequest = ChooseHokmRequest { gameId :: Game.Id
                                           , suit   :: Card.Suit
                                           }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

type ChooseHokmResponse = '[Response.Ok Game , Response.Unauthorized, Response.NotFound]

data PlayCardRequest = PlayCardRequest { gameId :: Game.Id
                                       , card   :: Card
                                       }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

type PlayCardResponse = '[Response.Ok Game , Response.Unauthorized, Response.NotFound]

data EndRoundRequest = EndRoundRequest { gameId :: Game.Id
                                       }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

type EndRoundResponse = '[Response.Ok Game , Response.Unauthorized, Response.NotFound]

data Routes route = Routes { findGame :: route :- "find" :> Auth :> Get '[JSON] FindGameResponse
                           , chooseHokm :: route :- "choose-hokm" :> ReqBody '[JSON] ChooseHokmRequest :> Auth :> Post '[JSON] ChooseHokmResponse
                           , playCard :: route :- "play" :> ReqBody '[JSON] PlayCardRequest :> Auth :> Post '[JSON] PlayCardResponse
                           , endRound :: route :- "end-round" :> ReqBody '[JSON] EndRoundRequest :> Auth :> Post '[JSON] EndRoundResponse
                           }
  deriving stock (Generic)
