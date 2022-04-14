module Hokm.Api.Network.Server.Api.Game
    ( server
    ) where

import           Control.Lens                      ( (^.) )
import qualified Data.Aeson                        as Aeson
import qualified Hokm.Api.Data.Game                as Game
import qualified Hokm.Api.Data.GameResponse        as GameResponse
import           Hokm.Api.Data.Session             ( Session )
import qualified Hokm.Api.Data.Session             as Session
import qualified Hokm.Api.Data.User                as User
import qualified Hokm.Api.Effect.Database.User     as Database ( UserL )
import qualified Hokm.Api.Effect.Database.User     as Database.User
import           Hokm.Api.Effect.GamesState        ( GamesStateL )
import qualified Hokm.Api.Effect.GamesState        as GamesState
import           Hokm.Api.Effect.Hub               ( HubL )
import qualified Hokm.Api.Effect.Hub               as Hub
import           Hokm.Api.Effect.Random            ( RandomL )
import qualified Hokm.Api.Effect.Random            as Random
import           Hokm.Api.Network.Anatomy.Api.Game
import qualified Hokm.Api.Servant.Response         as Response
import qualified Hokm.Data.Validation              as Validation
import           Polysemy                          ( Member, Members, Sem )
import           Servant                           ( Union, respond )
import           Servant.API.Generic               ( ToServant )
import           Servant.Server.Generic
import           Validation                        ( validation )

handleFindGame :: Members '[RandomL, GamesStateL] r => Session -> Sem r (Union FindGameResponse)
handleFindGame = Session.withAuthenticated \_ -> GamesState.findNotFullGame >>= maybe create_new_game found_game
  where
    create_new_game :: Members '[RandomL, GamesStateL] r => Sem r (Union FindGameResponse)
    create_new_game = do
     newId <- Random.randomUUID
     let notFullGame = Game.NotFull newId []
     GamesState.addGameToLobby notFullGame
     respond . Response.Ok . GameResponse.mkNotFullResponse <| notFullGame

    found_game :: Game.NotFull -> Sem r (Union FindGameResponse)
    found_game = respond . Response.Ok . GameResponse.mkNotFullResponse

handleChooseHokm :: Members '[GamesStateL, HubL] r => ChooseHokmRequest -> Session -> Sem r (Union ChooseHokmResponse)
handleChooseHokm ChooseHokmRequest {..} = Session.withAuthenticated \_ ->
  GamesState.modifyGame gameId (Game.startGame suit) >>= maybe (respond Response.NotFound) go
  where
    go :: Members '[HubL] r => Game.Game -> Sem r (Union ChooseHokmResponse)
    go game = do
      Hub.broadcastMessageWithUsername gameId (GameResponse.mk game)
      respond . Response.Ok <| game

handlePlayCard :: Members '[GamesStateL, HubL] r => PlayCardRequest -> Session -> Sem r (Union PlayCardResponse)
handlePlayCard PlayCardRequest {..} = Session.withAuthenticated \claims -> do
  GamesState.modifyGame gameId (Game.playCard card (claims ^. #username)) >>= maybe (respond Response.NotFound) go
  where
    go :: Members '[HubL] r => Game.Game -> Sem r (Union ChooseHokmResponse)
    go game = do
      Hub.broadcastMessageWithUsername gameId (GameResponse.mk game)
      respond . Response.Ok <| game

handleEndRound :: Members '[Database.UserL, GamesStateL, HubL] r => EndRoundRequest -> Session -> Sem r (Union EndRoundResponse)
handleEndRound EndRoundRequest {..} = Session.withAuthenticated \_ -> do
  undefined
   -- maybeGame <- GameState.modifyGame gameId <| Game.endRound
   --
   -- case maybeGame of
   --   Just game -> do
   --     Hub.broadcastMessageWithUsername gameId (\name -> GameResponse.mk name game |> fmap Aeson.encode |> fmap toStrict)
   --     respond <| Response.Ok <| game
   --   _ -> do
   --     respond <| Response.NotFound

server :: Members '[Database.UserL, HubL, RandomL, GamesStateL] r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { findGame = handleFindGame, chooseHokm = handleChooseHokm, playCard = handlePlayCard, endRound = handleEndRound }
