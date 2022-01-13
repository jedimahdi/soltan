module Hokm.Api.Network.Server.Api.Game
    ( server
    ) where

import           Control.Lens                      ( (^.) )
import qualified Data.Aeson                        as Aeson
import qualified Hokm.Api.Data.Game                as Game
import           Hokm.Api.Data.Session             ( Session )
import qualified Hokm.Api.Data.Session             as Session
import qualified Hokm.Api.Data.User                as User
import qualified Hokm.Api.Effect.Database.User     as Database ( UserL )
import qualified Hokm.Api.Effect.Database.User     as Database.User
import           Hokm.Api.Effect.GameState         ( GameStateL )
import qualified Hokm.Api.Effect.GameState         as GameState
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

handleFindGame :: Members '[Database.UserL, GameStateL, RandomL] r => Session -> Sem r (Union FindGameResponse)
handleFindGame = Session.withAuthenticated \_ -> do
  games <- GameState.getGameList
  let maybeGame = games |> filter Game.isNotFull |> viaNonEmpty head

  case maybeGame of
   Nothing -> do
     newId <- Random.randomUUID
     let game = Game.mk newId
     GameState.addGame game
     respond <| Response.Ok <| game

   Just game -> do
     respond <| Response.Ok <| game

handleChooseHokm :: Members '[Database.UserL, GameStateL, HubL] r => ChooseHokmRequest -> Session -> Sem r (Union ChooseHokmResponse)
handleChooseHokm ChooseHokmRequest {..} = Session.withAuthenticated \_ -> do
  maybeGame <- GameState.modifyGame gameId <| Game.startGame suit
  case maybeGame of
     Just game -> do
        Hub.broadcastMessage gameId (toStrict <| Aeson.encode game)
        respond <| Response.Ok <| game

     _ -> do
        respond <| Response.NotFound

handlePlayCard :: Members '[Database.UserL, GameStateL, HubL] r => PlayCardRequest -> Session -> Sem r (Union PlayCardResponse)
handlePlayCard PlayCardRequest {..} = Session.withAuthenticated \jwt -> do
  let username = jwt ^. #username
  maybeGame <- GameState.modifyGame gameId <| Game.playCard card username
  case maybeGame of
     Just game -> do
       Hub.broadcastMessage gameId (toStrict <| Aeson.encode game)
       respond <| Response.Ok <| game

     _ -> do
       respond <| Response.NotFound

handleEndRound :: Members '[Database.UserL, GameStateL, HubL] r => EndRoundRequest -> Session -> Sem r (Union EndRoundResponse)
handleEndRound EndRoundRequest {..} = Session.withAuthenticated \jwt -> do
  undefined
--   let userId = jwt ^. #userId
--   muser <- Database.User.findById userId
--   case muser of
--     Nothing   -> respond <| Response.Unauthorized
--     Just _user -> do
--        maybeGame <- GameState.endRound gameId
--        case maybeGame of
--          Just game -> do
--            Hub.broadcastMessage gameId (toStrict <| Aeson.encode game)
--            respond <| Response.Ok <| game
--
--          _ -> do
--            respond <| Response.NotFound

server :: Members '[Database.UserL , GameStateL, HubL, RandomL] r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { findGame = handleFindGame, chooseHokm = handleChooseHokm, playCard = handlePlayCard, endRound = handleEndRound }
