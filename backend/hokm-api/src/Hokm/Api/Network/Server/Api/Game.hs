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
import           Hokm.Api.Effect.GameState         ( GameStateL )
import qualified Hokm.Api.Effect.GameState         as GameState
import           Hokm.Api.Effect.Hub               ( HubL )
import qualified Hokm.Api.Effect.Hub               as Hub
import           Hokm.Api.Effect.Lobby             ( LobbyL )
import qualified Hokm.Api.Effect.Lobby             as Lobby
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

handleFindGame :: Members '[Database.UserL, RandomL, LobbyL] r => Session -> Sem r (Union FindGameResponse)
handleFindGame = Session.withAuthenticated \_ -> do
  games <- Lobby.getAll
  let maybeGame = games |> viaNonEmpty head

  case maybeGame of
   Nothing -> do
     newId <- Random.randomUUID
     let notFullGame = Game.NotFull newId []
     Lobby.addGame notFullGame
     respond <| Response.Ok <| GameResponse.mkNotFullResponse notFullGame

   Just game -> do
     respond <| Response.Ok <| GameResponse.mkNotFullResponse game

handleChooseHokm :: Members '[Database.UserL, GameStateL, HubL] r => ChooseHokmRequest -> Session -> Sem r (Union ChooseHokmResponse)
handleChooseHokm ChooseHokmRequest {..} = Session.withAuthenticated \_ -> do
  maybeGame <- GameState.gameAtomicState gameId <| Game.startGame suit

  case maybeGame of
     Just game -> do
        Hub.broadcastMessageWithUsername gameId (\username -> GameResponse.mk username game |> fmap Aeson.encode |> fmap toStrict)
        respond <| Response.Ok <| game

     _ -> do
        respond <| Response.NotFound

handlePlayCard :: Members '[Database.UserL, GameStateL, HubL] r => PlayCardRequest -> Session -> Sem r (Union PlayCardResponse)
handlePlayCard PlayCardRequest {..} = Session.withAuthenticated \jwt -> do
  let username = jwt ^. #username
  maybeGame <- GameState.gameAtomicState gameId <| Game.playCard card username

  case maybeGame of
     Just game -> do
       Hub.broadcastMessageWithUsername gameId (\name -> GameResponse.mk name game |> fmap Aeson.encode |> fmap toStrict)
       respond <| Response.Ok <| game

     _ -> do
       respond <| Response.NotFound

handleEndRound :: Members '[Database.UserL, GameStateL, HubL] r => EndRoundRequest -> Session -> Sem r (Union EndRoundResponse)
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

server :: Members '[Database.UserL , GameStateL, HubL, RandomL, LobbyL] r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { findGame = handleFindGame, chooseHokm = handleChooseHokm, playCard = handlePlayCard, endRound = handleEndRound }
