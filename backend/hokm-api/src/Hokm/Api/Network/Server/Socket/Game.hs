module Hokm.Api.Network.Server.Socket.Game
    ( server
    ) where

import           Control.Lens
    ( at, index, itraversed, ix, sans, traverseOf, traversed, view, (.~), (?~), (^.) )
import qualified Data.Aeson                           as Aeson
import qualified Hokm.Api.Data.Authentication         as Authentication
import           Hokm.Api.Data.Card                   ( Card )
import           Hokm.Api.Data.Game                   ( Game )
import qualified Hokm.Api.Data.Game                   as Game
import qualified Hokm.Api.Data.GameResponse           as GameResponse
import           Hokm.Api.Data.Session                ( Session )
import qualified Hokm.Api.Data.Session                as Session
import qualified Hokm.Api.Data.User                   as User
import qualified Hokm.Api.Effect.Database.User        as Database ( UserL )
import qualified Hokm.Api.Effect.Database.User        as Database.User
import           Hokm.Api.Effect.GameState            ( GameStateL )
import qualified Hokm.Api.Effect.GameState            as GameState
import           Hokm.Api.Effect.Hub                  ( HubL )
import qualified Hokm.Api.Effect.Hub                  as Hub
import           Hokm.Api.Effect.Lobby                ( LobbyL )
import qualified Hokm.Api.Effect.Lobby                as Lobby
import           Hokm.Api.Effect.Random               ( RandomL )
import qualified Hokm.Api.Effect.Random               as Random
import           Hokm.Api.Network.Anatomy.Socket.Game
import qualified Hokm.Api.Servant.Response            as Response
import qualified Hokm.Data.Validation                 as Validation
import qualified Network.WebSockets                   as WS
import           Polysemy                             ( Embed, Member, Members, Sem )
import           Servant                              ( Union, respond )
import           Servant.API.Generic                  ( ToServant )
import           Servant.Server.Generic
import           Validation                           ( validation )

handleJoinGame :: Members '[Embed IO, GameStateL, HubL, RandomL, LobbyL] r => Game.Id -> WS.Connection -> Sem r ()
handleJoinGame gameId conn = do
  maybeUser :: Maybe Authentication.User <- WS.receiveData conn |> fmap Aeson.decodeStrict |> liftIO
  case maybeUser of
    Nothing -> pass
    Just user -> do
      deck <- Random.makeSuffledDeck

      x <- Lobby.atomicState <| \m ->
        case m ^. at gameId of
          Nothing -> (m, Nothing)
          Just g ->
            let e = Game.joinGame deck (user ^. #username) g
            in
             case e of
               Left notFull -> (m |> ix gameId .~ notFull, Just <| Left notFull)
               Right game   -> (m |> sans gameId, Just <| Right game)

      case x of
        Nothing -> pass
        Just e -> case e of
                    Left notFull -> do
                        Hub.subscribe gameId conn (user ^. #username)
                        traceShowM "hello from join game ==="
                        traceShowM <| (GameResponse.mkNotFullResponse notFull |>  Aeson.encode |>  toStrict)
                        Hub.broadcastMessage gameId (toStrict <| Aeson.encode <| GameResponse.mkNotFullResponse notFull)
                        liftIO <| WS.forkPingThread conn 30
                        liftIO <| infinitely <| WS.receiveData @ByteString conn
                        pass

                    Right game -> do
                        GameState.addGame game
                        Hub.subscribe gameId conn (user ^. #username)
                        traceShowM "hello from join game ==="
                        traceShowM <| (GameResponse.mk (user ^. #username) game |> fmap Aeson.encode |> fmap toStrict)
                        Hub.broadcastMessageWithUsername gameId (\username ->
                          traceShow username (GameResponse.mk username game |> fmap Aeson.encode |> fmap toStrict)
                                                                )
                        liftIO <| WS.forkPingThread conn 30
                        liftIO <| infinitely <| WS.receiveData @ByteString conn
                        pass

server :: Members '[Embed IO, GameStateL, HubL, RandomL, LobbyL] r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { joinGame = handleJoinGame }
