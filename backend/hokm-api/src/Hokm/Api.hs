module Hokm.Api
    ( Config (..)
    , main
    ) where

import           Colog                            ( LogAction, usingLoggerT )
import qualified Data.Map                         as Map
import qualified Database.PostgreSQL.Simple       as Database
import qualified Hokm.Api.Effect.Database.User    as Database.User
import qualified Hokm.Api.Effect.GamesState       as GamesState
import qualified Hokm.Api.Effect.Hub              as Hub
import qualified Hokm.Api.Effect.Logger           as Effect.Logger
import qualified Hokm.Api.Effect.Random           as Random
import qualified Hokm.Api.Effect.Scrypt           as Scrypt
import qualified Hokm.Api.Effect.WebSocket        as WebSocket
import           Hokm.Api.Network.Wai.Application
import           Hokm.Api.Network.Wai.Cors        ( corsMiddleware )
import           Hokm.Api.Network.Wai.Log         ( logMiddleware )
import qualified Hokm.Logger                      as Logger
import qualified Hokm.Logger.Message              as Logger.Message
import qualified Network.Wai                      as Wai
import qualified Network.Wai.Handler.Warp         as Warp
import           Polysemy                         ( runFinal )
import           Polysemy.AtomicState             ( runAtomicStateTVar )
import           Polysemy.Error                   ( errorToIOFinal )
import           Polysemy.Final                   ( embedToFinal )
import           Polysemy.Reader                  ( runReader )
import           Prelude                          hiding ( runReader )
import qualified Servant

data Config = Config { port         :: Word16
                     , logAction    :: LogAction IO Logger.Message.Minimal
                     , dbConnection :: Database.Connection
                     }

main :: Config -> IO ()
main config@Config {..} = do
  let settings = Warp.defaultSettings |> Warp.setPort (fromIntegral port) |> Warp.setBeforeMainLoop (beforeMainLoopHook config)
  hub <- newTVarIO Map.empty
  gameState <- newTVarIO Map.empty
  lobby <- newTVarIO Map.empty

  Warp.runSettings settings . corsMiddleware . logMiddleware logAction <| application
    ( Servant.Handler
    . ExceptT
    . runFinal
    . errorToIOFinal
    . embedToFinal
    . runReader dbConnection
    . Scrypt.run
    . Database.User.run
    . runAtomicStateTVar hub
    . Hub.run
    . Random.run
    . GamesState.run (GamesState.Games lobby gameState)
    . WebSocket.run
    . Effect.Logger.run logAction
    )

beforeMainLoopHook :: Config -> IO ()
beforeMainLoopHook Config { logAction, port } = usingLoggerT logAction <| Logger.info ("Started listening on 127.0.0.1:" <> show port <> ".")
