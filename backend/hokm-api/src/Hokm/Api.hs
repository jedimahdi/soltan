module Hokm.Api
    ( main
    ) where

import qualified Data.Map                         as Map
import qualified Database.PostgreSQL.Simple       as Database
import qualified Hokm.Api.Effect.Database.User    as Database.User
import qualified Hokm.Api.Effect.GameState        as GameState
import qualified Hokm.Api.Effect.Hub              as Hub
import qualified Hokm.Api.Effect.Lobby            as Lobby
import qualified Hokm.Api.Effect.Random           as Random
import qualified Hokm.Api.Effect.Scrypt           as Scrypt
import           Hokm.Api.Network.Wai.Application
import qualified Network.Wai                      as Wai
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.Wai.Middleware.Cors
import           Polysemy                         ( runFinal )
import           Polysemy.AtomicState             ( runAtomicStateTVar )
import           Polysemy.Error                   ( errorToIOFinal )
import           Polysemy.Final                   ( embedToFinal )
import           Polysemy.Reader                  ( runReader )
import           Prelude                          hiding ( runReader )
import qualified Servant

port :: Int
port = 5000

connectionInfo :: Database.ConnectInfo
connectionInfo = Database.ConnectInfo { connectHost = "localhost", connectPort = 5432, connectUser = "mahdi", connectPassword = "", connectDatabase = "hokm"}

corsMiddleware :: Wai.Middleware
corsMiddleware = cors <| const <| Just policy
  where policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type", "Authorization"], corsMethods = "PUT" : simpleMethods}

main :: IO ()
main = do
  let settings = Warp.defaultSettings |> Warp.setPort (fromIntegral port) |> Warp.setBeforeMainLoop beforeMainLoopHook
  conn <- Database.connect connectionInfo
  hub <- newTVarIO Map.empty
  gameState <- newTVarIO Map.empty
  lobby <- newTVarIO Map.empty

  Warp.runSettings settings . corsMiddleware <| application
    ( Servant.Handler
    . ExceptT
    . runFinal
    . errorToIOFinal
    . embedToFinal
    . runReader conn
    . Scrypt.run
    . Database.User.run
    . runAtomicStateTVar gameState
    . GameState.run
    . runAtomicStateTVar hub
    . Hub.run
    . runAtomicStateTVar lobby
    . Lobby.run
    . Random.run
    )

beforeMainLoopHook :: IO ()
beforeMainLoopHook = putStrLn <| "Started listening on 127.0.0.1:" <> show port <> "."
