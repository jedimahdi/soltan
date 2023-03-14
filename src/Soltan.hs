module Soltan where

import Colog
import qualified Data.Map as Map
import Network.Wai.Handler.Warp (run)
import Soltan.App.Env
import Soltan.App.Monad
import Soltan.Server (application)

mainLogAction :: MonadIO m => Severity -> LogAction m Message
mainLogAction severity =
    filterBySeverity severity msgSeverity richMessageAction

mkAppEnv :: IO AppEnv
mkAppEnv = do
    -- IO configuration
    -- envDbPool   <- initialisePool cDbCredentials
    envLobby <- newTVarIO Map.empty
    envHub <- newTVarIO Map.empty
    let envLogAction = richMessageAction
    pure Env{..}

runServer :: AppEnv -> IO ()
runServer env = run 8080 $ application env

main :: IO ()
main = mkAppEnv >>= runServer
