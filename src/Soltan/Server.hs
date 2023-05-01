module Soltan.Server where

import Servant.API.Generic (toServant)
import Servant.Server (Application, Server, hoistServer, serve)
import Soltan.Server.Api
import Soltan.App.Monad (AppEnv)
import Servant.Server (Handler)
import Soltan.App.Monad (App, runAppAsIO)
import Soltan.App.Error (AppError, toHttpError)
import Control.Monad.Except (liftEither)
import Colog (LogAction, Message, Msg (..), Severity, filterBySeverity, richMessageAction)
import qualified Soltan.Logger as Logger

logMPErrorIO :: AppEnv -> AppError -> IO (Either AppError ())
logMPErrorIO env err = runAppAsIO env $ Logger.error $ show err

runAppAsHandler :: AppEnv -> App a -> Handler a
runAppAsHandler env app = do
    res <- liftIO $ runAppLogIO env app
    liftEither $ first toHttpError res

runAppLogIO :: AppEnv -> App a -> IO (Either AppError a)
runAppLogIO env app = do
    appRes <- runAppAsIO env app
    logRes <- whenLeft (Right ()) appRes (logMPErrorIO env)
    pure $ appRes <* logRes

soltanServer :: AppEnv -> Server Api
soltanServer env = hoistServer
    (Proxy @Api)
    (runAppAsHandler env)
    (toServant server)

application :: AppEnv -> Application
application env = serve
    (Proxy @Api)
    (soltanServer env)
