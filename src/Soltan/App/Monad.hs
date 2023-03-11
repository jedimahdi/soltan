module Soltan.App.Monad where

import Soltan.App.Env (Env)
import Control.Monad.Random.Class (MonadRandom)


type AppEnv = Env App

newtype App a = App
    { unApp :: ReaderT AppEnv IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadRandom)

-- runAppAsIO :: AppEnv -> App a -> IO (Either AppError a)
-- runAppAsIO env = firstF unAppException . try . runApp env

runApp :: AppEnv -> App a -> IO a
runApp env = usingReaderT env . unApp
