module Soltan.App.Monad where

import Control.Exception (catch, throwIO, try)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Random.Class (MonadRandom)
import Soltan.App.Env (Env)
import Soltan.App.Error (AppError, AppException (..))
import Relude.Extra.Bifunctor (firstF)
import Control.Exception.Safe (MonadMask, MonadCatch, MonadThrow)

type AppEnv = Env App

newtype App a = App
    { unApp :: ReaderT AppEnv IO a
    } 
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadRandom, MonadMask, MonadCatch, MonadThrow)

instance MonadError AppError App where
    throwError :: AppError -> App a
    throwError = liftIO . throwIO . AppException
    {-# INLINE throwError #-}

    catchError :: App a -> (AppError -> App a) -> App a
    catchError action handler = App $ ReaderT $ \env -> do
        let ioAction = runApp env action
        ioAction `catch` \(AppException e) -> runApp env $ handler e
    {-# INLINE catchError #-}

runAppAsIO :: AppEnv -> App a -> IO (Either AppError a)
runAppAsIO env = firstF unAppException . try . runApp env

runApp :: AppEnv -> App a -> IO a
runApp env = usingReaderT env . unApp
