{-# LANGUAGE TupleSections #-}

module Soltan.Effects.Stream where

import Control.Concurrent.STM (mkWeakTVar)
import qualified Control.Concurrent.STM as S
import Data.Conduit ((.|))
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Semigroup
import Pipes ((>->))
import qualified Pipes
import qualified Pipes.Concurrent
import Soltan.SocketApp (SocketApp)
import Unsafe.Coerce (unsafeCoerce)

class Monad m => MonadStreaming m where
  data Streaming m :: Type -> Type -> Type -> Type
  runStreaming :: Streaming m () Void () -> m ()
  (>->) :: Streaming m a b () -> Streaming m b c r -> Streaming m a c r
  yield :: o -> Streaming m i o ()
  iterM :: (a -> m ()) -> Streaming m a a ()
  filter :: (a -> Bool) -> Streaming m a a ()
  map :: (a -> b) -> Streaming m a b ()
  mapM :: (a -> m b) -> Streaming m a b ()

class MonadStreaming m => MonadStream (m :: Type -> Type) where
  type Stream m a :: Type
  new :: Proxy a -> m (Stream m a)
  fromStream :: Stream m a -> Producer m a
  toStream :: Stream m a -> Consumer m a

type Producer m o = Streaming m () o ()
type Consumer m i = Streaming m i Void ()

instance MonadStreaming SocketApp where
  data Streaming SocketApp i o r = Streaming (Conduit.ConduitT i o SocketApp r)
  runStreaming (Streaming app) = Conduit.runConduit app
  (Streaming m1) >-> (Streaming m2) = Streaming <| m1 .| m2
  iterM = Streaming . Conduit.iterM
  yield = Streaming . Conduit.yield

instance MonadStream SocketApp where
  type Stream SocketApp a = Mailbox a
  new _ = liftIO spawn
  fromStream (_, input) = Streaming <| fromInput input
  toStream (output, _) = Streaming <| toOutput output

fromInput :: (MonadIO m) => Input a -> Conduit.ConduitT () a m ()
fromInput input = loop
 where
  loop = do
    ma <- liftIO $ S.atomically $ recv input
    case ma of
      Nothing -> pass
      Just a -> do
        Conduit.yield a
        loop

toOutput :: (MonadIO m) => Output a -> Conduit.ConduitT a Void m ()
toOutput output = loop
 where
  loop = do
    maybeA <- Conduit.await
    case maybeA of
      Nothing -> pass
      Just a -> do
        alive <- liftIO $ S.atomically $ send output a
        when alive loop

type Mailbox a = (Output a, Input a)

newtype Input a = Input
  { recv :: S.STM (Maybe a)
  }

instance Functor Input where
  fmap f m = Input (f <<$>> recv m)

instance Applicative Input where
  pure r = Input (pure (pure r))
  mf <*> mx = Input ((<*>) <$> recv mf <*> recv mx)

instance Monad Input where
  m >>= f = Input $ do
    ma <- recv m
    case ma of
      Nothing -> return Nothing
      Just a -> recv (f a)

instance Alternative Input where
  empty = Input (return Nothing)
  x <|> y = Input $ do
    (i, ma) <- fmap (y,) (recv x) <|> fmap (x,) (recv y)
    case ma of
      Nothing -> recv i
      Just a -> return (Just a)

instance MonadPlus Input where
  mzero = empty
  mplus = (<|>)

instance Data.Semigroup.Semigroup (Input a) where
  (<>) = (<|>)

instance Monoid (Input a) where
  mempty = empty

{- | An exhaustible sink of values

    'send' returns 'False' if the sink is exhausted
-}
newtype Output a = Output
  { send :: a -> S.STM Bool
  }

instance Data.Semigroup.Semigroup (Output a) where
  i1 <> i2 = Output (\a -> (||) <$> send i1 a <*> send i2 a)

instance Monoid (Output a) where
  mempty = Output (\_ -> return False)
spawn :: IO (Output a, Input a)
spawn = fmap simplify spawn'
 where
  simplify (output, input, _) = (output, input)
spawn' :: IO (Output a, Input a, STM ())
spawn' = do
  (write, read) <- do
    m <- S.newEmptyTMVarIO
    return (\x -> S.tryTakeTMVar m *> S.putTMVar m x, S.takeTMVar m)

  sealed <- S.newTVarIO False
  let seal = S.writeTVar sealed True

  {- Use weak TVars to keep track of whether the 'Input' or 'Output' has been
     garbage collected.  Seal the mailbox when either of them becomes garbage
     collected.
  -}
  rSend <- newTVarIO ()
  void $ mkWeakTVar rSend (S.atomically seal)
  rRecv <- newTVarIO ()
  void $ mkWeakTVar rRecv (S.atomically seal)

  let sendOrEnd a = do
        b <- S.readTVar sealed
        if b
          then return False
          else do
            write a
            return True
      readOrEnd =
        (Just <$> read)
          <|> ( do
                  b <- S.readTVar sealed
                  S.check b
                  return Nothing
              )
      _send a = sendOrEnd a <* readTVar rSend
      _recv = readOrEnd <* readTVar rRecv
  return (Output _send, Input _recv, seal)
