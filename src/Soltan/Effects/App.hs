module Soltan.Effects.App where

import Control.Monad.Free (Free, liftF)

type VarId = Int

newtype StateVar a = StateVar {_varId :: VarId}

data StateF next where
  NewVar :: a -> (StateVar a -> next) -> StateF next
  ReadVar :: StateVar a -> (a -> next) -> StateF next
  WriteVar :: StateVar a -> a -> (() -> next) -> StateF next
  Retry :: (a -> next) -> StateF next

instance Functor StateF where
  fmap f (NewVar a next) = NewVar a (f . next)
  fmap f (ReadVar s next) = ReadVar s (f . next)
  fmap f (WriteVar s a next) = WriteVar s a (f . next)
  fmap f (Retry next) = Retry (f . next)

type StateL = Free StateF

newVar :: a -> StateL (StateVar a)
newVar a = liftF <| NewVar a identity

readVar :: StateVar a -> StateL a
readVar var = liftF <| ReadVar var identity

writeVar :: StateVar a -> a -> StateL ()
writeVar var a = liftF <| WriteVar var a identity

retry :: StateL a
retry = liftF <| Retry identity

data ConnectionId

data WebSocketF next where
  RunWebSocketServer :: Int -> (ConnectionId -> LangL ()) -> (() -> next) -> WebSocketF next

instance Functor WebSocketF where
  fmap f (RunWebSocketServer port app next) = RunWebSocketServer port app (f . next)

type WebSocketL = Free WebSocketF

runWebSocketServer :: Int -> (ConnectionId -> LangL ()) -> WebSocketL ()
runWebSocketServer port app = liftF <| RunWebSocketServer port app identity

data LangF next where
  EvalStateAtomically :: StateL a -> (a -> next) -> LangF next
  EvalWebSocket :: WebSocketL a -> (a -> next) -> LangF next

instance Functor LangF where
  fmap f (EvalStateAtomically s next) = EvalStateAtomically s (f . next)
  fmap f (EvalWebSocket s next) = EvalWebSocket s (f . next)

type LangL = Free LangF

evalAtomically :: StateL a -> LangL a
evalAtomically s = liftF <| EvalStateAtomically s identity

evalWebSocket :: WebSocketL a -> LangL a
evalWebSocket s = liftF <| EvalWebSocket s identity

myapp :: LangL ()
myapp = do
  evalWebSocket <| runWebSocketServer 5000 \conn -> do
    a <- evalAtomically do
      var <- newVar (2 :: Int)
      writeVar var 6
      readVar var
    pass
