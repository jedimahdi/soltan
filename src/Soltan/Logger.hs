module Soltan.Logger (
  Severity (..),
  debug,
  error,
  info,
  log,
  logScopedMessageToStdStreams,
  panic,
  warning,
  pipe,
  WithLog,
) where

import qualified Chronos
import Colog (
  LogAction (..),
  logMsg,
  logTextStderr,
  logTextStdout,
  (&>),
 )
import qualified Colog
import Pipes (Pipe, await, yield)
import qualified Soltan.Logger.Message as Message
import Soltan.Logger.Severity
import Prelude hiding (error)

type WithLog env m = Colog.WithLog env Message.Minimal m

log :: WithLog env m => MonadIO m => Severity -> Text -> m ()
log severity message = do
  time <- liftIO Chronos.now
  withFrozenCallStack (logMsg Message.Minimal{time, cs = callStack, ..})

debug :: WithLog env m => MonadIO m => Text -> m ()
debug = log Debug

info :: WithLog env m => MonadIO m => Text -> m ()
info = log Info

warning :: WithLog env m => MonadIO m => Text -> m ()
warning = log Warning

error :: WithLog env m => MonadIO m => Text -> m ()
error = log Error

panic :: WithLog env m => MonadIO m => Text -> m ()
panic = log Panic

pipe :: WithLog env m => MonadIO m => Show a => Pipe a a m ()
pipe = do
  a <- await
  lift . debug <| show a
  yield a

-------------------------------------------------------
-- Log Actions

logScopedMessageToStdStreams :: MonadIO m => LogAction m Message.Scoped
logScopedMessageToStdStreams = LogAction \message@(Message.Scoped _ Message.Minimal{severity}) ->
  let action = if severity == Error || severity == Panic then logTextStderr else logTextStdout in Message.prettyPrintScoped message &> action
