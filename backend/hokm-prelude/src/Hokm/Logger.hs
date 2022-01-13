module Hokm.Logger
    ( Severity (..)
    , debug
    , error
    , info
    , log
    , logScopedMessageToStdStreams
    , panic
    , warning
    ) where

import qualified Hokm.Logger.Message  as Message
import           Hokm.Logger.Severity
import qualified Chronos
import           Colog
    ( LogAction (..), WithLog, logMsg, logTextStderr, logTextStdout, (&>) )
import           Prelude                 hiding ( error )

log :: WithLog env Message.Minimal m => MonadIO m => Severity -> Text -> m ()
log severity message = do
  time <- liftIO Chronos.now
  withFrozenCallStack (logMsg Message.Minimal { time, cs = callStack, .. })

debug :: WithLog env Message.Minimal m => MonadIO m => Text -> m ()
debug = log Debug

info :: WithLog env Message.Minimal m => MonadIO m => Text -> m ()
info = log Info

warning :: WithLog env Message.Minimal m => MonadIO m => Text -> m ()
warning = log Warning

error :: WithLog env Message.Minimal m => MonadIO m => Text -> m ()
error = log Error

panic :: WithLog env Message.Minimal m => MonadIO m => Text -> m ()
panic = log Panic

-------------------------------------------------------
-- Log Actions

logScopedMessageToStdStreams :: MonadIO m => LogAction m Message.Scoped
logScopedMessageToStdStreams = LogAction \message@(Message.Scoped _ Message.Minimal { severity }) ->
  let action = if severity == Error || severity == Panic then logTextStderr else logTextStdout in Message.prettyPrintScoped message &> action
