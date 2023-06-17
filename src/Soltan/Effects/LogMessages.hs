module Soltan.Effects.LogMessages where

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
import Soltan.Effects.Now (Now)
import qualified Soltan.Effects.Now as Now
import qualified Soltan.Logger.Message as Message
import Soltan.Logger.Severity (Severity (..))
import Soltan.SocketApp (SocketApp)

class Monad m => LogMessages m where
  logMessage :: Message.Minimal -> m ()

instance LogMessages SocketApp where
  logMessage = logMsg

type HasLog m = (LogMessages m, Now m)

type WithLog env m = Colog.WithLog env Message.Minimal m

log :: LogMessages m => Now m => Severity -> Text -> m ()
log severity message = do
  time <- Now.now
  withFrozenCallStack (logMessage Message.Minimal{time, cs = callStack, ..})

debug :: LogMessages m => Now m => Text -> m ()
debug = log Debug

info :: LogMessages m => Now m => Text -> m ()
info = log Info

warning :: LogMessages m => Now m => Text -> m ()
warning = log Warning

error :: LogMessages m => Now m => Text -> m ()
error = log Error

panic :: LogMessages m => Now m => Text -> m ()
panic = log Panic

pipe :: LogMessages m => Now m => Show a => Pipe a a m ()
pipe = do
  a <- await
  lift . debug <| show a
  yield a
