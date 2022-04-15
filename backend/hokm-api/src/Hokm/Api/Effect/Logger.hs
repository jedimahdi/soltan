module Hokm.Api.Effect.Logger where

import           Colog               ( LogAction, usingLoggerT )
import qualified Hokm.Logger         as Logger
import qualified Hokm.Logger.Message as Logger.Message
import           Polysemy            ( Embed, Member, Sem, interpret, makeSem )

data LoggerL m a where Log :: Logger.Severity -> Text -> LoggerL m ()

makeSem ''LoggerL

run :: Member (Embed IO) r => LogAction IO Logger.Message.Minimal -> Sem (LoggerL ': r) a -> Sem r a
run logAction = interpret \case
  Log severity message  -> liftIO <| usingLoggerT logAction <| Logger.log severity message

debug :: Member LoggerL r => Text -> Sem r ()
debug = log Logger.Debug

info :: Member LoggerL r => Text -> Sem r ()
info = log Logger.Info

warning :: Member LoggerL r => Text -> Sem r ()
warning = log Logger.Warning

error :: Member LoggerL r => Text -> Sem r ()
error = log Logger.Error
