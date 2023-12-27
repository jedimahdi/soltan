module Soltan.Logger.Message (
  LogMessage (..),
  prettyPrintLogMessage,
) where

import Chronos (DatetimeFormat (..), Time)
import qualified Data.Text.Lazy.Builder as TB
import qualified Soltan.Chronos as Choronos
import qualified Soltan.Chronos as Chronos
import Soltan.Logger.Formatting (color, square)
import Soltan.Logger.Severity
import System.Console.ANSI (Color (Cyan))

data LogMessage = LogMessage
  { message :: Text
  , severity :: Severity
  , time :: Time
  , cs :: CallStack
  }

prettyPrintLogMessage :: LogMessage -> Text
prettyPrintLogMessage LogMessage{..} = prettyPrintSeverity severity cs <> prettyPrintTime time <> message

prettyPrintTime :: Time -> Text
prettyPrintTime = square . toStrict . TB.toLazyText . Choronos.builderDbyHMSz format . Choronos.timeToDatetime
 where
  format = DatetimeFormat (Just ' ') (Just ' ') (Just ':')
