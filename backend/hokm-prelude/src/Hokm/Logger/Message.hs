module Hokm.Logger.Message
    ( Minimal (..)
    , Scope (..)
    , Scoped (..)
    , prettyPrintScoped
    ) where

import qualified Hokm.Chronos           as Choronos
import           Hokm.Logger.Formatting ( color, square )
import           Hokm.Logger.Severity
import           Chronos                   ( DatetimeFormat (..), Time )
import qualified Data.Text.Lazy.Builder    as TB
import           System.Console.ANSI       ( Color (Cyan) )

data Minimal = Minimal { message  :: Text
                       , severity :: Severity
                       , time     :: Time
                       , cs       :: CallStack
                       }

prettyPrintMinimal :: Minimal -> Text
prettyPrintMinimal Minimal {..} = prettyPrintSeverity severity cs <> prettyPrintTime time <> message

prettyPrintTime :: Time -> Text
prettyPrintTime = square . toStrict . TB.toLazyText . Choronos.builderDbyHMSz format . Choronos.timeToDatetime
  where format = DatetimeFormat (Just ' ') (Just ' ') (Just ':')

data Scope = Cli | Api deriving stock (Eq)

prettyPrintScope :: Scope -> Text
prettyPrintScope scope = color Cyan . square <| text
 where
  text = case scope of
    Cli -> "CLI"
    Api -> "API"

data Scoped = Scoped Scope Minimal

prettyPrintScoped :: Scoped -> Text
prettyPrintScoped (Scoped scope minimal) = prettyPrintScope scope <> prettyPrintMinimal minimal
