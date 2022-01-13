module Hokm.Logger.Severity
    ( Severity (..)
    , prettyPrintSeverity
    ) where

import           Hokm.Logger.Formatting
import           GHC.Stack                 ( SrcLoc (SrcLoc, srcLocModule, srcLocStartLine) )
import           System.Console.ANSI       ( Color (..) )

data Severity = Debug | Info | Warning | Error | Panic deriving stock (Eq)

prettyPrintSeverity :: Severity -> CallStack -> Text
prettyPrintSeverity Debug   _  = color Green . square <| "Debug"
prettyPrintSeverity Info    _  = color Blue . square <| "Info"
prettyPrintSeverity Warning _  = color Yellow . square <| "Warning"
prettyPrintSeverity Error   _  = color Red . square <| "Error"
prettyPrintSeverity Panic   cs = color Red . square <| "Panic " <> prettyPrintStackTrace cs

prettyPrintStackTrace :: CallStack -> Text
prettyPrintStackTrace cs = case getCallStack cs of
  []                                  -> "<unknown location>"
  [(callerName, location)]            -> prettyPrintStackTraceLocation callerName location
  (_, location) : (callerName, _) : _ -> prettyPrintStackTraceLocation callerName location

prettyPrintStackTraceLocation :: String -> SrcLoc -> Text
prettyPrintStackTraceLocation callerName SrcLoc {..} =
  toText srcLocModule <> "." <> toText callerName <> "#" <> show srcLocStartLine
