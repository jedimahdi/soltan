module Soltan.Logger.Formatting (
  color,
  square,
) where

import System.Console.ANSI (
  Color,
  ColorIntensity (Vivid),
  ConsoleLayer (Foreground),
  SGR (..),
  setSGRCode,
 )

square :: Text -> Text
square t = "[" <> t <> "] "

color :: Color -> Text -> Text
color c txt = toText (setSGRCode [SetColor Foreground Vivid c]) <> txt <> toText (setSGRCode [Reset])
