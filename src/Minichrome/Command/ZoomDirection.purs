module Minichrome.Command.ZoomDirection
  ( ZoomDirection(..)
  , read
  , readTokens
  , help
  ) where

import Prelude

import Data.Either as Either
import Data.Int as Int
import Data.String as String

data ZoomDirection = In Int | Out Int | Reset

instance showDirection :: Show ZoomDirection where
  show (In step) = "+ " <> show step
  show (Out step) = "- " <> show step
  show Reset = "reset"

help :: String
help = String.joinWith "\n"
  [ "Zoom Directions (count is optional):"
  , "  [in|+] (<count>)"
  , "  [out|-] (<count>)"
  , "  reset"
  ]

read :: String -> Either.Either String ZoomDirection
read = String.split (String.Pattern " ") >>> readTokens

readTokens :: Array String -> Either.Either String ZoomDirection
readTokens [ "+", step ] = In <$> parseStep step
readTokens [ "in", step ] = In <$> parseStep step
readTokens [ "+" ] = pure $ In 1
readTokens [ "in" ] = pure $ In 1
readTokens [ "-", step ] = Out <$> parseStep step
readTokens [ "out", step ] = Out <$> parseStep step
readTokens [ "-" ] = pure $ Out 1
readTokens [ "out" ] = pure $ Out 1
readTokens [ "reset" ] = pure Reset
readTokens badDir =
  Either.Left $ "Invalid zoom direction: " <> String.joinWith " " badDir

parseStep :: String -> Either.Either String Int
parseStep step =
  Either.note ("Invalid zoom step: " <> step) $ Int.fromString step
