module Minichrome.Command.Direction
  ( Direction(..)
  , read
  , readTokens
  , scale
  , help
  ) where

import Prelude

import Data.Either as Either
import Data.Int as Int
import Data.String as String

data Direction = Left Int | Right Int | Up Int | Down Int | Top | Bottom

instance showDirection :: Show Direction where
  show (Left step) = "left " <> show step
  show (Right step) = "right " <> show step
  show (Up step) = "up " <> show step
  show (Down step) = "down " <> show step
  show Top = "top"
  show Bottom = "bottom"

help :: String
help = String.joinWith "\n"
  [ "Scroll Directions (count is optional):"
  , "  left (<count>)"
  , "  right (<count>)"
  , "  up (<count>)"
  , "  down (<count>)"
  , "  top"
  , "  bottom"
  ]

scale :: Direction -> Int -> Direction
scale (Left step) = (_ * step) >>> Left
scale (Right step) = (_ * step) >>> Right
scale (Up step) = (_ * step) >>> Up
scale (Down step) = (_ * step) >>> Down
scale direction = const direction

read :: String -> Either.Either String Direction
read = String.split (String.Pattern " ") >>> readTokens

readTokens :: Array String -> Either.Either String Direction
readTokens [ "left", step ] = Left <$> parseStep step
readTokens [ "left" ] = pure $ Left 1
readTokens [ "right", step ] = Right <$> parseStep step
readTokens [ "right" ] = pure $ Right 1
readTokens [ "up", step ] = Up <$> parseStep step
readTokens [ "up" ] = pure $ Up 1
readTokens [ "down", step ] = Down <$> parseStep step
readTokens [ "down" ] = pure $ Down 1
readTokens [ "top" ] = pure Top
readTokens [ "bottom" ] = pure Bottom
readTokens badDir =
  Either.Left $ "Invalid scroll direction: " <> String.joinWith " " badDir

parseStep :: String -> Either.Either String Int
parseStep step =
  Either.note ("Invalid scroll step: " <> step) $ Int.fromString step
