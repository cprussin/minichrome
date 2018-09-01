module Main where

import Prelude

import Effect as Effect
import Minichrome as Minichrome
import Minichrome.Config as Config

-- | The default `main` implementation, runs the `minichrome` entry point with
-- | default arguments.
main :: Effect.Effect Unit
main = Minichrome.minichrome Config.defaultConfig
