module Main where

import Prelude

import Effect as Effect
import Minichrome.CLI as CLI
import Minichrome.Config as Config

-- | The default `main` implementation, runs the `minichrome` entry point with
-- | default arguments.
main :: Effect.Effect Unit
main = CLI.minichrome Config.defaultConfig
