module Minichrome.Command.InputMode
  ( Mode(..)
  , read
  , help
  ) where

import Prelude

import Data.Either as Either
import Data.String as String

-- | The `Mode` type enumerates all possible input modes.
data Mode = Normal | Insert

derive instance eqMode :: Eq Mode

instance showMode :: Show Mode where
  show Normal = "normal"
  show Insert = "insert"

help :: String
help = String.joinWith "\n"
  [ "Modes:"
  , "  normal"
  , "  insert"
  ]

read :: String -> Either.Either String Mode
read "normal" = pure Normal
read "insert" = pure Insert
read badMode = Either.Left $ "Invalid mode: " <> badMode
