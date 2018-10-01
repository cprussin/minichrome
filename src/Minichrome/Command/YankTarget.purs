module Minichrome.Command.YankTarget
  ( YankTarget(..)
  , read
  , help
  ) where

import Prelude

import Data.Either as Either
import Data.String as String

data YankTarget = URL

instance showDirection :: Show YankTarget where
  show URL = "url"

help :: String
help = String.joinWith "\n"
  [ "Yank targets:"
  , "  url         yank the URL of the current page"
  ]

read :: String -> Either.Either String YankTarget
read "url" = pure URL
read badTarget = Either.Left $ "Invalid yank target: " <> badTarget
