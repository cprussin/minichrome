module Minichrome.UI.InputMode
  ( Mode(..)
  , read
  ) where

import Prelude

import Data.Maybe as Maybe

-- | The `Mode` type enumerates all possible input modes.
data Mode = Normal | Insert

derive instance eqMode :: Eq Mode

instance showMode :: Show Mode where
  show Normal = "NORMAL"
  show Insert = "INSERT"

read :: String -> Maybe.Maybe Mode
read "normal" = Maybe.Just Normal
read "insert" = Maybe.Just Insert
read _ = Maybe.Nothing
