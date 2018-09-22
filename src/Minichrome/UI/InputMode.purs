module Minichrome.UI.InputMode
  ( Mode(..)
  ) where

import Prelude

-- | The `Mode` type enumerates all possible input modes.
data Mode = Normal | Insert

derive instance eqMode :: Eq Mode

instance showMode :: Show Mode where
  show Normal = "NORMAL"
  show Insert = "INSERT"
