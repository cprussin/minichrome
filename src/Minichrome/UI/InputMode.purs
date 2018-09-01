module Minichrome.UI.InputMode
  ( Mode(..)
  ) where

import Prelude

-- | The `Mode` type enumerates all possible input modes.  Currently there's
-- | only normal mode, but eventually that will change.
data Mode = Normal
derive instance eqMode :: Eq Mode
instance showMode :: Show Mode where
  show Normal = "NORMAL"
