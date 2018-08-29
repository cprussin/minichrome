module Node.Electron.Event
  ( Event
  , preventDefault
  ) where

import Prelude

import Effect as Effect

-- | This type represents all events passed through Electron.
-- | TODO This should be less generic.
foreign import data Event :: Type

-- | Call `event.preventDefault`.
foreign import preventDefault :: Event -> Effect.Effect Unit
