module Node.Electron.Clipboard
  ( writeText
  ) where

import Prelude

import Effect as Effect

-- | Write a string to the
foreign import writeText :: String -> Effect.Effect Unit
