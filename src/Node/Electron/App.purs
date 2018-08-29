module Node.Electron.App
  ( onReady
  , onWindowAllClosed
  , onActivate
  , quit
  ) where

import Prelude

import Effect as Effect

-- | Attach an `Effect` to the Electron app's 'ready' event.
foreign import onReady :: Effect.Effect Unit -> Effect.Effect Unit

-- | Attach an `Effect` to the Electron app's 'window-all-closed' event.
foreign import onWindowAllClosed :: Effect.Effect Unit -> Effect.Effect Unit

-- | Attach an `Effect` to the Electron app's 'activate' event.
foreign import onActivate :: Effect.Effect Unit -> Effect.Effect Unit

-- | Exit the Electron app.
foreign import quit :: Effect.Effect Unit
