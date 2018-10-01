module Node.Electron.IPCRenderer
  ( on
  , sendToHost
  ) where

import Prelude

import Effect as Effect

import Node.Electron.Event as Event

-- | Attach a handler to an IPC message channel.
foreign import on
  :: String
  -> (Event.Event -> Array String -> Effect.Effect Unit)
  -> Effect.Effect Unit

-- | Send an IPC message to the host.
foreign import sendToHost :: String -> Array String -> Effect.Effect Unit
