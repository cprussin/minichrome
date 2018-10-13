module Minichrome.IPC.PageToUI
  ( Message(..)
  , Channel(..)
  , send
  , fromEvent
  ) where

import Prelude

import Data.Array ((!!))
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Effect as Effect
import Node.Electron.Halogen as HalogenElectron
import Node.Electron.IPCRenderer as IPCRenderer

import Minichrome.Command.InputMode as InputMode

data Message
  = SetScrollPosition Int
  | SetMode InputMode.Mode

data Channel
  = SetScrollPositionC
  | SetModeC

instance showChannel :: Show Channel where
  show SetScrollPositionC = "setScrollPosition"
  show SetModeC = "setMode"

readChannel :: String -> Maybe.Maybe Channel
readChannel str
  | str == show SetScrollPositionC = Maybe.Just SetScrollPositionC
  | str == show SetModeC = Maybe.Just SetModeC
  | otherwise = Maybe.Nothing

channel :: Message -> Channel
channel (SetScrollPosition _) = SetScrollPositionC
channel (SetMode _) = SetModeC

args :: Message -> Array String
args (SetScrollPosition pos) = [ show pos ]
args (SetMode mode) = [ show mode ]

send :: Message -> Effect.Effect Unit
send message = IPCRenderer.sendToHost (show $ channel message) (args message)

fromEvent :: HalogenElectron.IPCMessageEvent -> Maybe.Maybe Message
fromEvent event = readChannel event.channel >>= case _ of
  SetModeC -> event.args !! 0 >>= InputMode.read >>> Either.hush <#> SetMode
  SetScrollPositionC -> event.args !! 0 >>= Int.fromString <#> SetScrollPosition
