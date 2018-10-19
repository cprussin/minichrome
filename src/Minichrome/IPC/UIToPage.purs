module Minichrome.IPC.UIToPage
  ( Message(..)
  , send
  , subscribe
  ) where

import Prelude

import Data.Array ((!!))
import Data.Either as Either
import Data.Maybe as Maybe
import Data.String as String
import Effect as Effect
import Node.Electron.HTMLWebviewElement as HTMLWebviewElement
import Node.Electron.IPCRenderer as IPCRenderer

import Minichrome.Command.Direction as Direction
import Minichrome.Command.InputMode as InputMode

data Message
  = Scroll Direction.Direction
  | FocusNextForMode InputMode.Mode
  | FocusPreviousForMode InputMode.Mode
  | Blur

data Channel
  = ScrollC
  | FocusNextForModeC
  | FocusPreviousForModeC
  | BlurC

instance showChannel :: Show Channel where
  show ScrollC = "navigate"
  show FocusNextForModeC = "focusNextForMode"
  show FocusPreviousForModeC = "focusPreviousForMode"
  show BlurC = "blur"

channel :: Message -> Channel
channel (Scroll _) = ScrollC
channel (FocusNextForMode _) = FocusNextForModeC
channel (FocusPreviousForMode _) = FocusPreviousForModeC
channel Blur = BlurC

arguments :: Message -> Array String
arguments (Scroll direction) =
  String.split (String.Pattern " ") $ show direction
arguments (FocusNextForMode mode) = [ show mode ]
arguments (FocusPreviousForMode mode) = [ show mode ]
arguments Blur = [ ]

send :: HTMLWebviewElement.HTMLWebviewElement -> Message -> Effect.Effect Unit
send elem message =
  HTMLWebviewElement.send elem (show $ channel message) (arguments message)

subscribe :: (Message -> Effect.Effect Unit) -> Effect.Effect Unit
subscribe cb = do

  IPCRenderer.on (show ScrollC) \_ ->
    Direction.readTokens >>> Either.either mempty (Scroll >>> cb)

  IPCRenderer.on (show FocusNextForModeC) \_ args ->
    Maybe.maybe mempty (FocusNextForMode >>> cb) $
      args !! 0 >>= InputMode.read >>> Either.hush

  IPCRenderer.on (show FocusPreviousForModeC) \_ args ->
    Maybe.maybe mempty (FocusPreviousForMode >>> cb) $
      args !! 0 >>= InputMode.read >>> Either.hush

  IPCRenderer.on (show BlurC) \_ -> const $ cb Blur
