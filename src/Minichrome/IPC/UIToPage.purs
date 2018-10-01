module Minichrome.IPC.UIToPage
  ( Message(..)
  , send
  , subscribe
  ) where

import Prelude

import Data.Either as Either
import Data.String as String
import Effect as Effect
import Node.Electron.HTMLWebviewElement as HTMLWebviewElement
import Node.Electron.IPCRenderer as IPCRenderer

import Minichrome.Command.Direction as Direction

data Message
  = Scroll Direction.Direction
  | Blur

data Channel
  = ScrollC
  | BlurC

instance showChannel :: Show Channel where
  show ScrollC = "navigate"
  show BlurC = "blur"

channel :: Message -> Channel
channel (Scroll _) = ScrollC
channel Blur = BlurC

arguments :: Message -> Array String
arguments (Scroll direction) =
  String.split (String.Pattern " ") $ show direction
arguments Blur = [ ]

send :: HTMLWebviewElement.HTMLWebviewElement -> Message -> Effect.Effect Unit
send elem message =
  HTMLWebviewElement.send elem (show $ channel message) (arguments message)

subscribe :: (Message -> Effect.Effect Unit) -> Effect.Effect Unit
subscribe cb = do

  IPCRenderer.on (show ScrollC) \_ ->
    Direction.readTokens >>> Either.either mempty (Scroll >>> cb)

  IPCRenderer.on (show BlurC) \_ _ -> cb Blur
