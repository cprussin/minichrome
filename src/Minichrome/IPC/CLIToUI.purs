module Minichrome.IPC.CLIToUI
  ( Message(..)
  , send
  , subscribe
  ) where

import Prelude

import Data.Array ((!!))
import Data.Maybe as Maybe
import Effect as Effect
import Node.Electron.IPCRenderer as IPCRenderer
import Node.Electron.WebContents as WebContents

data Message
  = Exec String

data Channel
  = ExecC

instance showChannel :: Show Channel where
  show ExecC = "exec"

channel :: Message -> Channel
channel (Exec _) = ExecC

arguments :: Message -> Array String
arguments (Exec cmd) = [ cmd ]

send :: WebContents.WebContents -> Message -> Effect.Effect Unit
send webContents message =
  WebContents.send webContents (show $ channel message) (arguments message)

subscribe :: (Message -> Effect.Effect Unit) -> Effect.Effect Unit
subscribe cb =
  IPCRenderer.on (show ExecC) \_ args ->
    Maybe.maybe mempty (Exec >>> cb) $ args !! 0
