module Minichrome.Page
  ( main
  ) where

import Prelude

import Effect as Effect

import Minichrome.Command.Direction as Direction
import Minichrome.IPC.UIToPage as IPCDown
import Minichrome.Page.FocusCommands as FocusCommands
import Minichrome.Page.ModeDetection as ModeDetection
import Minichrome.Page.ScrollingCommands as ScrollingCommands
import Minichrome.Page.ScrollingPosition as ScrollingPosition

runIPC :: IPCDown.Message -> Effect.Effect Unit
runIPC (IPCDown.Scroll (Direction.Up step)) = ScrollingCommands.up step
runIPC (IPCDown.Scroll (Direction.Down step)) = ScrollingCommands.down step
runIPC (IPCDown.Scroll (Direction.Left step)) = ScrollingCommands.left step
runIPC (IPCDown.Scroll (Direction.Right step)) = ScrollingCommands.right step
runIPC (IPCDown.Scroll Direction.Top) = ScrollingCommands.toTop
runIPC (IPCDown.Scroll Direction.Bottom) = ScrollingCommands.toBottom
runIPC (IPCDown.FocusNextForMode mode) = FocusCommands.nextForMode mode
runIPC (IPCDown.FocusPreviousForMode mode) = FocusCommands.previousForMode mode
runIPC IPCDown.Blur = FocusCommands.blur

main :: Effect.Effect Unit
main = do
  IPCDown.subscribe runIPC
  ModeDetection.installHandlers
  ScrollingPosition.installScrollHandler
