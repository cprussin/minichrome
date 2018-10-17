module Minichrome.Page
  ( main
  ) where

import Prelude

import Data.Maybe as Maybe
import Effect as Effect
import Web.HTML.HTMLElement as HTMLElement

import Minichrome.Command.Direction as Direction
import Minichrome.IPC.UIToPage as IPCDown
import Minichrome.Page.AutoMode as AutoMode
import Minichrome.Page.ScrollingCommands as ScrollingCommands
import Minichrome.Page.ScrollingPosition as ScrollingPosition
import Minichrome.Page.Util as Util

watchIPC :: Effect.Effect Unit
watchIPC = IPCDown.subscribe $ case _ of
  (IPCDown.Scroll (Direction.Up step)) -> ScrollingCommands.up step
  (IPCDown.Scroll (Direction.Down step)) -> ScrollingCommands.down step
  (IPCDown.Scroll (Direction.Left step)) -> ScrollingCommands.left step
  (IPCDown.Scroll (Direction.Right step)) -> ScrollingCommands.right step
  (IPCDown.Scroll Direction.Top) -> ScrollingCommands.toTop
  (IPCDown.Scroll Direction.Bottom) -> ScrollingCommands.toBottom
  (IPCDown.FocusNextForMode mode) -> AutoMode.focusNextForMode mode
  IPCDown.Blur -> Util.getActiveElement >>= Maybe.maybe mempty HTMLElement.blur

main :: Effect.Effect Unit
main = do
  watchIPC
  AutoMode.installOnFocusHandler
  AutoMode.installOnNavigateHandler
  ScrollingPosition.installScrollHandler
