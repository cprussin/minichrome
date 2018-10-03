module Minichrome.Page
  ( main
  ) where

import Prelude

import Effect as Effect

import Minichrome.Page.AutoMode as AutoMode
import Minichrome.Page.IPC as IPC
import Minichrome.Page.ScrollingPosition as ScrollingPosition

main :: Effect.Effect Unit
main = do
  AutoMode.setup
  IPC.setup
  ScrollingPosition.setup
