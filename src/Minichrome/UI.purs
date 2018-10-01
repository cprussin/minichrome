module Minichrome.UI
  ( main
  ) where

import Prelude

import Effect as Effect
import Halogen as Halogen
import Halogen.Aff as HalogenAff
import Halogen.VDom.Driver as VDomDriver

import Minichrome.Config as Config
import Minichrome.UI.Components.FrameContainer as FrameContainer
import Minichrome.UI.IPC as IPC
import Minichrome.UI.Keybindings as Keybindings
import Minichrome.UI.State as State

-- | Given a `Config`, boot the UI.
runUI :: Config.Config -> Effect.Effect Unit
runUI config = HalogenAff.runHalogenAff do
  state <- Halogen.liftEffect State.initialState
  body <- HalogenAff.awaitBody
  ui <- VDomDriver.runUI (FrameContainer.frameContainer config state) unit body
  Halogen.liftEffect $ Keybindings.attach config ui.query
  Halogen.liftEffect $ IPC.attach config ui.query

-- | The entry point for booting the UI.
main :: Effect.Effect Unit
main = runUI Config.defaultConfig
