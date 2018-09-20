module Minichrome.UI
  ( main
  ) where

import Prelude

import Effect as Effect
import Effect.Class as EffectClass
import Halogen.Aff as HalogenAff
import Halogen.VDom.Driver as VDomDriver

import Minichrome.Config as Config
import Minichrome.UI.Components.Page as Page
import Minichrome.UI.IPC as IPC
import Minichrome.UI.Keybindings as Keybindings

-- | Given a `Config`, boot the UI.
runUI :: Config.Config -> Effect.Effect Unit
runUI config = HalogenAff.runHalogenAff do
  body <- HalogenAff.awaitBody
  page <- VDomDriver.runUI (Page.page config) unit body
  EffectClass.liftEffect $ Keybindings.attach config page.query
  EffectClass.liftEffect $ IPC.attach config page.query

-- | The entry point for booting the UI.
main :: Effect.Effect Unit
main = runUI Config.defaultConfig
