module Minichrome.UI
  ( main
  ) where

import Prelude

import Control.Coroutine as Coroutine
import Data.Maybe as Maybe
import Effect as Effect
import Effect.Aff as Aff
import Effect.Class as EffectClass
import Halogen.Aff as HalogenAff
import Halogen.VDom.Driver as VDomDriver

import Minichrome.CLI.Client as Client
import Minichrome.Config as Config
import Minichrome.UI.Components.Page as Page
import Minichrome.UI.IPC as IPC
import Minichrome.UI.Keybindings as Keybindings

-- | Handle any messages raised by the top-level `Page` component.
handlePageMessage :: forall t m. EffectClass.MonadEffect m =>
                     Config.Config ->
                     Page.Message ->
                     m (Maybe.Maybe t)
handlePageMessage config (Page.RunEx command) = do
  EffectClass.liftEffect $ Aff.launchAff_ $ Client.exec config command
  pure Maybe.Nothing

-- | Given a `Config`, boot the UI.
runUI :: Config.Config -> Effect.Effect Unit
runUI config = HalogenAff.runHalogenAff do
  body <- HalogenAff.awaitBody
  page <- VDomDriver.runUI (Page.page config) unit body
  EffectClass.liftEffect $ Keybindings.attach config page.query
  EffectClass.liftEffect $ IPC.attach config page.query
  page.subscribe $ Coroutine.consumer $ handlePageMessage config

-- | The entry point for booting the UI.
main :: Effect.Effect Unit
main = runUI Config.defaultConfig
