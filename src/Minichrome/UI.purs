module Minichrome.UI
  ( main
  ) where

import Prelude

import Effect as Effect
import Halogen.Aff as HalogenAff
import Halogen.VDom.Driver as VDomDriver

import Minichrome.UI.Components.Page as Page

-- | The entry point for booting the UI.
main :: Effect.Effect Unit
main = HalogenAff.runHalogenAff do
  body <- HalogenAff.awaitBody
  VDomDriver.runUI Page.page unit body
