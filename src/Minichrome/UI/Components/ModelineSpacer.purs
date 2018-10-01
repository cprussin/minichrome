module Minichrome.UI.Components.ModelineSpacer
  ( spacer
  ) where

import Prelude

import CSS as CSS
import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.CSS as HalogenCSS
import Halogen.HTML.Properties as HalogenProperties

spacer :: forall a b. Halogen.HTML a b
spacer = HalogenHTML.div [ style ] []

style :: forall t p. HalogenProperties.IProp (style :: String | p) t
style = HalogenCSS.style $ CSS.flexGrow 1
