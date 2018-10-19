module Minichrome.UI.Components.ModelineLoadingIndicator
  ( loadingIndicator
  ) where

import Prelude

import CSS as CSS
import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.CSS as HalogenCSS
import Halogen.HTML.Properties as HalogenProperties

import Minichrome.Config as Config
import Minichrome.UI.State as State

loadingIndicator
  :: forall a b
   . Config.Config
  -> State.LoadingState
  -> Halogen.HTML a b
loadingIndicator config loadingState =
  HalogenHTML.span [ style config loadingState ] []

style
  :: forall t p
   . Config.Config
  -> State.LoadingState
  -> HalogenProperties.IProp (style :: String | p) t
style _ State.Loaded = HalogenCSS.style $ CSS.width $ CSS.px 10.0
style config State.Loading = HalogenCSS.style do
  CSS.width $ CSS.px 10.0
  CSS.height $ CSS.px 1.0
  CSS.marginRight $ CSS.px 5.0
  CSS.marginLeft $ CSS.px 5.0
  CSS.background config.modeline.spinnerColor
  CSS.animation
    (CSS.fromString "spin")
    (CSS.sec 0.5)
    CSS.linear
    (CSS.sec 0.0)
    CSS.infinite
    CSS.normalAnimationDirection
    CSS.forwards
