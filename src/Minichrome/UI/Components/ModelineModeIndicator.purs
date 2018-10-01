module Minichrome.UI.Components.ModelineModeIndicator
  ( modeIndicator
  ) where

import Prelude

import CSS as CSS
import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.CSS as HalogenCSS
import Halogen.HTML.Properties as HalogenProperties

import Minichrome.Command.InputMode as InputMode
import Minichrome.UI.CSS as MinichromeCSS

modeIndicator :: forall a b. InputMode.Mode -> Halogen.HTML a b
modeIndicator mode =
  HalogenHTML.span [ style mode ] [ HalogenHTML.text $ indicatorText mode ]

style
  :: forall t p
   . InputMode.Mode
  -> HalogenProperties.IProp (style :: String | p) t
style mode = HalogenCSS.style do
  CSS.color CSS.white
  CSS.fontWeight CSS.bold
  CSS.paddingLeft $ CSS.px 8.0
  CSS.paddingRight $ CSS.px 8.0
  CSS.marginRight $ CSS.px 10.0
  MinichromeCSS.cursor MinichromeCSS.defaultCursor
  MinichromeCSS.userSelect MinichromeCSS.none
  case mode of
    InputMode.Normal -> CSS.background CSS.blue
    InputMode.Insert -> CSS.background CSS.magenta

indicatorText :: InputMode.Mode -> String
indicatorText InputMode.Normal = "NORMAL"
indicatorText InputMode.Insert = "INSERT"
