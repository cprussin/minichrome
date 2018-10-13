module Minichrome.UI.Components.ModelineModeIndicator
  ( modeIndicator
  ) where

import Prelude

import CSS as CSS
import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.CSS as HalogenCSS
import Halogen.HTML.Properties as HalogenProperties

import Minichrome.Config as Config
import Minichrome.Command.InputMode as InputMode
import Minichrome.Temp.CSS as TCSS

modeIndicator :: forall a b. Config.Config -> InputMode.Mode -> Halogen.HTML a b
modeIndicator config mode = HalogenHTML.span [ style config mode ]
  [ HalogenHTML.text $ indicatorText mode ]

style
  :: forall t p
   . Config.Config
  -> InputMode.Mode
  -> HalogenProperties.IProp (style :: String | p) t
style config mode = HalogenCSS.style do
  CSS.fontWeight CSS.bold
  CSS.paddingLeft $ CSS.px 8.0
  CSS.paddingRight $ CSS.px 8.0
  CSS.marginRight $ CSS.px 10.0
  TCSS.cursor TCSS.defaultCursor
  TCSS.userSelect TCSS.none
  CSS.background $ case mode of
    InputMode.Normal -> config.modeline.modeIndicator.normal.bg
    InputMode.Insert -> config.modeline.modeIndicator.insert.bg
  CSS.color $ case mode of
    InputMode.Normal -> config.modeline.modeIndicator.normal.fg
    InputMode.Insert -> config.modeline.modeIndicator.insert.fg

indicatorText :: InputMode.Mode -> String
indicatorText InputMode.Normal = "NORMAL"
indicatorText InputMode.Insert = "INSERT"
