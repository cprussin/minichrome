module Minichrome.UI.Components.ModelineModeIndicator
  ( modeIndicator
  ) where

import Prelude

import CSS as CSS
import CSS.TextAlign as CSSTextAlign
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
  CSS.width $ CSS.px 70.0
  CSS.marginRight $ CSS.px 5.0
  CSS.marginLeft $ CSS.px 5.0
  CSSTextAlign.textAlign $ CSSTextAlign.center
  TCSS.cursor TCSS.defaultCursor
  TCSS.userSelect TCSS.none
  CSS.background $ case mode of
    InputMode.Normal -> config.modeline.modeIndicator.normal.bg
    InputMode.Insert -> config.modeline.modeIndicator.insert.bg
    InputMode.Follow -> config.modeline.modeIndicator.follow.bg
    InputMode.Select -> config.modeline.modeIndicator.select.bg
    InputMode.Toggle -> config.modeline.modeIndicator.toggle.bg
    InputMode.AV -> config.modeline.modeIndicator.av.bg
  CSS.color $ case mode of
    InputMode.Normal -> config.modeline.modeIndicator.normal.fg
    InputMode.Insert -> config.modeline.modeIndicator.insert.fg
    InputMode.Follow -> config.modeline.modeIndicator.follow.fg
    InputMode.Select -> config.modeline.modeIndicator.select.fg
    InputMode.Toggle -> config.modeline.modeIndicator.toggle.fg
    InputMode.AV -> config.modeline.modeIndicator.av.fg

indicatorText :: InputMode.Mode -> String
indicatorText InputMode.Normal = "NORMAL"
indicatorText InputMode.Insert = "INSERT"
indicatorText InputMode.Follow = "FOLLOW"
indicatorText InputMode.Select = "SELECT"
indicatorText InputMode.Toggle = "TOGGLE"
indicatorText InputMode.AV = "AV"
