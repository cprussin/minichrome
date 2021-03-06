module Minichrome.UI.Components.ModelinePageURL
  ( pageURL
  ) where

import Prelude

import CSS as CSS
import CSS.Overflow as CSSOverflow
import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.CSS as HalogenCSS
import Halogen.HTML.Properties as HalogenProperties

import Minichrome.Config as Config
import Minichrome.Temp.CSS as TCSS

pageURL :: forall a b. Config.Config -> String -> Halogen.HTML a b
pageURL config address = HalogenHTML.span [ style config ]
  [ HalogenHTML.text address ]

style
  :: forall t p
   . Config.Config
  -> HalogenProperties.IProp (style :: String | p) t
style config = HalogenCSS.style do
  CSS.marginRight $ CSS.px 5.0
  CSS.marginLeft $ CSS.px 5.0
  CSS.color config.modeline.url.fg
  CSS.background config.modeline.url.bg
  CSSOverflow.overflow CSSOverflow.hidden
  TCSS.textOverflow TCSS.Ellipsis
  CSS.textWhitespace CSS.whitespaceNoWrap
  CSS.maxWidth $ CSS.pct 60.0
