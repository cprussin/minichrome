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
import Minichrome.UI.CSS as MinichromeCSS

pageURL :: forall a b. Config.Config -> String -> Halogen.HTML a b
pageURL config address = HalogenHTML.span [ style config ]
  [ HalogenHTML.text address ]

style
  :: forall t p
   . Config.Config
  -> HalogenProperties.IProp (style :: String | p) t
style config = HalogenCSS.style do
  CSS.color config.modeline.url.fg
  CSS.background config.modeline.url.bg
  CSSOverflow.overflow CSSOverflow.hidden
  MinichromeCSS.textOverflow MinichromeCSS.Ellipsis
  CSS.textWhitespace CSS.whitespaceNoWrap
  CSS.maxWidth $ CSS.pct 60.0
  CSS.marginRight $ CSS.px 20.0
