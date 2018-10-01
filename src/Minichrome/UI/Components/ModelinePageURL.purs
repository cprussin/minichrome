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

import Minichrome.UI.CSS as MinichromeCSS

pageURL :: forall a b. String -> Halogen.HTML a b
pageURL address = HalogenHTML.span [ style ] [ HalogenHTML.text address ]

style :: forall t p. HalogenProperties.IProp (style :: String | p) t
style = HalogenCSS.style do
  CSS.color CSS.blue
  CSSOverflow.overflow CSSOverflow.hidden
  MinichromeCSS.textOverflow MinichromeCSS.Ellipsis
  CSS.textWhitespace CSS.whitespaceNoWrap
  CSS.maxWidth $ CSS.pct 60.0
  CSS.marginRight $ CSS.px 20.0
