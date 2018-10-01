module Minichrome.UI.Components.ModelinePageTitle
  ( pageTitle
  ) where

import Prelude

import CSS as CSS
import CSS.Overflow as CSSOverflow
import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.CSS as HalogenCSS
import Halogen.HTML.Properties as HalogenProperties

import Minichrome.UI.CSS as MinichromeCSS

pageTitle :: forall a b. String -> Halogen.HTML a b
pageTitle title = HalogenHTML.span [ style ] [ HalogenHTML.text title ]

style :: forall t p. HalogenProperties.IProp (style :: String | p) t
style = HalogenCSS.style do
  CSS.marginRight $ CSS.px 20.0
  CSS.maxWidth $ CSS.pct 20.0
  CSSOverflow.overflow CSSOverflow.hidden
  MinichromeCSS.textOverflow MinichromeCSS.Ellipsis
  CSS.textWhitespace CSS.whitespaceNoWrap
