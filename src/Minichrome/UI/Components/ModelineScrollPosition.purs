module Minichrome.UI.Components.ModelineScrollPosition
  ( scrollPosition
  ) where

import Prelude

import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.CSS as HalogenCSS
import Halogen.HTML.Properties as HalogenProperties

import Minichrome.UI.CSS as MinichromeCSS

scrollPosition :: forall a b. Int -> Halogen.HTML a b
scrollPosition position =
  HalogenHTML.span [ style ] [ HalogenHTML.text $ formatPosition position ]

style :: forall t p. HalogenProperties.IProp (style :: String | p) t
style = HalogenCSS.style do
  MinichromeCSS.cursor MinichromeCSS.defaultCursor
  MinichromeCSS.userSelect MinichromeCSS.none

formatPosition :: Int -> String
formatPosition 0 = "Top"
formatPosition 100 = "Bot"
formatPosition pct = show pct <> "%"
