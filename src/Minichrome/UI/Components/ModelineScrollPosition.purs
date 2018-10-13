module Minichrome.UI.Components.ModelineScrollPosition
  ( scrollPosition
  ) where

import Prelude

import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.CSS as HalogenCSS
import Halogen.HTML.Properties as HalogenProperties

import Minichrome.Temp.CSS as TCSS

scrollPosition :: forall a b. Int -> Halogen.HTML a b
scrollPosition position =
  HalogenHTML.span [ style ] [ HalogenHTML.text $ formatPosition position ]

style :: forall t p. HalogenProperties.IProp (style :: String | p) t
style = HalogenCSS.style do
  TCSS.cursor TCSS.defaultCursor
  TCSS.userSelect TCSS.none

formatPosition :: Int -> String
formatPosition 0 = "Top"
formatPosition 100 = "Bot"
formatPosition pct = show pct <> "%"
