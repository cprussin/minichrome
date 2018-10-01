module Minichrome.UI.Components.Modeline
  ( Props
  , modeline
  ) where

import Prelude

import CSS as CSS
import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.CSS as HalogenCSS
import Halogen.HTML.Properties as HalogenProperties

import Minichrome.Command.InputMode as InputMode
import Minichrome.UI.Components.ModelineSpacer as Spacer
import Minichrome.UI.Components.ModelineModeIndicator as ModeIndicator
import Minichrome.UI.Components.ModelinePageTitle as PageTitle
import Minichrome.UI.Components.ModelinePageURL as PageURL
import Minichrome.UI.Components.ModelineScrollPosition as ScrollPosition
import Minichrome.UI.CSS as MinichromeCSS

type Props p =
  ( mode :: InputMode.Mode
  , title :: String
  , address :: String
  , position :: Int
  | p
  )

modeline :: forall a b p. Record (Props p) -> Halogen.HTML a b
modeline { mode, title, address, position } =
  HalogenHTML.div [ style ]
    [ ModeIndicator.modeIndicator mode
    , PageTitle.pageTitle title
    , PageURL.pageURL address
    , Spacer.spacer
    , ScrollPosition.scrollPosition position
    ]

style :: forall t p. HalogenProperties.IProp (style :: String | p) t
style = HalogenCSS.style do
  CSS.height $ CSS.px 20.0
  CSS.lineHeight $ CSS.px 20.0
  CSS.paddingLeft $ CSS.px 10.0
  CSS.paddingRight $ CSS.px 10.0
  CSS.fontFamily [ ] MinichromeCSS.monospace
  CSS.background CSS.darkgrey
  CSS.display CSS.flex
  CSS.flexFlow CSS.row CSS.nowrap
  CSS.borderTop CSS.solid (CSS.px 1.0) $ CSS.rgb 2 2 2
  CSS.borderBottom CSS.solid (CSS.px 1.0) $ CSS.rgb 2 2 2
