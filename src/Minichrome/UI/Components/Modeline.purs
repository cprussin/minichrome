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
import Minichrome.Config as Config
import Minichrome.Temp.CSS as TCSS
import Minichrome.UI.Components.ModelineSpacer as Spacer
import Minichrome.UI.Components.ModelineModeIndicator as ModeIndicator
import Minichrome.UI.Components.ModelinePageTitle as PageTitle
import Minichrome.UI.Components.ModelinePageURL as PageURL
import Minichrome.UI.Components.ModelineScrollPosition as ScrollPosition

type Props p =
  ( mode :: InputMode.Mode
  , title :: String
  , address :: String
  , position :: Int
  | p
  )

modeline
  :: forall a b p
   . Config.Config
  -> Record (Props p)
  -> Halogen.HTML a b
modeline config props = HalogenHTML.div [ style config ] $
  field props <$> config.modeline.fields <@> config

field
  :: forall a b p
   . Record (Props p)
  -> Config.ModelineField
  -> Config.Config
  -> Halogen.HTML a b
field { mode } Config.ModeIndicator = flip ModeIndicator.modeIndicator mode
field { title } Config.PageTitle = const $ PageTitle.pageTitle title
field { address } Config.PageURL = flip PageURL.pageURL address
field _ Config.Spacer =  const $ Spacer.spacer
field { position } Config.ScrollPosition =
  const $ ScrollPosition.scrollPosition position

style
  :: forall t p
   . Config.Config
  -> HalogenProperties.IProp (style :: String | p) t
style config = HalogenCSS.style do
  CSS.height $ CSS.px 20.0
  CSS.lineHeight $ CSS.px 20.0
  CSS.paddingLeft $ CSS.px 5.0
  CSS.paddingRight $ CSS.px 5.0
  CSS.fontFamily [ ] TCSS.monospace
  CSS.background config.modeline.colors.bg
  CSS.color config.modeline.colors.fg
  CSS.display CSS.flex
  CSS.flexFlow CSS.row CSS.nowrap
  CSS.borderTop CSS.solid (CSS.px 1.0) $ CSS.rgb 2 2 2
  CSS.borderBottom CSS.solid (CSS.px 1.0) $ CSS.rgb 2 2 2
