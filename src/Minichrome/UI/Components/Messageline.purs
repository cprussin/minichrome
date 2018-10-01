module Minichrome.UI.Components.Messageline
  ( Props
  , messageline
  ) where

import Prelude

import CSS as CSS
import Data.Maybe as Maybe
import Data.String as String
import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.CSS as HalogenCSS
import Halogen.HTML.Properties as HalogenProperties

import Minichrome.UI.CSS as MinichromeCSS
import Minichrome.UI.Components.MessagelineInput as MessagelineInput
import Minichrome.UI.State as State

type Props p =
  ( message :: String
  , sequence :: String
  , ex :: Boolean
  , exRef :: Halogen.RefLabel
  , onExClear :: Maybe.Maybe (State.Query Unit)
  , onExEnter :: String -> Maybe.Maybe (State.Query Unit)
  | p
  )

messageline :: forall a p. Record (Props p) -> Halogen.HTML a State.Query
messageline props = HalogenHTML.div [ style ] [ child props ]

child :: forall a p. Record (Props p) -> Halogen.HTML a State.Query
child props@{ ex }
  | ex = exInput props
  | otherwise = text props

style :: forall t p. HalogenProperties.IProp (style :: String | p) t
style = HalogenCSS.style do
  CSS.height $ CSS.px 20.0
  CSS.lineHeight $ CSS.px 20.0
  CSS.paddingLeft $ CSS.px 10.0
  CSS.paddingRight $ CSS.px 10.0
  CSS.fontFamily [ ] MinichromeCSS.monospace
  CSS.background CSS.lightgrey

exInput :: forall a p. Record (Props p) -> Halogen.HTML a State.Query
exInput { onExClear, onExEnter, exRef } = MessagelineInput.messagelineInput
  { prefix: ":"
  , onClear: onExClear
  , onEnter: onExEnter
  , ref: exRef
  }

text :: forall a p. Record (Props p) -> Halogen.HTML a State.Query
text { message, sequence } = HalogenHTML.text
  if String.null sequence
     then message
     else sequence
