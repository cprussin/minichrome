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

import Minichrome.Config as Config
import Minichrome.Temp.CSS as TCSS
import Minichrome.UI.Components.MessagelineInput as MessagelineInput
import Minichrome.UI.State as State

type Props p =
  ( message :: String
  , sequence :: String
  , messagelineInput :: Maybe.Maybe State.MessagelineInput
  , messagelineInputRef :: Halogen.RefLabel
  , onExClear :: Maybe.Maybe (State.Query Unit)
  , onExEnter :: String -> Maybe.Maybe (State.Query Unit)
  , onSearchClear :: Maybe.Maybe (State.Query Unit)
  , onSearchEnter :: String -> Maybe.Maybe (State.Query Unit)
  , onSearchChange :: String -> Maybe.Maybe (State.Query Unit)
  | p
  )

messageline
  :: forall a p
   . Config.Config
  -> Record (Props p)
  -> Halogen.HTML a State.Query
messageline config props = HalogenHTML.div [ style config ] [ child props ]

child :: forall a p. Record (Props p) -> Halogen.HTML a State.Query
child props@{ messagelineInput } = case messagelineInput of
  Maybe.Just State.ExInput -> exInput props
  Maybe.Just State.SearchInput -> searchInput props
  Maybe.Nothing -> text props

style
  :: forall t p
   . Config.Config
  -> HalogenProperties.IProp (style :: String | p) t
style config = HalogenCSS.style do
  CSS.height $ CSS.px 20.0
  CSS.lineHeight $ CSS.px 20.0
  CSS.paddingLeft $ CSS.px 10.0
  CSS.paddingRight $ CSS.px 10.0
  CSS.fontFamily [ ] TCSS.monospace
  CSS.background config.modeline.messageline.bg
  CSS.color config.modeline.messageline.fg

exInput :: forall a p. Record (Props p) -> Halogen.HTML a State.Query
exInput props = MessagelineInput.messagelineInput
  { prefix: ":"
  , onClear: props.onExClear
  , onEnter: props.onExEnter
  , onChange: const Maybe.Nothing
  , ref: props.messagelineInputRef
  }

searchInput :: forall a p. Record (Props p) -> Halogen.HTML a State.Query
searchInput props = MessagelineInput.messagelineInput
  { prefix: "/"
  , onClear: props.onSearchClear
  , onEnter: props.onSearchEnter
  , onChange: props.onSearchChange
  , ref: props.messagelineInputRef
  }

text :: forall a p. Record (Props p) -> Halogen.HTML a State.Query
text { message, sequence } = HalogenHTML.text
  if String.null sequence
     then message
     else sequence
