module Minichrome.UI.Components.MessagelineInput
  ( messagelineInput
  ) where

import Prelude

import CSS as CSS
import Data.Maybe as Maybe
import Data.String as String
import Effect.Unsafe as UnsafeEffect
import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.CSS as HalogenCSS
import Halogen.HTML.Events as HalogenEvents
import Halogen.HTML.Properties as HalogenProperties
import Web.Event.Event as Event
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.UIEvent.KeyboardEvent as KeyboardEvent

import Minichrome.Temp.CSS as TCSS
import Minichrome.UI.State as State

type Props p =
  ( prefix :: String
  , ref :: Halogen.RefLabel
  , onClear :: Maybe.Maybe (State.Query Unit)
  , onEnter :: String -> Maybe.Maybe (State.Query Unit)
  , onChange :: String -> Maybe.Maybe (State.Query Unit)
  | p
  )

messagelineInput :: forall a p. Record (Props p) -> Halogen.HTML a State.Query
messagelineInput props = HalogenHTML.div [ style ]
  [ HalogenHTML.span [ prefixStyle ] [ HalogenHTML.text props.prefix ]
  , HalogenHTML.input
    [ inputStyle
    , HalogenProperties.ref props.ref
    , HalogenProperties.autofocus true
    , HalogenEvents.onBlur $ const props.onClear
    , HalogenEvents.onKeyDown $ handleKeyDown props
    , HalogenEvents.onInput $ withValue props.onChange
    ]
  ]

style :: forall t p. HalogenProperties.IProp (style :: String | p) t
style = HalogenCSS.style do
  CSS.width $ CSS.pct 100.0
  CSS.height $ CSS.pct 100.0
  CSS.display CSS.flex
  CSS.flexFlow CSS.row CSS.nowrap

prefixStyle :: forall t p. HalogenProperties.IProp (style :: String | p) t
prefixStyle = HalogenCSS.style do
  CSS.width $ CSS.px 10.0
  CSS.fontFamily [ ] TCSS.monospace

inputStyle :: forall t p. HalogenProperties.IProp (style :: String | p) t
inputStyle = HalogenCSS.style do
  CSS.flexGrow 1
  CSS.background $ CSS.rgba 0 0 0 0.0
  CSS.fontFamily [ ] TCSS.monospace
  CSS.padding (CSS.px 0.0) (CSS.px 0.0) (CSS.px 0.0) (CSS.px 0.0)
  TCSS.borderWidth $ CSS.px 0.0
  TCSS.outlineWidth $ CSS.px 0.0

handleKeyDown
  :: forall p
   . Record (Props p)
  -> KeyboardEvent.KeyboardEvent
  -> Maybe.Maybe (State.Query Unit)
handleKeyDown props event = UnsafeEffect.unsafePerformEffect do
  Event.stopPropagation $ KeyboardEvent.toEvent event
  pure $ case KeyboardEvent.key event of
    "Escape" -> props.onClear
    "Enter" -> flip withValue (KeyboardEvent.toEvent event) \val ->
      if String.null val then props.onClear else props.onEnter val
    "Backspace" -> flip withValue (KeyboardEvent.toEvent event) \val ->
      if String.null val then props.onClear else Maybe.Nothing
    _ -> Maybe.Nothing

withValue :: forall a. (String -> Maybe.Maybe a) -> Event.Event -> Maybe.Maybe a
withValue cb =
  Event.target >=> HTMLInputElement.fromEventTarget >=>
    HTMLInputElement.value >>> UnsafeEffect.unsafePerformEffect >>> cb
