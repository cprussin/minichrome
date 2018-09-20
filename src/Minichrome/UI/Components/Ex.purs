module Minichrome.UI.Components.Ex
  ( Query
  , Message(..)
  , ex
  ) where

import Prelude

import CSS as CSS
import Data.Maybe as Maybe
import Data.String as String
import Effect.Class as EffectClass
import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.Events as HalogenEvents
import Halogen.HTML.Properties as HalogenProperties
import Halogen.HTML.CSS as HalogenCSS
import Web.Event.Event as Event
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.UIEvent.FocusEvent as FocusEvent
import Web.UIEvent.KeyboardEvent as KeyboardEvent

import Minichrome.UI.CSS as MinichromeCSS

data Query a
  = Initialize a
  | Blur FocusEvent.FocusEvent a
  | KeyDown KeyboardEvent.KeyboardEvent a

data Message
  = UnEx
  | RunEx String

render :: Halogen.ComponentHTML Query
render = HalogenHTML.input
  [ HalogenProperties.value ":"
  , HalogenProperties.ref $ Halogen.RefLabel "ex"
  , HalogenProperties.autofocus true
  , HalogenEvents.onBlur $ HalogenEvents.input Blur
  , HalogenEvents.onKeyDown $ HalogenEvents.input KeyDown
  , HalogenCSS.style do
    CSS.background $ CSS.rgba 0 0 0 0.0
    CSS.width $ CSS.pct 100.0
    CSS.height $ CSS.pct 100.0
    CSS.padding (CSS.px 0.0) (CSS.px 0.0) (CSS.px 0.0) (CSS.px 0.0)
    CSS.fontFamily [ ] MinichromeCSS.monospace
    MinichromeCSS.borderWidth $ CSS.px 0.0
    MinichromeCSS.outlineWidth $ CSS.px 0.0
  ]

withValue :: forall m. EffectClass.MonadEffect m =>
             KeyboardEvent.KeyboardEvent ->
             (String -> m Unit) ->
             m Unit
withValue event cb =
  Maybe.maybe (pure unit) (_ >>= cb) $ readValue <$> getInputElement
  where
    readValue = HTMLInputElement.value >>> Halogen.liftEffect
    event' = KeyboardEvent.toEvent event
    getInputElement = Event.target event' >>= HTMLInputElement.fromEventTarget

stripColon :: String -> String
stripColon = String.replace (String.Pattern ":") (String.Replacement "")

eval :: forall m. EffectClass.MonadEffect m =>
        Query ~>
        Halogen.ComponentDSL Unit Query Message m
eval (Initialize next) = do
  elem <- Halogen.getHTMLElementRef (Halogen.RefLabel "ex")
  Maybe.maybe (pure unit) (HTMLElement.focus >>> Halogen.liftEffect) elem
  pure next
eval (Blur _ next) = do
  Halogen.raise $ UnEx
  pure next
eval (KeyDown event next) = do
  case KeyboardEvent.key event of
    "Escape" -> Halogen.raise UnEx
    "Enter" -> withValue event $ stripColon >>> RunEx >>> Halogen.raise
    "Backspace" ->
      withValue event \val -> when (val == ":") $ Halogen.raise UnEx
    _ -> pure unit
  pure next

ex :: forall m. EffectClass.MonadEffect m =>
      Halogen.Component HalogenHTML.HTML Query Unit Message m
ex = Halogen.lifecycleComponent
  { initialState: const unit
  , render: const render
  , eval
  , receiver: const Maybe.Nothing
  , initializer: Maybe.Just $ Halogen.action Initialize
  , finalizer: Maybe.Nothing
  }
