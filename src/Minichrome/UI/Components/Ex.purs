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
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.UIEvent.FocusEvent as FocusEvent
import Web.UIEvent.KeyboardEvent as KeyboardEvent

import Minichrome.UI.CSS as MinichromeCSS

type Input = Unit
type State = Input

data Query a
  = Initialize a
  | Blur FocusEvent.FocusEvent a
  | KeyDown KeyboardEvent.KeyboardEvent a

data Message
  = UnEx
  | RunEx String

type DSL = Halogen.ComponentDSL State Query Message
type Component = Halogen.Component HalogenHTML.HTML Query Input Message

prefix :: String
prefix = ":"

exRef :: Halogen.RefLabel
exRef = Halogen.RefLabel "ex"

render :: Halogen.ComponentHTML Query
render = HalogenHTML.input
  [ HalogenProperties.value prefix
  , HalogenProperties.ref exRef
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

withInput :: forall m t. EffectClass.MonadEffect m => Monoid t =>
             (HTMLInputElement.HTMLInputElement -> DSL m t) ->
             DSL m t
withInput cb = do
  elem <- Halogen.getHTMLElementRef exRef
  Maybe.maybe (Halogen.liftEffect mempty) cb $
    elem >>= HTMLInputElement.fromHTMLElement

withValue :: forall m. EffectClass.MonadEffect m =>
             (String -> DSL m Unit) ->
             DSL m Unit
withValue = bind $ withInput $ HTMLInputElement.value >>> Halogen.liftEffect

stripColon :: String -> String
stripColon = String.replace (String.Pattern prefix) (String.Replacement "")

eval :: forall m. EffectClass.MonadEffect m => Query ~> DSL m
eval (Initialize next) = do
  withInput $
    HTMLInputElement.toHTMLElement >>> HTMLElement.focus >>> Halogen.liftEffect
  pure next
eval (Blur _ next) = do
  Halogen.raise $ UnEx
  pure next
eval (KeyDown event next) = do
  case KeyboardEvent.key event of
    "Escape" -> Halogen.raise UnEx
    "Enter" -> withValue $ stripColon >>> RunEx >>> Halogen.raise
    "Backspace" -> withValue \val -> when (val == prefix) $ Halogen.raise UnEx
    _ -> pure unit
  pure next

ex :: forall m. EffectClass.MonadEffect m => Component m
ex = Halogen.lifecycleComponent
  { initialState: const unit
  , render: const render
  , eval
  , receiver: const Maybe.Nothing
  , initializer: Maybe.Just $ Halogen.action Initialize
  , finalizer: Maybe.Nothing
  }
