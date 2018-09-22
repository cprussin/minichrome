module Minichrome.UI.Components.Messageline
  ( Query
  , messageline
  ) where

import Prelude

import CSS as CSS
import Effect.Class as EffectClass
import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.Events as HalogenEvents
import Halogen.HTML.CSS as HalogenCSS

import Minichrome.UI.Components.Ex as Ex
import Minichrome.UI.CSS as MinichromeCSS
import Minichrome.UI.State as State

type Input = Record
  ( message :: String
  , ex :: Boolean
  )

type State = Input

data Query a
  = HandleInput Input a
  | HandleEx Ex.Message a

type Message = Ex.Message
type DSL = Halogen.ParentDSL State Query Ex.Query Unit Message
type Component = Halogen.Component HalogenHTML.HTML Query State Message
type HTML m = Halogen.ParentHTML Query Ex.Query Unit m

render :: forall m. EffectClass.MonadEffect m => Input -> HTML m
render input =
  HalogenHTML.div
    [ HalogenCSS.style do
        CSS.height $ CSS.px 20.0
        CSS.lineHeight $ CSS.px 20.0
        CSS.paddingLeft $ CSS.px 10.0
        CSS.paddingRight $ CSS.px 10.0
        CSS.fontFamily [ ] MinichromeCSS.monospace
        CSS.background CSS.lightgrey
    ]
    [ if input.ex
        then HalogenHTML.slot unit Ex.ex unit $ HalogenEvents.input HandleEx
        else HalogenHTML.text input.message
    ]

eval :: forall m. EffectClass.MonadEffect m => Query ~> DSL m
eval (HandleInput n next) = do
  oldN <- Halogen.get
  when (oldN /= n) $ Halogen.put n
  pure next
eval (HandleEx message next) = do
  Halogen.raise message
  pure next

messageline :: forall m. EffectClass.MonadEffect m =>
               Record State.State ->
               Component m
messageline initialState = Halogen.parentComponent
  { initialState: const
    { message: initialState.message
    , ex: initialState.ex
    }
  , render
  , eval
  , receiver: HalogenEvents.input HandleInput
  }
