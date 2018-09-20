module Minichrome.UI.Components.Messageline
  ( Query
  , messageline
  ) where

import Prelude

import CSS as CSS
import Effect.Aff as Aff
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

data Query a
  = HandleInput Input a
  | HandleEx Ex.Message a

render :: Input -> Halogen.ParentHTML Query Ex.Query Unit Aff.Aff
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

eval :: Query ~> Halogen.ParentDSL Input Query Ex.Query Unit Ex.Message Aff.Aff
eval (HandleInput n next) = do
  oldN <- Halogen.get
  when (oldN /= n) $ Halogen.put n
  pure next
eval (HandleEx message next) = do
  Halogen.raise message
  pure next

messageline :: Halogen.Component HalogenHTML.HTML Query Input Ex.Message Aff.Aff
messageline = Halogen.parentComponent
  { initialState: const
    { message: State.initialState.message
    , ex: State.initialState.ex
    }
  , render
  , eval
  , receiver: HalogenEvents.input HandleInput
  }
