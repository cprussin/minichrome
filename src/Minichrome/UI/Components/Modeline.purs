module Minichrome.UI.Components.Modeline
  ( Query
  , modeline
  ) where

import Prelude

import CSS as CSS
import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.Events as HalogenEvents
import Halogen.HTML.CSS as HalogenCSS

import Minichrome.UI.CSS as MinichromeCSS
import Minichrome.UI.InputMode as InputMode
import Minichrome.UI.State as State

type Input = Record
  ( mode :: InputMode.Mode
  , title :: String
  , address :: String
  , position :: Int
  )

data Query a = HandleInput Input a

initialState :: Input
initialState =
  { mode: State.initialState.mode
  , title: State.initialState.title
  , address: State.initialState.address
  , position: State.initialState.position
  }

render :: Input -> Halogen.ComponentHTML Query
render input =
  HalogenHTML.div
    [ HalogenCSS.style do
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
    ]
    [ HalogenHTML.span
      [ HalogenCSS.style do
        CSS.background CSS.blue
        CSS.fontWeight CSS.bold
        CSS.paddingLeft $ CSS.px 8.0
        CSS.paddingRight $ CSS.px 8.0
        CSS.marginRight $ CSS.px 10.0
        MinichromeCSS.cursor MinichromeCSS.defaultCursor
        MinichromeCSS.userSelect MinichromeCSS.none
        case input.mode of
          InputMode.Normal -> CSS.color CSS.white
      ]
      [ HalogenHTML.text $ show input.mode ]
    , HalogenHTML.span
      [ HalogenCSS.style $ CSS.marginRight $ CSS.px 20.0 ]
      [ HalogenHTML.text input.title ]
    , HalogenHTML.span
      [ HalogenCSS.style $ CSS.color CSS.blue ]
      [ HalogenHTML.text input.address ]
    , HalogenHTML.div [ HalogenCSS.style $ CSS.flexGrow 1 ] []
    , HalogenHTML.span
      [ HalogenCSS.style do
        MinichromeCSS.cursor MinichromeCSS.defaultCursor
        MinichromeCSS.userSelect MinichromeCSS.none
      ]
      [ HalogenHTML.text $ show input.position <> "%" ]
    ]

eval :: forall m. Query ~> Halogen.ComponentDSL Input Query Void m
eval (HandleInput n next) = do
  oldN <- Halogen.get
  when (oldN /= n) $ Halogen.put n
  pure next

modeline :: forall m. Halogen.Component HalogenHTML.HTML Query Input Void m
modeline = Halogen.component
  { initialState: const initialState
  , render
  , eval
  , receiver: HalogenEvents.input HandleInput
  }
