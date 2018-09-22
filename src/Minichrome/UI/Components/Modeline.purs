module Minichrome.UI.Components.Modeline
  ( Query
  , modeline
  ) where

import Prelude

import CSS as CSS
import CSS.Overflow as CSSOverflow
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

type State = Input

data Query a = HandleInput Input a

type Message = Void
type DSL = Halogen.ComponentDSL State Query Message
type Component = Halogen.Component HalogenHTML.HTML Query Input Message

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
          CSS.color CSS.white
          CSS.fontWeight CSS.bold
          CSS.paddingLeft $ CSS.px 8.0
          CSS.paddingRight $ CSS.px 8.0
          CSS.marginRight $ CSS.px 10.0
          MinichromeCSS.cursor MinichromeCSS.defaultCursor
          MinichromeCSS.userSelect MinichromeCSS.none
          case input.mode of
            InputMode.Normal -> CSS.background CSS.blue
            InputMode.Insert -> CSS.background CSS.magenta
      ]
      [ HalogenHTML.text $ show input.mode ]
    , HalogenHTML.span
      [ HalogenCSS.style do
          CSS.marginRight $ CSS.px 20.0
          CSS.maxWidth $ CSS.pct 20.0
          CSSOverflow.overflow CSSOverflow.hidden
          MinichromeCSS.textOverflow MinichromeCSS.Ellipsis
          CSS.textWhitespace CSS.whitespaceNoWrap
      ]
      [ HalogenHTML.text input.title ]
    , HalogenHTML.span
      [ HalogenCSS.style do
          CSS.color CSS.blue
          CSSOverflow.overflow CSSOverflow.hidden
          MinichromeCSS.textOverflow MinichromeCSS.Ellipsis
          CSS.textWhitespace CSS.whitespaceNoWrap
          CSS.maxWidth $ CSS.pct 60.0
          CSS.marginRight $ CSS.px 20.0
      ]
      [ HalogenHTML.text input.address ]
    , HalogenHTML.div [ HalogenCSS.style $ CSS.flexGrow 1 ] []
    , HalogenHTML.span
      [ HalogenCSS.style do
          MinichromeCSS.cursor MinichromeCSS.defaultCursor
          MinichromeCSS.userSelect MinichromeCSS.none
      ]
      [ HalogenHTML.text $ show input.position <> "%" ]
    ]

eval :: forall m. Query ~> DSL m
eval (HandleInput n next) = do
  oldN <- Halogen.get
  when (oldN /= n) $ Halogen.put n
  pure next

modeline :: forall m. Record State.State -> Component m
modeline initialState = Halogen.component
  { initialState: const
    { mode: initialState.mode
    , title: initialState.title
    , address: initialState.address
    , position: initialState.position
    }
  , render
  , eval
  , receiver: HalogenEvents.input HandleInput
  }
