module Minichrome.UI.Components.Page
  ( page
  , Query
  ) where

import Prelude

import CSS as CSS
import Data.Maybe as Maybe
import Data.Either.Nested as NestedEither
import Data.Functor.Coproduct.Nested as NestedCoproduct
import Halogen as Halogen
import Halogen.Component.ChildPath as ChildPath
import Halogen.HTML as HalogenHTML
import Halogen.HTML.CSS as HalogenCSS
import Halogen.HTML.Events as HalogenEvents

import Minichrome.UI.CSS as MinichromeCSS
import Minichrome.UI.State as State
import Minichrome.UI.Components.Modeline as Modeline
import Minichrome.UI.Components.Webview as Webview

type ChildQuery = NestedCoproduct.Coproduct2 Webview.Query Modeline.Query
type ChildSlot = NestedEither.Either2 Unit Unit

data Query a = HandleWebview Webview.Message a

messageline :: forall p i. Halogen.HTML p i
messageline =
  HalogenHTML.div
    [ HalogenCSS.style do
      CSS.height $ CSS.px 20.0
      CSS.lineHeight $ CSS.px 20.0
      CSS.paddingLeft $ CSS.px 10.0
      CSS.paddingRight $ CSS.px 10.0
      CSS.fontFamily [ ] MinichromeCSS.monospace
      CSS.background CSS.lightgrey
    ]
    []

render :: forall m.
          State.State ->
          Halogen.ParentHTML Query ChildQuery ChildSlot m
render state =
  HalogenHTML.div
    [ HalogenCSS.style do
      CSS.height $ CSS.pct 100.0
      CSS.width $ CSS.pct 100.0
      CSS.display $ CSS.flex
      CSS.flexFlow CSS.column CSS.nowrap
    ]
    [ webview, modeline, messageline ]
    where
      webview =
        HalogenHTML.slot'
          ChildPath.cp1
          unit
          Webview.webview
          { address: state.address }
          $ HalogenEvents.input HandleWebview
      modeline =
        HalogenHTML.slot'
          ChildPath.cp2
          unit
          Modeline.modeline
          state
          absurd

eval :: forall m.
        Query ~>
        Halogen.ParentDSL State.State Query ChildQuery ChildSlot Void m
eval (HandleWebview (Webview.TitleUpdated title) next) = do
  Halogen.modify_ \st -> st { title = title }
  pure next
eval (HandleWebview (Webview.URLUpdated url) next) = do
  Halogen.modify_ \st -> st { address = url }
  pure next

page :: forall m. Halogen.Component HalogenHTML.HTML Query Unit Void m
page = Halogen.parentComponent
  { initialState: const State.initialState
  , render
  , eval
  , receiver: const Maybe.Nothing
  }
