module Minichrome.UI.Components.Page
  ( page
  , Query(..)
  ) where

import Prelude

import CSS as CSS
import Data.Maybe as Maybe
import Data.Either.Nested as NestedEither
import Data.Functor.Coproduct.Nested as NestedCoproduct
import Data.Time.Duration as Duration
import Effect.Aff as Aff
import Effect.Exception as Exception
import Halogen as Halogen
import Halogen.Component.ChildPath as ChildPath
import Halogen.HTML as HalogenHTML
import Halogen.HTML.CSS as HalogenCSS
import Halogen.HTML.Events as HalogenEvents
import Halogen.Query.HalogenM as HalogenM

import Minichrome.UI.CSS as MinichromeCSS
import Minichrome.UI.State as State
import Minichrome.UI.Components.Modeline as Modeline
import Minichrome.UI.Components.Webview as Webview

type ChildQuery = NestedCoproduct.Coproduct2 Webview.Query Modeline.Query
type ChildSlot = NestedEither.Either2 Unit Unit

data Query a
  = HandleWebview Webview.Message a
  | ShowMessage String a
  | GoBack a
  | GoForward a
  | OpenDevTools a

type State = Record
  ( messageCanceler :: Maybe.Maybe (Exception.Error -> Aff.Aff Unit)
  | State.State
  )

messageline :: forall p i. String -> Halogen.HTML p i
messageline message =
  HalogenHTML.div
    [ HalogenCSS.style do
      CSS.height $ CSS.px 20.0
      CSS.lineHeight $ CSS.px 20.0
      CSS.paddingLeft $ CSS.px 10.0
      CSS.paddingRight $ CSS.px 10.0
      CSS.fontFamily [ ] MinichromeCSS.monospace
      CSS.background CSS.lightgrey
    ]
    [ HalogenHTML.text message ]

render :: State -> Halogen.ParentHTML Query ChildQuery ChildSlot Aff.Aff
render state =
  HalogenHTML.div
    [ HalogenCSS.style do
      CSS.height $ CSS.pct 100.0
      CSS.width $ CSS.pct 100.0
      CSS.display $ CSS.flex
      CSS.flexFlow CSS.column CSS.nowrap
    ]
    [ webview, modeline, messageline state.message ]
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
          { mode: state.mode
          , title: state.title
          , address: state.address
          , position: state.position
          }
          absurd

eval :: Query ~> Halogen.ParentDSL State Query ChildQuery ChildSlot Void Aff.Aff
eval (HandleWebview (Webview.TitleUpdated title) next) = do
  Halogen.modify_ _{ title = title }
  pure next
eval (HandleWebview (Webview.URLUpdated url) next) = do
  Halogen.modify_ _{ address = url }
  pure next
eval (GoBack next) = do
  eval $ ShowMessage "Going back..." unit
  _ <- Halogen.query' ChildPath.cp1 unit $ Halogen.request Webview.GoBack
  pure next
eval (GoForward next) = do
  eval $ ShowMessage "Going forward..." unit
  _ <- Halogen.query' ChildPath.cp1 unit $ Halogen.request Webview.GoForward
  pure next
eval (OpenDevTools next) = do
  eval $ ShowMessage "Opening dev tools..." unit
  _ <- Halogen.query' ChildPath.cp1 unit $ Halogen.request Webview.OpenDevTools
  pure next
eval (ShowMessage message next) = do
  -- Cancel any existing message clear timeout
  messageCanceller <- Halogen.gets _.messageCanceler
  Maybe.maybe (pure unit) Halogen.liftAff do
    messageCanceller <*> pure (Exception.error "")
  -- Create the new message clear timeout
  canceler <- HalogenM.fork do
    Halogen.liftAff $ Aff.delay $ Duration.Milliseconds 2000.0
    Halogen.modify_ _{ message = "", messageCanceler = Maybe.Nothing }
  -- Set the message
  Halogen.modify_ _{ message = message, messageCanceler = Maybe.Just canceler }
  pure next

page :: Halogen.Component HalogenHTML.HTML Query Unit Void Aff.Aff
page = Halogen.parentComponent
  { initialState: const
    { messageCanceler: Maybe.Nothing
    , mode: State.initialState.mode
    , title: State.initialState.title
    , address: State.initialState.address
    , position: State.initialState.position
    , message: State.initialState.message
    }
  , render
  , eval
  , receiver: const Maybe.Nothing
  }
