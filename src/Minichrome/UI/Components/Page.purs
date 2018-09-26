module Minichrome.UI.Components.Page
  ( page
  , Query(..)
  , Message(..)
  ) where

import Prelude

import CSS as CSS
import Control.Monad.State.Class as MonadState
import Data.Maybe as Maybe
import Data.Either.Nested as NestedEither
import Data.Functor.Coproduct.Nested as NestedCoproduct
import Data.Time.Duration as Duration
import Effect.Aff as Aff
import Effect.Aff.Class as AffClass
import Effect.Class as EffectClass
import Effect.Exception as Exception
import Halogen as Halogen
import Halogen.Component.ChildPath as ChildPath
import Halogen.HTML as HalogenHTML
import Halogen.HTML.CSS as HalogenCSS
import Halogen.HTML.Events as HalogenEvents
import Halogen.Query.HalogenM as HalogenM
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

import Minichrome.Config as Config
import Minichrome.UI.InputMode as InputMode
import Minichrome.UI.State as State
import Minichrome.UI.Components.Ex as Ex
import Minichrome.UI.Components.Messageline as Messageline
import Minichrome.UI.Components.Modeline as Modeline
import Minichrome.UI.Components.Webview as Webview

type ChildQuery =
  NestedCoproduct.Coproduct3 Webview.Query Modeline.Query Messageline.Query
type ChildSlot = NestedEither.Either3 Unit Unit Unit

type Input = Unit

type State m = Record
  ( messageCanceler :: Maybe.Maybe (Exception.Error -> m Unit)
  | State.State
  )

data Query a
  = HandleWebview Webview.Message a
  | HandleMessageline Ex.Message a
  | ShowMessage String a
  | GoBack a
  | GoForward a
  | OpenDevTools a
  | Ex a
  | Insert a
  | Normal a
  | Navigate String a
  | ZoomIn a
  | ZoomOut a
  | ZoomDefault a
  | GetCurrentMode (InputMode.Mode -> a)
  | GetEx (Boolean -> a)

data Message = RunEx String

type DSL m = Halogen.ParentDSL (State m) Query ChildQuery ChildSlot Message
type Component = Halogen.Component HalogenHTML.HTML Query Input Message
type HTML m = Halogen.ParentHTML Query ChildQuery ChildSlot m

zoomStep :: Number
zoomStep = 0.1

webviewSlot :: forall m t. EffectClass.MonadEffect m =>
               Config.Config ->
               Record State.State ->
               State t ->
               HTML m
webviewSlot config initialState state =
  HalogenHTML.slot' ChildPath.cp1 unit component state' handler
  where
    component = Webview.webview config initialState
    state' = { address: state.address }
    handler = HalogenEvents.input HandleWebview

modelineSlot :: forall m t. EffectClass.MonadEffect m =>
                Record State.State ->
                State t ->
                HTML m
modelineSlot initialState state =
  HalogenHTML.slot' ChildPath.cp2 unit component state' absurd
  where
    component = Modeline.modeline initialState
    state' =
      { mode: state.mode
      , title: state.title
      , address: state.address
      , position: state.position
      }

messagelineSlot :: forall m t. EffectClass.MonadEffect m =>
                   Record State.State ->
                   State t ->
                   HTML m
messagelineSlot initialState state =
  HalogenHTML.slot' ChildPath.cp3 unit component state' handler
  where
    component = Messageline.messageline initialState
    state' = { message: state.message , ex: state.ex }
    handler = HalogenEvents.input HandleMessageline

render :: forall m t. EffectClass.MonadEffect m =>
          Config.Config ->
          Record State.State ->
          State t ->
          HTML m
render config initialState state =
  HalogenHTML.div
    [ HalogenCSS.style do
        CSS.height $ CSS.pct 100.0
        CSS.width $ CSS.pct 100.0
        CSS.display $ CSS.flex
        CSS.flexFlow CSS.column CSS.nowrap
    ]
    [ webviewSlot config initialState state
    , modelineSlot initialState state
    , messagelineSlot initialState state
    ]

clearMessage :: forall m t. MonadState.MonadState (State t) m => m Unit
clearMessage =
  Halogen.modify_ _{ message = "", messageCanceler = Maybe.Nothing }

-- | Cancel any existing message clear timeout
cancelMessageCanceler :: forall m. AffClass.MonadAff m => DSL m m Unit
cancelMessageCanceler = do
  messageCanceller <- Halogen.gets _.messageCanceler
  Maybe.maybe (pure unit) Halogen.lift $ messageCanceller <@> Exception.error ""

queryWebview :: forall m t a. Webview.Query a -> DSL m t (Maybe.Maybe a)
queryWebview = Halogen.query' ChildPath.cp1 unit

queryModeline :: forall m t a. Modeline.Query a -> DSL m t (Maybe.Maybe a)
queryModeline = Halogen.query' ChildPath.cp2 unit

queryMessageline :: forall m t a. Messageline.Query a -> DSL m t (Maybe.Maybe a)
queryMessageline = Halogen.query' ChildPath.cp3 unit

showMessage :: forall m. AffClass.MonadAff m => String -> DSL m m Unit
showMessage message = do
  cancelMessageCanceler
  -- Create the new message clear timeout
  canceler <- HalogenM.fork do
    Halogen.liftAff $ Aff.delay $ Duration.Milliseconds 2000.0
    clearMessage
  -- Set the message
  Halogen.modify_ _{ message = message, messageCanceler = Maybe.Just canceler }

eval :: forall m. AffClass.MonadAff m => Query ~> DSL m m
eval (HandleWebview (Webview.TitleUpdated title) next) = do
  Halogen.modify_ _{ title = title }
  pure next
eval (HandleWebview (Webview.URLUpdated url) next) = do
  Halogen.modify_ _{ address = url }
  pure next
eval (HandleWebview (Webview.ShowMessage message) next) = do
  showMessage message
  pure next
eval (HandleWebview (Webview.SetMode mode) next) = do
  Halogen.modify_ _{ mode = mode }
  pure next
eval (HandleWebview (Webview.SetScrollPosition pos) next) = do
  _ <- queryModeline $ Halogen.action $ Modeline.SetScrollPosition pos
  Halogen.modify_ _{ position = pos }
  pure next
eval (HandleMessageline Ex.UnEx next) = do
  Halogen.modify_ _{ ex = false }
  pure next
eval (HandleMessageline (Ex.RunEx cmd) next) = do
  Halogen.modify_ _{ ex = false }
  Halogen.raise $ RunEx cmd
  pure next
eval (GoBack next) = do
  _ <- queryWebview $ Halogen.action Webview.GoBack
  pure next
eval (GoForward next) = do
  _ <- queryWebview $ Halogen.action Webview.GoForward
  pure next
eval (OpenDevTools next) = do
  _ <- queryWebview $ Halogen.action Webview.OpenDevTools
  pure next
eval (Navigate command next) = do
  _ <- queryWebview $ Halogen.action $ Webview.Navigate command
  pure next
eval (ZoomIn next) = do
  zoom <- Halogen.gets _.zoomFactor
  let newZoom = zoom + zoomStep
  Halogen.modify_ _{ zoomFactor = newZoom }
  _ <- queryWebview $ Halogen.action $ Webview.SetZoom newZoom
  pure next
eval (ZoomOut next) = do
  zoom <- Halogen.gets _.zoomFactor
  let newZoom = zoom - zoomStep
  Halogen.modify_ _{ zoomFactor = newZoom }
  _ <- queryWebview $ Halogen.action $ Webview.SetZoom newZoom
  pure next
eval (ZoomDefault next) = do
  zoom <- Halogen.gets _.zoomFactor
  Halogen.modify_ _{ zoomFactor = 1.0 }
  _ <- queryWebview $ Halogen.action $ Webview.SetZoom 1.0
  pure next
eval (Ex next) = do
  cancelMessageCanceler
  clearMessage
  Halogen.modify_ _{ ex = true }
  pure next
eval (Insert next) = do
  Halogen.modify_ _{ mode = InputMode.Insert }
  pure next
eval (Normal next) = do
  let active = HTML.window >>= Window.document >>= HTMLDocument.activeElement
  Halogen.liftEffect $ active >>= Maybe.maybe (pure unit) HTMLElement.blur
  Halogen.modify_ _{ mode = InputMode.Normal }
  pure next
eval (ShowMessage message next) = do
  showMessage message
  pure next
eval (GetCurrentMode reply) = Halogen.gets _.mode >>= reply >>> pure
eval (GetEx reply) = Halogen.gets _.ex >>= reply >>> pure

page :: forall m. AffClass.MonadAff m =>
        Config.Config ->
        Record State.State ->
        Component m
page config initialState = Halogen.parentComponent
  { initialState: const
    { messageCanceler: Maybe.Nothing
    , ex: initialState.ex
    , mode: initialState.mode
    , title: initialState.title
    , address: initialState.address
    , position: initialState.position
    , message: initialState.message
    , zoomFactor: initialState.zoomFactor
    }
  , render: render config initialState
  , eval
  , receiver: const Maybe.Nothing
  }
