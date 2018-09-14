module Minichrome.UI.Components.Webview
  ( Query(..)
  , PageTitleUpdatedEvent
  , DidNavigateEvent
  , Input
  , Message(..)
  , webview
  ) where

import Prelude

import CSS as CSS
import Data.Maybe as Maybe
import Effect as Effect
import Effect.Class as EffectClass
import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.Events as HalogenEvents
import Halogen.HTML.Properties as HalogenProperties
import Halogen.HTML.CSS as HalogenCSS
import Unsafe.Coerce as Unsafe
import Web.Event.Event as Event
import Web.HTML.HTMLWebviewElement as HTMLWebviewElement

import Minichrome.UI.State as State

type Input = Record
  ( address :: String
  )

data Query a
  = GoForward (Unit -> a)
  | GoBack (Unit -> a)
  | OpenDevTools (Unit -> a)
  | UpdateTitle PageTitleUpdatedEvent a
  | UpdateURL DidNavigateEvent a
  | HandleInput Input a

data Message = TitleUpdated String | URLUpdated String

initialState :: Input
initialState =
  { address: State.initialState.address
  }

type PageTitleUpdatedEvent = Record ( title :: String )

onPageTitleUpdated :: forall r i.
                      (PageTitleUpdatedEvent -> Maybe.Maybe i) ->
                      HalogenProperties.IProp
                        (onPageTitleUpdated :: PageTitleUpdatedEvent | r) i
onPageTitleUpdated = Unsafe.unsafeCoerce >>>
  HalogenEvents.handler (Event.EventType "page-title-updated")

type DidNavigateEvent = Record ( url :: String )

onDidNavigate :: forall r i.
                 (DidNavigateEvent -> Maybe.Maybe i) ->
                 HalogenProperties.IProp
                   (onDidNavigate :: DidNavigateEvent | r) i
onDidNavigate = Unsafe.unsafeCoerce >>>
  HalogenEvents.handler (Event.EventType "did-navigate")

onDidNavigateInPage :: forall r i.
                       (DidNavigateEvent -> Maybe.Maybe i) ->
                       HalogenProperties.IProp
                         (onDidNavigate :: DidNavigateEvent | r) i
onDidNavigateInPage = Unsafe.unsafeCoerce >>>
  HalogenEvents.handler (Event.EventType "did-navigate-in-page")

render :: Input -> Halogen.ComponentHTML Query
render input =
  HalogenHTML.element (HalogenHTML.ElemName "webview")
    [ HalogenProperties.src input.address
    , HalogenProperties.ref $ Halogen.RefLabel "webview"
    , onPageTitleUpdated $ HalogenEvents.input UpdateTitle
    , onDidNavigate $ HalogenEvents.input UpdateURL
    , onDidNavigateInPage $ HalogenEvents.input UpdateURL
    , HalogenCSS.style $ CSS.flexGrow 1
    ]
    [ ]

withWebviewElement ::  forall m t. EffectClass.MonadEffect m => Monoid t =>
  (HTMLWebviewElement.HTMLWebviewElement -> Effect.Effect t) ->
  Halogen.ComponentDSL Input Query Message m t
withWebviewElement cb = do
  elem <- Halogen.getHTMLElementRef (Halogen.RefLabel "webview")
  EffectClass.liftEffect $ Maybe.maybe mempty cb $
     elem >>= HTMLWebviewElement.fromHTMLElement

eval :: forall m. EffectClass.MonadEffect m =>
        Query ~>
        Halogen.ComponentDSL Input Query Message m
eval (GoForward reply) = do
  withWebviewElement \wv ->
    whenM (HTMLWebviewElement.canGoForward wv) $ HTMLWebviewElement.goForward wv
  pure $ reply unit
eval (GoBack reply) = do
  withWebviewElement \wv ->
    whenM (HTMLWebviewElement.canGoBack wv) $ HTMLWebviewElement.goBack wv
  pure $ reply unit
eval (OpenDevTools reply) = do
  withWebviewElement HTMLWebviewElement.openDevTools
  pure $ reply unit
eval (UpdateTitle event next) = do
  Halogen.raise $ TitleUpdated event.title
  pure next
eval (UpdateURL event next) = do
  Halogen.raise $ URLUpdated event.url
  pure next
eval (HandleInput n next) = do
  oldN <- Halogen.get
  oldURL <- withWebviewElement \wv -> HTMLWebviewElement.getURL wv
  when (oldN /= n && oldURL /= n.address) $ Halogen.put n
  pure next

webview :: forall m. EffectClass.MonadEffect m =>
           Halogen.Component HalogenHTML.HTML Query Input Message m
webview = Halogen.component
  { initialState: const initialState
  , render
  , eval
  , receiver: HalogenEvents.input HandleInput
  }
