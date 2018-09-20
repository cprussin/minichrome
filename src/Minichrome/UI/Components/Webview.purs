module Minichrome.UI.Components.Webview
  ( Query(..)
  , PageTitleUpdatedEvent
  , DidNavigateEvent
  , NewWindowEvent
  , Input
  , Message(..)
  , webview
  ) where

import Prelude

import CSS as CSS
import Data.Maybe as Maybe
import Effect.Aff as Aff
import Effect.Class as EffectClass
import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.Events as HalogenEvents
import Halogen.HTML.Properties as HalogenProperties
import Halogen.HTML.CSS as HalogenCSS
import Node.ChildProcess as ChildProcess
import Unsafe.Coerce as Unsafe
import Web.Event.Event as Event
import Web.HTML.HTMLWebviewElement as HTMLWebviewElement

import Minichrome.CLI.Client as Client
import Minichrome.Config as Config
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
  | NewWindow NewWindowEvent a
  | HandleInput Input a

data Message = TitleUpdated String | URLUpdated String | ShowMessage String

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

type NewWindowEvent = Record ( url :: String )

onNewWindow :: forall r i.
               (NewWindowEvent -> Maybe.Maybe i) ->
               HalogenProperties.IProp
                 (onNewWindow :: NewWindowEvent | r) i
onNewWindow = Unsafe.unsafeCoerce >>>
  HalogenEvents.handler (Event.EventType "new-window")

render :: Input -> Halogen.ComponentHTML Query
render input =
  HalogenHTML.element (HalogenHTML.ElemName "webview")
    [ HalogenProperties.src input.address
    , HalogenProperties.ref $ Halogen.RefLabel "webview"
    , onPageTitleUpdated $ HalogenEvents.input UpdateTitle
    , onDidNavigate $ HalogenEvents.input UpdateURL
    , onDidNavigateInPage $ HalogenEvents.input UpdateURL
    , onNewWindow $ HalogenEvents.input NewWindow
    , HalogenCSS.style $ CSS.flexGrow 1
    ]
    [ ]

withWebviewElement ::  forall m t. EffectClass.MonadEffect m => Monoid t =>
  (HTMLWebviewElement.HTMLWebviewElement ->
   Halogen.ComponentDSL Input Query Message m t) ->
  Halogen.ComponentDSL Input Query Message m t
withWebviewElement cb = do
  elem <- Halogen.getHTMLElementRef (Halogen.RefLabel "webview")
  Maybe.maybe (Halogen.liftEffect mempty) cb $
    elem >>= HTMLWebviewElement.fromHTMLElement

eval :: forall m. EffectClass.MonadEffect m =>
        Config.Config ->
        Query ~>
        Halogen.ComponentDSL Input Query Message m
eval _ (GoForward reply) = do
  withWebviewElement \wv ->
    whenM (EffectClass.liftEffect $ HTMLWebviewElement.canGoForward wv) do
      Halogen.raise $ ShowMessage "Going forward..."
      EffectClass.liftEffect $ HTMLWebviewElement.goForward wv
  pure $ reply unit
eval _ (GoBack reply) = do
  withWebviewElement \wv ->
    whenM (EffectClass.liftEffect $ HTMLWebviewElement.canGoBack wv) do
      Halogen.raise $ ShowMessage "Going back..."
      EffectClass.liftEffect $ HTMLWebviewElement.goBack wv
  pure $ reply unit
eval _ (OpenDevTools reply) = do
  withWebviewElement \wv -> do
    unlessM (EffectClass.liftEffect $ HTMLWebviewElement.isDevToolsOpened wv) do
      Halogen.raise $ ShowMessage "Opening dev tools..."
      EffectClass.liftEffect $ HTMLWebviewElement.openDevTools wv
  pure $ reply unit
eval _ (UpdateTitle event next) = do
  Halogen.raise $ TitleUpdated event.title
  pure next
eval _ (UpdateURL event next) = do
  Halogen.raise $ URLUpdated event.url
  pure next
eval config (NewWindow event next) = do
  Halogen.raise $ ShowMessage $ "Opening " <> event.url <> " in a new window..."
  EffectClass.liftEffect $ case config.browser of
    (Maybe.Just browser) -> void $
      ChildProcess.spawn browser [ event.url ] ChildProcess.defaultSpawnOptions
    Maybe.Nothing -> Aff.launchAff_ $ Client.browse config event.url
  pure next
eval _ (HandleInput n next) = do
  oldN <- Halogen.get
  oldURL <- withWebviewElement $
    HTMLWebviewElement.getURL >>> EffectClass.liftEffect
  when (oldN /= n && oldURL /= n.address) $ Halogen.put n
  pure next

webview :: forall m. EffectClass.MonadEffect m =>
           Config.Config ->
           Halogen.Component HalogenHTML.HTML Query Input Message m
webview config = Halogen.component
  { initialState: const initialState
  , render
  , eval: eval config
  , receiver: HalogenEvents.input HandleInput
  }
