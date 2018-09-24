module Minichrome.UI.Components.Webview
  ( Query(..)
  , PageTitleUpdatedEvent
  , DidNavigateEvent
  , NewWindowEvent
  , IPCMessageEvent
  , Input
  , Message(..)
  , webview
  ) where

import Prelude

import CSS as CSS
import Data.Array ((!!))
import Data.Maybe as Maybe
import Effect.Aff as Aff
import Effect.Class as EffectClass
import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.Core as HalogenHTMLCore
import Halogen.HTML.Events as HalogenEvents
import Halogen.HTML.Properties as HalogenProperties
import Halogen.HTML.CSS as HalogenCSS
import Node.ChildProcess as ChildProcess
import Node.Electron.HTMLWebviewElement as HTMLWebviewElement
import Unsafe.Coerce as Unsafe
import Web.Event.Event as Event
import Web.UIEvent.FocusEvent as FocusEvent

import Minichrome.CLI.Client as Client
import Minichrome.Config as Config
import Minichrome.UI.State as State

type Input = Record
  ( address :: String
  )

type State = Input

data Query a
  = GoForward a
  | GoBack a
  | OpenDevTools a
  | UpdateTitle PageTitleUpdatedEvent a
  | UpdateURL DidNavigateEvent a
  | NewWindow NewWindowEvent a
  | IPCMessage IPCMessageEvent a
  | HandleInput Input a
  | Focus FocusEvent.FocusEvent a

data Message
  = TitleUpdated String
  | URLUpdated String
  | ShowMessage String
  | Insert
  | SetScrollPosition String

type DSL = Halogen.ComponentDSL State Query Message
type Component = Halogen.Component HalogenHTML.HTML Query Input Message

webviewRef :: Halogen.RefLabel
webviewRef = Halogen.RefLabel "webview"

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
               HalogenProperties.IProp (onNewWindow :: NewWindowEvent | r) i
onNewWindow = Unsafe.unsafeCoerce >>>
  HalogenEvents.handler (Event.EventType "new-window")

type IPCMessageEvent = Record ( channel :: String, args :: Array String )

onIPCMessage :: forall r i.
                (IPCMessageEvent -> Maybe.Maybe i) ->
                HalogenProperties.IProp (onIPCMessage :: IPCMessageEvent | r) i
onIPCMessage = Unsafe.unsafeCoerce >>>
  HalogenEvents.handler (Event.EventType "ipc-message")

render :: Input -> Halogen.ComponentHTML Query
render input =
  HalogenHTML.element (HalogenHTML.ElemName "webview")
    [ HalogenProperties.src input.address
    , HalogenProperties.ref webviewRef
    , HalogenEvents.onFocus $ HalogenEvents.input Focus
    , HalogenProperties.attr (HalogenHTMLCore.AttrName "preload") "./webview.js"
    , onPageTitleUpdated $ HalogenEvents.input UpdateTitle
    , onDidNavigate $ HalogenEvents.input UpdateURL
    , onDidNavigateInPage $ HalogenEvents.input UpdateURL
    , onNewWindow $ HalogenEvents.input NewWindow
    , onIPCMessage $ HalogenEvents.input IPCMessage
    , HalogenCSS.style $ CSS.flexGrow 1
    ]
    [ ]

withWebviewElement :: forall m t. EffectClass.MonadEffect m => Monoid t =>
                      (HTMLWebviewElement.HTMLWebviewElement -> DSL m t) ->
                      DSL m t
withWebviewElement cb = do
  elem <- Halogen.getHTMLElementRef webviewRef
  Maybe.maybe (Halogen.liftEffect mempty) cb $
    elem >>= HTMLWebviewElement.fromHTMLElement

eval :: forall m. EffectClass.MonadEffect m => Config.Config -> Query ~> DSL m
eval _ (GoForward next) = do
  withWebviewElement \wv ->
    whenM (EffectClass.liftEffect $ HTMLWebviewElement.canGoForward wv) do
      Halogen.raise $ ShowMessage "Going forward..."
      EffectClass.liftEffect $ HTMLWebviewElement.goForward wv
  pure next
eval _ (GoBack next) = do
  withWebviewElement \wv ->
    whenM (EffectClass.liftEffect $ HTMLWebviewElement.canGoBack wv) do
      Halogen.raise $ ShowMessage "Going back..."
      EffectClass.liftEffect $ HTMLWebviewElement.goBack wv
  pure next
eval _ (OpenDevTools next) = do
  withWebviewElement \wv -> do
    unlessM (EffectClass.liftEffect $ HTMLWebviewElement.isDevToolsOpened wv) do
      Halogen.raise $ ShowMessage "Opening dev tools..."
      EffectClass.liftEffect $ HTMLWebviewElement.openDevTools wv
  pure next
eval _ (UpdateTitle event next) = do
  Halogen.raise $ TitleUpdated event.title
  pure next
eval _ (UpdateURL event next) = do
  Halogen.raise $ URLUpdated event.url
  pure next
eval _ (Focus event next) = do
  Halogen.raise Insert
  pure next
eval config (NewWindow event next) = do
  Halogen.raise $ ShowMessage $ "Opening " <> event.url <> " in a new window..."
  EffectClass.liftEffect $ case config.browser of
    (Maybe.Just browser) -> void $
      ChildProcess.spawn browser [ event.url ] ChildProcess.defaultSpawnOptions
    Maybe.Nothing -> Aff.launchAff_ $ Client.browse config event.url
  pure next
eval config (IPCMessage event next) = do
  case event.channel of
    "setScrollPosition" ->
      Halogen.raise $ SetScrollPosition $ Maybe.fromMaybe "" $ event.args !! 0
    _ -> pure unit
  pure next
eval _ (HandleInput n next) = do
  oldN <- Halogen.get
  oldURL <- withWebviewElement $
    HTMLWebviewElement.getURL >>> EffectClass.liftEffect
  when (oldN /= n && oldURL /= n.address) $ Halogen.put n
  pure next

webview :: forall m. EffectClass.MonadEffect m =>
           Config.Config ->
           Record State.State ->
           Component m
webview config initialState = Halogen.component
  { initialState: const { address: initialState.address }
  , render
  , eval: eval config
  , receiver: HalogenEvents.input HandleInput
  }
