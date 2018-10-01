module Minichrome.Page
  ( main
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe as Maybe
import Data.Time.Duration as Duration
import Effect as Effect
import Effect.Aff as Aff
import Effect.Class as EffectClass
import Effect.Ref as Ref
import Math as Math
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
import Web.HTML as HTML
import Web.HTML.Event.EventTypes as EventTypes
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.Window as Window

import Minichrome.Command.Direction as Direction
import Minichrome.Command.InputMode as InputMode
import Minichrome.IPC.PageToUI as IPCUp
import Minichrome.IPC.UIToPage as IPCDown

foreign import addPassiveEventListener
  :: Event.EventType
  -> EventTarget.EventListener
  -> Boolean
  -> EventTarget.EventTarget
  -> Effect.Effect Unit

foreign import addOnceEventListener
  :: Event.EventType
  -> EventTarget.EventListener
  -> Boolean
  -> EventTarget.EventTarget
  -> Effect.Effect Unit

scroll :: Event.EventType
scroll = Event.EventType "scroll"

focusin :: Event.EventType
focusin = Event.EventType "focusin"

beforeunload :: Event.EventType
beforeunload = Event.EventType "beforeunload"

scrollHandlerThrottle :: Number
scrollHandlerThrottle = 50.0

intPct :: Number -> Number -> Int
intPct num denom =
  Maybe.fromMaybe 0 $ Int.fromNumber $ Math.floor $ 100.0 * num / denom

withDocumentElement :: Effect.Effect (Maybe.Maybe Element.Element)
withDocumentElement =
  HTML.window >>=
  Window.document >>=
  HTMLDocument.toDocument >>>
  Document.documentElement

withActiveElement :: Effect.Effect (Maybe.Maybe HTMLElement.HTMLElement)
withActiveElement =
  HTML.window >>=
  Window.document >>=
  HTMLDocument.activeElement

scrollHandler :: Event.Event -> Effect.Effect Unit
scrollHandler = const $
  withDocumentElement >>= Maybe.maybe mempty \documentElement -> do
    scrollPos <- Element.scrollTop documentElement
    windowHeight <- Element.clientHeight documentElement
    docHeight <- Element.scrollHeight documentElement
    let scrollPct = intPct scrollPos $ docHeight - windowHeight
    IPCUp.send $ IPCUp.SetScrollPosition scrollPct

isInsertableInput :: HTMLInputElement.HTMLInputElement -> Effect.Effect Boolean
isInsertableInput elem =
  HTMLInputElement.type_ elem <#> flip Array.elem
    [ "color"
    , "date"
    , "datetime-local"
    , "email"
    , "month"
    , "number"
    , "password"
    , "range"
    , "search"
    , "tel"
    , "text"
    , "time"
    , "url"
    , "week"
    ]

isInsertable :: Element.Element -> Effect.Effect Boolean
isInsertable elem =
  case Element.tagName elem of
    "INPUT" ->
      Maybe.maybe (pure false) isInsertableInput $
        HTMLInputElement.fromElement elem
    "TEXTAREA" -> pure true
    otherwise -> pure false

focusHandler :: Event.Event -> Effect.Effect Unit
focusHandler event = Maybe.fromMaybe mempty do
  target <- Event.target event >>= Element.fromEventTarget
  pure $ ifM (isInsertable target) (setInsertMode target) mempty
  where
    setInsertMode element = do
      IPCUp.send $ IPCUp.SetMode InputMode.Insert
      setNormalMode <- EventTarget.eventListener $ const $
        IPCUp.send $ IPCUp.SetMode InputMode.Normal
      addOnceEventListener EventTypes.blur setNormalMode false $
        Element.toEventTarget element
      HTML.window >>= Window.toEventTarget >>>
        addOnceEventListener beforeunload setNormalMode false

throttledEventListener
  :: Number
  -> Boolean
  -> (Event.Event -> Effect.Effect Unit)
  -> Effect.Effect EventTarget.EventListener
throttledEventListener delay queueEvent handler = do
  isThrottled <- Ref.new false
  queuedEvent <- Ref.new Maybe.Nothing
  EventTarget.eventListener \event ->
    ifM (Ref.read isThrottled)
      (when queueEvent $ Ref.write (Maybe.Just event) queuedEvent)
      (runHandler isThrottled queuedEvent event)
  where
    runHandler isThrottled' queuedEvent' event = do
      Ref.write true isThrottled'
      handler event
      Aff.launchAff_ $ awaitClearThrottle isThrottled' queuedEvent'
    awaitClearThrottle isThrottled' queuedEvent' = do
      Aff.delay $ Duration.Milliseconds delay
      EffectClass.liftEffect $ clearThrottle isThrottled' queuedEvent'
    clearThrottle isThrottled' queuedEvent' = do
      Ref.write false isThrottled'
      when queueEvent $ Ref.read queuedEvent' >>=
        Maybe.maybe mempty (runHandler isThrottled' queuedEvent')
      Ref.write Maybe.Nothing queuedEvent'

setupNavigation :: Effect.Effect Unit
setupNavigation = IPCDown.subscribe $ case _ of

  (IPCDown.Scroll (Direction.Up step)) ->
    HTML.window >>= Window.scrollBy 0 (-step)

  (IPCDown.Scroll (Direction.Down step)) ->
    HTML.window >>= Window.scrollBy 0 step

  (IPCDown.Scroll (Direction.Left step)) ->
    HTML.window >>= Window.scrollBy (-step) 0

  (IPCDown.Scroll (Direction.Right step)) ->
    HTML.window >>= Window.scrollBy step 0

  (IPCDown.Scroll Direction.Top) -> do
    window <- HTML.window
    currentX <- Window.scrollX window
    Window.scroll currentX 0 window

  (IPCDown.Scroll Direction.Bottom) ->
    withDocumentElement >>= Maybe.maybe mempty \documentElement -> do
      window <- HTML.window
      currentX <- Window.scrollX window
      windowHeight <- Element.clientHeight documentElement
      docHeight <- Element.scrollHeight documentElement
      Window.scroll currentX (Int.ceil $ docHeight - windowHeight) window

  IPCDown.Blur -> withActiveElement >>= Maybe.maybe mempty HTMLElement.blur

main :: Effect.Effect Unit
main = do
  onScroll <- throttledEventListener scrollHandlerThrottle true scrollHandler
  HTML.window >>= Window.toEventTarget >>>
    EventTarget.addEventListener scroll onScroll false
  onFocus <- EventTarget.eventListener focusHandler
  HTML.window >>= Window.toEventTarget >>>
    EventTarget.addEventListener focusin onFocus false
  setupNavigation
