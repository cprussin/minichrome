module Minichrome.WebviewScript
  ( main
  ) where

import Prelude

import Data.Int as Int
import Data.Maybe as Maybe
import Data.Time.Duration as Duration
import Effect as Effect
import Effect.Aff as Aff
import Effect.Class as EffectClass
import Effect.Ref as Ref
import Math as Math
import Node.Electron.IPCRenderer as IPCRenderer
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

scroll :: Event.EventType
scroll = Event.EventType "scroll"

scrollHandlerThrottle :: Number
scrollHandlerThrottle = 50.0

intPct :: Number -> Number -> Int
intPct num denom =
  Maybe.fromMaybe 0 $ Int.fromNumber $ Math.floor $ 100.0 * num / denom

getDocumentElement :: Effect.Effect (Maybe.Maybe Element.Element)
getDocumentElement =
  HTML.window >>=
  Window.document >>=
  HTMLDocument.toDocument >>>
  Document.documentElement

scrollHandler :: Event.Event -> Effect.Effect Unit
scrollHandler = const $
  getDocumentElement >>= Maybe.maybe mempty \documentElement -> do
    scrollPos <- Element.scrollTop documentElement
    windowHeight <- Element.clientHeight documentElement
    docHeight <- Element.scrollHeight documentElement
    let scrollPct = intPct scrollPos $ docHeight - windowHeight
    IPCRenderer.sendToHost "setScrollPosition" [show scrollPct]

foreign import addPassiveEventListener
  :: Event.EventType
  -> EventTarget.EventListener
  -> Boolean
  -> EventTarget.EventTarget
  -> Effect.Effect Unit

throttledEventListener :: Number ->
                          Boolean ->
                          (Event.Event -> Effect.Effect Unit) ->
                          Effect.Effect EventTarget.EventListener
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

main :: Effect.Effect Unit
main = do
  onScroll <- throttledEventListener scrollHandlerThrottle true scrollHandler
  HTML.window >>= Window.toEventTarget >>>
    EventTarget.addEventListener scroll onScroll false
