module Minichrome.Page.Util
  ( getDocumentElement
  , getActiveElement
  , throttle
  , attachListener
  ) where

import Prelude

import Data.Maybe as Maybe
import Data.Options as Options
import Data.Time.Duration as Duration
import Effect as Effect
import Effect.Aff as Aff
import Effect.Class as EffectClass
import Effect.Ref as Ref
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

import Minichrome.Page.Temp as Temp

getDocumentElement :: Effect.Effect (Maybe.Maybe Element.Element)
getDocumentElement =
  HTML.window >>=
  Window.document >>=
  HTMLDocument.toDocument >>>
  Document.documentElement

getActiveElement :: Effect.Effect (Maybe.Maybe HTMLElement.HTMLElement)
getActiveElement =
  HTML.window >>=
  Window.document >>=
  HTMLDocument.activeElement

throttle
  :: Number
  -> Boolean
  -> (Event.Event -> Effect.Effect Unit)
  -> Effect.Effect (Event.Event -> Effect.Effect Unit)
throttle delay queueEvent handler = do
  isThrottled <- Ref.new false
  queuedEvent <- Ref.new Maybe.Nothing
  pure \event ->
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

attachListener
  :: Event.EventType
  -> (Event.Event -> Effect.Effect Unit)
  -> EventTarget.EventTarget
  -> Options.Options Temp.EventListenerOptions
  -> Effect.Effect Unit
attachListener eventType handler target opts = do
  listener <- EventTarget.eventListener handler
  Temp.addEventListener eventType listener target opts
