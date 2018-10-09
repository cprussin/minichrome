module Minichrome.Page.Util
  ( attachListener
  , eventTarget
  , findM
  , findMapM
  , getActiveElement
  , getDocumentElement
  , isInsertable
  , isVisible
  , maybeBoolM
  , relatedFocusTarget
  , throttle
  , withAllInsertableElements
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable as Foldable
import Data.Maybe as Maybe
import Data.Options as Options
import Data.String as String
import Data.Time.Duration as Duration
import Effect as Effect
import Effect.Aff as Aff
import Effect.Class as EffectClass
import Effect.Ref as Ref
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode as ParentNode
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window
import Web.UIEvent.FocusEvent as FocusEvent

import Minichrome.Page.Temp as Temp

insertableInputTypes :: Array String
insertableInputTypes =
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

insertableSelector :: ParentNode.QuerySelector
insertableSelector = ParentNode.QuerySelector $ String.joinWith "," selectors
  where
    makeInputSelector inputType = "input[type=" <> inputType <> "]"
    inputSelectors = makeInputSelector <$> insertableInputTypes
    otherSelectors = [ "textarea", "*[contenteditable=true]" ]
    selectors = Array.concat [ otherSelectors, inputSelectors ]

isInsertable :: Element.Element -> Effect.Effect Boolean
isInsertable = Temp.matches insertableSelector

isVisible :: HTMLElement.HTMLElement -> Effect.Effect Boolean
isVisible element = do
  offsetWidth <- HTMLElement.offsetWidth element
  offsetHeight <- HTMLElement.offsetHeight element
  pure $ offsetWidth > 0.0 && offsetHeight > 0.0

withAllInsertableElements
  :: (NodeList.NodeList -> Effect.Effect Unit)
  -> Effect.Effect Unit
withAllInsertableElements = bind $
  HTML.window >>=
  Window.document >>=
  HTMLDocument.toParentNode >>>
  ParentNode.querySelectorAll insertableSelector

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

relatedFocusTarget :: Event.Event -> Maybe.Maybe Element.Element
relatedFocusTarget =
  FocusEvent.fromEvent >=> FocusEvent.relatedTarget >=> Element.fromEventTarget

eventTarget :: Event.Event -> Maybe.Maybe Element.Element
eventTarget = Event.target >=> Element.fromEventTarget

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

maybeBoolM :: forall a m. Monad m => (a -> m Boolean) -> a -> m (Maybe.Maybe a)
maybeBoolM predicate item = do
  match <- predicate item
  pure $ if match then Maybe.Just item else Maybe.Nothing

findM
  :: forall a m f
   . Monad m
  => Foldable.Foldable f
  => (a -> m Boolean)
  -> f a
  -> m (Maybe.Maybe a)
findM = maybeBoolM >>> findMapM

findMapM
  :: forall a b m f
   . Monad m
  => Foldable.Foldable f
  => (a -> m (Maybe.Maybe b))
  -> f a
  -> m (Maybe.Maybe b)
findMapM predicate = Foldable.foldM match Maybe.Nothing
  where
    match Maybe.Nothing = predicate
    match found = const $ pure found
