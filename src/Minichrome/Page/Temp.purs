-- | This module is for stuff that shouldn't be in Minichrome and eventually
-- | I'll pull out and put in PRs elsewhere.
module Minichrome.Page.Temp
  ( scroll
  , focusin
  , EventListenerOptions
  , once
  , passive
  , capture
  , addEventListener
  , matches
  , scrollIntoView
  ) where

import Prelude

import Data.Options as Options
import Effect as Effect
import Foreign as Foreign
import Web.DOM.Element as Element
import Web.DOM.ParentNode as ParentNode
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
import Web.HTML.HTMLElement as HTMLElement

scroll :: Event.EventType
scroll = Event.EventType "scroll"

focusin :: Event.EventType
focusin = Event.EventType "focusin"

data EventListenerOptions

once :: Options.Option EventListenerOptions Boolean
once = Options.opt "once"

passive :: Options.Option EventListenerOptions Boolean
passive = Options.opt "passive"

capture :: Options.Option EventListenerOptions Boolean
capture = Options.opt "capture"

foreign import addEventListenerImpl
  :: Event.EventType
  -> EventTarget.EventListener
  -> EventTarget.EventTarget
  -> Foreign.Foreign
  -> Effect.Effect Unit

addEventListener
  :: Event.EventType
  -> EventTarget.EventListener
  -> EventTarget.EventTarget
  -> Options.Options EventListenerOptions
  -> Effect.Effect Unit
addEventListener eventType listener target =
  Options.options >>> addEventListenerImpl eventType listener target

foreign import matches
  :: ParentNode.QuerySelector
  -> Element.Element
  -> Effect.Effect Boolean

foreign import scrollIntoView
  :: HTMLElement.HTMLElement
  -> Effect.Effect Unit
