module Minichrome.Temp.Event
  ( scroll
  , focusin
  , keypress
  , EventListenerOptions
  , once
  , passive
  , capture
  , addEventListener
  ) where

import Prelude

import Data.Options as Options
import Effect as Effect
import Foreign as Foreign
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget

scroll :: Event.EventType
scroll = Event.EventType "scroll"

focusin :: Event.EventType
focusin = Event.EventType "focusin"

keypress :: Event.EventType
keypress = Event.EventType "keypress"

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
