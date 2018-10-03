module Minichrome.Page.AutoMode
  ( setup
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe as Maybe
import Data.Options ((:=))
import Effect as Effect
import Web.DOM.Element as Element
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.Event.EventTypes as EventTypes
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.Window as Window
import Web.UIEvent.FocusEvent as FocusEvent

import Minichrome.Command.InputMode as InputMode
import Minichrome.IPC.PageToUI as IPCUp
import Minichrome.Page.Temp as Temp
import Minichrome.Page.Util as Util

setup :: Effect.Effect Unit
setup = setupOnFocus

setupOnFocus :: Effect.Effect Unit
setupOnFocus = do
  target <- Window.toEventTarget <$> HTML.window
  Util.attachListener Temp.focusin (\e -> onFocus e) target $ Temp.once := true

onFocus :: Event.Event -> Effect.Effect Unit
onFocus =
  eventTarget >>> ifIsInsertable
    (\target -> toInsertMode target)
    (\_ -> setupOnFocus)

setupOnBlur :: Element.Element -> Effect.Effect Unit
setupOnBlur element =
  Util.attachListener EventTypes.blur onBlur target $ Temp.once := true
  where
    target = Element.toEventTarget element

onBlur :: Event.Event -> Effect.Effect Unit
onBlur =
  relatedFocusTarget >>> ifIsInsertable
    (\target -> setupOnBlur target)
    (\_ -> toNormalMode)

isInsertable :: Element.Element -> Effect.Effect Boolean
isInsertable elem =
  case Element.tagName elem of
    "INPUT" ->
      Maybe.maybe (pure false) isInsertableInput $
        HTMLInputElement.fromElement elem
    "TEXTAREA" -> pure true
    otherwise -> pure false

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

setMode :: InputMode.Mode -> Effect.Effect Unit
setMode = IPCUp.SetMode >>> IPCUp.send

toInsertMode :: Element.Element -> Effect.Effect Unit
toInsertMode target = do
  setMode InputMode.Insert
  setupOnBlur target

toNormalMode :: Effect.Effect Unit
toNormalMode = do
  setMode InputMode.Normal
  setupOnFocus

relatedFocusTarget :: Event.Event -> Maybe.Maybe Element.Element
relatedFocusTarget =
  FocusEvent.fromEvent >=> FocusEvent.relatedTarget >=> Element.fromEventTarget

eventTarget :: Event.Event -> Maybe.Maybe Element.Element
eventTarget = Event.target >=> Element.fromEventTarget

ifIsInsertable
  :: (Element.Element -> Effect.Effect Unit)
  -> (Unit -> Effect.Effect Unit)
  -> Maybe.Maybe Element.Element
  -> Effect.Effect Unit
ifIsInsertable insertable notInsertable =
  Maybe.maybe' notInsertable \target' ->
    ifM (isInsertable target') (insertable target') $ notInsertable unit
