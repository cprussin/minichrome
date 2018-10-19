module Minichrome.Page.ModeDetection
  ( installHandlers
  ) where

import Prelude

import Data.Maybe as Maybe
import Data.Options ((:=))
import Effect as Effect
import Web.DOM.Element as Element
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.Event.EventTypes as EventTypes
import Web.HTML.Window as Window

import Minichrome.Command.InputMode as InputMode
import Minichrome.IPC.PageToUI as IPCUp
import Minichrome.Page.Util as Util
import Minichrome.Temp.Event as TEvent

installHandlers :: Effect.Effect Unit
installHandlers = do
  installOnNavigateHandler
  installOnFocusHandler

installOnNavigateHandler :: Effect.Effect Unit
installOnNavigateHandler = do
  target <- Window.toEventTarget <$> HTML.window
  Util.attachListener EventTypes.unload clearMode target $ TEvent.once := true
  where
    clearMode = const $ setMode InputMode.Normal

installOnFocusHandler :: Effect.Effect Unit
installOnFocusHandler = do
  target <- Window.toEventTarget <$> HTML.window
  Util.attachListener TEvent.focusin (\e -> onFocus e) target $
    TEvent.once := true

installOnBlurHandler :: Element.Element -> Effect.Effect Unit
installOnBlurHandler element =
  Util.attachListener EventTypes.blur onBlur target $ TEvent.once := true
  where
    target = Element.toEventTarget element

onFocus :: Event.Event -> Effect.Effect Unit
onFocus =
  Util.eventTarget >>> unlessIsNormal
    (\target -> toModeFor target)
    (\_ -> installOnFocusHandler)

onBlur :: Event.Event -> Effect.Effect Unit
onBlur =
  Util.relatedFocusTarget >>> unlessIsNormal
    (\target -> do
      InputMode.modeFor target >>= setMode
      installOnBlurHandler target
    )
    (\_ -> toNormalMode)

setMode :: InputMode.Mode -> Effect.Effect Unit
setMode = IPCUp.SetMode >>> IPCUp.send

toModeFor :: Element.Element -> Effect.Effect Unit
toModeFor target = do
  InputMode.modeFor target >>= setMode
  installOnBlurHandler target

toNormalMode :: Effect.Effect Unit
toNormalMode = do
  setMode InputMode.Normal
  installOnFocusHandler

unlessIsNormal
  :: (Element.Element -> Effect.Effect Unit)
  -> (Unit -> Effect.Effect Unit)
  -> Maybe.Maybe Element.Element
  -> Effect.Effect Unit
unlessIsNormal notNormal normal =
  Maybe.maybe' normal \target' -> do
    mode <- InputMode.modeFor target'
    if mode == InputMode.Normal
       then normal unit
       else notNormal target'
