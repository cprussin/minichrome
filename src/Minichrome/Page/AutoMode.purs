module Minichrome.Page.AutoMode
  ( focusNextInsertable
  , installOnFocusHandler
  ) where

import Prelude

import Data.Maybe as Maybe
import Data.Options ((:=))
import Effect as Effect
import Web.DOM.Element as Element
import Web.DOM.NodeList as NodeList
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.Event.EventTypes as EventTypes
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

import Minichrome.Command.InputMode as InputMode
import Minichrome.IPC.PageToUI as IPCUp
import Minichrome.Page.Temp as Temp
import Minichrome.Page.Util as Util

focusNextInsertable :: Effect.Effect Unit
focusNextInsertable =
  Util.getActiveElement >>= Maybe.maybe moveFocus \elem ->
    unlessM (Util.isInsertable $ HTMLElement.toElement elem) moveFocus
  where
    moveFocus = Util.withAllInsertableElements $
      NodeList.toArray >=> firstVisible >=> Maybe.maybe mempty HTMLElement.focus
    firstVisible = Util.findMapM $ HTMLElement.fromNode >>>
      Maybe.maybe (pure Maybe.Nothing) (Util.maybeBoolM Util.isVisible)

installOnFocusHandler :: Effect.Effect Unit
installOnFocusHandler = do
  target <- Window.toEventTarget <$> HTML.window
  Util.attachListener Temp.focusin (\e -> onFocus e) target $ Temp.once := true

installOnBlurHandler :: Element.Element -> Effect.Effect Unit
installOnBlurHandler element =
  Util.attachListener EventTypes.blur onBlur target $ Temp.once := true
  where
    target = Element.toEventTarget element

onFocus :: Event.Event -> Effect.Effect Unit
onFocus =
  Util.eventTarget >>> ifIsInsertable
    (\target -> toInsertMode target)
    (\_ -> installOnFocusHandler)

onBlur :: Event.Event -> Effect.Effect Unit
onBlur =
  Util.relatedFocusTarget >>> ifIsInsertable
    (\target -> installOnBlurHandler target)
    (\_ -> toNormalMode)

setMode :: InputMode.Mode -> Effect.Effect Unit
setMode = IPCUp.SetMode >>> IPCUp.send

toInsertMode :: Element.Element -> Effect.Effect Unit
toInsertMode target = do
  setMode InputMode.Insert
  installOnBlurHandler target

toNormalMode :: Effect.Effect Unit
toNormalMode = do
  setMode InputMode.Normal
  installOnFocusHandler

ifIsInsertable
  :: (Element.Element -> Effect.Effect Unit)
  -> (Unit -> Effect.Effect Unit)
  -> Maybe.Maybe Element.Element
  -> Effect.Effect Unit
ifIsInsertable insertable notInsertable =
  Maybe.maybe' notInsertable \target' ->
    ifM (Util.isInsertable target') (insertable target') $ notInsertable unit
