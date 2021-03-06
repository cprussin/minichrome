module Minichrome.Page.ScrollingPosition
  ( installScrollHandler
  ) where

import Prelude

import Data.Int as Int
import Data.Maybe as Maybe
import Data.Options ((:=))
import Effect as Effect
import Math as Math
import Web.DOM.Element as Element
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.Window as Window

import Minichrome.IPC.PageToUI as IPCUp
import Minichrome.Page.Util as Util
import Minichrome.Temp.Event as TEvent

scrollHandlerThrottle :: Number
scrollHandlerThrottle = 50.0

installScrollHandler :: Effect.Effect Unit
installScrollHandler = do
  target <- Window.toEventTarget <$> HTML.window
  listener <- Util.throttle scrollHandlerThrottle true onScroll
  Util.attachListener TEvent.scroll listener target $ TEvent.passive := true

onScroll :: Event.Event -> Effect.Effect Unit
onScroll = const $
  Util.getDocumentElement >>= Maybe.maybe mempty \documentElement -> do
    scrollPos <- Element.scrollTop documentElement
    windowHeight <- Element.clientHeight documentElement
    docHeight <- Element.scrollHeight documentElement
    let scrollPct = intPct scrollPos $ docHeight - windowHeight
    IPCUp.send $ IPCUp.SetScrollPosition scrollPct

intPct :: Number -> Number -> Int
intPct num denom =
  Maybe.fromMaybe 0 $ Int.fromNumber $ Math.floor $ 100.0 * num / denom
