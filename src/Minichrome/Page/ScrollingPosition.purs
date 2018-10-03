module Minichrome.Page.ScrollingPosition
  ( setup
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
import Minichrome.Page.Temp as Temp

scrollHandlerThrottle :: Number
scrollHandlerThrottle = 50.0

setup :: Effect.Effect Unit
setup = do
  target <- Window.toEventTarget <$> HTML.window
  listener <- Util.throttle scrollHandlerThrottle true onScroll
  Util.attachListener Temp.scroll listener target $ Temp.passive := true

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
