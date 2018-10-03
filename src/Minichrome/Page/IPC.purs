module Minichrome.Page.IPC
  ( setup
  ) where

import Prelude

import Data.Int as Int
import Data.Maybe as Maybe
import Effect as Effect
import Web.DOM.Element as Element
import Web.HTML as HTML
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

import Minichrome.Command.Direction as Direction
import Minichrome.IPC.UIToPage as IPCDown
import Minichrome.Page.Util as Util

setup :: Effect.Effect Unit
setup = IPCDown.subscribe $ case _ of
  (IPCDown.Scroll (Direction.Up step)) -> scrollUp step
  (IPCDown.Scroll (Direction.Down step)) -> scrollDown step
  (IPCDown.Scroll (Direction.Left step)) -> scrollLeft step
  (IPCDown.Scroll (Direction.Right step)) -> scrollRight step
  (IPCDown.Scroll Direction.Top) -> scrollToTop
  (IPCDown.Scroll Direction.Bottom) -> scrollToBottom
  IPCDown.Blur -> blurActiveElement

blurActiveElement :: Effect.Effect Unit
blurActiveElement =
  Util.getActiveElement >>= Maybe.maybe mempty HTMLElement.blur

scrollUp :: Int -> Effect.Effect Unit
scrollUp step = HTML.window >>= Window.scrollBy 0 (-step)

scrollDown :: Int -> Effect.Effect Unit
scrollDown step = HTML.window >>= Window.scrollBy 0 step

scrollLeft :: Int -> Effect.Effect Unit
scrollLeft step = HTML.window >>= Window.scrollBy (-step) 0

scrollRight :: Int -> Effect.Effect Unit
scrollRight step = HTML.window >>= Window.scrollBy step 0

scrollToTop :: Effect.Effect Unit
scrollToTop = do
  window <- HTML.window
  currentX <- Window.scrollX window
  Window.scroll currentX 0 window

scrollToBottom :: Effect.Effect Unit
scrollToBottom = Util.getDocumentElement >>= Maybe.maybe mempty \document -> do
  window <- HTML.window
  currentX <- Window.scrollX window
  windowHeight <- Element.clientHeight document
  docHeight <- Element.scrollHeight document
  Window.scroll currentX (Int.ceil $ docHeight - windowHeight) window
