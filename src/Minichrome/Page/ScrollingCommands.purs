module Minichrome.Page.ScrollingCommands
  ( up
  , down
  , left
  , right
  , toTop
  , toBottom
  ) where

import Prelude

import Data.Int as Int
import Data.Maybe as Maybe
import Effect as Effect
import Web.DOM.Element as Element
import Web.HTML as HTML
import Web.HTML.Window as Window

import Minichrome.Page.Util as Util

up :: Int -> Effect.Effect Unit
up step = HTML.window >>= Window.scrollBy 0 (-step)

down :: Int -> Effect.Effect Unit
down step = HTML.window >>= Window.scrollBy 0 step

left :: Int -> Effect.Effect Unit
left step = HTML.window >>= Window.scrollBy (-step) 0

right :: Int -> Effect.Effect Unit
right step = HTML.window >>= Window.scrollBy step 0

toTop :: Effect.Effect Unit
toTop = do
  window <- HTML.window
  currentX <- Window.scrollX window
  Window.scroll currentX 0 window

toBottom :: Effect.Effect Unit
toBottom = Util.getDocumentElement >>= Maybe.maybe mempty \document -> do
  window <- HTML.window
  currentX <- Window.scrollX window
  windowHeight <- Element.clientHeight document
  docHeight <- Element.scrollHeight document
  Window.scroll currentX (Int.ceil $ docHeight - windowHeight) window
