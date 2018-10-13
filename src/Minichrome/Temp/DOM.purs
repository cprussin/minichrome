module Minichrome.Temp.DOM
  ( matches
  , scrollIntoView
  , setWindowTitle
  ) where

import Effect as Effect
import Web.DOM.Element as Element
import Web.DOM.ParentNode as ParentNode
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement

import Prelude

foreign import matches
  :: ParentNode.QuerySelector
  -> Element.Element
  -> Effect.Effect Boolean

foreign import scrollIntoView
  :: HTMLElement.HTMLElement
  -> Effect.Effect Unit

foreign import setWindowTitle
  :: HTMLDocument.HTMLDocument
  -> String
  -> Effect.Effect Unit
