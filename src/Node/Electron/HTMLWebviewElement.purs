module Node.Electron.HTMLWebviewElement
  ( HTMLWebviewElement
  , fromHTMLElement
  , fromElement
  , fromNode
  , fromChildNode
  , fromNonDocumentTypeChildNode
  , fromParentNode
  , fromEventTarget
  , toHTMLElement
  , toElement
  , toNode
  , toChildNode
  , toNonDocumentTypeChildNode
  , toParentNode
  , toEventTarget
  , canGoBack
  , goBack
  , canGoForward
  , goForward
  , reload
  , reloadIgnoringCache
  , isDevToolsOpened
  , openDevTools
  , getURL
  , send
  , setZoomFactor
  , findInPage
  , StopFindInPageAction(..)
  , stopFindInPage
  ) where

import Prelude

import Data.Maybe as Maybe
import Effect as Effect
import Unsafe.Coerce as Coerce
import Web.DOM as DOM
import Web.Event.EventTarget as EventTarget
import Web.HTML.HTMLElement as HTMLElement
import Web.Internal.FFI as FFI

foreign import data HTMLWebviewElement :: Type

fromHTMLElement :: HTMLElement.HTMLElement -> Maybe.Maybe HTMLWebviewElement
fromHTMLElement = FFI.unsafeReadProtoTagged "webview"

fromElement :: DOM.Element -> Maybe.Maybe HTMLWebviewElement
fromElement = FFI.unsafeReadProtoTagged "webview"

fromNode :: DOM.Node -> Maybe.Maybe HTMLWebviewElement
fromNode = FFI.unsafeReadProtoTagged "webview"

fromChildNode :: DOM.ChildNode -> Maybe.Maybe HTMLWebviewElement
fromChildNode = FFI.unsafeReadProtoTagged "webview"

fromNonDocumentTypeChildNode :: DOM.NonDocumentTypeChildNode ->
                                Maybe.Maybe HTMLWebviewElement
fromNonDocumentTypeChildNode = FFI.unsafeReadProtoTagged "webview"

fromParentNode :: DOM.ParentNode -> Maybe.Maybe HTMLWebviewElement
fromParentNode = FFI.unsafeReadProtoTagged "webview"

fromEventTarget :: EventTarget.EventTarget -> Maybe.Maybe HTMLWebviewElement
fromEventTarget = FFI.unsafeReadProtoTagged "webview"

toHTMLElement :: HTMLWebviewElement -> HTMLElement.HTMLElement
toHTMLElement = Coerce.unsafeCoerce

toElement :: HTMLWebviewElement -> DOM.Element
toElement = Coerce.unsafeCoerce

toNode :: HTMLWebviewElement -> DOM.Node
toNode = Coerce.unsafeCoerce

toChildNode :: HTMLWebviewElement -> DOM.ChildNode
toChildNode = Coerce.unsafeCoerce

toNonDocumentTypeChildNode :: HTMLWebviewElement -> DOM.NonDocumentTypeChildNode
toNonDocumentTypeChildNode = Coerce.unsafeCoerce

toParentNode :: HTMLWebviewElement -> DOM.ParentNode
toParentNode = Coerce.unsafeCoerce

toEventTarget :: HTMLWebviewElement -> EventTarget.EventTarget
toEventTarget = Coerce.unsafeCoerce

foreign import canGoBack :: HTMLWebviewElement -> Effect.Effect Boolean
foreign import goBack :: HTMLWebviewElement -> Effect.Effect Unit
foreign import canGoForward :: HTMLWebviewElement -> Effect.Effect Boolean
foreign import goForward :: HTMLWebviewElement -> Effect.Effect Unit
foreign import reload :: HTMLWebviewElement -> Effect.Effect Unit
foreign import reloadIgnoringCache :: HTMLWebviewElement -> Effect.Effect Unit
foreign import isDevToolsOpened :: HTMLWebviewElement -> Effect.Effect Boolean
foreign import openDevTools :: HTMLWebviewElement -> Effect.Effect Unit
foreign import getURL :: HTMLWebviewElement -> Effect.Effect String

foreign import send
  :: HTMLWebviewElement
  -> String
  -> Array String
  -> Effect.Effect Unit

foreign import setZoomFactor
  :: HTMLWebviewElement
  -> Number
  -> Effect.Effect Unit

foreign import findInPage
  :: HTMLWebviewElement
  -> String
  -> Boolean
  -> Boolean
  -> Effect.Effect Unit

foreign import stopFindInPageImpl
  :: HTMLWebviewElement
  -> String
  -> Effect.Effect Unit

data StopFindInPageAction = ClearSelection | KeepSelection | ActivateSelection

instance showStopFindInPageAction :: Show StopFindInPageAction where
  show ClearSelection = "clearSelection"
  show KeepSelection = "keepSelection"
  show ActivateSelection = "activateSelection"

stopFindInPage
  :: HTMLWebviewElement
  -> StopFindInPageAction
  -> Effect.Effect Unit
stopFindInPage elem = show >>> stopFindInPageImpl elem
