module Minichrome.UI.State
  ( State
  , initialState
  ) where

import Prelude

import Control.Monad.Maybe.Trans as MaybeT
import Data.Maybe as Maybe
import Effect as Effect
import Web.DOM.Element as Element
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLScriptElement as HTMLScriptElement
import Web.HTML.Window as Window

import Minichrome.UI.InputMode as InputMode

-- | This record describes the state of the UI application.
type State =
  ( mode :: InputMode.Mode
  , title :: String
  , address :: String
  , position :: String
  , message :: String
  , ex :: Boolean
  , zoomFactor :: Number
  )

-- | Get the initial URL by reading the 'data-url' attribute on the script tag.
initialURL :: Effect.Effect String
initialURL = MaybeT.runMaybeT url >>= Maybe.fromMaybe "" >>> pure
  where
    document = HTML.window >>= Window.document
    currentScript = MaybeT.MaybeT $ document >>= HTMLDocument.currentScript
    urlAttribute = Element.getAttribute "data-url" >>> MaybeT.MaybeT
    url = currentScript >>= HTMLScriptElement.toElement >>> urlAttribute

-- | This is the state to use when creating the app.
initialState :: Effect.Effect (Record State)
initialState = initialURL <#>
  { mode: InputMode.Normal
  , title: "minichrome"
  , address: _
  , position: "0"
  , message: ""
  , ex: false
  , zoomFactor: 1.0
  }
