module Node.Electron.WebContents
  ( WebContents
  , InputProperties
  , onNewWindow
  , beforeInputEvent
  , canGoBack
  , goBack
  , canGoForward
  , goForward
  , openDevTools
  , setDevToolsWebContents
  ) where

import Prelude

import Effect as Effect
import Node.Electron.Event as Event

-- | This type represents the web contents of a `BrowserWindow` frame.
foreign import data WebContents :: Type

-- | Attach an Effect to the 'new-window' event of a `WebContents` value.
foreign import onNewWindow ::
  WebContents ->
  (Event.Event -> String -> Effect.Effect Unit) ->
  Effect.Effect Unit

-- | These are the properties that are sent in a `before-input-event`.
type InputProperties =
  Record
    ( type :: String
    , key :: String
    , code :: String
    , isAutoRepeat :: Boolean
    , shift :: Boolean
    , control :: Boolean
    , alt :: Boolean
    , meta :: Boolean
    )

-- | Attach an Effect to the 'before-input-event' event.
foreign import beforeInputEvent ::
  WebContents ->
  (Event.Event -> InputProperties -> Effect.Effect Unit) ->
  Effect.Effect Unit

-- | True if the web contents can go back.
foreign import canGoBack :: WebContents -> Boolean

-- | Make the web contents go back.
foreign import goBack :: WebContents -> Effect.Effect Unit

-- | True if the web contents can go forward.
foreign import canGoForward :: WebContents -> Boolean

-- | Make the web contents go forward.
foreign import goForward :: WebContents -> Effect.Effect Unit

-- | Open the dev tools window for the web contents.
foreign import openDevTools :: WebContents -> Effect.Effect Unit

-- | Set the web contents target for the dev tools for the current wbe contents.
foreign import setDevToolsWebContents ::
  WebContents ->
  WebContents ->
  Effect.Effect Unit
