module Node.Electron.BrowserWindow
  ( BrowserWindow
  , BrowserWindowOptions
  , width
  , height
  , showOpt
  , backgroundColor
  , createBrowserWindow
  , setMenu
  , LoadURLOptions
  , baseURLForDataURL
  , loadURL
  , loadFile
  , onceReadyToShow
  , show
  , onShow
  , focusedWindow
  ) where

import Prelude

import Control.Monad.Maybe.Trans as MaybeT
import Data.Maybe as Maybe
import Data.Options as Options
import Effect as Effect
import Node.Electron.Menu as Menu
import Node.Electron.WebContents as WebContents
import Foreign as Foreign

-- | A type representing an Electron frame.
type BrowserWindow = Record (webContents :: WebContents.WebContents, id :: Int)

-- | A type containing the valid options that can be passed when creating a
-- | `BrowserWindow`.
data BrowserWindowOptions

-- | The width in pixels of the new window.
width :: Options.Option BrowserWindowOptions Int
width = Options.opt "width"

-- | The height in pixels of the new window.
height :: Options.Option BrowserWindowOptions Int
height = Options.opt "height"

-- | True if the new window should show immediately, false otherwise.
showOpt :: Options.Option BrowserWindowOptions Boolean
showOpt = Options.opt "show"

-- | The background color of the new window.
backgroundColor :: Options.Option BrowserWindowOptions String
backgroundColor = Options.opt "backgroundColor"

-- | Return an `Effect` containing a new `BrowserWindow`.
foreign import createBrowserWindowImpl ::
  Foreign.Foreign ->
  Effect.Effect BrowserWindow

-- | Given a `BrowserWindowOptions`, return an `Effect` containing a new
-- | `BrowserWindow`.
createBrowserWindow ::
  Options.Options BrowserWindowOptions ->
  Effect.Effect BrowserWindow
createBrowserWindow = Options.options >>> createBrowserWindowImpl

-- | Set the frame's menu to a given `Menu` object.
foreign import setMenuImpl :: BrowserWindow -> Menu.Menu -> Effect.Effect Unit

-- | Remove the frame's menu.
foreign import clearMenu :: BrowserWindow -> Effect.Effect Unit

-- | A type for options that can be passed while calling `loadURL`.
data LoadURLOptions

-- | The base URL for external assets when using `loadURL` with a data URL.
baseURLForDataURL :: Options.Option LoadURLOptions String
baseURLForDataURL = Options.opt "baseURLForDataURL"

-- | Point the frame at the given URL.
foreign import loadURLImpl ::
  BrowserWindow ->
  String ->
  Foreign.Foreign ->
  Effect.Effect Unit

loadURL :: BrowserWindow ->
           String ->
           Options.Options LoadURLOptions ->
           Effect.Effect Unit
loadURL window url = Options.options >>> loadURLImpl window url

-- | Point the frame at the given file.
foreign import loadFile :: BrowserWindow -> String -> Effect.Effect Unit

-- | Set or clear the frame's menu, depending on if the argument is `Just` or
-- | `Nothing`.
setMenu :: BrowserWindow -> Maybe.Maybe Menu.Menu -> Effect.Effect Unit
setMenu window (Maybe.Just menu) = setMenuImpl window menu
setMenu window Maybe.Nothing = clearMenu window

-- | Attach an effect to the browser window's `ready-to-show` event.
foreign import onceReadyToShow ::
  BrowserWindow ->
  Effect.Effect Unit ->
  Effect.Effect Unit

-- | Show a window that's been hidden.
foreign import show :: BrowserWindow -> Effect.Effect Unit

-- | Attach an effect to the window's `show` event
foreign import onShow ::
  BrowserWindow ->
  Effect.Effect Unit ->
  Effect.Effect Unit

-- | Return the currently focused window.
foreign import focusedWindowImpl ::
  (BrowserWindow -> Maybe.Maybe BrowserWindow) ->
  (Maybe.Maybe BrowserWindow) ->
  Effect.Effect (Maybe.Maybe BrowserWindow)

-- | Return the currently focused window.
focusedWindow :: MaybeT.MaybeT Effect.Effect BrowserWindow
focusedWindow = MaybeT.MaybeT $ focusedWindowImpl Maybe.Just Maybe.Nothing
