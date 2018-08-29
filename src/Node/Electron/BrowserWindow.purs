module Node.Electron.BrowserWindow
  ( BrowserWindow
  , BrowserWindowOptions
  , createBrowserWindow
  , setMenu
  , loadURL
  ) where

import Prelude

import Data.Maybe as Maybe
import Effect as Effect
import Node.Electron.Menu as Menu
import Node.Electron.WebContents as WebContents

-- | A type representing an Electron frame.
type BrowserWindow = Record (webContents :: WebContents.WebContents, id :: Int)

-- | A type containing the valid options that can be passed when creating a
-- | `BrowserWindow`.
-- | TODO This should probably be an `Options` type instead.
type BrowserWindowOptions = Record (width :: Int, height :: Int)

-- | Given a `Record` of `BrowserWindowOptions`, return an `Effect` containing a
-- | new `BrowserWindow`.
foreign import createBrowserWindow ::
  BrowserWindowOptions ->
  Effect.Effect BrowserWindow

-- | Set the frame's menu to a given `Menu` object.
foreign import setMenuImpl :: BrowserWindow -> Menu.Menu -> Effect.Effect Unit

-- | Remove the frame's menu.
foreign import clearMenu :: BrowserWindow -> Effect.Effect Unit

-- | Point the frame at the given URL.
foreign import loadURL :: BrowserWindow -> String -> Effect.Effect Unit

-- | Set or clear the frame's menu, depending on if the argument is `Just` or
-- | `Nothing`.
setMenu :: BrowserWindow -> Maybe.Maybe Menu.Menu -> Effect.Effect Unit
setMenu window (Maybe.Just menu) = setMenuImpl window menu
setMenu window Maybe.Nothing = clearMenu window
