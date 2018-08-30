module Minichrome.Actions
  ( Action
  , goBack
  , goForward
  , openDevTools
  ) where

import Prelude

import Data.Maybe as Maybe
import Effect as Effect
import Node.Electron.BrowserWindow as BrowserWindow
import Node.Electron.WebContents as WebContents

type Action = BrowserWindow.BrowserWindow -> Effect.Effect Unit

-- | If the window has a back state available, go to it.
goBack :: Action
goBack window =
  if WebContents.canGoBack window.webContents
    then WebContents.goBack window.webContents
    else pure unit

-- | If the window has a forward state available, go to it.
goForward :: Action
goForward window =
  if WebContents.canGoForward window.webContents
    then WebContents.goForward window.webContents
    else pure unit

-- | Show the web inspector.
openDevTools :: Action
openDevTools window = do
  devTools <- BrowserWindow.createBrowserWindow { width: 800, height: 600 }
  BrowserWindow.setMenu devTools Maybe.Nothing
  WebContents.setDevToolsWebContents window.webContents devTools.webContents
  WebContents.openDevTools window.webContents
