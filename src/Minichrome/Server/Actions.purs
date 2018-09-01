module Minichrome.Server.Actions
  ( Action
  , goBack
  , goForward
  , openDevTools
  ) where

import Prelude

import Data.Maybe as Maybe
import Data.Options as Options
import Effect as Effect
import Node.Electron.BrowserWindow as BrowserWindow
import Node.Electron.WebContents as WebContents

type Action = Effect.Effect Unit

-- | If the window has a back state available, go to it.
goBack :: Action
goBack = do
  focusedContents <- WebContents.focusedWebContents
  case focusedContents of
    (Maybe.Just contents) -> do
      if WebContents.canGoBack contents
        then WebContents.goBack contents
        else pure unit
    _ -> pure unit

-- | If the window has a forward state available, go to it.
goForward :: Action
goForward = do
  focusedContents <- WebContents.focusedWebContents
  case focusedContents of
    (Maybe.Just contents) -> do
      if WebContents.canGoForward contents
        then WebContents.goForward contents
        else pure unit
    _ -> pure unit

-- | Show the web inspector.
openDevTools :: Action
openDevTools = do
  focusedContents <- WebContents.focusedWebContents
  case focusedContents of
    (Maybe.Just contents) -> do
      devTools <- BrowserWindow.createBrowserWindow $ Options.Options []
      BrowserWindow.setMenu devTools Maybe.Nothing
      WebContents.setDevToolsWebContents contents devTools.webContents
      WebContents.openDevTools contents
    _ -> pure unit
