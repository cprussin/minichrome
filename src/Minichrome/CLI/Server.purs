module Minichrome.CLI.Server
  ( start
  ) where

import Prelude

import Control.Monad.Maybe.Trans as MaybeT
import Data.Maybe as Maybe
import Data.Options ((:=))
import Effect as Effect
import Effect.Class as EffectClass
import Effect.Console as Console
import Global.Unsafe as Unsafe
import HTTPure as HTTPure
import Node.Electron.App as App
import Node.Electron.BrowserWindow as BrowserWindow
import Node.Electron.WebContents as WebContents
import Node.Path as Path

import Minichrome.Config as Config
import Minichrome.IPC.CLIToUI as IPC
import Minichrome.Temp.NodeModule as NodeModule

-- | The data URL encoding the page contents for the window.
pageDataURL :: String -> String
pageDataURL url = "data:text/html;charset=UTF-8," <> pageData
  where
    pageData = Unsafe.unsafeEncodeURIComponent $
      "<!DOCTYPE html>" <>
      "<html style='width: 100%; height: 100%; margin: 0;'>" <>
        "<head>" <>
          "<meta charset='UTF-8' />" <>
        "</head>" <>
        "<body style='width: 100%; height: 100%; margin: 0;'>" <>
          "<script data-url='"<> url <>"'>" <>
            "require(__filename + '/ui.js');" <>
          "</script>" <>
        "</body>" <>
      "</html>"

baseURL :: String
baseURL = "file://" <> Path.dirname NodeModule.main.filename <> "/"

-- | Open a new Minichrome window with the given config and URL.
openWindow :: Config.Config -> String -> Effect.Effect Unit
openWindow config url = do
  window <- BrowserWindow.createBrowserWindow $ BrowserWindow.showOpt := false
  BrowserWindow.setMenu window Maybe.Nothing
  when config.developerMode $ WebContents.openDevTools window.webContents
  BrowserWindow.loadURL window (pageDataURL url) $
    BrowserWindow.baseURLForDataURL := baseURL
  BrowserWindow.onceReadyToShow window $ BrowserWindow.show window

-- | Handle HTTP requests to open a new window to a URL given by the HTTP body.
browse :: Config.Config -> String -> HTTPure.ResponseM
browse config url = do
  EffectClass.liftEffect do
    Console.log $ "Opening new window to " <> url
    openWindow config url
  HTTPure.ok $ "Created new window to " <> url

-- | Handle HTTP requests to execute a command in the currently focused window.
exec :: Config.Config -> String -> HTTPure.ResponseM
exec config cmd = exec' >>= Maybe.maybe noCurrentWindow success
  where
    exec' = EffectClass.liftEffect $ MaybeT.runMaybeT do
      MaybeT.lift $ Console.log $ "Running command in current window: " <> cmd
      window <- BrowserWindow.focusedWindow
      MaybeT.lift $ IPC.send window.webContents $ IPC.Exec cmd
    noCurrentWindow = do
      EffectClass.liftEffect $ Console.log "No window available!"
      HTTPure.serviceUnavailable
    success = const $ HTTPure.ok $ "Ran command in current window: " <> cmd

-- | Handle bad HTTP requests.
badRequest :: HTTPure.Request -> HTTPure.ResponseM
badRequest request = do
  EffectClass.liftEffect $ Console.log $ show request
  HTTPure.notFound

-- | Route inbound HTTP requests to the appropriate method.
router :: Config.Config -> HTTPure.Request -> HTTPure.ResponseM
router config request@{ method, path, body } =
  case method, path of
    HTTPure.Post, [ "browse" ] -> browse config body
    HTTPure.Post, [ "exec" ] -> exec config body
    _, _ -> badRequest request

-- | Start the Electron app and HTTP server.
start :: Config.Config -> HTTPure.ServerM
start config = App.onReady do
  App.onWindowAllClosed mempty
  HTTPure.serve config.port (router config) $
    Console.log $ "Minichrome server started!"
