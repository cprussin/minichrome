module Minichrome.CLI.Server
  ( start
  ) where

import Prelude

import Data.Maybe as Maybe
import Data.Options ((:=))
import Effect as Effect
import Effect.Class as EffectClass
import Effect.Console as Console
import Global.Unsafe as Unsafe
import HTTPure as HTTPure
import Node.ChildProcess as ChildProcess
import Node.Electron.App as App
import Node.Electron.BrowserWindow as BrowserWindow
import Node.Electron.Event as Event
import Node.Electron.WebContents as WebContents
import Node.Globals as Globals

import Minichrome.Config as Config

-- | Run a new external browser process.  Used if the configuration specifies a
-- | `browser` property other than `Maybe.Nothing`.
runBrowser :: String -> String -> Effect.Effect Unit
runBrowser browser url =
  void $ ChildProcess.spawn browser [ url ] ChildProcess.defaultSpawnOptions

-- | Handle links that should be opened in a new window, e.g. by middle click.
openNewWindow :: Config.Config -> Event.Event -> String -> Effect.Effect Unit
openNewWindow config event newUrl = void do
  Event.preventDefault event
  case config.browser of
    (Maybe.Just browser) -> runBrowser browser newUrl
    Maybe.Nothing -> openWindow config newUrl

-- | The data URL encoding the page contents for the window.
pageDataURL :: String -> String
pageDataURL = pageData >>> prependPrefix
  where
    prependPrefix = ("data:text/html;charset=UTF-8," <> _)
    pageData url = Unsafe.unsafeEncodeURIComponent $
      "<!DOCTYPE html>" <>
      "<html style='width: 100%; height: 100%; margin: 0;'>" <>
        "<head>" <>
          "<meta charset='UTF-8' />" <>
        "</head>" <>
        "<body style='width: 100%; height: 100%; margin: 0;'>" <>
          "<script src='ui.js' data-url='"<> url <>"'></script>" <>
        "</body>" <>
      "</html>"

-- | Open a new Minichrome window with the given config and URL.
openWindow :: Config.Config -> String -> Effect.Effect Unit
openWindow config url = do
  window <- BrowserWindow.createBrowserWindow $ BrowserWindow.showOpt := false
  BrowserWindow.setMenu window Maybe.Nothing
  --WebContents.openDevTools window.webContents
  BrowserWindow.loadURL window (pageDataURL url) $
    BrowserWindow.baseURLForDataURL := ("file://" <> Globals.__dirname <> "/")
  BrowserWindow.onceReadyToShow window $ BrowserWindow.show window
  --WebContents.onNewWindow window.webContents $ openNewWindow config

-- | Handle HTTP requests to open a new window to a URL given by the HTTP body.
browse :: Config.Config -> String -> HTTPure.ResponseM
browse config url = do
  EffectClass.liftEffect do
    Console.log $ "Opening new window to " <> url
    openWindow config url
  HTTPure.ok $ "Opening new window to " <> url

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
    _, _ -> badRequest request

-- | Start the Electron app and HTTP server.
start :: Config.Config -> HTTPure.ServerM
start config = App.onReady do
  App.onWindowAllClosed mempty
  HTTPure.serve config.port (router config) $
    Console.log $ "Minichrome server started!"
