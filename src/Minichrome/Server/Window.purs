module Minichrome.Server.Window
  ( open
  ) where

import Prelude

import Data.Foldable as Foldable
import Data.Maybe as Maybe
import Data.Options ((:=))
import Data.Tuple as Tuple
import Effect as Effect
import Global.Unsafe as Unsafe
import Node.ChildProcess as ChildProcess
import Node.Electron.BrowserWindow as BrowserWindow
import Node.Electron.Event as Event
import Node.Electron.WebContents as WebContents
import Node.Globals as Globals

import Minichrome.Config as Config

-- | Return `True` if the given keybinding matches the given input.
matchKeybinding :: WebContents.InputProperties -> Config.Keybinding -> Boolean
matchKeybinding input (Tuple.Tuple (Config.Shortcut key alt) _) =
  key == input.key && alt == input.alt

-- | Given a config, a window, an event, and a input, run any keybindings
-- | configured for the input.
runKeybinding ::
  Config.Config ->
  BrowserWindow.BrowserWindow ->
  Event.Event ->
  WebContents.InputProperties ->
  Effect.Effect Unit
runKeybinding config window event input =
  if input.type == "keyDown"
    then case Foldable.find (matchKeybinding input) config.keybindings of
      (Maybe.Just match) -> Tuple.snd match
      Maybe.Nothing -> pure unit
    else pure unit

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
    Maybe.Nothing -> open config newUrl

-- | The page contents for each window.
page :: String -> String
page url =
  "<!DOCTYPE html>" <>
  "<html style='width: 100%; height: 100%; margin: 0;'>" <>
    "<head>" <>
      "<meta charset='UTF-8' />" <>
    "</head>" <>
    "<body style='width: 100%; height: 100%; margin: 0;'>" <>
      "<script src='ui.js' data-url='"<> url <>"'></script>" <>
    "</body>" <>
  "</html>"

-- | The data URL encoding the page contents for the window.
pageDataURL :: String -> String
pageDataURL = page >>> Unsafe.unsafeEncodeURIComponent >>>
  ((<>) "data:text/html;charset=UTF-8,")

-- | Open a new Minichrome window with the given config and URL.
open :: Config.Config -> String -> Effect.Effect Unit
open config url = do
  window <- BrowserWindow.createBrowserWindow $ BrowserWindow.showOpt := false
  BrowserWindow.setMenu window Maybe.Nothing
  BrowserWindow.loadURL window (pageDataURL url) $
    BrowserWindow.baseURLForDataURL := ("file://" <> Globals.__dirname <> "/")
  BrowserWindow.onceReadyToShow window $ BrowserWindow.show window
  WebContents.beforeInputEvent window.webContents $ runKeybinding config window
  WebContents.onNewWindow window.webContents $ openNewWindow config
