module Minichrome.UI.Keybindings
  ( attach
  ) where

import Prelude

import Data.Foldable as Foldable
import Data.Maybe as Maybe
import Data.Tuple as Tuple
import Effect as Effect
import Effect.Aff as Aff
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent.EventTypes as EventTypes
import Web.UIEvent.KeyboardEvent as KeyboardEvent

import Minichrome.CLI.Client as Client
import Minichrome.Config as Config
import Minichrome.UI.Components.Page as Page

-- | Return `True` if the given keybinding matches the given event.
matchKeybinding :: KeyboardEvent.KeyboardEvent -> Config.Keybinding -> Boolean
matchKeybinding event (Tuple.Tuple (Config.Shortcut key alt) _) =
  key == KeyboardEvent.key event && alt == KeyboardEvent.altKey event

-- | Given a `Config` and an `Event`, return the command that should be ran, if
-- | any.
lookupKeybinding :: Config.Config -> Event.Event -> Maybe.Maybe String
lookupKeybinding config event = do
  keyboardEvent <- KeyboardEvent.fromEvent event
  Tuple.snd <$> Foldable.find (matchKeybinding keyboardEvent) config.keybindings

-- | Given a `Config` and a query callback, return the callback that should be
-- | attached as the event listener for handling keybindings.
keybindingListener :: Config.Config ->
                      (Page.Query Unit -> Aff.Aff Unit) ->
                      Effect.Effect EventTarget.EventListener
keybindingListener config query = EventTarget.eventListener \event ->
  Maybe.maybe mempty (run event) $ lookupKeybinding config event
  where
    run event command = do
      Event.preventDefault event
      Aff.launchAff_ $ Client.exec config command

-- | Gevin a `Config` and a query callback, attach the keybindings in the config
-- | to the window.
attach :: Config.Config ->
          (Page.Query Unit -> Aff.Aff Unit) ->
          Effect.Effect Unit
attach config query = do
  document <- HTML.window >>= Window.document <#> HTMLDocument.toEventTarget
  listener <- keybindingListener config query
  EventTarget.addEventListener EventTypes.keydown listener false document
