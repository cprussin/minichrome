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

import Minichrome.Config as Config
import Minichrome.UI.Components.Page as Page

-- | Return `True` if the given keybinding matches the given event.
matchKeybinding :: KeyboardEvent.KeyboardEvent -> Config.Keybinding -> Boolean
matchKeybinding event (Tuple.Tuple (Config.Shortcut key alt) _) =
  key == KeyboardEvent.key event && alt == KeyboardEvent.altKey event

-- | Given a `Config` and an `Event`, return the `Query` that should be ran, if
-- | any.
lookupKeybinding :: forall a.
                    Config.Config ->
                    Event.Event ->
                    Maybe.Maybe (a -> Page.Query a)
lookupKeybinding config event = do
  keyboardEvent <- KeyboardEvent.fromEvent event
  match <- Foldable.find (matchKeybinding keyboardEvent) config.keybindings
  case Tuple.snd match of
    "back" -> pure Page.GoBack
    "forward" -> pure Page.GoForward
    "dev-tools" -> pure Page.OpenDevTools
    _ -> Maybe.Nothing

-- | Given a `Config` and a query callback, return the callback that should be
-- | attached as the event listener for handling keybindings.
keybindingListener :: Config.Config ->
                      (Page.Query Unit -> Aff.Aff Unit) ->
                      Effect.Effect EventTarget.EventListener
keybindingListener config query = EventTarget.eventListener $
  lookupKeybinding config >>> Maybe.maybe mempty runQuery >>> Aff.launchAff
  where
    runQuery f = query $ f unit

-- | Gevin a `Config` and a query callback, attach the keybindings in the config
-- | to the window.
attach :: Config.Config ->
          (Page.Query Unit -> Aff.Aff Unit) ->
          Effect.Effect Unit
attach config query = do
  document <- HTML.window >>= Window.document <#> HTMLDocument.toEventTarget
  listener <- keybindingListener config query
  EventTarget.addEventListener EventTypes.keydown listener false document
