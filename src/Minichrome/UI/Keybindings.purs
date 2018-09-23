module Minichrome.UI.Keybindings
  ( attach
  ) where

import Prelude

import Data.Foldable as Foldable
import Data.Maybe as Maybe
import Effect as Effect
import Effect.Aff as Aff
import Effect.Class as EffectClass
import Halogen as Halogen
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
import Minichrome.UI.InputMode as InputMode

-- | This isn't defined in `Web.UIEvent.KeyboardEvent.EventTypes` but should be.
-- | TODO put a PR in to purescript-web-uievents to add this.
keypress :: Event.EventType
keypress = Event.EventType "keypress"

-- | Return `True` if the given keybinding matches the given event.
match :: InputMode.Mode ->
         KeyboardEvent.KeyboardEvent ->
         Config.Keybinding ->
         Boolean
match mode event (Config.Keybinding mode' (Config.Shortcut key alt) _) =
  mode' == mode &&
  key == KeyboardEvent.key event &&
  alt == KeyboardEvent.altKey event

-- | Given a `Config` and an `Event`, return the command that should be ran, if
-- | any.
lookupKeybinding :: Config.Config ->
                    InputMode.Mode ->
                    Event.Event ->
                    Maybe.Maybe String
lookupKeybinding config mode event = do
  keyboardEvent <- KeyboardEvent.fromEvent event
  Config.getCommand <$>
    Foldable.find (match mode keyboardEvent) config.keybindings

-- | This is the keydown handler.
onKey :: Config.Config -> (Page.Query ~> Aff.Aff) -> Event.Event -> Aff.Aff Unit
onKey config query event = do
  ex <- query $ Halogen.request Page.GetEx
  when (not ex) do
    mode <- query $ Halogen.request Page.GetCurrentMode
    Maybe.maybe (pure unit) run $ lookupKeybinding config mode event
  where
    run command = do
      EffectClass.liftEffect do
        Event.preventDefault event
        Event.stopPropagation event
      Client.exec config command

-- | Gevin a `Config` and a query callback, attach the keybindings in the config
-- | to the window.
attach :: Config.Config -> (Page.Query ~> Aff.Aff) -> Effect.Effect Unit
attach config query = do
  document <- HTML.window >>= Window.document <#> HTMLDocument.toEventTarget
  listener <- EventTarget.eventListener $ onKey config query >>> Aff.launchAff_
  EventTarget.addEventListener EventTypes.keydown listener false document
