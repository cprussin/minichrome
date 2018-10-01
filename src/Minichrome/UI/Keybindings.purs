module Minichrome.UI.Keybindings
  ( attach
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable as Foldable
import Data.Maybe as Maybe
import Data.String as String
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
import Minichrome.Command.Command as Command
import Minichrome.Command.InputMode as InputMode
import Minichrome.Config as Config
import Minichrome.UI.State as State

-- | This isn't defined in `Web.UIEvent.KeyboardEvent.EventTypes` but should be.
-- | TODO put a PR in to purescript-web-uievents to add this.
keypress :: Event.EventType
keypress = Event.EventType "keypress"

-- | Return `True` if the given keybinding matches the current mode and
-- | sequence.
match :: InputMode.Mode -> String -> Config.Keybinding -> Boolean
match currentMode sequence (Config.Keybinding validModes matchSequence _) =
  sequence == matchSequence && Array.elem currentMode validModes

-- | Given a `Config` and an `Event`, return the command that should be ran, if
-- | any.
lookupKeybinding
  :: Config.Config
  -> InputMode.Mode
  -> String
  -> Maybe.Maybe Command.Command
lookupKeybinding config mode sequence =
  Config.getCommand <$> Foldable.find (match mode sequence) config.keybindings

-- | Take an event and a current sequence and return the new sequence including
-- | any added components from the event.
appendKey :: KeyboardEvent.KeyboardEvent -> String -> String
appendKey event currentSequence =
  if isModifier
    then currentSequence
  else if currentSequence == ""
    then newKey
    else currentSequence <> " " <> newKey
  where
    newKey = alt <> ctrl <> meta <> KeyboardEvent.key event
    isModifier = Array.elem (KeyboardEvent.key event) modifiers
    modifiers = [ "Alt", "Control", "Meta", "Shift" ]
    alt = if KeyboardEvent.altKey event then "A-" else ""
    ctrl = if KeyboardEvent.ctrlKey event then "C-" else ""
    meta = if KeyboardEvent.metaKey event then "M-" else ""

-- | Given an `Event` and a callback, try to convert the event into a
-- | `KeyboardEvent`.  If successful, pass the `KeyboardEvent` to the callback
-- | and evaluate the result.  Otherwise, just evaluate a `pure unit`.
withKeyboardEvent
  :: forall a. Applicative a
  => Event.Event
  -> (KeyboardEvent.KeyboardEvent -> a Unit)
  -> a Unit
withKeyboardEvent event cb =
  Maybe.maybe (pure unit) cb $ KeyboardEvent.fromEvent event

-- | Run a command, stopping propagation and default handling of the associated
-- | event, and clearing the sequence ref.
run
  :: Config.Config
  -> (State.Query ~> Aff.Aff)
  -> Event.Event
  -> Command.Command
  -> Aff.Aff Unit
run config query event command = do
  EffectClass.liftEffect do
    Event.preventDefault event
    Event.stopPropagation event
  query $ Halogen.action $ State.SetSequence ""
  Client.exec config command

-- | True if the given sequence is the prefix of the given keybinding.
isPrefix :: InputMode.Mode -> String -> Config.Keybinding -> Boolean
isPrefix currentMode sequence (Config.Keybinding validModes matchSequence _) =
  Array.elem currentMode validModes && Maybe.fromMaybe false
    ((_ == 0) <$> String.indexOf (String.Pattern sequence) matchSequence)

-- | True if the given keyboard event has any modifiers pressed.
hasModifier :: KeyboardEvent.KeyboardEvent -> Boolean
hasModifier event =
  Foldable.any (_ $ event)
    [ KeyboardEvent.altKey
    , KeyboardEvent.ctrlKey
    , KeyboardEvent.metaKey
    ]

-- | If the current sequence is a possible prefix for a keybinding, then show it
-- | in the message line.  Otherwise, clear it and show a message indicating
-- | that there's no match.
noMatch
  :: Config.Config
  -> (State.Query ~> Aff.Aff)
  -> InputMode.Mode
  -> KeyboardEvent.KeyboardEvent
  -> Aff.Aff Unit
noMatch config query mode event = do
  sequence <- query $ Halogen.request State.GetSequence
  unless (String.null sequence || isPrefix' sequence) do
    query $ Halogen.action $ State.SetSequence ""
    when (mode /= InputMode.Insert || hasModifier event) do
      query $ Halogen.action $
        State.ShowMessage $ sequence <> " is undefined"
  where
    isPrefix' = isPrefix mode >>> flip Foldable.any config.keybindings

-- | This is the keydown handler.
onKey
  :: Config.Config
  -> (State.Query ~> Aff.Aff)
  -> Event.Event
  -> Effect.Effect Unit
onKey config query event = Aff.launchAff_ $
  withKeyboardEvent event \event' -> do
    mode <- query $ Halogen.request State.GetCurrentMode
    oldSequence <- query $ Halogen.request State.GetSequence
    let sequence = appendKey event' oldSequence
    query $ Halogen.action $ State.SetSequence sequence
    Maybe.maybe
      (noMatch config query mode event')
      (run config query event)
      (lookupKeybinding config mode sequence)

-- | Gevin a `Config` and a query callback, attach the keybindings in the config
-- | to the window.
attach :: Config.Config -> (State.Query ~> Aff.Aff) -> Effect.Effect Unit
attach config query = do
  document <- HTML.window >>= Window.document <#> HTMLDocument.toEventTarget
  listener <- EventTarget.eventListener $ onKey config query
  EventTarget.addEventListener EventTypes.keydown listener false document
