module Node.Electron.IPCRenderer
  ( on
  ) where

import Prelude

import Effect as Effect

import Node.Electron.Event as Event

foreign import on ::
  String ->
  (Event.Event -> Array String -> Effect.Effect Unit) ->
  Effect.Effect Unit
