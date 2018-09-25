module Minichrome.UI.IPC
  ( attach
  ) where

import Prelude

import Data.Array ((!!))
import Data.Maybe as Maybe
import Effect as Effect
import Effect.Aff as Aff
import Node.Electron.IPCRenderer as IPCRenderer

import Minichrome.Config as Config
import Minichrome.UI.Commands as Commands
import Minichrome.UI.Components.Page as Page

-- | Gevin a `Config` and a query callback, attach the IPC handlers.
attach :: Config.Config ->
          (Page.Query Unit -> Aff.Aff Unit) ->
          Effect.Effect Unit
attach config query = IPCRenderer.on "exec" \_ args ->
  Maybe.maybe mempty (Commands.run query) $ args !! 0
