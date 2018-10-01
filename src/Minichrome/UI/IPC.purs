module Minichrome.UI.IPC
  ( attach
  ) where

import Prelude

import Data.Either as Either
import Effect as Effect
import Effect.Aff as Aff
import Halogen as Halogen

import Minichrome.Command.Command as Command
import Minichrome.Config as Config
import Minichrome.IPC.CLIToUI as IPC
import Minichrome.UI.State as State

exec :: (State.Query Unit -> Aff.Aff Unit) -> String -> Effect.Effect Unit
exec query cmd =
  Aff.launchAff_ $ query $ Halogen.action $
    Either.either State.ShowMessage State.RunCommand $ Command.read cmd

-- | Gevin a `Config` and a query callback, attach the IPC handlers.
attach
  :: Config.Config
  -> (State.Query Unit -> Aff.Aff Unit)
  -> Effect.Effect Unit
attach config query = IPC.subscribe $ case _ of
  (IPC.Exec cmd) -> exec query cmd
