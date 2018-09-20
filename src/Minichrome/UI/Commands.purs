module Minichrome.UI.Commands
  ( run
  ) where

import Prelude

import Data.Maybe as Maybe
import Effect as Effect
import Effect.Aff as Aff

import Minichrome.UI.Components.Page as Page

-- | Given a query callback and a command string, lookup and run the appropriate
-- | command, if any.
run :: (Page.Query Unit -> Aff.Aff Unit) -> String -> Effect.Effect Unit
run query cmd = Aff.launchAff_ $ Maybe.maybe mempty query $ Page.lookup cmd <@> unit
