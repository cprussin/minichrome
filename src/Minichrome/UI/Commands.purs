module Minichrome.UI.Commands
  ( run
  ) where

import Prelude

import Data.Maybe as Maybe
import Effect as Effect
import Effect.Aff as Aff
import Halogen as Halogen

import Minichrome.UI.Components.Page as Page

-- | Given a command string, return the appropriate `Query` on `Page`.
lookup :: forall a. String -> Maybe.Maybe (a -> Page.Query a)
lookup "back" = pure Page.GoBack
lookup "forward" = pure Page.GoForward
lookup "dev-tools" = pure Page.OpenDevTools
lookup "ex" = pure Page.Ex
lookup "insert" = pure Page.Insert
lookup "normal" = pure Page.Normal
lookup cmd = pure $ Page.ShowMessage $ "Command not recognized: " <> cmd

-- | Given a query callback and a command string, lookup and run the appropriate
-- | command, if any.
run :: (Page.Query Unit -> Aff.Aff Unit) -> String -> Effect.Effect Unit
run query cmd =
  Aff.launchAff_ $ Maybe.maybe mempty query $ Halogen.action <$> lookup cmd
