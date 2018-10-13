module Minichrome.Temp.Filterable
  ( maybeBoolM
  ) where

import Prelude

import Data.Maybe as Maybe

maybeBoolM :: forall a m. Monad m => (a -> m Boolean) -> a -> m (Maybe.Maybe a)
maybeBoolM predicate item = do
  match <- predicate item
  pure $ if match then Maybe.Just item else Maybe.Nothing
