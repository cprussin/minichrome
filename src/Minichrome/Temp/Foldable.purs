module Minichrome.Temp.Foldable
  ( findM
  , findMapM
  ) where

import Prelude

import Data.Foldable as Foldable
import Data.Maybe as Maybe

import Minichrome.Temp.Filterable as Filterable

findM
  :: forall a m f
   . Monad m
  => Foldable.Foldable f
  => (a -> m Boolean)
  -> f a
  -> m (Maybe.Maybe a)
findM = Filterable.maybeBoolM >>> findMapM

findMapM
  :: forall a b m f
   . Monad m
  => Foldable.Foldable f
  => (a -> m (Maybe.Maybe b))
  -> f a
  -> m (Maybe.Maybe b)
findMapM predicate = Foldable.foldM match Maybe.Nothing
  where
    match Maybe.Nothing = predicate
    match found = const $ pure found
