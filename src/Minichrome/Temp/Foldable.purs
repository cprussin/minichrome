module Minichrome.Temp.Foldable
  ( findM
  , findWithIndexM
  , findMapM
  ) where

import Prelude

import Data.Foldable as Foldable
import Data.FoldableWithIndex as FoldableWithIndex
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

findWithIndexM
  :: forall i a m f
   . FoldableWithIndex.FoldableWithIndex i f
  => Monad m
  => (i -> a -> m Boolean)
  -> f a
  -> m (Maybe.Maybe { index :: i, value :: a })
findWithIndexM predicate = FoldableWithIndex.foldWithIndexM match Maybe.Nothing
  where
    match i Maybe.Nothing a =
      ifM (predicate i a)
        (pure $ Maybe.Just { index: i, value: a })
        (pure Maybe.Nothing)
    match _ found a = pure found
