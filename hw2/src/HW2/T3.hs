{-# LANGUAGE LambdaCase #-}

module HW2.T3
  ( epart
  , mcat
  ) where

mcat :: Monoid a => [Maybe a] -> a
mcat =
  foldMap
    (\case
       Just v -> v
       Nothing -> mempty)

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart =
  foldMap
    (\case
       Left a -> (a, mempty)
       Right b -> (mempty, b))
