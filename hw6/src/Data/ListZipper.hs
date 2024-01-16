-- | This module defines 'ListZipper' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
{-# LANGUAGE LambdaCase #-}

module Data.ListZipper
  ( ListZipper(..)
  , listLeft
  , listRight
  , genericMove
  , listWrite
  , toList
  ) where

import           Control.Comonad (Comonad (..))

data ListZipper a =
  LZ [a] a [a]

instance Functor ListZipper where
  fmap f (LZ left x right) = LZ (fmap f left) (f x) (fmap f right)

instance Comonad ListZipper where
  extract (LZ _ x _) = x
  extend f = fmap f . genericMove listLeft listRight

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

genericMove :: (t -> t) -> (t -> t) -> t -> ListZipper t
genericMove f g l = LZ (iterateTail f l) l (iterateTail g l)

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

listLeft, listRight :: ListZipper t -> ListZipper t
listLeft =
  \case
    LZ (l:ls) x r -> LZ ls l (x : r)
    _ -> error "Left part is empty!"

listRight =
  \case
    LZ l x (r:rs) -> LZ (x : l) r rs
    _ -> error "Right part is empty!"
