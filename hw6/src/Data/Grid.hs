{-# LANGUAGE InstanceSigs #-}

-- | This module defines 'Grid' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.Grid
  ( Grid(..)
  , gridGet
  , focusDown
  , focusUp
  , focusLeft
  , focusRight
  , gridWrite
  , neighbourGetters
  ) where

import           Control.Comonad (Comonad (..))

import           Data.ListZipper (ListZipper (..), genericMove, listLeft,
                                  listRight, listWrite)

newtype Grid a = Grid
  { unGrid :: ListZipper (ListZipper a)
  }

instance Functor Grid where
  fmap :: (a -> b) -> Grid a -> Grid b
  fmap f = Grid . fmap (fmap f) . unGrid

instance Comonad Grid where
  extract :: Grid a -> a
  extract = gridGet
  extend :: (Grid a -> b) -> Grid a -> Grid b
  extend f = fmap f . (Grid . fmap getColZipper . getRowZipper)

gridGet :: Grid t -> t
gridGet = extract . extract . unGrid

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite (listWrite x (extract g)) g

focusLeft, focusRight, focusUp, focusDown :: Grid t -> Grid t
focusLeft = moveFocus (fmap listLeft)

focusRight = moveFocus (fmap listRight)

focusUp = moveFocus listLeft

focusDown = moveFocus listRight

moveFocus ::
     (ListZipper (ListZipper t) -> ListZipper (ListZipper t))
  -> Grid t
  -> Grid t
moveFocus f = Grid . f . unGrid

getColZipper, getRowZipper :: Grid t -> ListZipper (Grid t)
getColZipper = genericMove focusLeft focusRight

getRowZipper = genericMove focusUp focusDown

neighbourGetters :: [Grid t -> Grid t]
neighbourGetters = [focusLeft, focusRight, focusUp, focusDown]
