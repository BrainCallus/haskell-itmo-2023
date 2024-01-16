module HW2.T1
  ( Tree(..)
  , tfoldr
  ) where

data Tree a
  = Leaf
  | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr func accum tree =
  case tree of
    Leaf                  -> accum
    Branch _ left v right -> tfoldr func (func v (tfoldr func accum right)) left
