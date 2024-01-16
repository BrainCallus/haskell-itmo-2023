{-# LANGUAGE LambdaCase #-}

module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import           Data.Function   (fix)
import           Numeric.Natural (Natural)

repeat' :: a -> [a]
repeat' = fix . (:)

map' :: (a -> b) -> [a] -> [b]
map' f =
  fix
    (\g ->
       \case
         [] -> []
         (x:xs) -> f x : g xs)

fib :: Natural -> Natural
fib =
  fix
    (\inFib prev acc n ->
       if n == 0
         then acc
         else inFib acc (prev + acc) (n - 1))
    1
    0

fac :: Natural -> Natural
fac =
  fix
    (\f acc n ->
       if n < 2
         then acc
         else f (n * acc) (n - 1))
    1
