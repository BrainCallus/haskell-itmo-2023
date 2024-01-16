module Tasks.T4
  ( gcdWithLog
  , simpleGCD
  , identityGCD
  , writerGCD
  ) where

import           Control.Monad.Identity
import           Control.Monad.Writer
import           Data.Bifunctor

gcdWithLog :: (Integral a) => a -> a -> ([(a, a)], a)
gcdWithLog = identityGCD

simpleGCD :: (Integral a) => a -> a -> ([(a, a)], a)
simpleGCD x y
  | x < y = simpleGCD y x
  | y == 0 = ([(x, y)], x)
  | otherwise = Data.Bifunctor.first ((x, y) :) res
  where
    res = simpleGCD y (x `mod` y)

identityGCD :: Integral a => a -> a -> ([(a, a)], a)
identityGCD x y = runIdentity (gcdIdInternal x y)

gcdIdInternal :: (Monad m, Integral a) => a -> a -> m ([(a, a)], a)
gcdIdInternal x y
  | x < y = gcdIdInternal y x
  | y == 0 = return ([(x, y)], x)
  | otherwise =
    gcdIdInternal y (x `mod` y) >>= \(lst, res) -> return ((x, y) : lst, res)

writerGCD :: (Integral a, Show a) => a -> a -> (([(a, a)], a), String)
writerGCD x y = runWriter $ gcdWriterInternal x y

gcdWriterInternal ::
     (Integral a, Show a) => a -> a -> Writer String ([(a, a)], a)
gcdWriterInternal x y
  | x < y = gcdWriterInternal y x
  | y == 0 = return ([(x, y)], x)
  | otherwise =
    gcdWriterInternal y (x `mod` y) >>= \(lst, res) ->
      tell ("GCD " ++ show x ++ " " ++ show y ++ " = " ++ show res ++ "\n")
        >> return ((x, y) : lst, res)
