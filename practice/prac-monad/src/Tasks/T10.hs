module Tasks.T10
  ( mergeSortCPS
  , splitCPS
  , mergeCPS
  , mergeSortId
  ) where

import           Control.Monad.Cont

mergeSortId :: (Ord a) => [a] -> [a]
mergeSortId ar = mergeSort ar id

mergeSort :: (Ord a) => [a] -> ([a] -> r) -> r
mergeSort = runCont . mergeSortCPS

mergeSortCPS :: (Ord a) => [a] -> Cont r [a]
mergeSortCPS arr =
  if length arr > 1
    then let splitted = splitCPS arr
          in splitted
               >>= (\res ->
                      mergeSortCPS (fst res)
                        >>= (\res1 -> mergeSortCPS (snd res) >>= mergeCPS res1))
    else return arr

splitCPS :: [a] -> Cont r ([a], [a])
splitCPS a = return (splitAt (div (length a) 2) a)

mergeCPS :: (Ord a) => [a] -> [a] -> Cont r [a]
mergeCPS [] a = return a
mergeCPS a [] = return a
mergeCPS (x:xs) (y:ys) =
  if x <= y
    then mergeCPS xs (y : ys) >>= (\res -> return (x : res))
    else mergeCPS (x : xs) ys >>= (\res -> return (y : res))
