module HW2.T2
  ( joinWith
  , splitOn
  ) where

import           Data.List.NonEmpty (NonEmpty ((:|)))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn separator = foldr (splitOnHelper separator) ([] :| [])

splitOnHelper :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
splitOnHelper separator ch (x :| xs) =
  if ch == separator
    then [] :| (x : xs)
    else (ch : x) :| xs

joinWith :: a -> NonEmpty [a] -> [a]
joinWith joiner (x :| xs) = x ++ foldMap (joinHelper joiner) xs

joinHelper :: a -> [a] -> [a]
joinHelper joiner list = joiner : list
