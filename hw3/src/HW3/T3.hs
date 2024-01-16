{-# LANGUAGE LambdaCase #-}

module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import           HW3.T1
import           HW3.T2 (unwrapList)

joinOption :: Option (Option a) -> Option a
joinOption =
  \case
    Some (Some x) -> Some x
    _ -> None

joinExcept :: Except e (Except e a) -> Except e a
joinExcept =
  \case
    Success x -> x
    Error e -> Error e

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((x :# e) :# r) = x :# (r <> e)

joinList :: List (List a) -> List a
joinList = unwrapList

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) =
  F (\x ->
       let (F g) = f x
        in g x)
