{-# LANGUAGE LambdaCase #-}

module HW3.T1
  ( Option(..)
  , Pair(..)
  , Quad(..)
  , Annotated(..)
  , Except(..)
  , Prioritised(..)
  , Stream(..)
  , List(..)
  , Fun(..)
  , Tree(..)
  , mapOption
  , mapPair
  , mapQuad
  , mapAnnotated
  , mapExcept
  , mapPrioritised
  , mapStream
  , mapList
  , mapFun
  , mapTree
  ) where

import           Control.Monad  (join)
import           Data.Bifunctor (bimap)

data Option a
  = None
  | Some a
  deriving (Show, Eq)

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption f =
  \case
    Some x -> Some (f x)
    None -> None

data Pair a =
  P a a
  deriving (Show, Eq)

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f = toP . (join bimap f . fromP)

toP :: (a, a) -> Pair a
toP (x, y) = P x y

fromP :: Pair a -> (a, a)
fromP (P frst scnd) = (frst, scnd)

data Quad a =
  Q a a a a
  deriving (Show, Eq)

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q x1 x2 x3 x4) = Q (f x1) (f x2) (f x3) (f x4)

data Annotated e a =
  a :# e
  deriving (Show)

instance (Eq e, Eq a) => Eq (Annotated e a) where
  (==) (e1 :# a1) (e2 :# a2) = e1 == e2 && a1 == a2

infix 0 :#

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = f a :# e

data Except e a
  = Error e
  | Success a
  deriving (Show)

instance (Eq e, Eq a) => Eq (Except e a) where
  (==) (Success x) (Success y) = x == y
  (==) (Error x) (Error y)     = x == y
  (==) _ _                     = False

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept f =
  \case
    Success x -> Success (f x)
    Error y -> Error y

data Prioritised a
  = Low a
  | Medium a
  | High a
  deriving (Show, Eq)

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f =
  \case
    Low x -> Low (f x)
    Medium x -> Medium (f x)
    High x -> High (f x)

data Stream a =
  a :> Stream a
  deriving (Show, Eq)

infixr 5 :>

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (x :> xs) = f x :> mapStream f xs

data List a
  = Nil
  | a :. List a
  deriving (Show, Eq)

infixr 5 :.

mapList :: (a -> b) -> (List a -> List b)
mapList f =
  \case
    Nil -> Nil
    (x :. xs) -> f x :. mapList f xs

data Fun i a =
  F (i -> a)

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F phi) = F (f . phi)

data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)
  deriving (Show, Eq)

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree f =
  \case
    Leaf -> Leaf
    Branch lft x rht -> Branch (mapTree f lft) (f x) (mapTree f rht)
