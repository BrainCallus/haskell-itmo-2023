{-# LANGUAGE LambdaCase #-}

module HW3.T4
  ( State(..)
  , Prim(..)
  , Expr(..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import           HW3.T1

newtype State s a = S
  { runS :: s -> Annotated s a
  }

mapState :: (a -> b) -> State s a -> State s b
mapState f (S rs) = S (mapAnnotated f . rs)

wrapState :: a -> State s a
wrapState x = S (x :#)

joinState :: State s (State s a) -> State s a
joinState (S rs) =
  S (\x ->
       let (S inner_rs :# an) = rs x
        in inner_rs an)

modifyState :: (s -> s) -> State s ()
modifyState f = S (\x -> () :# f x)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) x y = do
    x1 <- x
    x1 <$> y

instance Monad (State s) where
  (>>=) ms f = joinState (fmap f ms)

data Prim a
  = Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving (Show)

data Expr
  = Val Double
  | Op (Prim Expr)
  deriving (Show)

instance Num Expr where
  (+) = \x y -> Op (Add x y)
  (-) = \x y -> Op (Sub x y)
  (*) = \x y -> Op (Mul x y)
  abs = Op . Abs
  signum = Op . Sgn
  fromInteger = Val . fromInteger

instance Fractional Expr where
  (/) = \x y -> Op (Div x y)
  fromRational = Val . fromRational

eval :: Expr -> State [Prim Double] Double
eval =
  \case
    Val x -> pure x
    Op (Add x y) -> evalBinOp x y (+) Add
    Op (Sub x y) -> evalBinOp x y (-) Sub
    Op (Mul x y) -> evalBinOp x y (*) Mul
    Op (Div x y) -> evalBinOp x y (/) Div
    Op (Abs x) -> evalUnoOp x abs Abs
    Op (Sgn x) -> evalUnoOp x signum Sgn

evalBinOp ::
     Expr
  -> Expr
  -> (Double -> Double -> Double)
  -> (Double -> Double -> Prim Double)
  -> State [Prim Double] Double
evalBinOp x y oper prim = do
  resX <- eval x
  resY <- eval y
  modifyState (prim resX resY :)
  pure (oper resX resY)

evalUnoOp ::
     Expr
  -> (Double -> Double)
  -> (Double -> Prim Double)
  -> State [Prim Double] Double
evalUnoOp x oper prim = do
  resX <- eval x
  modifyState (prim resX :)
  pure (oper resX)
