{-# LANGUAGE LambdaCase #-}

module Tasks.T1
  ( Expr(..)
  , ErrMsg(..)
  , evalExpr
  ) where

import           Control.Applicative (liftA2)

data Expr a
  = Const a
  | MinusInfinity
  | PlusInfinity
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  | Pow (Expr a) (Expr a)

data ErrMsg
  = DivisionByZero
  | NonIntExponentWithNegativeBase
  | ResultIsUndefined String
  deriving (Show)

minf, inf :: (RealFloat a) => a -> a
inf _ = 1.0 / 0.0

minf = (* (-1)) . inf

evalExpr :: (RealFloat a) => Expr a -> Either ErrMsg a
evalExpr =
  \case
    Const a -> Right a
    PlusInfinity -> Right (1.0 / 0.0)
    MinusInfinity -> evalExpr PlusInfinity >>= (\x -> Right (x * (-1)))
    Add x y -> add x y
    Sub x y -> sub x y
    Mul x y -> liftA2 (*) (evalExpr x) (evalExpr y)
    Div x y -> divide x y
    Pow x y -> pow x y

add :: (RealFloat a) => Expr a -> Expr a -> Either ErrMsg a
add x y =
  binaryOp
    x
    y
    (+)
    (\a b -> (a == inf a && b == minf b) || (a == minf a && b == inf b))
    (ResultIsUndefined "Forbidden to add infinities have different signs")

sub :: (RealFloat a) => Expr a -> Expr a -> Either ErrMsg a
sub x y =
  binaryOp
    x
    y
    (-)
    (\a b -> (a == inf a && b == inf b) || (a == minf a && b == minf b))
    (ResultIsUndefined "Forbidden to subtract infinities have same signs")

pow :: (RealFloat a) => Expr a -> Expr a -> Either ErrMsg a
pow x y =
  binaryOp x y (**) (\a b -> isNaN ((**) a b)) NonIntExponentWithNegativeBase

binaryOp ::
     (RealFloat a)
  => Expr a
  -> Expr a
  -> (a -> a -> a)
  -> (a -> a -> Bool)
  -> ErrMsg
  -> Either ErrMsg a
binaryOp x y oper predicate msg =
  evalExpr y >>= (\resY -> evalFirstAndReturn x resY oper predicate msg)

evalFirstAndReturn ::
     (RealFloat a)
  => Expr a
  -> a
  -> (a -> a -> a)
  -> (a -> a -> Bool)
  -> ErrMsg
  -> Either ErrMsg a
evalFirstAndReturn exprX resY oper predicate msg =
  evalExpr exprX
    >>= (\resX ->
           if predicate resX resY
             then Left msg
             else Right (oper resX resY))

divide :: (RealFloat a) => Expr a -> Expr a -> Either ErrMsg a
divide x y =
  evalExpr y
    >>= (\resY ->
           if resY == 0
             then Left DivisionByZero
             else evalFirstAndReturn
                    x
                    resY
                    (/)
                    (\xx yy ->
                       (xx == inf xx || xx == minf xx)
                         && (yy == inf yy || yy == minf yy))
                    (ResultIsUndefined "Forbidden to divide 2 infinities"))
