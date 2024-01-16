{-# LANGUAGE LambdaCase #-}

module Tasks.T2
  ( Expr(..)
  , VarMap(..)
  , evalExpr
  ) where

import           Control.Monad.Reader
import           Data.Map

data Expr a
  = Const a
  | Var String
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)

newtype VarMap a =
  VarMap (Map String a)

evalExpr :: (Num a) => VarMap a -> Expr a -> a
evalExpr (VarMap mp) =
  \case
    Const x -> x
    Var s -> runReader (readVar s) mp
    Add x y -> (+) (evalExpr varMap x) (evalExpr varMap y)
    Sub x y -> (-) (evalExpr varMap x) (evalExpr varMap y)
    Mul x y -> (*) (evalExpr varMap x) (evalExpr varMap y)
  where
    varMap = VarMap mp

readVar :: String -> Reader (Map String a) a
readVar s = do
  asks (Data.Map.lookup s)
    >>= (\case
           Just v -> return v
           Nothing -> error ("Variable " ++ s ++ " not found"))
