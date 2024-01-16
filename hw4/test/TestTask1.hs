{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TestTask1
  ( testEval
  ) where

import           HW4.T1
import           HW4.Types
import           Test.Hspec
import           Test.Tasty.Hspec     (testSpec)
import           Test.Tasty.Providers (TestTree)

testEval :: IO TestTree
testEval = testSpec "eval test" test1

test1 :: Spec
test1 = do
  describe "eval Task1" $ do
    it
      "shoul correctly evaluate expressions"
      do
        checkEval (Op (Add (Val 5.5) (Val 10.5))) (16.0 :# [Add 5.5 10.5])
        checkEval
          (Op
             $ Mul
                 (Op $ Div (Val 20) (Op $ Sub (Val 2) (Val (-2))))
                 (Op $ Add (Op $ Abs (Val (-10.25))) (Op $ Sgn (Val 4))))
          (56.25
             :# [ Mul 5.0 11.25
                , Add 10.25 1.0
                , Sgn 4.0
                , Abs (-10.25)
                , Div 20.0 4.0
                , Sub 2.0 (-2.0)
                ])
    it
      "should produse error on division by zero"
      do
        assertDBZ (Op (Div (Val 1) (Val 0)))
        assertDBZ
          (Op
             $ Mul
                 (Op $ Div (Val 20) (Op $ Sub (Val 2) (Val 2)))
                 (Op $ Add (Op $ Abs (Val (-10.25))) (Op $ Sgn (Val 4))))

checkEval :: Expr -> Annotated [Prim Double] Double -> Expectation
checkEval expr answer = runEval expr `shouldBe` Success answer

assertDBZ :: Expr -> Expectation
assertDBZ expr = runEval expr `shouldBe` Error DivideByZero

runEval :: Expr -> Except EvaluationError (Annotated [Prim Double] Double)
runEval expr = runES (eval expr) []
