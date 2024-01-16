{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module BoolTests
  ( testBools
  , boolProps
  ) where

import           Hedgehog            (Gen, forAll, property, (===))
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           HW5.Base
import           Test.Hspec
import           Test.Tasty          (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.Hspec    (testSpecs)
import           TestBase

testBools :: IO [TestTree]
testBools = mconcat [testSpecs boolsTest, testSpecs failOnEvalBoolTests]

boolsTest :: Spec
boolsTest = do
  describe "test bool operations" $ do
    it "Equality checking" $ do
      runOnInput "equals(10, 10)" "true"
      runOnInput "equals(false, false)" "true"
      runOnInput "equals(3, 10)" "false"
      runOnInput "equals(1, true)" "false"
    it "comparations" $ do
      runOnInput "less-than(3, 10)" "true"
      runOnInput "less-than(false, true)" "true"
      runOnInput "less-than(false, 0)" "true"

failOnEvalBoolTests :: Spec
failOnEvalBoolTests = do
  describe "tests that should fail on evaluating" $ do
    it "error DBZ" $ do
      failOnEval HiErrorDivideByZero "or(1/0, true)"
      failOnEval HiErrorDivideByZero "and(true, 1+2/0)"
      failOnEval HiErrorDivideByZero "not(2/0)"
      failOnEval HiErrorDivideByZero "0/0 == 0/0"
      failOnEval HiErrorDivideByZero "0/0 /= 0/0"
      failOnEval HiErrorDivideByZero "0/0 <= 0/0"
      failOnEval HiErrorDivideByZero "0/0 >= 0/0"
      failOnEval HiErrorDivideByZero "0/0 < 0/0"
      failOnEval HiErrorDivideByZero "0/0 > 0/0"
    it "arity mismatch or(true, 1, 10)" $ do
      failOnEval HiErrorArityMismatch "or(true, 1, 10)"
    it "arity mismatch or(true)" $ do
      failOnEval HiErrorArityMismatch "or(true)"
    it "arity mismatch and(true, true, or(false, true))" $ do
      failOnEval HiErrorArityMismatch "and(true, true, or(false, true))"
    it "arity mismatch and(false, true, or(false, true))" $ do
      failOnEval HiErrorArityMismatch "and(false, true, or(false, true))"
    it "arity mismatch not(false true)" $ do
      failOnEval HiErrorArityMismatch "not(false, true)"
    it "invalid function" $ do
      failOnEval HiErrorInvalidFunction "(true && true)(1)"
      failOnEval HiErrorInvalidFunction "(1==1)(0)"
      failOnEval HiErrorInvalidFunction "not(false).true"
      failOnEval HiErrorInvalidFunction "(true && true).true"
      failOnEval HiErrorInvalidFunction "(true && true).and(1)"
    it "invalid argument" $ do
      failOnEval HiErrorInvalidArgument "not(2)"
      failOnEval HiErrorInvalidArgument "not(0)"
      failOnEval HiErrorInvalidArgument "not(\"str\")"

boolProps :: TestTree
boolProps = testGroup "bool props" [propFirstOnTrue, propScnOnFalse]

propFirstOnTrue, propScnOnFalse :: TestTree
propFirstOnTrue =
  testProperty
    "if(true, A, B) ≡ A"
    (property $ do
       forAll genString >>= \s -> do
         runOnStringToString ("if(true, \"" ++ s ++ "\", add )")
           === ("\"" ++ s ++ "\""))

propScnOnFalse =
  testProperty
    "if(false, A, B) ≡ B"
    (property $ do
       forAll genString >>= \s -> do
         runOnStringToString ("if(false, 1, \"" ++ s ++ "\")")
           === ("\"" ++ s ++ "\""))

genString :: Hedgehog.Gen String
genString =
  let listLength = Range.linear 2 100
   in Gen.list listLength Gen.alpha
