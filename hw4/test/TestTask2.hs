{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TestTask2
  ( testParser
  ) where

import           HW4.T2
import           HW4.Types
import           Test.Hspec
import           Test.Tasty.Hspec     (testSpec)
import           Test.Tasty.Providers (TestTree)

testParser :: IO TestTree
testParser = testSpec "eval test" parserTest

parserTest :: Spec
parserTest = do
  describe
    "parser tests with success"
    do
      it
        "parse numbers"
        do
          checkParse "1" (Val 1)
          checkParse "999999999999999999.9" (Val 999999999999999999.9)
          checkParse "0.00000000000123" (Val 0.00000000000123)
          checkParse "12.398746" (Val 12.398746)
      it
        "parse numbers surrounded brackets"
        do
          checkParse "(1)" (Val 1)
          checkParse
            "(((((((999999999999999999.9)))))))"
            (Val 999999999999999999.9)
      it
        "parse binary operations"
        do
          checkParse "33+6" (Op (Add (Val 33.0) (Val 6.0)))
          checkParse "0.55-199" (Op (Sub (Val 0.55) (Val 199.0)))
          checkParse "0.018*0.12345" (Op (Mul (Val 1.8e-2) (Val 0.12345)))
          checkParse "349.123/0.987" (Op (Div (Val 349.123) (Val 0.987)))
      it
        "skip all spaces"
        do
          checkParse
            "      999999999999999999.9                      "
            (Val 999999999999999999.9)
          checkParse
            "                   ( (   (( 0.00000000000123)    )) )  "
            (Val 0.00000000000123)
          checkParse "  33+ 6" (Op (Add (Val 33.0) (Val 6.0)))
          checkParse "0.55 -199" (Op (Sub (Val 0.55) (Val 199.0)))
          checkParse
            "      ( ( (0.018 ))  *      0.12345          )"
            (Op (Mul (Val 1.8e-2) (Val 0.12345)))
          checkParse
            "349.123         /(0.987)    "
            (Op (Div (Val 349.123) (Val 0.987)))
      it
        "parse complex expressions"
        do
          checkParse
            "(1 / (2.44 - (35.5))) - ((32 + 3) * (10/2) + 144.5) /3 "
            (Op
               (Sub
                  (Op (Div (Val 1.0) (Op (Sub (Val 2.44) (Val 35.5)))))
                  (Op
                     (Div
                        (Op
                           (Add
                              (Op
                                 (Mul
                                    (Op (Add (Val 32.0) (Val 3.0)))
                                    (Op (Div (Val 10.0) (Val 2.0)))))
                              (Val 144.5)))
                        (Val 3.0)))))
          checkParse
            "(1 / ( 2 / 3)) - ((340 / ( (    ( 3))  )) * (5 + 7) + 144.5) / (0.519) "
            (Op
               (Sub
                  (Op (Div (Val 1.0) (Op (Div (Val 2.0) (Val 3.0)))))
                  (Op
                     (Div
                        (Op
                           (Add
                              (Op
                                 (Mul
                                    (Op (Div (Val 340.0) (Val 3.0)))
                                    (Op (Add (Val 5.0) (Val 7.0)))))
                              (Val 144.5)))
                        (Val 0.519)))))
  describe
    "parser tests with fail"
    do
      it
        "fail on wrong bracket sequence"
        do
          assertFail " (  (2)"
          assertFail "(1+1"
          assertFail "1+ 1)"
          assertFail "2 + (3-1/(10)"
      it
        "fail on blank string"
        do
          assertFail ""
          assertFail "                "
      it
        "fail on expression with variables"
        do
          assertFail "x + 1"
          assertFail "10 - t"
          assertFail "x+y"
      it
        "fail on invalid expression strings"
        do
          assertFail "abc"
          assertFail "1 2"
          assertFail "1 2 3"
          assertFail "+ 3"
          assertFail "/ 67"
          assertFail "-"
          assertFail "3 + 4 0"
          assertFail "( )"

checkParse :: String -> Expr -> Expectation
checkParse input answer = parseExpr input `shouldBe` Success answer

assertFail :: String -> Expectation
assertFail input =
  case parseExpr input of
    Error (ErrorAtPos _) -> True `shouldBe` True
    _                    -> False `shouldBe` True
