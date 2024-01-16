{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TestBase
  ( runOnInput
  , runOnStringToString
  , failParse
  , failOnEval
  ) where

import           Data.Maybe      (fromMaybe)
import           Data.Void       (Void)
import           HW5.Base
import           HW5.Evaluator
import           HW5.Parser
import           HW5.Pretty
import           Test.Hspec
import           Text.Megaparsec (ParseErrorBundle)

runOnInput :: String -> String -> Expectation
runOnInput input res =
  fromParse (const $ 1 `shouldBe` 0) (`verifyEval` res) input

runOnStringToString :: String -> String
runOnStringToString = fromParse (const "ERROR!") (evalResToString . getEvalRes)

failParse :: String -> Expectation
failParse = fromParse (const $ 1 `shouldBe` 0) (const $ 0 `shouldBe` 0)

failOnEval :: HiError -> String -> Expectation
failOnEval err =
  fromParse
    (const $ 1 `shouldBe` 0)
    (\expr ->
       case getEvalRes expr of
         Left e  -> e `shouldBe` err
         Right _ -> 0 `shouldBe` 1)

fromParse :: (ParseErrorBundle String Void -> t) -> (HiExpr -> t) -> String -> t
fromParse onFail onSuccess input =
  case parse input of
    Left err   -> onFail err
    Right expr -> onSuccess expr

verifyEval :: HiExpr -> String -> Expectation
verifyEval expr expected =
  case getEvalRes expr of
    Left _    -> 1 `shouldBe` 0
    Right res -> (show . prettyValue) res `shouldBe` expected

evalResToString :: Either HiError HiValue -> String
evalResToString =
  \case
    Left _ -> "ERROR!"
    Right v -> (show . prettyValue) v

getEvalRes :: HiExpr -> Either HiError HiValue
getEvalRes expr = fromMaybe (Left HiErrorInvalidArgument) (eval expr)
