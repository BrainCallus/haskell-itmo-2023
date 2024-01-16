module PropTask1
  ( mapExceptStateTests
  , wrapExceptTests
  , modifyExceptTests
  , throwExceptTests
  , evalTests
  ) where

import           Hedgehog            (Gen, Property, forAll, property, (===))
import           Test.Tasty          (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import           HW4.T1
import           HW4.Types

import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

evalTests :: TestTree
evalTests = testGroup "eval" [evalDBZ]

evalDBZ :: TestTree
evalDBZ =
  testProperty
    "eval with division by zero always produce \"Error DivideByZero\" "
    (property $ do
       forAll genDouble
         >>= (\d ->
                runES (eval (Op (Div (Val d) (Val 0)))) []
                  === Error DivideByZero))

throwExceptTests :: TestTree
throwExceptTests =
  testGroup
    "throwExceptState"
    [ testProperty
        "throwExceptState throw Error with given argument"
        throwExceptTest
    ]

throwExceptTest :: Property
throwExceptTest =
  property $ do
    forAll genInt
      >>= (\err -> do
             forAll genString
               >>= (\i ->
                      case runES (throwExceptState err) i of
                        Error e -> e === err
                        _       -> True === False))

modifyExceptTests :: TestTree
modifyExceptTests =
  testGroup
    "modifyExceptState"
    (map
       (\fn ->
          testProperty
            ("modifyExceptState with function " ++ snd fn)
            (modifyExceptTest (fst fn)))
       stringFunctions)

modifyExceptTest :: (String -> String) -> Property
modifyExceptTest fun =
  property $ do
    forAll genString
      >>= (\s ->
             case runES (modifyExceptState fun) s of
               Success (() :# ss) -> ss === fun s
               _                  -> True === False)

wrapExceptTests :: TestTree
wrapExceptTests =
  testGroup
    "wrapExceptState"
    [testProperty "wrapExceptState wrap in Success" wrapExceptTest]

wrapExceptTest :: Property
wrapExceptTest =
  property $ do
    forAll genString >>= \s -> do
      forAll genInt
        >>= (\i ->
               case runES (wrapExceptState i) s of
                 Success (ii :# annotat) -> (ii, annotat) === (i, s)
                 _                       -> True === False)

mapExceptStateTests :: TestTree
mapExceptStateTests = testGroup "mapExceptState" mapExceptStateTest

mapExceptStateTest :: [TestTree]
mapExceptStateTest =
  let annotations = ["", "abc", "123"]
   in foldMap
        (\annotation ->
           map
             (\fn ->
                testProperty
                  ("mapExceptState map result with function "
                     ++ snd fn
                     ++ ", annotation "
                     ++ annotation
                     ++ " immutable")
                  (mapExceptTest (fst fn) annotation))
             stringFunctions)
        annotations

mapExceptTest :: (String -> String) -> String -> Property
mapExceptTest func an = do
  let state =
        ES
          (\s ->
             if length s < 10
               then Error "to short"
               else Success (s :# an))
   in property
        $ forAll genString
            >>= (\s ->
                   runES (mapExceptState func state) s
                     === (if length s >= 10
                            then Success (func s :# an)
                            else Error "to short"))

stringFunctions :: [(String -> String, String)]
stringFunctions =
  [ (id, "id")
  , (const "", "const \"\"")
  , (const "123456789", "const \"123456789\"")
  , (concat . replicate 3, "concat . replicate 3")
  , ((++) "123456789", "(++) \"123456789\"")
  ]

genString :: Hedgehog.Gen String
genString =
  let listLength = Range.linear 0 100
   in Gen.list listLength Gen.alpha

genInt :: Hedgehog.Gen [Int]
genInt =
  let listLength = Range.linear 0 100
   in Gen.list listLength (Gen.int (Range.linear 0 100000))

genDouble :: Hedgehog.Gen Double
genDouble = Gen.double (Range.constant 0 1000000)
