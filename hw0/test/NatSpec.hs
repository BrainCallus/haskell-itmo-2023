module NatSpec
  ( natTests
  ) where

import           Hedgehog            (Gen, forAll, property, withTests, (===))
import           HW0.T5
import           Numeric.Natural     (Natural)
import           Test.Hspec
import           Test.Tasty          (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)
import           TestBase

natTests :: TestTree
natTests =
  testGroup "nat tests" [toFromTest, nsTest, nplusTest, nmultTest, nzTest]

toFromTest, nsTest, nplusTest, nmultTest, nzTest :: TestTree
toFromTest =
  testProperty "to-from test"
    $ withTests 10
    $ property
    $ do
        forAll limitedNatural >>= \x -> do
          x === nToNum (nFromNatural x)

nsTest =
  testProperty "ns test"
    $ withTests 10
    $ property
    $ do
        forAll limitedNatural >>= \x -> do
          x + 1 === nToNum (ns (nFromNatural x))

nplusTest =
  testProperty "nplus test"
    $ withTests 10
    $ property
    $ do
        forAll limitedNatPair >>= \(x, y) -> do
          x + y === nToNum (nplus (nFromNatural x) (nFromNatural y))

nmultTest =
  testProperty "nmult test"
    $ withTests 10
    $ property
    $ do
        forAll limitedNatPair >>= \(x, y) -> do
          x * y === nToNum (nmult (nFromNatural x) (nFromNatural y))

nzTest =
  testGroup "nz test"
    $ toTestTree . (\(x, y) -> nz y x `shouldBe` x)
        <$> [ ("abc", const "")
            , ("123", mconcat . replicate 10)
            , ("lol", const "kek")
            , ("kek", reverse)
            ]

limitedNatural :: Hedgehog.Gen Natural
limitedNatural = (`max` 10) <$> genNatural

limitedNatPair :: Hedgehog.Gen (Natural, Natural)
limitedNatPair = (\(x, y) -> (max x 10, max y 10)) <$> genNatPair
