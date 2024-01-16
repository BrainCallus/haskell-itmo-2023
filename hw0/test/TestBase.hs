module TestBase
  ( genNatural
  , toTestTree
  , genNatPair
  ) where

import           Hedgehog         (Gen)
import qualified Hedgehog.Gen     as Gen
import qualified Hedgehog.Range   as Range
import           Numeric.Natural  (Natural)
import           Test.Hspec
import           Test.Tasty       (TestTree)
import           Test.Tasty.HUnit (testCase)

toTestTree :: Expectation -> TestTree
toTestTree = testCase "" . hspec . describe "" . it ""

intToNatural :: Int -> Natural
intToNatural x =
  if x < 0
    then 0
    else fromIntegral x

genNatural :: Hedgehog.Gen Natural
genNatural = do
  intToNatural <$> Gen.int (Range.linear 1 100)

genNatPair :: Hedgehog.Gen (Natural, Natural)
genNatPair = do
  x1 <- genNatural
  x2 <- genNatural
  return (x1, x2)
