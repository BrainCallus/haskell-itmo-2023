import           ChtTest
import           Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  testCht >>= \unitCht -> defaultMain (testGroup "cht" [unitCht])
