import           FacFibSpec
import           NatSpec
import           Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain (testGroup "All tests" [sampleFibTest, facTest, natTests])
