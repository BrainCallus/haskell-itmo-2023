import           BoolTests
import           ByteTests
import           Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  testBools >>= \unitBools ->
    byteTests >>= \unitBytes ->
      let specBools = testGroup "Bools" unitBools
          specBytes = testGroup "Bytes" [unitBytes]
          specBoolProps = testGroup "BoolsProp" [boolProps]
       in defaultMain
            $ testGroup
                "All tests"
                [specBools, specBoolProps, specBytes, bytePropTests]
