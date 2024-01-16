import           PropTask1
import           Test.Tasty (defaultMain, testGroup)
import           TestTask1
import           TestTask2

main :: IO ()
main = do
  testEval >>= \unitEval ->
    testParser >>= \unitParser ->
      let specEval = testGroup "Eval" [unitEval]
          specParser = testGroup "Parser" [unitParser]
          propTests =
            testGroup
              "Property tests exceptState functions"
              [ mapExceptStateTests
              , wrapExceptTests
              , modifyExceptTests
              , throwExceptTests
              , evalTests
              ]
       in defaultMain $ testGroup "All tests" [propTests, specEval, specParser]
