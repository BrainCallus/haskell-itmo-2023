{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import           Test.Hspec
import           TestTask1
import           TestTask2
import           TestTask3

main :: IO ()
main = do
  hspec test1
  hspec test2
  hspec test3
