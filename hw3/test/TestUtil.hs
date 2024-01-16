{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TestUtil
  ( assertFalse
  , assertTrue
  ) where

import           Test.Hspec

assertTrue, assertFalse :: Expectation
assertTrue = 1 `shouldBe` 1

assertFalse = 0 `shouldBe` 1
