{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TestTask2
  ( test2
  ) where

import           HW3.T1
import           HW3.T2
import           Test.Hspec
import           TestUtil

test2 :: Spec
test2 = do
  describe "Task 2" $ do
    it
      "distOption"
      do
        distOption (Some 2, Some "abc") `shouldBe` Some (2, "abc")
        case distOption (None, Some 1) of
          None -> assertTrue
          _    -> assertFalse
        case distOption (Some "abc", None) of
          None -> assertTrue
          _    -> assertFalse
    it
      "distAnnotated"
      do
        distAnnotated ("123" :# "123", "slsl" :# "abc")
          `shouldBe` (("123", "slsl") :# "123abc")
        distAnnotated (1.9 :# [3], "3.0" :# [3])
          `shouldBe` ((1.9, "3.0") :# [3, 3])
        case distAnnotated (None :# (), "klmn" :# ()) of
          ((None, "klmn") :# ()) -> assertTrue
          _                      -> assertFalse
        distAnnotated (wrapAnnotated 3.6, wrapAnnotated (-10))
          `shouldBe` ((3.6, -10.0) :# "")
        wrapAnnotated (3.6, -10.0) `shouldBe` ((3.6, -10.0) :# "")
    it
      "distPrioritized"
      do
        distPrioritised (Low 3, Low "asdffghk") `shouldBe` Low (3, "asdffghk")
        distPrioritised (Low 3, Medium 0) `shouldBe` Medium (3, 0)
        distPrioritised (Low 3, High "asdffghk") `shouldBe` High (3, "asdffghk")
        distPrioritised (Medium 3, Low "asdffghk")
          `shouldBe` Medium (3, "asdffghk")
        distPrioritised (Medium 3, Medium 2) `shouldBe` Medium (3, 2)
        distPrioritised (Medium 3, High 0) `shouldBe` High (3, 0)
        distPrioritised (High 3, Low 33) `shouldBe` High (3, 33)
        distPrioritised (High "", Medium "asdffghk")
          `shouldBe` High ("", "asdffghk")
        distPrioritised (High 1, High (-9)) `shouldBe` High (1, -9)
    it
      "distList"
      do
        distList (4 :. 3 :. 10 :. Nil, 0 :. 1 :. Nil)
          `shouldBe` (4, 0)
          :. (4, 1)
          :. (3, 0)
          :. (3, 1)
          :. (10, 0)
          :. (10, 1)
          :. Nil
        case distList (Nil, 4 :. 3 :. 10 :. Nil) of
          Nil -> assertTrue
          _   -> assertFalse
        case distList (wrapList 10, Nil) of
          Nil -> assertTrue
          _   -> assertFalse
    it
      "wrapFun"
      do
        let F f = wrapFun "abc"
         in f 239 `shouldBe` "abc"
        let F f = wrapFun 239
         in f 2 `shouldBe` 239
        let F f = wrapFun 239
         in f "abc" `shouldBe` 239
        let F f = wrapFun "abc"
         in f None `shouldBe` "abc"
    it
      "distFun"
      do
        let F f = distFun (F id, F concat)
         in f ["a", "b"] `shouldBe` (["a", "b"], "ab")
        let F f = distFun (F (* (-3)), F (/ 2.0))
         in f 3 `shouldBe` (-9, 1.5)
