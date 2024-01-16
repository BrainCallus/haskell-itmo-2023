{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TestTask1
  ( test1
  ) where

import           HW3.T1
import           HW3.T2
import           Test.Hspec
import           TestUtil

test1 :: Spec
test1 = do
  describe "Task 1:" $ do
    it
      "mapOption"
      do
        mapOption (++ "abc") None `shouldBe` None
        mapOption (/ 8) (Some 2.0) `shouldBe` Some 0.25
    it
      "mapPair"
      do
        mapPair (* 5) (P 0 10) `shouldBe` P 0 50
        mapPair (replicate 3 . (* (-2))) (P (-3.5) (sqrt 3))
          `shouldBe` P [7, 7, 7] [-2 * sqrt 3, -2 * sqrt 3, -2 * sqrt 3]
        mapPair (mapPair (* 3)) (P (P 3 4) (P 1 2))
          `shouldBe` P (P 9 12) (P 3 6)
    it
      "mapQuad"
      do
        mapQuad (* 5) (Q 0 10 (-8.5) 0.5) `shouldBe` Q 0 50 (-42.5) 2.5
        mapQuad (replicate 3 . (* (-2))) (Q (-3.5) (sqrt 3) 0 1)
          `shouldBe` Q [7, 7, 7]
                       [-2 * sqrt 3, -2 * sqrt 3, -2 * sqrt 3]
                       [0, 0, 0]
                       [-2, -2, -2]
        mapQuad
          (mapQuad (* 1))
          (Q (Q 3 4 5 6) (Q 1 2 (-10) 1.1) (Q 5 5 5 5) (Q 1 (-2) 3 (-8)))
          `shouldBe` Q (Q 3 4 5 6)
                       (Q 1 2 (-10) 1.1)
                       (Q 5 5 5 5)
                       (Q 1 (-2) 3 (-8))
    it
      "mapQuad"
      do
        mapAnnotated wrapPair ("something" :# "annotated")
          `shouldBe` (P "something" "something" :# "annotated")
        mapAnnotated (+ 10) (3 :# 'c') `shouldBe` (13 :# 'c')
    it
      "mapExcept"
      do
        mapExcept (++ "descript") (Error "err") `shouldBe` Error "err"
        mapExcept (+ 3) (Error 3) `shouldBe` Error 3
        case mapExcept id (Success None) of
          Success None -> assertTrue
          _            -> assertFalse
        case mapExcept (: []) (Success "abc") of
          Success ["abc"] -> assertTrue
          _               -> assertFalse
    it
      "mapFun"
      do
        let F g = mapFun (+ 2) (F (* 2))
         in g 10 `shouldBe` 22
        let F g = mapFun (foldl (\x y -> x ++ y ++ "0") "") (F (replicate 5))
         in g "1" `shouldBe` "1010101010"
        let F g = mapFun (foldr (\x y -> x ++ y ++ "0") "") (F (replicate 5))
         in g "1" `shouldBe` "1111100000"
    it
      "mapTree"
      do
        let tree = Branch (Branch Leaf 0 (Branch Leaf (-6) Leaf)) 33 Leaf
         in (mapTree (+ 1) tree
               `shouldBe` Branch (Branch Leaf 1 (Branch Leaf (-5) Leaf)) 34 Leaf)
              >> (mapTree (: []) tree
                    `shouldBe` Branch
                                 (Branch Leaf [0] (Branch Leaf [-6] Leaf))
                                 [33]
                                 Leaf)
              >> (mapTree show tree
                    `shouldBe` Branch
                                 (Branch Leaf "0" (Branch Leaf "-6" Leaf))
                                 "33"
                                 Leaf)
              >> (mapTree (\x -> Branch Leaf x Leaf) tree
                    `shouldBe` Branch
                                 (Branch
                                    Leaf
                                    (Branch Leaf 0 Leaf)
                                    (Branch Leaf (Branch Leaf (-6) Leaf) Leaf))
                                 (Branch Leaf 33 Leaf)
                                 Leaf)
        mapTree (+ 1) (Branch (Branch Leaf 0 (Branch Leaf (-6) Leaf)) 33 Leaf)
          `shouldBe` Branch (Branch Leaf 1 (Branch Leaf (-5) Leaf)) 34 Leaf
        mapTree (* 5) Leaf `shouldBe` Leaf
    it
      "mapList"
      do
        case mapList (* 9) Nil of
          Nil -> assertTrue
          _   -> assertFalse
        mapList (replicate 2) ("a" :. "ab" :. Nil)
          `shouldBe` ["a", "a"]
          :. ["ab", "ab"]
          :. Nil
        mapList
          (:. Nil)
          ((0 :. Nil) :. (8 :. 9 :. Nil) :. Nil :. (1 :. 2 :. Nil) :. Nil)
          `shouldBe` ((0 :. Nil) :. Nil)
          :. ((8 :. (9 :. Nil)) :. Nil)
          :. (Nil :. Nil)
          :. ((1 :. (2 :. Nil)) :. Nil)
          :. Nil
