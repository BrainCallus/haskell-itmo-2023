{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TestTask3
  ( test3
  ) where

import           HW3.T1
import           HW3.T2
import           HW3.T3
import           Test.Hspec
import           TestUtil

test3 :: Spec
test3 = do
  describe "Task 3" $ do
    it
      "joinOption"
      do
        joinOption (Some (Some 24)) `shouldBe` Some 24
        case joinOption (Some (Some (P None None))) of
          Some (P None None) -> assertTrue
          _                  -> assertFalse
        case joinOption None of
          None -> assertTrue
          _    -> assertFalse
        case joinOption (Some None) of
          None -> assertTrue
          _    -> assertFalse
    it
      "joinExcept"
      do
        case joinExcept (Error 123) of
          Error 123 -> assertTrue
          _         -> assertFalse
        case joinExcept (Error (Success "123")) of
          Error (Success "123") -> assertTrue
          _                     -> assertFalse
        case joinExcept (Error (Error [])) of
          Error (Error []) -> assertTrue
          _                -> assertFalse
        case joinExcept (Success (Error 123)) of
          Error 123 -> assertTrue
          _         -> assertFalse
        case joinExcept (Success (wrapExcept "fail")) of
          Success "fail" -> assertTrue
          _              -> assertFalse
        case joinExcept (Error (Error (Error "a"))) of
          Error (Error (Error "a")) -> assertTrue
          _                         -> assertFalse
        case joinExcept (Error (Success (Success "a"))) of
          Error (Success (Success "a")) -> assertTrue
          _                             -> assertFalse
        case joinExcept (joinExcept (Error (Success (Success "a")))) of
          Error (Success (Success "a")) -> assertTrue
          _                             -> assertFalse
        case joinExcept (Success (Success (Error "a"))) of
          Success (Error "a") -> assertTrue
          _                   -> assertFalse
        case joinExcept
               $ joinExcept
               $ joinExcept
               $ joinExcept
               $ Success (Success (Error (Success "a"))) of
          Error (Success "a") -> assertTrue
          _                   -> assertFalse
        case joinExcept
               $ joinExcept
               $ joinExcept
               $ joinExcept
               $ Success (Success (Error (Success "a"))) of
          Error (Success "a") -> assertTrue
          _                   -> assertFalse
        case joinExcept $ mapExcept Error (Success "1") of
          (Error "1") -> assertTrue
          _           -> assertFalse
        case joinExcept $ mapExcept Success (Error "1") of
          (Error "1") -> assertTrue
          _           -> assertFalse
        case wrapExcept $ joinExcept $ mapExcept Success (Error "1") of
          Success (Error "1") -> assertTrue
          _                   -> assertFalse
    it
      "joinAnnotated"
      do
        joinAnnotated ((2 :# "12") :# "f") `shouldBe` (2 :# "f12")
        joinAnnotated (((2 :# "12") :# "only") :# "external ")
          `shouldBe` ((2 :# "12") :# "external only")
        joinAnnotated (("list" :# [1, 3, 0]) :# [0, 4])
          `shouldBe` ("list" :# [0, 4, 1, 3, 0])
    it
      "joinList"
      do
        joinList (wrapList 1 :. Nil) `shouldBe` 1 :. Nil
        case joinList (wrapList Nil) of
          Nil -> assertTrue
          _   -> assertFalse
        case joinList Nil of
          Nil -> assertTrue
          _   -> assertFalse
        joinList ((2 :. 3 :. 0 :. Nil) :. Nil :. (9 :. Nil) :. Nil)
          `shouldBe` 2
          :. 3
          :. 0
          :. 9
          :. Nil
    it
      "joinFun"
      do
        let toFun = (\_ -> F (* 3))
         in let F g = joinFun (F toFun)
             in (g 4 `shouldBe` 12) >> (g 0 `shouldBe` 0)
        let toFun = (\x -> F (* x))
         in let F g = joinFun (F toFun)
             in g 4 `shouldBe` 16
        let toFun = (F . replicate)
         in let F g = joinFun (F toFun)
             in (g 3 `shouldBe` [3, 3, 3])
                  >> (g 0 `shouldBe` [])
                  >> (g 2 `shouldBe` [2, 2])
