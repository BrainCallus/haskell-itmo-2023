{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module ChtTest
  ( testCht
  ) where

import           HW6.T1

import           Control.Concurrent        (forkIO)
import           Control.Concurrent.Classy (STM, threadDelay)
import           Control.Monad             (forM_)
import           Data.Functor              ((<&>))
import           Test.Hspec                (Spec, describe, it, shouldBe)
import           Test.Tasty.Hspec          (testSpec)
import           Test.Tasty.Providers      (TestTree)

testCht :: IO TestTree
testCht = testSpec "Concurrent test" chtTest

chtTest :: Spec
chtTest = do
  describe "Hash table test" $ do
    let elems = [1 .. 100]
    it "should work correct in 1 thread" $ do
      cht <- newCHT :: IO (CHT (STM IO) String Int)
      runPutCHT elems cht
      checkCHT 100 256 cht
      checkValues elems cht
    it "should contains only unique keys" $ do
      cht <- newCHT :: IO (CHT (STM IO) String Int)
      forM_ elems (\_ -> putCHT "elem" 0 cht)
      checkCHT 1 16 cht
    it "should work correct in 3 threads" $ do
      cht <- newCHT :: IO (CHT (STM IO) String Int)
      _ <-
        forkIO $ do
          runPutCHT elems cht
      _ <-
        forkIO $ do
          runPutCHT [201 .. 300] cht
      _ <-
        forkIO $ do
          runPutCHT elems cht
      threadDelay 1000000
      checkCHT 200 512 cht
    it "should be able to provide get and put in several threads" $ do
      cht <- newCHT :: IO (CHT (STM IO) String Int)
      _ <-
        forkIO $ do
          runPutCHT elems cht
      _ <-
        forkIO $ do
          forM_ (reverse elems) (\e -> getCHT (show e) cht)
      _ <-
        forkIO $ do
          forM_ elems (\e -> getCHT (show e) cht)
      _ <-
        forkIO $ do
          runPutCHT (reverse elems) cht
      threadDelay 1000000
      checkCHT 100 256 cht
    it "should change values for same keys" $ do
      cht <- newCHT :: IO (CHT (STM IO) String Int)
      forM_ elems (\e -> putCHT (show e) (e * 1000) cht)
      runPutCHT elems cht
      checkValues elems cht

checkValues :: [Int] -> CHT (STM IO) String Int -> IO ()
checkValues elems cht =
  forM_
    elems
    (\e -> maybe (False `shouldBe` True) (`shouldBe` e) <$> getCHT (show e) cht)

runPutCHT :: [Int] -> CHT (STM IO) String Int -> IO ()
runPutCHT elems cht = forM_ elems (\e -> putCHT (show e) e cht)

checkCHT :: Int -> Int -> CHT (STM IO) k v -> IO ()
checkCHT exSize exCap cht = do
  sz <- sizeCHT cht
  lenElems <- getEntries cht <&> length
  cap <- capacityCHT cht
  sz `shouldBe` exSize
  lenElems `shouldBe` exSize
  cap `shouldBe` exCap
