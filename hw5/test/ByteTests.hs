module ByteTests
  ( byteTests
  , bytePropTests
  ) where

import qualified Data.ByteString       as BS
import           Data.ByteString.Char8 (pack)
import           Hedgehog              (Gen, forAll, property, (===))
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range
import           Test.Hspec
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.Hedgehog   (testProperty)
import           Test.Tasty.Hspec      (testSpec)
import           TestBase

bytePropTests :: TestTree
bytePropTests = testGroup "byte props" [indexTest, sliceTest]

byteTests :: IO TestTree
byteTests = testSpec "test operations with bytes" testBytes

testBytes :: Spec
testBytes = do
  describe "bytes operations" $ do
    it "pack bytes [ 3, 255, 158, 32 ]" $ do
      runOnInput "pack-bytes([ 3, 255, 158, 32 ])" "[# 03 ff 9e 20 #]"
    it "pack-bytes(range(30, 40))" $ do
      runOnInput
        "pack-bytes(range(30, 40))"
        "[# 1e 1f 20 21 22 23 24 25 26 27 28 #]"
    it "unpack-bytes([# 10 20 30 #])" $ do
      runOnInput "unpack-bytes([# 10 20 30 #])" "[ 16, 32, 48 ]"
    it "encode-utf8(\"Hello!\")" $ do
      runOnInput "encode-utf8(\"Hello!\")" "[# 48 65 6c 6c 6f 21 #]"
    it "decode-utf8([# 48 65 6c 6c 6f #])" $ do
      runOnInput "decode-utf8([# 48 65 6c 6c 6f #])" "\"Hello\""
    it "decode-utf8([# c3 28 #])" $ do
      runOnInput "decode-utf8([# c3 28 #])" "null"
    it "decode-utf8([# 68 69 #] * 5)" $ do
      runOnInput "decode-utf8([# 68 69 #] * 5)" "\"hihihihihi\""
    it "[# 00 ff #] + [# 01 e3 #]" $ do
      runOnInput "[# 00 ff #] + [# 01 e3 #]" "[# 00 ff 01 e3 #]"
    it "[# 00 ff #] * 3" $ do
      runOnInput "[# 00 ff #] * 3" "[# 00 ff 00 ff 00 ff #]"
    it "zip(encode-utf8(\"Hello, World!\" * 1000))" $ do
      runOnInput
        "zip(encode-utf8(\"Hello, World!\" * 1000))"
        ("[# 78 da ed c7 31 0d 00 20 0c 00 30 2b f0 23 64 0e 30 00 df 92 25 f3 7f a0 82 af"
           ++ " fd 1a 37 b3 d6 d8 d5 79 66 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88"
           ++ " 88 88 88 88 88 88 88 88 fc c9 03 ca 0f 3b 28 #]")
    it "unzip([# 78 da 63 64 62 06 00 00 0d 00 07 #])" $ do
      runOnInput
        "unzip([# 78 da 63 64 62 06 00 00 0d 00 07 #])"
        "[# 01 02 03 #]"
    it "serialise(\"Hello\")" $ do
      runOnInput "serialise(\"Hello\")" "[# 82 04 65 48 65 6c 6c 6f #]"
    it "serialise(\"hell, world\")" $ do
      runOnInput
        "serialise(\"hell, world\")"
        "[# 82 04 6b 68 65 6c 6c 2c 20 77 6f 72 6c 64 #]"
    it "serialise(\"hell, world\")(1,8)" $ do
      runOnInput
        "serialise(\"hell, world\"(1,8))"
        "[# 82 04 67 65 6c 6c 2c 20 77 6f #]"
    it "deserialise([# 82 04 65 48 65 6c 6c 6f #])" $ do
      runOnInput "deserialise([# 82 04 65 48 65 6c 6c 6f #])" "\"Hello\""
    it "deserialise([# 82 04 65 68 65 6c 6c 6f #])" $ do
      runOnInput "deserialise([# 82 04 65 68 65 6c 6c 6f #])" "\"hello\""

indexTest, sliceTest :: TestTree
indexTest =
  testProperty
    "index bytes"
    (property $ do
       forAll genInt >>= \x -> do
         runOnStringToString
           ("encode-utf8(\"HelloHelloHello\")(" ++ show x ++ ")")
           === if x < 0 || x > 14
                 then "null"
                 else show $ BS.index (pack "HelloHelloHello") x)

sliceTest =
  testProperty
    "slice bytes"
    (property $ do
       forAll genIntPair >>= \xx -> do
         runOnStringToString
           ("decode-utf8(encode-utf8(\"HelloHelloHello\")("
              ++ show (fst xx)
              ++ ","
              ++ show (snd xx)
              ++ "))")
           === uncurry (getSlicedBs "HelloHelloHello") xx)

getSlicedBs :: String -> Int -> Int -> String
getSlicedBs s x1 x2 =
  let idx1 = toSliceIndex x1 (length s)
      idx2 = toSliceIndex x2 (length s)
      bs =
        BS.take
          (max idx1 idx2 - min idx1 idx2)
          (BS.drop (min idx1 idx2) (pack s))
   in show bs

toSliceIndex :: Int -> Int -> Int
toSliceIndex i len
  | i > len = len
  | i < 0 && abs i > len = 0
  | i < 0 = len + i
  | otherwise = i

genIntPair :: Hedgehog.Gen (Int, Int)
genIntPair = do
  x1 <- genInt
  x2 <- genInt
  return (x1, x2)

genInt :: Hedgehog.Gen Int
genInt = Gen.int (Range.linear 0 1000)
