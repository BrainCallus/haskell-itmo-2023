{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances   #-}

module HW5.Sliceable
  ( Sliceable(..)
  ) where

import           Data.ByteString
import           Data.Sequence
import qualified Data.Text       as Text
import           HW5.Base

class Sliceable t where
  lengthS :: t -> Int
  fromIndexS :: Int -> t -> HiValue
  section :: Int -> Int -> t -> HiValue

instance Sliceable Text.Text where
  lengthS = Text.length
  fromIndexS n t = HiValueString (Text.pack [Text.index t n])
  section i1 i2 = HiValueString . Text.take (i2 - i1) . Text.drop i1

instance Sliceable ByteString where
  lengthS = Data.ByteString.length
  fromIndexS n t = HiValueNumber (toRational (Data.ByteString.index t n))
  section i1 i2 =
    HiValueBytes . Data.ByteString.take (i2 - i1) . Data.ByteString.drop i1

instance Sliceable (Seq HiValue) where
  lengthS = Data.Sequence.length
  fromIndexS n s = Data.Sequence.index s n
  section i1 i2 =
    HiValueList . Data.Sequence.take (i2 - i1) . Data.Sequence.drop i1
