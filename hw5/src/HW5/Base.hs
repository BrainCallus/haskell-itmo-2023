{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module HW5.Base
  ( HiFun(..)
  , hiFunMap
  , HiValue(..)
  , HiExpr(..)
  , HiError(..)
  , HiAction(..)
  , HiMonad(..)
  ) where

import           Codec.Serialise (Serialise)
import           Data.ByteString (ByteString)
import           Data.Map        (Map, fromList)
import           Data.Sequence   (Seq)
import           Data.Text       (Text)
import           Data.Time       (UTCTime)
import           GHC.Generics    (Generic)

data HiFun
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Eq, Ord, Show, Serialise, Generic, Bounded)

hiFunMap :: Map HiFun String
hiFunMap =
  fromList
    [ (HiFunDiv, "div")
    , (HiFunMul, "mul")
    , (HiFunAdd, "add")
    , (HiFunSub, "sub")
    , (HiFunNot, "not")
    , (HiFunAnd, "and")
    , (HiFunOr, "or")
    , (HiFunLessThan, "less-than")
    , (HiFunGreaterThan, "greater-than")
    , (HiFunEquals, "equals")
    , (HiFunNotLessThan, "not-less-than")
    , (HiFunNotGreaterThan, "not-greater-than")
    , (HiFunNotEquals, "not-equals")
    , (HiFunIf, "if")
    , (HiFunLength, "length")
    , (HiFunToUpper, "to-upper")
    , (HiFunToLower, "to-lower")
    , (HiFunReverse, "reverse")
    , (HiFunTrim, "trim")
    , (HiFunList, "list")
    , (HiFunRange, "range")
    , (HiFunFold, "fold")
    , (HiFunPackBytes, "pack-bytes")
    , (HiFunUnpackBytes, "unpack-bytes")
    , (HiFunEncodeUtf8, "encode-utf8")
    , (HiFunDecodeUtf8, "decode-utf8")
    , (HiFunZip, "zip")
    , (HiFunUnzip, "unzip")
    , (HiFunSerialise, "serialise")
    , (HiFunDeserialise, "deserialise")
    , (HiFunRead, "read")
    , (HiFunWrite, "write")
    , (HiFunMkDir, "mkdir")
    , (HiFunChDir, "cd")
    , (HiFunParseTime, "parse-time")
    , (HiFunRand, "rand")
    , (HiFunEcho, "echo")
    , (HiFunCount, "count")
    , (HiFunKeys, "keys")
    , (HiFunValues, "values")
    , (HiFunInvert, "invert")
    ]

data HiValue
  = HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Eq, Ord, Show, Serialise, Generic)

data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Eq, Ord, Show, Serialise, Generic)

data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Eq, Show)

data HiAction
  = HiActionRead FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Eq, Ord, Show, Serialise, Generic)

class Monad m =>
      HiMonad m
  where
  runAction :: HiAction -> m HiValue

instance HiMonad Maybe -- god..
                                where
  runAction = return (Just HiValueNull)
