{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HW5.Evaluator
  ( eval
  ) where

import           Codec.Compression.Zlib
import           Codec.Serialise            (deserialise, serialise)
import           Control.Monad
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import           Data.Foldable              (toList)
import           Data.Function              (on)
import           Data.Map                   (Map, fromList, (!))
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Ratio                 (denominator, numerator)
import           Data.Semigroup             (stimes)
import           Data.Sequence              (Seq (Empty, (:<|)), empty,
                                             singleton, (><), (|>))
import qualified Data.Sequence              as SQ
import           Data.Text                  (strip, toLower, toUpper)
import qualified Data.Text                  as Txt
import           Data.Text.Encoding         (decodeUtf8', encodeUtf8)
import           Data.Time                  (diffUTCTime)
import           Data.Time.Clock            (addUTCTime)
import           Data.Word                  (Word8)
import           HW5.Base
import           HW5.Sliceable
import           Text.Read                  (readMaybe)

type HiExT m a = ExceptT HiError m a

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalExpression

evalExpression :: HiMonad m => HiExpr -> HiExT m HiValue
evalExpression =
  \case
    HiExprValue value -> return value
    HiExprApply expr args -> evalApplicative expr args
    HiExprRun expr -> evalAction expr
    HiExprDict dict -> evalDict dict

evalApplicative :: HiMonad m => HiExpr -> [HiExpr] -> HiExT m HiValue
evalApplicative expr args = do
  f <- evalExpression expr
  case f of
    HiValueString s     -> evalSliceable s args
    HiValueBytes b      -> evalSliceable b args
    HiValueList l       -> evalSliceable l args
    HiValueDict d       -> evalDictGet d args
    HiValueFunction fun -> (!) descriptionMap fun args
    _                   -> throwE HiErrorInvalidFunction

evalAction :: HiMonad m => HiExpr -> HiExT m HiValue
evalAction expr = do
  act <- evalExpression expr
  case act of
    HiValueAction action -> lift (runAction action)
    _                    -> throwInvalidArg

evalDict :: HiMonad m => [(HiExpr, HiExpr)] -> HiExT m HiValue
evalDict dict =
  HiValueDict . Map.fromList
    <$> mapM (uncurry (on (liftM2 (,)) evalExpression)) dict

evalDictGet :: HiMonad m => Map HiValue HiValue -> [HiExpr] -> HiExT m HiValue
evalDictGet dict =
  \case
    [a] -> do
      key <- evalExpression a
      return (fromMaybe HiValueNull (Map.lookup key dict))
    _ -> throwInvalidArg

evalSliceable :: (HiMonad m, Sliceable s) => s -> [HiExpr] -> HiExT m HiValue
evalSliceable s =
  \case
    [a] -> getIndex s a
    [a1, a2] -> getSlice s a1 a2
    _ -> throwE HiErrorArityMismatch

getSlice :: (HiMonad m, Sliceable s) => s -> HiExpr -> HiExpr -> HiExT m HiValue
getSlice s ex1 ex2 = do
  i1 <- evalExpression ex1
  i2 <- evalExpression ex2
  case (i1, i2) of
    (HiValueNumber start, HiValueNumber end) -> evalSlice s start end
    (HiValueNull, HiValueNumber end) -> evalSlice s 0 end
    (HiValueNumber start, HiValueNull) ->
      evalSlice s start (toRational (lengthS s))
    _ -> throwInvalidArg

evalSlice ::
     (HiMonad m, Sliceable s) => s -> Rational -> Rational -> HiExT m HiValue
evalSlice s i1 i2 =
  if isInt i1 && isInt i2
    then let toSliceIndex i len
               | i > len = len
               | i < 0 && abs i > len = 0
               | i < 0 = len + i
               | otherwise = i
             idx1 = toSliceIndex (rationalToInt i1) (lengthS s)
             idx2 = toSliceIndex (rationalToInt i2) (lengthS s)
          in return (section (min idx1 idx2) (max idx1 idx2) s)
    else throwInvalidArg

getIndex :: (HiMonad m, Sliceable s) => s -> HiExpr -> HiExT m HiValue
getIndex s expr = do
  i <- evalExpression expr
  case i of
    (HiValueNumber idx) ->
      return
        (if idx >= 0 && (rationalToInt idx < lengthS s)
           then fromIndexS (rationalToInt idx) s
           else HiValueNull)
    _ -> throwInvalidArg

descriptionMap :: HiMonad m => Map HiFun ([HiExpr] -> HiExT m HiValue)
descriptionMap =
  fromList
    [ (HiFunDiv, evalDiv)
    , (HiFunMul, evalMul)
    , (HiFunAdd, evalAdd)
    , (HiFunSub, evalSub)
    , (HiFunNot, evalNot)
    , (HiFunAnd, binFunction lazyAnd)
    , (HiFunOr, binFunction lazyOr)
    , (HiFunLessThan, evalLessThan)
    , (HiFunGreaterThan, evalGreaterThan)
    , (HiFunEquals, evalEq)
    , (HiFunNotLessThan, evalNotLessThan)
    , (HiFunNotGreaterThan, evalNotGreaterThan)
    , (HiFunNotEquals, evalNEq)
    , (HiFunIf, ternaryFunction evalIf)
    , (HiFunLength, evalLength)
    , (HiFunToUpper, evalToUpper)
    , (HiFunToLower, evalToLower)
    , (HiFunReverse, evalReverse)
    , (HiFunTrim, evalTrim)
    , (HiFunList, mkList)
    , (HiFunRange, evalRange)
    , (HiFunFold, evalFold)
    , (HiFunPackBytes, evalPackBytes)
    , (HiFunUnpackBytes, evalUnpackBytes)
    , (HiFunEncodeUtf8, evalEncodeUtf8)
    , (HiFunDecodeUtf8, evalDecodeUtf8)
    , (HiFunZip, evalZip)
    , (HiFunUnzip, evalUnzip)
    , (HiFunSerialise, evalSerialise)
    , (HiFunDeserialise, evalDeserialise)
    , (HiFunRead, evalRead)
    , (HiFunWrite, evalWrite)
    , (HiFunMkDir, evalMkDir)
    , (HiFunChDir, evalChDir)
    , (HiFunParseTime, evalParseTime)
    , (HiFunRand, evalRand)
    , (HiFunEcho, evalEcho)
    , (HiFunCount, evalCount)
    , (HiFunKeys, evalKeys)
    , (HiFunValues, evalValues)
    , (HiFunInvert, evalInvert)
    ]

mkList :: HiMonad m => [HiExpr] -> HiExT m HiValue
mkList args =
  HiValueList
    <$> foldM
          (\acc expr -> do
             value <- evalExpression expr
             return (acc |> value))
          empty
          args

unoFunction :: HiMonad m => (HiValue -> HiExT m a) -> [HiExpr] -> HiExT m a
unoFunction f =
  \case
    [a] -> evalExpression a >>= f
    _ -> throwE HiErrorArityMismatch

binFunction ::
     HiMonad m => (HiExpr -> HiExpr -> HiExT m a) -> [HiExpr] -> HiExT m a
binFunction f =
  \case
    [a1, a2] -> f a1 a2
    _ -> throwE HiErrorArityMismatch

binaryNonLazy ::
     HiMonad m => ((HiValue, HiValue) -> HiExT m a) -> [HiExpr] -> HiExT m a
binaryNonLazy f =
  binFunction
    (\expr1 expr2 -> do
       v1 <- evalExpression expr1
       v2 <- evalExpression expr2
       f (v1, v2))

ternaryFunction ::
     HiMonad m
  => (HiExpr -> HiExpr -> HiExpr -> HiExT m a)
  -> [HiExpr]
  -> HiExT m a
ternaryFunction f =
  \case
    [a1, a2, a3] -> f a1 a2 a3
    _ -> throwE HiErrorArityMismatch

actionUnary :: (FilePath -> HiAction) -> Txt.Text -> HiValue
actionUnary action = HiValueAction . action . Txt.unpack

getDictEntry ::
     HiMonad m
  => (Map HiValue HiValue -> [HiValue])
  -> Map HiValue HiValue
  -> HiExT m HiValue
getDictEntry f = return . HiValueList . SQ.fromList . f

unaryWithTextSeqOrBytes ::
     HiMonad m
  => (Txt.Text -> HiExT m HiValue)
  -> (Seq HiValue -> HiExT m HiValue)
  -> (ByteString -> HiExT m HiValue)
  -> [HiExpr]
  -> HiExT m HiValue
unaryWithTextSeqOrBytes textFun listFun byteFun =
  unoFunction
    (\case
       HiValueString t -> textFun t
       HiValueList l -> listFun l
       HiValueBytes b -> byteFun b
       _ -> throwInvalidArg)

evalLength, evalReverse, evalCount :: HiMonad m => [HiExpr] -> HiExT m HiValue
evalLength =
  unaryWithTextSeqOrBytes
    (internalGetLength Txt.length)
    (internalGetLength SQ.length)
    (internalGetLength BS.length)

evalReverse =
  unaryWithTextSeqOrBytes
    (internalReverse Txt.reverse HiValueString)
    (internalReverse SQ.reverse HiValueList)
    (internalReverse BS.reverse HiValueBytes)

evalCount =
  let textFun =
        internalEvalCount Txt.unpack (\ch -> HiValueString (Txt.pack [ch]))
      listFun = internalEvalCount Data.Foldable.toList id
      byteFun = internalEvalCount BS.unpack (HiValueNumber . toRational)
   in unaryWithTextSeqOrBytes textFun listFun byteFun

internalEvalCount ::
     (Ord t1, HiMonad m)
  => (t -> [t1])
  -> (t1 -> HiValue)
  -> t
  -> HiExT m HiValue
internalEvalCount toListT toHi t =
  (return . HiValueDict . Map.mapKeys toHi)
    (Map.map HiValueNumber (Map.fromListWith (+) $ zip (toListT t) (repeat 1)))

evalInvert' :: HiMonad m => Map HiValue HiValue -> HiExT m HiValue
evalInvert' dict =
  (return . HiValueDict)
    (Map.map
       HiValueList
       (Map.fromListWith
          (><)
          (zip (Map.elems dict) (singleton <$> Map.keys dict))))

internalReverse ::
     HiMonad m => (t -> t) -> (t -> HiValue) -> t -> HiExT m HiValue
internalReverse f constr = return . constr . f

internalGetLength :: HiMonad m => (t -> Int) -> t -> HiExT m HiValue
internalGetLength f t = return (HiValueNumber (toRational (f t)))

internalPackBytes :: HiMonad m => Seq HiValue -> HiExT m HiValue
internalPackBytes list =
  if areAllBytes list
    then (return . HiValueBytes . BS.pack . toW8List) list
    else throwInvalidArg

areAllBytes :: Seq HiValue -> Bool
areAllBytes =
  all
    (\case
       HiValueNumber n -> 0 <= n && n <= 255
       _ -> False)

toW8List :: Seq HiValue -> [Word8]
toW8List =
  foldr
    (\hi acc ->
       case hi of
         (HiValueNumber n) -> fromInteger (numerator n) : acc
         _                 -> undefined)
    []

zipUnzip :: Bool -> ByteString -> HiValue
zipUnzip isZip bytes =
  HiValueBytes
    (toStrict
       $ (if isZip
            then compressWith
                   defaultCompressParams {compressLevel = bestCompression}
            else decompressWith defaultDecompressParams)
           (fromStrict bytes))

evalNot, evalPackBytes, evalSerialise ::
     HiMonad m => [HiExpr] -> HiExT m HiValue
evalNot =
  unoFunction
    (\case
       HiValueBool b -> (return . HiValueBool) (not b)
       _ -> throwInvalidArg)

evalPackBytes =
  unoFunction
    (\case
       HiValueList list -> internalPackBytes list
       _ -> throwInvalidArg)

evalSerialise = unoFunction (return . HiValueBytes . toStrict . serialise)

evalDecodeUtf8, evalUnpackBytes, evalZip, evalUnzip, evalDeserialise ::
     HiMonad m => [HiExpr] -> HiExT m HiValue
evalDecodeUtf8 =
  unaryByteStringArg
    (\bs ->
       case decodeUtf8' bs of
         Right c -> HiValueString c
         Left _  -> HiValueNull)

evalUnpackBytes =
  unaryByteStringArg
    (\bs ->
       HiValueList (SQ.fromList (HiValueNumber . toRational <$> BS.unpack bs)))

evalZip = unaryByteStringArg (zipUnzip True)

evalUnzip = unaryByteStringArg (zipUnzip False)

evalDeserialise = unaryByteStringArg (deserialise . fromStrict)

unaryByteStringArg ::
     HiMonad m => (ByteString -> HiValue) -> [HiExpr] -> HiExT m HiValue
unaryByteStringArg f =
  unoFunction
    (\case
       HiValueBytes bs -> return $ f bs
       _ -> throwInvalidArg)

evalTrim, evalToLower, evalToUpper, evalEcho, evalParseTime, evalRead, evalMkDir, evalChDir, evalEncodeUtf8 ::
     HiMonad m => [HiExpr] -> HiExT m HiValue
evalTrim = unaryStringArg (HiValueString . strip)

evalToLower = unaryStringArg (HiValueString . toLower)

evalToUpper = unaryStringArg (HiValueString . toUpper)

evalEcho = unaryStringArg (HiValueAction . HiActionEcho)

evalParseTime =
  unaryStringArg (maybe HiValueNull HiValueTime . (readMaybe . Txt.unpack))

evalRead = unaryStringArg (actionUnary HiActionRead)

evalMkDir = unaryStringArg (actionUnary HiActionMkDir)

evalChDir = unaryStringArg (actionUnary HiActionChDir)

evalEncodeUtf8 = unaryStringArg (HiValueBytes . encodeUtf8)

unaryStringArg ::
     HiMonad m => (Txt.Text -> HiValue) -> [HiExpr] -> HiExT m HiValue
unaryStringArg f =
  unoFunction
    (\case
       HiValueString txt -> return $ f txt
       _ -> throwInvalidArg)

evalKeys, evalValues, evalInvert :: HiMonad m => [HiExpr] -> HiExT m HiValue
evalKeys = unaryDictArg (getDictEntry Map.keys)

evalValues = unaryDictArg (getDictEntry Map.elems)

evalInvert = unaryDictArg evalInvert'

unaryDictArg ::
     HiMonad m
  => (Map HiValue HiValue -> HiExT m HiValue)
  -> [HiExpr]
  -> HiExT m HiValue
unaryDictArg f =
  unoFunction
    (\case
       HiValueDict d -> f d
       _ -> throwInvalidArg)

lazyOr, lazyAnd :: HiMonad m => HiExpr -> HiExpr -> HiExT m HiValue
lazyOr ex1 ex2 = do
  res1 <- evalExpression ex1
  if falseNulable res1
    then evalExpression ex2
    else return res1

lazyAnd ex1 ex2 = do
  res1 <- evalExpression ex1
  if falseNulable res1
    then return res1
    else evalExpression ex2

evalAdd, evalSub, evalMul, evalDiv, evalRand, evalWrite, evalRange, evalFold ::
     HiMonad m => [HiExpr] -> HiExT m HiValue
evalAdd =
  binaryNonLazy
    (\case
       (HiValueNumber n1, HiValueNumber n2) -> return (HiValueNumber (n1 + n2))
       (HiValueString t1, HiValueString t2) ->
         return (HiValueString (Txt.append t1 t2))
       (HiValueList l1, HiValueList l2) -> return (HiValueList (l1 >< l2))
       (HiValueBytes b1, HiValueBytes b2) ->
         return (HiValueBytes (BS.append b1 b2))
       (HiValueTime time, HiValueNumber n) ->
         return (HiValueTime (addUTCTime (fromRational n) time))
       _ -> throwInvalidArg)

evalSub =
  binaryNonLazy
    (\case
       (HiValueNumber n1, HiValueNumber n2) -> return (HiValueNumber (n1 - n2))
       (HiValueTime t1, HiValueTime t2) ->
         (return . HiValueNumber . toRational) (diffUTCTime t1 t2)
       _ -> throwInvalidArg)

evalMul =
  binaryNonLazy
    (\case
       (HiValueNumber n1, HiValueNumber n2) -> return (HiValueNumber (n1 * n2))
       (HiValueString t1, HiValueNumber n) -> checkedEvalMul n HiValueString t1
       (HiValueList l1, HiValueNumber n) -> checkedEvalMul n HiValueList l1
       (HiValueBytes b1, HiValueNumber n) -> checkedEvalMul n HiValueBytes b1
       _ -> throwInvalidArg)

evalDiv =
  binaryNonLazy
    (\case
       (HiValueNumber a, HiValueNumber b) ->
         if b == 0
           then throwE HiErrorDivideByZero
           else return (HiValueNumber (a / b))
       (HiValueString s1, HiValueString s2) ->
         (return . HiValueString . Txt.concat) [s1, Txt.pack "/", s2]
       _ -> throwInvalidArg)

evalRand =
  binaryNonLazy
    (\case
       (HiValueNumber n1, HiValueNumber n2) ->
         if n1 > n2
           then throwInvalidArg
           else (return . HiValueAction)
                  (HiActionRand (rationalToInt n1) (rationalToInt n2))
       _ -> throwInvalidArg)

evalWrite =
  binaryNonLazy
    (\case
       (HiValueString path, HiValueString t) ->
         internalEvalWrite path (encodeUtf8 t)
       (HiValueString path, HiValueBytes b) -> internalEvalWrite path b
       _ -> throwInvalidArg)

evalRange =
  binaryNonLazy
    (\case
       (HiValueNumber n1, HiValueNumber n2) ->
         (return . HiValueList . SQ.fromList) (HiValueNumber <$> [n1 .. n2])
       _ -> throwInvalidArg)

evalFold =
  binaryNonLazy
    (\case
       (HiValueFunction f, HiValueList l) ->
         case l of
           Empty -> return HiValueNull
           hed :<| tal ->
             foldM
               (\x y ->
                  evalApplicative
                    ((HiExprValue . HiValueFunction) f)
                    [HiExprValue x, HiExprValue y])
               hed
               tal
       _ -> throwInvalidArg)

evalEq, evalNEq, evalLessThan, evalGreaterThan, evalNotLessThan, evalNotGreaterThan ::
     HiMonad m => [HiExpr] -> HiExT m HiValue
evalEq = binaryNonLazy (uncurry $ evalBool (==))

evalNEq = binaryNonLazy (uncurry $ evalBool (/=))

evalLessThan = binaryNonLazy (uncurry $ evalBool (<))

evalGreaterThan = binaryNonLazy (uncurry $ evalBool (>))

evalNotLessThan = binaryNonLazy (uncurry $ evalBool (>=))

evalNotGreaterThan = binaryNonLazy (uncurry $ evalBool (<=))

evalBool ::
     HiMonad m
  => (HiValue -> HiValue -> Bool)
  -> HiValue
  -> HiValue
  -> HiExT m HiValue
evalBool f x y = return (HiValueBool (f x y))

internalEvalWrite :: HiMonad m => Txt.Text -> ByteString -> HiExT m HiValue
internalEvalWrite path =
  return . HiValueAction . HiActionWrite (Txt.unpack path)

checkedEvalMul ::
     (HiMonad m, Semigroup s)
  => Rational
  -> (s -> HiValue)
  -> s
  -> HiExT m HiValue
checkedEvalMul n constr arg =
  toIntegerOrThrow n >>= \t -> return (constr (stimes t arg))

evalIf :: HiMonad m => HiExpr -> HiExpr -> HiExpr -> HiExT m HiValue
evalIf cond onTrue onFalse = do
  c <- evalExpression cond
  evalExpression
    (if falseNulable c
       then onFalse
       else onTrue)

falseNulable :: HiValue -> Bool
falseNulable value = (value == HiValueBool False) || value == HiValueNull

throwInvalidArg :: HiMonad m => HiExT m t
throwInvalidArg = throwE HiErrorInvalidArgument

toIntegerOrThrow :: HiMonad m => Rational -> HiExT m Integer
toIntegerOrThrow n =
  if isNatural n
    then lift $ return (numerator n)
    else throwInvalidArg

rationalToInt :: Rational -> Int
rationalToInt = fromInteger . numerator

isInt, isPositive, isNatural :: Rational -> Bool
isInt n = denominator n == 1

isPositive = (> 0)

isNatural x = isInt x && isPositive x
