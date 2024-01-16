{-# LANGUAGE LambdaCase #-}

module HW5.Pretty
  ( prettyValue
  , errorStyle
  ) where

import           Data.ByteString               (ByteString, unpack)
import           Data.Foldable                 (fold)
import           Data.List                     (intersperse)
import           Data.Map                      (Map, (!))
import           Data.Maybe                    (isNothing)
import           Data.Scientific               (FPFormat (Fixed),
                                                formatScientific,
                                                fromRationalRepetendUnlimited)
import           Data.Sequence                 (Seq)
import           Data.Text                     (pack, strip, unpack)
import           Data.Time.Clock               (UTCTime)
import           Data.Word                     (Word8)
import           GHC.Exts                      (toList)
import           GHC.Real                      (Ratio ((:%)))
import           HW5.Base
import           Numeric                       (showHex)
import           Prettyprinter                 (Doc, Pretty, annotate, pretty,
                                                (<+>))
import           Prettyprinter.Render.Terminal (AnsiStyle, Color (..),
                                                bgColorDull, bold, color,
                                                italicized)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue =
  \case
    HiValueNull -> nullStyle "null"
    HiValueNumber n -> prettifyNumber n
    HiValueBool b -> prettifyBool b
    HiValueString s -> prettifyString s
    HiValueTime time ->
      funStyle "parse-time" <> surroundBrackets "(" ")" "" [prettifyTime time]
    HiValueList l -> prettifyList l
    HiValueBytes bytes -> prettifyBytes bytes
    HiValueDict d -> prettifyDict d
    HiValueFunction f -> prettifyFunction f
    HiValueAction action -> prettifyAction action

prettifyAction :: HiAction -> Doc AnsiStyle
prettifyAction =
  \case
    HiActionCwd -> funStyle "cwd"
    HiActionNow -> funStyle "now"
    HiActionRead p -> prettyActionWithArgs "read" [prettifyString p]
    HiActionWrite p b ->
      prettyActionWithArgs "write" [prettifyString p, prettifyBytes b]
    HiActionMkDir p -> prettyActionWithArgs "mkdir" [prettifyString p]
    HiActionChDir p -> prettyActionWithArgs "cd" [prettifyString p]
    HiActionRand l r ->
      prettyActionWithArgs "rand" [numberStyle l, numberStyle r]
    HiActionEcho a -> prettyActionWithArgs "echo" [stringStyle a]

prettyActionWithArgs :: String -> [Doc AnsiStyle] -> Doc AnsiStyle
prettyActionWithArgs name args =
  funStyle name <> surroundBrackets "(" ")" ", " args

prettifyFunction :: HiFun -> Doc AnsiStyle
prettifyFunction = funStyle . (hiFunMap !)

prettifyTime :: UTCTime -> Doc AnsiStyle
prettifyTime time = timeStyle ("\"" ++ show time ++ "\"")

prettifyDict :: Map HiValue HiValue -> Doc AnsiStyle
prettifyDict dict =
  surroundBrackets "{ " " }" ", " (map prettifyMapEntry (toList dict))

prettifyMapEntry :: (HiValue, HiValue) -> Doc AnsiStyle
prettifyMapEntry (key, value) =
  prettyValue key <> pretty ":" <+> prettyValue value

prettifyBytes :: ByteString -> Doc AnsiStyle
prettifyBytes bytes =
  surroundBrackets
    "[# "
    " #]"
    " "
    (prettifyByte <$> Data.ByteString.unpack bytes)

prettifyByte :: Word8 -> Doc AnsiStyle
prettifyByte byte =
  pretty
    $ (if byte < 16
         then "0"
         else "")
        ++ showHex byte ""

prettifyList :: Seq HiValue -> Doc AnsiStyle
prettifyList values =
  surroundBrackets "[ " " ]" ", " (prettyValue <$> toList values)

prettifyString :: Show s => s -> Doc AnsiStyle
prettifyString t = stringStyle (show t)

prettifyBool :: Bool -> Doc AnsiStyle
prettifyBool b =
  boolStyle
    (if b
       then "true"
       else "false")

prettifyNumber :: Rational -> Doc AnsiStyle
prettifyNumber n@(num :% den)
  | den == 1 = numberStyle num
  | isNothing mb = numberStyle (formatScientific Fixed Nothing sci)
  | abs num < den = prettyFraction num den
  | otherwise =
    let (q1, q2) = quotRem num den
     in numberStyle q1
          <+> pretty
                (if q2 > 0
                   then "+"
                   else "-")
          <+> prettyFraction (abs q2) den
  where
    (sci, mb) = fromRationalRepetendUnlimited n

prettyFraction :: Integer -> Integer -> Doc AnsiStyle
prettyFraction num den = numberStyle num <> pretty "/" <> numberStyle den

surroundBrackets ::
     String -> String -> String -> [Doc AnsiStyle] -> Doc AnsiStyle
surroundBrackets left right sep list =
  if null list
    then bracketStyle (trim left) <> pretty " " <> bracketStyle (trim right)
    else bracketStyle left <> prettyConcat sep list <> bracketStyle right

prettyConcat :: String -> [Doc AnsiStyle] -> Doc AnsiStyle
prettyConcat joiner = intercalate (pretty joiner)

intercalate :: Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle
intercalate e list = mconcat (intersperse e list)

stringStyle, numberStyle, boolStyle, nullStyle, timeStyle, funStyle, bracketStyle, errorStyle ::
     (Pretty t) => t -> Doc AnsiStyle
stringStyle = prettify (applyStyleSheet [color Green, italicized])

numberStyle = prettify (applyColor Blue)

boolStyle = prettify (applyStyleSheet [color Cyan, bold])

nullStyle =
  prettify
    (applyStyleSheet [color Yellow, bold, italicized, bgColorDull Magenta])

bracketStyle = prettify (applyColor Yellow)

timeStyle = prettify (applyColor Blue)

funStyle = prettify (applyColor Magenta)

errorStyle = prettify (applyColor Red)

applyColor :: Color -> Doc AnsiStyle -> Doc AnsiStyle
applyColor col = annotate $ color col

applyStyleSheet :: [AnsiStyle] -> Doc AnsiStyle -> Doc AnsiStyle
applyStyleSheet styleSheet = annotate (fold styleSheet)

prettify :: (Pretty t) => (Doc AnsiStyle -> Doc AnsiStyle) -> t -> Doc AnsiStyle
prettify a = a . pretty

trim :: String -> String
trim = Data.Text.unpack . strip . pack
