{-# LANGUAGE OverloadedStrings #-}

module HW5.Parser
  ( parse
  ) where

import           Control.Monad.Combinators.Expr (Operator (InfixL, InfixN, InfixR),
                                                 makeExprParser)
import           Data.Char                      (isAlpha, isAlphaNum)
import           Data.List                      (intercalate)
import           Data.Map                       (toDescList)
import qualified Data.Text                      as Text
import           Data.Void                      (Void)
import           Data.Word                      (Word8)
import           GHC.Exts                       (fromList)
import           HW5.Base
import qualified Text.Megaparsec
import           Text.Megaparsec                (ParseErrorBundle, Parsec,
                                                 between, choice, empty, eof,
                                                 many, manyTill, notFollowedBy,
                                                 satisfy, sepBy, sepEndBy, try,
                                                 (<?>), (<|>))
import           Text.Megaparsec.Char           (char, hexDigitChar, space1,
                                                 string)
import qualified Text.Megaparsec.Char.Lexer     as Lexer

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = Text.Megaparsec.runParser (between skipSpaces eof parseHiExpr) ""

parseHiExpr :: Parser HiExpr
parseHiExpr = makeExprParser (getExpr >>= parseArgsOptional) operatorDescriptors

parseArgsOptional :: HiExpr -> Parser HiExpr
parseArgsOptional expr =
  many (inRoundBrackets comaSplit) >>= parseEndOptional . foldl HiExprApply expr

parseEndOptional :: HiExpr -> Parser HiExpr
parseEndOptional expr =
  choice (map (\p -> p expr) [parseRun, parseDotApply, return])

parseRun :: HiExpr -> Parser HiExpr
parseRun expr = HiExprRun expr <$ parseStripString "!" >>= parseArgsOptional

parseDotApply :: HiExpr -> Parser HiExpr
parseDotApply expr =
  string "."
    >> HiExprValue . HiValueString . Text.pack <$> parseStringArg
    >>= (\arg -> return (HiExprApply expr [arg]))
    >>= parseArgsOptional
  where
    parseStringArg =
      Data.List.intercalate "-"
        <$> sepBy
              ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum))
              (char '-')

getExpr :: Parser HiExpr
getExpr =
  choice
    [ parseAsHiValue
    , inRoundBrackets parseHiExpr
    , parseListBytes
    , parseList
    , parseDict
    ]

parseListBytes :: Parser HiExpr
parseListBytes =
  HiExprValue . HiValueBytes . fromList
    <$> byteListBrackets (sepEndBy parseByte space1) <?> "Failed to parse bytes"

parseList :: Parser HiExpr
parseList =
  HiExprApply (HiExprValue (HiValueFunction HiFunList))
    <$> inSquareBrackets comaSplit

parseDict :: Parser HiExpr
parseDict = HiExprDict <$> inFigureBrackets (sepBy parseMapEntry comma)

parseMapEntry :: Parser (HiExpr, HiExpr)
parseMapEntry = do
  key <- parseHiExpr <* colon
  value <- parseHiExpr
  return (key, value)

parseAsHiValue :: Parser HiExpr
parseAsHiValue = HiExprValue <$> parseHiValue <* skipSpaces

parseHiValue :: Parser HiValue
parseHiValue =
  choice
    [ HiValueNumber <$> parseNumber
    , HiValueBool <$> parseBool
    , HiValueString <$> parseText
    , HiValueFunction <$> parseFunction
    , HiValueNull <$ parseExpected "null"
    , HiValueAction HiActionNow <$ parseExpected "now"
    , HiValueAction HiActionCwd <$ parseExpected "cwd"
    ]

parseNumber :: Parser Rational
parseNumber =
  toRational <$> Lexer.signed skipSpaces Lexer.scientific <* skipSpaces

parseBool :: Parser Bool
parseBool =
  True <$ parseExpected "true"
    <|> False <$ parseExpected "false" <?> "boolean 'true' or 'false' expected"

parseText :: Parser Text.Text
parseText = Text.pack <$> (char '"' >> manyTill Lexer.charLiteral (char '"'))

parseFunction :: Parser HiFun
parseFunction =
  choice (map (\(f, name) -> f <$ parseExpected name) (toDescList hiFunMap))
    <?> "No such function"

parseExpected :: String -> Parser String
parseExpected = lexeme . string

comaSplit :: Parser [HiExpr]
comaSplit = sepBy parseHiExpr comma

operatorDescriptors :: [[Operator Parser HiExpr]]
operatorDescriptors =
  [ [infixL HiFunMul "*", infixL HiFunDiv "/"]
  , [infixL HiFunAdd "+", infixL HiFunSub "-"]
  , [ infixN HiFunNotLessThan ">="
    , infixN HiFunNotGreaterThan "<="
    , infixN HiFunLessThan "<"
    , infixN HiFunGreaterThan ">"
    , infixN HiFunEquals "=="
    , infixN HiFunNotEquals "/="
    ]
  , [infixR HiFunAnd "&&"]
  , [infixR HiFunOr "||"]
  ]

infixL, infixR, infixN :: HiFun -> String -> Operator Parser HiExpr
infixL = infix_ InfixL

infixR = infix_ InfixR

infixN = infix_ InfixN

infix_ ::
     (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr)
  -> HiFun
  -> String
  -> Operator Parser HiExpr
infix_ infx fun oper =
  infx
    $ (\f s -> (HiExprApply . HiExprValue . HiValueFunction) fun [f, s])
        <$ (lexeme . try) (string oper <* notFollowedBy "=")

-- =========== Primitives ============
inBrackets :: Parser String -> Parser String -> Parser t -> Parser t
inBrackets = between

inRoundBrackets, inSquareBrackets, inFigureBrackets, byteListBrackets ::
     Parser t -> Parser t
inSquareBrackets = inBrackets openBr closeBr

inRoundBrackets = inBrackets openPar closePar

inFigureBrackets = inBrackets openFB closeFB

byteListBrackets = inBrackets openByteBr closeByteBr

parseStripString :: String -> Parser String
parseStripString = (<* skipSpaces) . string

parseByte :: Parser Word8
parseByte = do
  senior <- hexDigitChar
  minor <- hexDigitChar
  return $ read ['0', 'x', senior, minor]

openPar, closePar, openBr, closeBr, openFB, closeFB, openByteBr, closeByteBr, comma, colon ::
     Parser String
openPar = parseSymbol "("

closePar = parseSymbol ")"

openBr = parseSymbol "["

closeBr = parseSymbol "]"

openFB = parseSymbol "{"

closeFB = parseSymbol "}"

comma = parseSymbol ","

colon = parseSymbol ":"

openByteBr = parseSymbol "[#"

closeByteBr = parseSymbol "#]"

parseSymbol :: String -> Parser String
parseSymbol = Lexer.symbol skipSpaces

skipSpaces :: Parser ()
skipSpaces = Lexer.space space1 empty empty

lexeme :: Parser t -> Parser t
lexeme = Lexer.lexeme skipSpaces
