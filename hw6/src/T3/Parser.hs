{-# LANGUAGE TupleSections #-}

module T3.Parser
  ( parseConfig
  , parsePositiveInt
  , parseProgComand
  , ProgramOpts(..)
  ) where

import           Control.Monad              (guard, when)
import           Data.List                  (group, sort)
import           Data.Scientific            (Scientific, toRealFloat)
import           Data.Void                  (Void)
import           T3.Config
import qualified Text.Megaparsec
import           Text.Megaparsec            (ParseErrorBundle, Parsec, between,
                                             choice, empty, eof, many, (<?>))
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void String

data ProgramOpts = Prog
  { seed      :: Maybe Int
  , fieldSize :: Maybe Int
  , limit     :: Maybe Int
  }

parseConfig :: String -> Either (ParseErrorBundle String Void) Config
parseConfig = Text.Megaparsec.runParser (between skipSpaces eof parseConf) ""

parsePositiveInt :: String -> Either (ParseErrorBundle String Void) Int
parsePositiveInt =
  Text.Megaparsec.runParser (between skipSpaces eof parsePosInt) ""

parseProgComand :: String -> Either (ParseErrorBundle String Void) ProgramOpts
parseProgComand =
  Text.Megaparsec.runParser
    (between skipSpaces eof (parseComand *> parseOptions))
    ""

parseOptions :: Parser ProgramOpts
parseOptions = do
  options <- many parseOptionFlag
  when (hasDublicates options)
    $ fail "At most 1 value for each parameter expected"
  let s = lookup "-s" options
  let f = lookup "-f" options
  let l = lookup "-l" options
  return (Prog s f l)

flags :: [String]
flags = ["-s", "-f", "-l"]

hasDublicates :: [(String, Int)] -> Bool
hasDublicates = any ((> 1) . length) . (group . sort . map fst)

parseOptionFlag :: Parser (String, Int)
parseOptionFlag = choice $ map (\s -> (s, ) <$> parseOptionValue s) flags

parseOptionValue :: String -> Parser Int
parseOptionValue s = parseExpected s *> skipSpaces *> parsePosInt

parseConf :: Parser Config
parseConf = do
  probb <- parseProbability
  inc <- parsePosInt
  illdur <- parsePosInt
  Config probb inc illdur <$> parsePosInt

parseProbability :: Parser Double
parseProbability = do
  p <- parseDouble
  guard (p > 0 && p < 1) <?> "Probability must be in (0.0; 1.0)"
  return p

parseDouble :: Parser Double
parseDouble = toRealFloat <$> parseSci <?> "Double value expected"

parsePosInt :: Parser Int
parsePosInt = do
  int <- round <$> parseSci
  parsePositive int

parseSci :: Parser Scientific
parseSci = Lexer.signed skipSpaces Lexer.scientific <* skipSpaces

parsePositive :: (Ord t, Num t) => t -> Parser t
parsePositive num = do
  guard (num > 0) <?> "Positive value expected"
  return num

parseComand :: Parser String
parseComand = parseExpected "simulate"

parseExpected :: String -> Parser String
parseExpected = lexeme . string

lexeme :: Parser t -> Parser t
lexeme = Lexer.lexeme skipSpaces

skipSpaces :: Parser ()
skipSpaces = Lexer.space space1 empty empty
