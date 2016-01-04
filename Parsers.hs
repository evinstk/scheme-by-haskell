module Parsers where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Char     (digitToInt, toLower, toUpper)
import Numeric       (readInt, readOct, readHex)
import Data.Maybe    (listToMaybe)
import Control.Monad (liftM)

data LispVal = Atom String
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | List [LispVal]
             | DottedList [LispVal] LispVal

instance Show LispVal where
  show (Atom name) = name
  show (Number number) = show number
  show (String contents) = "\"" ++ contents ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Character c) = "#\\" ++ [c]
  show (List list) = "(" ++ (unwords . map show $ list) ++ ")"
  show (DottedList car cdr) = "("
    ++ (unwords . map show $ car)
    ++ " . "
    ++ show cdr
    ++ ")"

parseExpr :: Parser LispVal
parseExpr = try parseNumber
            <|> try parseCharacter
            <|> try parseAtom
            <|> try parseString
            <|> try parseQuoted
            <|> do char '('
                   skipMany space
                   x <- try parseList <|> parseDottedList
                   char ')'
                   return x

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> symbol <|> digit)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $
       (char '\\' >> oneOf "\"nrt\\")
       <|> noneOf "\""
  char '"'
  return $ String x

parseNumber :: Parser LispVal
parseNumber = do
  first <- digit <|> char '#'
  if first == '#'
    then do
    let readAndConvert :: (String -> [(Integer, String)]) -> Parser LispVal
        readAndConvert readFn = do
          digits <- many1 (digit <|> oneOf "abcdefABCDEF")
          let maybeDecVal = listToMaybe . readFn $ digits
          case maybeDecVal of
            Just (decVal,_) -> return $ Number decVal
            Nothing         -> fail "Invalid digit string"

        readBin :: String -> [(Integer, String)]
        readBin = readInt 2 (`elem` "01") digitToInt

        dispatch :: Char -> Parser LispVal
        dispatch radix | radix `elem` ['b', 'B'] =
                           readAndConvert readBin
                       | radix `elem` ['o', 'O'] =
                           readAndConvert readOct
                       | radix `elem` ['d', 'D'] = do
                           digits <- many1 digit
                           return . Number . read $ digits
                       | radix `elem` ['h', 'H'] =
                           readAndConvert readHex

    radix <- oneOf "bodhBODH"
    dispatch radix
    else do
    rest <- many digit
    let digits = first:rest
    return . Number. read $ digits

parseCharacter :: Parser LispVal
parseCharacter = do
  char '#' >> char '\\'
  let ciString :: String -> Parser String
      ciString s = try $ mapM (\c -> char (toLower c) <|> char (toUpper c)) s
  c <- (ciString "space" >> (return ' '))
       <|> (ciString "newline" >> (return '\n'))
       <|> letter
       <|> symbol
       <|> digit
       <|> char ' '
  return . Character $ c

parseList :: Parser LispVal
parseList = liftM List $ do
  list <- try $ do list <- endBy parseExpr spaces
                   notFollowedBy $ char '.'
                   return list
    <|> sepBy parseExpr spaces
  return list

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  skipMany space
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]
