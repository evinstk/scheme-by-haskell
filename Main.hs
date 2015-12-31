-- file: Main.hs
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric       (readInt, readOct, readHex)
import Data.Char     (digitToInt)
import Data.Maybe    (listToMaybe)
import Control.Monad (liftM)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 spaces

data LispVal = Atom String
             | Number Integer
             | String String
             | Bool Bool
             | List [LispVal]
             | DottedList [LispVal] LispVal

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
          digits <- many1 digit
          let maybeDecVal = listToMaybe . readFn $ digits
          case maybeDecVal of
            Just (decVal,_) -> return $ Number decVal
            Nothing         -> fail "Invalid digit string"
    let readBin = readInt 2 (`elem` "01") digitToInt
    radix <- oneOf "bodh"
    case radix of
      'b' -> readAndConvert readBin
      'o' -> readAndConvert readOct
      'd' -> do
        digits <- many1 digit
        return . Number . read $ digits
      'h' -> readAndConvert readHex
    else do
    rest <- many1 digit
    let digits = first:rest
    return . Number. read $ digits

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
            <|> parseQuoted
            <|> do char '('
                   x <- try parseList <|> parseDottedList
                   char ')'
                   return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right _  -> "Found value"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn . readExpr $ expr
