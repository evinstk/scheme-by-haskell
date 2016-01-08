module Main where

import LispData
import Parsers
import Error
import Text.ParserCombinators.Parsec (parse)
import System.Environment
import System.IO
import Control.Exception
import Control.Monad                 (liftM)
import Control.Monad.Except          (throwError)

main :: IO ()
main = do
  arg:_ <- getArgs
  if arg == "-i"
    then repl
    else readEvalPrint arg

repl :: IO ()
repl = do
  putStr "scheme> "
  hFlush stdout
  val <- getLine >>= return . readExpr
  -- TODO: Write custom exception
  let handler :: (PatternMatchFail -> IO ())
      handler e = do
        putStrLn $ "Error: Cannot evaluate \"" ++ (show $ extractValue val) ++ "\""
  case val of
    Right (List [Atom "quit"]) -> putStrLn "Goodbye!"
    Right val                  -> do
      catch
        (putStrLn $ extractValue $ trapError $ liftM show $ eval val)
        handler
      repl
    Left err                   -> do
      putStrLn $ show err
      repl

readEvalPrint :: String -> IO ()
readEvalPrint expr = do
  evaled <- return $ readEval expr
  putStrLn $ extractValue $ trapError evaled

readEval :: String -> ThrowsError String
readEval expr = liftM show $ readExpr expr >>= eval

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err   -> throwError $ Parser err
  Right val  -> return val

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "symbol?", val]) = do
  evaled <- eval val
  case evaled of
    (Atom _) -> return $ Bool True
    _        -> return $ Bool False
eval (List [Atom "string?", val]) = do
  evaled <- eval val
  case evaled of
    (String _) -> return $ Bool True
    _          -> return $ Bool False
eval (List [Atom "number?", val]) = do
  evaled <- eval val
  case evaled of
    (Number _) -> return $ Bool True
    _          -> return $ Bool False
eval (List [Atom "symbol->string", val]) = do
  evaled <- eval val
  return $ convert evaled
    where convert (Atom symbol) = String symbol
eval (List [Atom "string->symbol", val]) = do
  evaled <- eval val
  return $ convert evaled
    where convert (String string) = Atom string
eval (List (Atom func:args)) = mapM eval args >>= apply func

apply :: String -> [LispVal] -> ThrowsError LispVal
-- Interesting that #f is the default value
apply func args = maybe
  (throwError $ NotFunction "Unrecognized primitive function args" func)
  ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op
  where unpackNum :: LispVal -> ThrowsError Integer
        unpackNum (Number n) = return n
        unpackNum (String n) = let parsed = reads n in
          if null parsed
          then throwError $ TypeMismatch "number" $ String n
          else return $ fst $ parsed !! 0
        unpackNum (List [n]) = unpackNum n
        unpackNum notNum     = throwError $ TypeMismatch "number" notNum
