module Main where

import Parsers
import Text.ParserCombinators.Parsec (parse)
import System.Environment
import System.IO
import Control.Exception

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
        putStrLn $ "Error: Cannot evaluate \"" ++ (show val) ++ "\""
  case val of
    List [Atom "quit"] -> putStrLn "Goodbye!"
    _                  -> do
      catch (print . eval $ val) handler
      repl

readEvalPrint :: String -> IO ()
readEvalPrint = print . readEval

readEval :: String -> LispVal
readEval = eval . readExpr

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err   -> String $ "No match: " ++ show err
  Right val  -> val

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List [Atom "symbol?", val]) = case eval val of
  (Atom _) -> Bool True
  _        -> Bool False
eval (List [Atom "string?", val]) = case eval val of
  (String _) -> Bool True
  _          -> Bool False
eval (List [Atom "number?", val]) = case eval val of
  (Number _) -> Bool True
  _          -> Bool False
eval (List [Atom "symbol->string", val]) = convert $ eval val
  where convert (Atom symbol) = String symbol
eval (List [Atom "string->symbol", val]) = convert $ eval val
  where convert (String string) = Atom string
eval (List (Atom func:args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
-- Interesting that #f is the default value
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params
  where unpackNum :: LispVal -> Integer
        unpackNum (Number n) = n
        -- Following cases allow weak typing
        unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
          if null parsed
          then 0
          else fst $ parsed !! 0
        unpackNum (List [n]) = unpackNum n
        unpackNum _ = 0
