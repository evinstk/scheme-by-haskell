module Main where

import Parsers
import Text.ParserCombinators.Parsec (parse)
import System.Environment
import Control.Exception

main :: IO ()
main = getArgs >>= readEvalPrint . head

repl :: IO ()
repl = do
  putStr "scheme> "
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
eval (List (Atom func:args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
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
