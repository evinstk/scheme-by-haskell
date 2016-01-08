module Main where

import LispData
import Parsers
import Error
import Eval
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
