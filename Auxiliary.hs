-- file: Auxiliary.hs
-- Functions useful in debugging
module Auxiliary where

import Main
import Text.ParserCombinators.Parsec hiding (spaces)

makeReader :: Show a => Parser a -> String -> String
makeReader parser input = case parse parser "custom" input of
  Left err  -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

printExpr :: String -> IO ()
printExpr = putStrLn . readExpr
