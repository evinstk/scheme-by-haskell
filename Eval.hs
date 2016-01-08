module Eval (eval) where

import LispData
import Error
import Control.Monad.Except (throwError)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val

eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool True -> eval conseq
    otherwise -> eval alt
eval form@(List (Atom "if":_)) = throwError $ BadSpecialForm "Bad special form" form

eval (List (Atom func:args)) = mapM eval args >>= apply func

apply :: String -> [LispVal] -> ThrowsError LispVal
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
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("string?", testString),
              ("symbol?", testSymbol),
              ("number?", testNumber),
              ("symbol->string", symbolToString),
              ("string->symbol", stringToSymbol)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool
  where unpackBool :: LispVal -> ThrowsError Bool
        unpackBool (Bool b) = return b
        unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackString
  where unpackString :: LispVal -> ThrowsError String
        unpackString (String s) = return s
        unpackString (Number s) = return $ show s
        unpackString (Bool s)   = return $ show s
        unpackString notStr     = throwError $ TypeMismatch "string" notStr

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
  if null parsed
  then throwError $ TypeMismatch "number" $ String n
  else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)]         = return x
car [DottedList (x:_) _] = return x
car [badArg]             = throwError $ TypeMismatch "pair" badArg
car badArgList           = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgsList           = throwError $ NumArgs 1 badArgsList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []]             = return $ List [x]
cons [x, List xs]             = return $ List (x:xs)
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) &&
                                                             (all eqvPair $ zip arg1 arg2)
  where eqvPair (x1, x2) = case eqv [x1, x2] of
          Left err -> False
          Right (Bool val) -> val
eqv [_,_]                                  = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

testString :: [LispVal] -> ThrowsError LispVal
testString [(String _)] = return $ Bool True
testString [_]          = return $ Bool False
testString badArgList   = throwError $ NumArgs 1 badArgList

testSymbol :: [LispVal] -> ThrowsError LispVal
testSymbol [(Atom _)] = return $ Bool True
testSymbol [_]        = return $ Bool False
testSymbol badArgList = throwError $ NumArgs 1 badArgList

testNumber :: [LispVal] -> ThrowsError LispVal
testNumber [(Number _)] = return $ Bool True
testNumber [_]          = return $ Bool False
testNumber badArgList   = throwError $ NumArgs 1 badArgList

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [(Atom symbol)] = return $ String symbol
symbolToString [x]             = throwError $ TypeMismatch "symbol" x
symbolToString badArgList      = throwError $ NumArgs 1 badArgList

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [(String str)] = return $ Atom str
stringToSymbol [x]            = throwError $ TypeMismatch "string" x
stringToSymbol badArgList     = throwError $ NumArgs 1 badArgList
