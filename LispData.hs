module LispData where

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
  show (List list) = "(" ++ (unwordsList list) ++ ")"
  show (DottedList car cdr) = "("
    ++ (unwords . map show $ car)
    ++ " . "
    ++ show cdr
    ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show
