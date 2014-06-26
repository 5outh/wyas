module Types where

data LispVal = 
    Atom String -- Atom with a name
  | List [LispVal]
  | DottedList [LispVal] LispVal -- (a b . c) where c is the last element
  | Number Integer 
  | String String
  | Bool Bool
    deriving Eq

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (Atom a) = a
showVal (List xs) =  "(" ++ unwordsList xs ++ ")"
showVal (DottedList xs lst) = "(" ++ unwordsList xs ++ " . " ++ show lst ++ ")" 
showVal (Number i) = show i 
showVal (String s) = show s
showVal (Bool True) = "#t"
showVal (Bool f)    = "#f"

instance Show LispVal where
  show = showVal