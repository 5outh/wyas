module Types where

data LispVal = 
    Atom String -- Atom with a name
  | List [LispVal]
  | DottedList [LispVal] LispVal -- (a b . c) where c is the last element
  | Number Integer 
  | String String
  | Bool Bool
    deriving (Show, Eq)