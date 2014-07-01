{-# LANGUAGE ExistentialQuantification, NoMonomorphismRestriction #-}
module Types where

import Control.Monad.Error
import Data.IORef
import Text.Parsec.Error(ParseError)

data LispVal = 
    Atom String -- Atom with a name
  | List [LispVal]
  | DottedList [LispVal] LispVal -- (a b . c) where c is the last element
  | Number Integer 
  | String String
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> Either LispError LispVal)
  | Func { params :: [String]
         , vararg :: Maybe String
         , body :: [LispVal]
         , closure :: Env }

data LispError = 
    NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

data Unpacker = 
  forall a. Eq a => Unpacker (LispVal -> Either LispError a)

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ErrorT LispError IO

instance Show LispError where
  show (UnboundVar m v) = m ++ ": " ++ v
  show (BadSpecialForm m f) = m ++ ": " ++ show f 
  show (NotFunction m f) = m ++ ": " ++ f
  show (NumArgs e f) = "Expected " ++ show e 
                     ++ " args; found values " 
                     ++ unwordsList f
  show (TypeMismatch e f) = "Invalid type: expected " ++ e
                          ++ ", found " ++ show f
  show (Parser e) = "Parse error at " ++ show e

instance Error LispError where
  noMsg  = Default "An error has ocurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right v) = v
-- otherwise, a programmer error.


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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func args' vargs body env) =
   "(lambda (" ++ unwords (map show args') ++
      (case vargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"

instance Show LispVal where
  show = showVal

mkFunc varargs env params body = return $ Func (map showVal params) varargs body env
mkNormalFunc = mkFunc Nothing
mkVarArgs = mkFunc . Just . showVal
