module Error where

import Control.Monad.Error
import Types
import Text.Parsec.Error(ParseError)

data LispError = 
    NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

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

