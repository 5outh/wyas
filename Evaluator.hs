import System.Environment
import Data.List(foldl1')
import Control.Monad.Error

import Parser
import Types
import Error


-- Stricter than in the book, only accept real numbers.
unpackNum :: LispVal -> Either LispError Integer
unpackNum (Number n) = return n
unpackNum v = throwError $ TypeMismatch "number" v

numericBinop :: (Integer -> Integer -> Integer) 
             -> [LispVal]
             -> Either LispError LispVal
numericBinop f []    = throwError $ NumArgs 2 []
numericBinop f x@[_] = throwError $ NumArgs 2 x
numericBinop f ls = mapM unpackNum ls >>= return . Number . foldl1 f

-- Exercise
isNumber, isString, isSymbol, isBoolean :: LispVal -> Bool
isNumber (Number _)   = True
isNumber _            = False
isString (String _)   = True
isString _            = False
isBoolean (Bool _)    = True
isBoolean _           = False 
isSymbol (Atom _)     = True
isSymbol _            = False

primitives :: [(String, [LispVal] -> Either LispError LispVal)]
primitives = [ ("+", numericBinop (+)),
               ("-", numericBinop (-)),
               ("*", numericBinop (*)),
               ("/", numericBinop div),
               ("mod", numericBinop mod),
               ("quotient", numericBinop quot),
               ("remainder", numericBinop rem),
               ("number?", return . Bool . all isNumber),
               ("string?", return . Bool . all isString),
               ("symbol?", return . Bool . all isSymbol) ]

apply :: String -> [LispVal] -> Either LispError LispVal
apply f args = maybe 
  (throwError $ NotFunction "Unrecognized primitive function args" f)
  ($ args)
  (lookup f primitives)

eval :: LispVal -> Either LispError LispVal
eval expr = case expr of
  v@(String _) -> return v
  v@(Number _) -> return v
  v@(Bool _) -> return v
  (List [Atom "quote", v]) -> return v
  (List (Atom f : args))   -> mapM eval args >>= apply f
  form -> throwError $ BadSpecialForm "Unrecognized Special Form" form

runEval :: String -> IO ()
runEval s =
  putStrLn . extractValue . trapError $ liftM show $ readExpr s >>= eval

main :: IO ()
main = do
  args <- getArgs
  case args of 
    []    -> error "Please provide Scheme source string in args."
    (x:_) -> runEval x
