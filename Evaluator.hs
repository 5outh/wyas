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

-- @MAYBE: No coercion!
unpackStr :: LispVal -> Either LispError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr a = throwError $ TypeMismatch "string" a

unpackBool :: LispVal -> Either LispError Bool
unpackBool (Bool b) = return b
unpackBool a = throwError $ TypeMismatch "boolean" a

boolBinop :: (LispVal -> Either LispError a)
          -> (a -> a -> Bool)
          -> [LispVal]
          -> Either LispError LispVal
boolBinop unpacker f args = case args of
  [x, y] -> do 
    [l, r] <- mapM unpacker args
    return . Bool $ f l r
  _ -> throwError $ NumArgs 2 args
numericBinop :: (Integer -> Integer -> Integer) 
             -> [LispVal]
             -> Either LispError LispVal
numericBinop f []    = throwError $ NumArgs 2 []
numericBinop f x@[_] = throwError $ NumArgs 2 x
numericBinop f ls = mapM unpackNum ls >>= return . Number . foldl1 f

numBoolBinop :: (Integer -> Integer -> Bool)
             -> [LispVal]
             -> Either LispError LispVal
numBoolBinop = boolBinop unpackNum

boolBoolBinop :: (Bool -> Bool -> Bool)
             -> [LispVal]
             -> Either LispError LispVal
boolBoolBinop = boolBinop unpackBool

strBoolBinop :: (String -> String -> Bool)
             -> [LispVal]
             -> Either LispError LispVal
strBoolBinop = boolBinop unpackStr

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
               ("symbol?", return . Bool . all isSymbol),
               -- Eval, pt. 2
               ("=", numBoolBinop (==)),
               ("<", numBoolBinop (<)),
               (">", numBoolBinop (>)),
               ("/=", numBoolBinop (/=)),
               (">=", numBoolBinop (>=)),
               ("<=", numBoolBinop (<=)),
               ("&&", boolBoolBinop (&&)),
               ("||", boolBoolBinop (||)),
               ("string=?", strBoolBinop (==)),
               ("string<?", strBoolBinop (<)),
               ("string>?", strBoolBinop (>)),
               ("string<=?", strBoolBinop (<=)),
               ("string>=?", strBoolBinop (>=)) ]

-- "less primitives"
-- NB. car == head
car :: [LispVal] -> Either LispError LispVal
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [a]                   = throwError $ TypeMismatch "pair" a
car args                  = throwError $ NumArgs 1 args

cdr :: [LispVal] -> Either LispError LispVal
cdr [List (x:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [a]                   = throwError $ TypeMismatch "pair" a
cdr args                  = throwError $ NumArgs 1 args

cons :: [LispVal] -> Either LispError LispVal
cons [x, List xs]  = return $ List (x:xs)
cons [x, DottedList ys y] = return $ DottedList (x:ys) y
cons [x1, x2] = return $ DottedList [x1] x2
cons args = throwError $ NumArgs 2 args

eqv :: [LispVal] -> Either LispError LispVal
eqv [Bool a, Bool b] = return $ Bool $ a == b
eqv [Number a, Number b] = return $ Bool $ a == b
eqv [String a, String b] = return $ Bool $ a == b
eqv [Atom a, Atom b] = return $ Bool $ a == b
eqv [DottedList xs x, DottedList ys y] = 
  eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List xs, List ys] = return $ Bool $ 
     (length xs == length ys)
  && (and $ zipWith eqvPair xs ys)
    where eqvPair x y = case eqv [x, y] of
                          Left err -> False
                          Right (Bool val) -> val
eqv [_,_] = return $ Bool False
eqv args = throwError $ NumArgs 2 args

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
  (List [Atom "if", pred, conseq, alt]) -> do
    result <- eval pred
    case result of 
      Bool False -> eval alt
      otherwise  -> eval conseq
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
