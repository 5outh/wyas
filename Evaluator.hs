{-# LANGUAGE TupleSections #-}
module Evaluator where
import System.Environment
import Data.List(foldl1')
import Control.Monad.Error
import Data.IORef

import Parser
import Types

liftThrows :: Either LispError a -> IOThrowsError a
liftThrows (Right a) = return a
liftThrows (Left e) = throwError e

nullEnv :: IO Env
nullEnv = newIORef []

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = do
  ref <- readIORef envRef
  return . maybe False (const True) $ lookup var ref

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . (`writeIORef` val))
        (lookup var env)
  return val

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var val = do
  def <- liftIO $ isBound envRef var
  case def of
    True -> setVar envRef var val
    _    -> liftIO $ do
      valRef <- newIORef val
      env    <- readIORef envRef
      writeIORef envRef ((var, valRef) : env)
      return val

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = 
      readIORef envRef
  >>= extendEnv bindings
  >>= newIORef 
  where extendEnv bs env = liftM (++ env) (mapM addBinding bs)
        addBinding (var, val) = newIORef val >>= return . (var,)

unpackEquals :: LispVal -> LispVal -> Unpacker -> Either LispError Bool
unpackEquals a b (Unpacker u) =
  catchError 
    (do [u, v] <- mapM u [a, b]
        return $ u == v)
    (const $ return False)

equal :: [LispVal] -> Either LispError LispVal
equal [a, b] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals a b)
                     [Unpacker unpackNum, Unpacker unpackStr, Unpacker unpackBool]
  eqvEquals <- eqv [a, b]
  return $ Bool $ primitiveEquals || let (Bool x) = eqvEquals in x
equal args = throwError $ NumArgs 2 args

-- Stricter than in the book, only accept real numbers.
unpackNum :: LispVal -> Either LispError Integer
unpackNum (Number n) = return n
unpackNum v = throwError $ TypeMismatch "number" v

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
               ("string>=?", strBoolBinop (>=)),
               -- string stuff
               ("car", car),
               ("cdr", cdr),
               ("cons", cons),
               ("eq?", eqv),
               ("eqv?", eqv),
               ("equal?", equal) ]

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

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
  where makePrimitiveFunc (var, f) = (var, PrimitiveFunc f)

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc f) args = liftThrows $ f args
apply (Func params varargs body closure) args
  | num params /= num args && varargs == Nothing =
      throwError $ NumArgs (num params) args
  | otherwise = (liftIO $ bindVars closure $ zip params args)
            >>= bindVarArgs varargs 
            >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
            Nothing      -> return env

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env expr = case expr of
  v@(String _) -> return v
  v@(Number _) -> return v
  v@(Bool _) -> return v
  (Atom i)  -> getVar env i
  (List [Atom "quote", v]) -> return v
  (List [Atom "if", pred, conseq, alt]) -> do
    result <- eval env pred
    case result of 
      Bool False -> eval env alt
      otherwise  -> eval env conseq
  (List [Atom "set!", Atom var, form]) ->
    eval env form >>= setVar env var
  (List [Atom "define", Atom var, form]) ->
    eval env form >>= defineVar env var
  (List (Atom "define" : List (Atom var : params) : body)) ->
    mkNormalFunc env params body >>= defineVar env var
  (List (Atom "define" : DottedList (Atom var : params) varargs : body)) ->
    mkVarArgs varargs env params body >>= defineVar env var
  (List (Atom "lambda" : List params : body)) ->
    mkNormalFunc env params body
  (List (Atom "lambda" : DottedList params varargs : body)) ->
    mkVarArgs varargs env params body
  (List (Atom "lambda" : varargs@(Atom _) : body)) ->
    mkVarArgs varargs env [] body
  (List (f : args)) -> do
    f' <- eval env f
    argVals <- mapM (eval env) args
    apply f' argVals
  --(List (Atom f : args)) -> mapM (eval env) args >>= apply f
  form -> throwError $ BadSpecialForm "Unrecognized Special Form" form

evalString :: Env -> String -> IO String
evalString env expr = 
  runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env
