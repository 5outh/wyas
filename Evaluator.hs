import System.Environment
import Parser
import Types
import Data.List(foldl1')

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
-- Exercise...haha
--unpackNum (String n) = let parsed = reads n :: [(Integer, String)] 
--                       in case parsed of
--                        [] -> 0
--                        ((x,_):_) -> x                           
--unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop f ls = Number $ foldl1' f $ map unpackNum ls

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

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [ ("+", numericBinop (+)),
               ("-", numericBinop (-)),
               ("*", numericBinop (*)),
               ("/", numericBinop div),
               ("mod", numericBinop mod),
               ("quotient", numericBinop quot),
               ("remainder", numericBinop rem),
               ("number?", Bool . all isNumber),
               ("string?", Bool . all isString),
               ("symbol?", Bool . all isSymbol) ]

apply :: String -> [LispVal] -> LispVal
apply f args = maybe (Bool False) ($ args) $ lookup f primitives

eval :: LispVal -> LispVal
eval expr = case expr of
  (List [Atom "quote", val]) -> val
  (List (Atom f : args))     -> apply f $ map eval args
  a                          -> a

runEval :: String -> IO ()
runEval = print . eval . readExpr

main :: IO ()
main = do
  args <- getArgs
  print (eval . readExpr $ head args)