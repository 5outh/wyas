module Parser where

import Text.ParserCombinators.Parsec hiding (spaces, (<|>))
import System.Environment
import Control.Applicative hiding (many)
import Types

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = char '"' 
           *> (liftA String $ many (noneOf "\"")) 
           <* char '"'

parseNumber :: Parser LispVal
parseNumber = liftA (Number . read) (many1 digit)

parseAtom :: Parser LispVal
parseAtom = do
  atom <- liftA2 (:) (letter <|> symbol) (many (letter <|> digit <|> symbol))
  pure $ case atom of
    "#f" -> Bool False
    "#t" -> Bool True
    _    -> Atom atom

parseList :: Parser LispVal
parseList = liftA List $ parseExpr `sepBy` spaces

parseDottedList :: Parser LispVal
parseDottedList = liftA2 DottedList (parseExpr `endBy` spaces) (char '.' *> spaces *> parseExpr)

parseQuoted :: Parser LispVal
parseQuoted = char '\'' *> liftA (\x -> List [Atom "quote", x]) parseExpr

anyOf :: Alternative f => [f a] -> f a
anyOf = foldr1 (<|>)

parseExpr :: Parser LispVal
parseExpr = anyOf
            [ parseAtom 
            , parseString 
            , parseNumber
            , parseQuoted
            , char '(' *> (try parseList <|> parseDottedList) <* char ')'
            ]

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> error $ "No match: " ++ show err
  Right val -> val