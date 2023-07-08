module Parser (
  Parser(runParser),
  parser,
  anyChar,
  char,
  Parser.void,
  letter,
  digit,
  alphaNum,
  string,
  (<:>),
  (~>),
  (<~),
  opt,
  (Parser.*),
  (Parser.+),
  Parser.sepBy,
  Parser.const,
  Parser.map,
) where

import Control.Applicative ( Alternative((<|>), empty, some, many), optional )
import Data.Char ( isDigit, isAlpha )

type ParseError = String

newtype Parser a = Parser { runParser :: String -> Either ParseError (a, String) }

instance Functor Parser where
  fmap f p = Parser $ \s -> do
    (a, s') <- runParser p s
    return (f a, s')

instance Applicative Parser where
  pure a = Parser $ \s -> Right (a, s)
  p1 <*> p2 = Parser $ \s -> do
    (f, s') <- runParser p1 s
    (a, s'') <- runParser p2 s'
    return (f a, s'')

instance Alternative Parser where
  empty = Parser $ \_ -> Left ""
  p1 <|> p2 = Parser $ \s -> case runParser p1 s of
    Left _ -> runParser p2 s
    Right a -> Right a

-- Primeros Parsers :)
parser :: (String -> Either ParseError (a, String)) -> Parser a
parser f = Parser guarda
  where 
    guarda "" = Left "Texto Vacio"
    guarda (x:xs) = f (x:xs)

anyChar :: Parser Char
anyChar = parser $ \s -> Right (head s, tail s)

conditionChar :: (Char -> Bool) -> Parser Char
conditionChar f = satisfies f anyChar

char :: Char -> Parser Char
char c = conditionChar (== c)

void :: Parser ()
void = () <$ anyChar

letter :: Parser Char
letter = conditionChar isAlpha

digit :: Parser Char
digit = conditionChar isDigit

alphaNum :: Parser Char
alphaNum = letter <|> digit -- o conditionChar isAlphaNum

string :: String -> Parser String
string = traverse char

-- Combinators :)
(<:>) :: Parser a -> Parser b -> Parser (a, b)
p1 <:> p2 = (,) <$> p1 <*> p2

(~>) :: Parser a -> Parser b -> Parser b
(~>) = (*>)

(<~) :: Parser a -> Parser b -> Parser a
(<~) = (<*)

satisfies :: (a -> Bool) -> Parser a -> Parser a
satisfies f p = Parser $ \s -> do
  (a, s') <- runParser p s
  if f a then Right () else Left "No satisface condicion"
  return (a, s')

opt :: Parser a -> Parser (Maybe a)
opt = optional

(*) :: Parser a -> Parser [a]
(*) = many

(+) :: Parser a -> Parser [a]
(+) = some

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p1 p2 = (:) <$> p1 <*> many (p2 *> p1)

const :: b -> Parser a -> Parser b
const = (<$)

map :: (a -> b) -> Parser a -> Parser b
map = fmap