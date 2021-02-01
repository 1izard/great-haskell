import Control.Applicative
import Data.Char
import System.IO

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item =
  P
    ( \inp -> case inp of
        [] -> []
        (x : xs) -> [(x, xs)]
    )

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> [(g v, out)]
      )

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v, inp)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px =
    P
      ( \inp -> case parse pg inp of
          [] -> []
          [(g, out)] -> parse (fmap g px) out
      )

instance Monad Parser where
  -- (>>=) Parser a -> (a -> Parser b) -> (Parser b)
  p >>= f =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> parse (f v) out
      )

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q =
    P
      ( \inp -> case parse p inp of
          [] -> parse q inp
          [(v, out)] -> [(v, out)]
      )

  -- many :: Parser a -> Parser [a]
  many x = some x <|> pure []

  -- some :: Parser a -> Parser [a]
  some x = pure (:) <*> x <*> many x

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x : xs)

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

int :: Parser Int
int =
  do
    char '-'
    n <- nat
    return (- n)
    <|> nat

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do
  symbol "["
  n <- natural
  ns <-
    many
      ( do
          symbol ","
          natural
      )
  symbol "]"
  return (n : ns)

comment :: Parser ()
comment = do
  symbol "--"
  many (sat (/= '\n'))
  char '\n'
  return ()

expr :: Parser Int
expr = do
  t <- term
  do
    symbol "+"
    e <- expr
    return (t + e)
    <|> return t

term :: Parser Int
term = do
  f <- factor
  do
    symbol "*"
    t <- term
    return (f * t)
    <|> return f

factor :: Parser Int
factor =
  do
    symbol "("
    e <- expr
    symbol ")"
    return e
    <|> natural

eval :: String -> Int
eval xs = case parse expr xs of
  [(n, [])] -> n
  [(_, out)] -> error ("Unused input " ++ out)
  [] -> error "Invalid input"

data Expr = Val Int | Add Expr Expr | Mul Expr Expr

blak (Val n) = "(Val " ++ show n ++ ")"
blak e = "(" ++ show e ++ ")"

instance Show Expr where
  show (Val n) = "Val " ++ show n
  show (Add l r) = "Add " ++ blak l ++ blak r
  show (Mul l r) = "Mul " ++ blak l ++ blak r

expr' :: Parser Expr
expr' = do
  t <- term'
  do
    symbol "+"
    e <- expr'
    return (Add t e)
    <|> return t

term' :: Parser Expr
term' = do
  f <- factor'
  do
    symbol "*"
    t <- term'
    return (Mul f t)
    <|> return f

factor' :: Parser Expr
factor' =
  do
    symbol "("
    e <- expr'
    symbol ")"
    return e
    <|> do
      n <- natural
      return (Val n)

main = do
  print (eval "2 * 3 + 4")
  print (eval "2 * (3 + 4)")
  -- print (eval "2 * 3 ^ 4")
  -- print (eval "one plus two")
  print (parse expr' "2 * 3 + 4")
  print (parse expr' "2 * (3 + 4)")