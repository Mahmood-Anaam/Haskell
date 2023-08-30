-- CS 340: Programming Paradigms and Patterns
-- Lect 11 - Monadic Parsing
-- Michael Lee

module Lect11 where
import Prelude hiding (fail)
import Data.Char

{-# ANN module "HLint: Use <$>" #-}

data State s a = State { runState :: s -> Maybe (s, a) }


instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f st = State $ \s -> 
    case runState st s of Nothing -> Nothing
                          Just (s', x) -> Just (s', f x)


instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State $ \s -> Just (s, x)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  stf <*> stx = State $ \s -> 
    case runState stf s of Nothing -> Nothing
                           Just (s', f) -> runState (f <$> stx) s'


instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  st >>= f = State $ \s -> 
    case runState st s of Nothing -> Nothing
                          Just (s', x) -> runState (f x) s'

-- Problem: using the state monad, write a parser that attempts to parse and
--          evaluate an infix arithmetic expression

-- Example: "1 + 2 * 3" -> 7
--          "1 + 2 * 3 + 4" -> 11
--          "(1 + 2) * (3 + 4))" -> 21

type Parser a = State String a


item :: Parser Char
item = State $ \s -> case s of "" -> Nothing
                               (c:cs) -> Just (cs, c)


sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item
           if p c then return c else fail


fail :: Parser a
fail = State $ \s -> Nothing


char :: Char -> Parser Char
char c = sat (== c)


string :: String -> Parser String
string "" = return ""
string (x:xs) = do char x
                   string xs
                   return (x:xs)


digit :: Parser Char
digit = sat isDigit


digits :: Parser [Char]
digits = do d <- digit
            ds <- digits
            return (d:ds)


pOr :: Parser a -> Parser a -> Parser a
p `pOr` q = State $ \s -> case runState p s of Nothing -> runState q s
                                               Just x -> Just x


(<|>) = pOr


digits' :: Parser [Char]
digits' = do d <- digit 
             ds <- digits' <|> return []
             return $ d:ds


oneOrMore :: Parser a -> Parser [a]
oneOrMore p = do x <- p 
                 xs <- oneOrMore p <|> return []
                 return $ x:xs


digits'' = oneOrMore digit


zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> return []


oneOrMore' :: Parser a -> Parser [a]
oneOrMore' p = do x <- p
                  xs <- zeroOrMore p
                  return (x:xs)


digits''' = oneOrMore' digit



int :: Parser Int
int = do cs <- oneOrMore digit
         return (read cs)


space :: Parser ()
space = do zeroOrMore (sat isSpace)
           return ()


token :: Parser a -> Parser a
token p = do space
             x <- p
             space
             return x


symbol :: String -> Parser String
symbol s = token (string s)


-- Grammar (in Backus-Naur Form) for infix arithmetic expressions:
--   expr   ::= term + expression | term
--   term   ::= factor * term | factor
--   factor ::= ( expr ) | integer

data Expr = Lit Int
          | Add Expr Expr
          | Mul Expr Expr
          deriving (Show)


expr :: Parser Expr
expr = do t1 <- term
          symbol "+"
          t2 <- expr
          return (Add t1 t2)
       <|> term


term :: Parser Expr
term = do f1 <- factor
          symbol "*"
          f2 <- term
          return (Mul f1 f2)
       <|> factor


factor :: Parser Expr
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         <|> do i <- int
                return (Lit i)                    


eval :: Expr -> Int
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2