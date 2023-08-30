{-# LANGUAGE InstanceSigs #-}

module Parser where

import Prelude hiding (fail)
import Data.Char
import Data.Either


data Parser a = Parser { parse :: String -> Either String (String, a) }



instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f pas = Parser $ \s -> 
    case parse pas s of 
                  Left ss -> Left ss
                  Right (ss, x) -> Right (ss,f x)
                                                    


instance Applicative Parser where
  pure :: a -> Parser a
  pure x= Parser $ \s ->Right (s,x)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = Parser $ \s -> 
    case parse p1 s of 
                  Left ss -> Left ss
                  Right (ss, f) -> parse (f <$> p2) ss




instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser $ \s -> 
    case parse p s of 
                  Left ss -> Left ss
                  Right (ss, x) -> parse (f x) ss




fail :: String -> Parser a
fail s = Parser $ \_ -> Left s


item :: Parser Char         
item = Parser $ \s -> 
    case s of  
      "" -> Left "Unexpected end of input" 
      (c:cs) -> Right (cs, c)



sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item
           if p c then return c else fail $ "Unexpected character: "++[c]

-- ....................................................


char :: Char -> Parser Char
char c = sat (== c) 

string :: String -> Parser String
string "" = return ""
string (x:xs) = do char x
                   string xs
                   return (x:xs)


digit :: Parser Char
digit = sat isDigit


pOr :: Parser a -> Parser a -> Parser a
p `pOr` q = Parser $ \s -> 
  case parse p s of 
    Left ss -> parse q s
    Right (ss, x) -> Right (ss, x)



(<|>) = pOr
                                          

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = do x <- p 
                 xs <- oneOrMore p <|> return []
                 return $ x:xs


zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> return []


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


-- ....................................................
intersperse' :: a-> [[a]] -> [a]
intersperse' _ []=[]
intersperse' s (x:xs)
            | null xs   = x
            | otherwise = x++[s]++ intersperse' s xs

cackesym :: [String] ->String -> Int ->Either String (String, String)
cackesym cs s nc 
            | isRight p       = p
            | (nc+1)>=length cs   =Left $ "Expected one of:" ++ intersperse' ',' cs
            | otherwise  = cackesym cs s (nc+1)

          where
            p=parse (symbol (cs!!nc)) s


oneOf :: [String] -> Parser String
oneOf  cs= Parser $ \s -> cackesym cs s 0


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

checkidentfier::String ->Either String (String, String)
checkidentfier s=if not(null sp) && isAlpha (head fs) then Right (scd, fs) else Left "Expected identifier" 
                 where
                  sp=words (trim s)
                  fs=head sp
                  scd=intersperse' ' ' (tail sp)
                  
identifier :: Parser String
identifier = Parser $ \s -> checkidentfier s
                  
                  
                  
