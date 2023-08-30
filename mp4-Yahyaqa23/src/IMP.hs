{-# LANGUAGE InstanceSigs #-}

module IMP where

import Parser


data Program = Program {contant::[Stmt]} deriving (Show, Eq)


data Stmt = Assign String Expr 
          | If Expr Program Program 
          | While Expr Program 
          deriving (Show, Eq)


data Expr = Lit Int 
          | Var String 
          | Bin String Expr Expr 
          deriving (Show, Eq)





program :: Parser Program
program = do sm <- stmt
             symbol ";"
             p <- program
             return (Program (sm:contant p))
          <|> do sm2 <- stmt
                 symbol ";"
                 return (Program [sm2])

-- ........................


stmt :: Parser Stmt
stmt = do ass <- assignment
          return ass
       <|> do f <- iff
              return f
       <|> do w <- while
              return w             


assignment :: Parser Stmt 
assignment= do id <- identifier
               symbol "="
               ex <- expr
               return (Assign id ex)


iff :: Parser Stmt 
iff = do symbol "if"
         ex <- expr
         symbol "then"
         p1 <- program
         symbol "else"
         p2 <- program
         symbol "end"
         return (If ex p1 p2)
         
         
while :: Parser Stmt        
while = do symbol "while"
           ex <- expr
           symbol "do"
           p <- program
           symbol "end"
           return (While ex p)


-- ........................

expr :: Parser Expr
expr = do t1 <- term
          p <- op
          t2 <- expr
          return (Bin p t1 t2)
       <|> term
       

term :: Parser Expr
term = do id <- identifier
          return (Var id)
       <|> do i <- int
              return (Lit i) 


op :: Parser String
op = do s1 <- symbol "+"
        return s1
      <|> do s2 <- symbol "*"
             return s2
      <|> do s3 <- symbol ">"
             return s3          
      <|> do s4 <- symbol "<"
             return s4   
      <|> do s5 <- symbol "=="
             return s5 
      <|> do s6 <- symbol "!="
             return s6

-- ............................................................................

type Env = [(String, Int)]


eval :: Program -> Env -> Env
eval (Program stmts) env = foldl evalStmt env stmts


evalStmt :: Env -> Stmt -> Env
evalStmt env stm= case stm of
   (Assign name exp) ->[(name,evalExpr env exp)]


-- ........................

evalExpr :: Env -> Expr -> Int
evalExpr env exp= case exp of
   (Lit num)                      ->   num
   (Bin p (Lit num) (Var vnam))   ->   opreations p num (serchenv vnam env)
   (Bin p (Var vnam) (Lit num))   ->   opreations p (serchenv vnam env) num
   (Bin p (Lit num1) (Lit num2))   ->   opreations p num1 num2
   (Bin p (Var vnam1) (Var vnam2))   ->   opreations p (serchenv vnam1 env) (serchenv vnam2 env)





serchenv :: String -> Env-> Int
serchenv _ [] = 0
serchenv x ((a,b):xs) = if x == a then b else serchenv x xs

opreations :: String -> Int -> Int -> Int
opreations p x y=case p of
   "*"  -> x*y
   "+"  -> x+y
   "==" -> if x==y then 1 else 0
   "!=" -> if x==y then 0 else 1
   ">" -> if x>y then 1 else 0
   "<" -> if x<y then 1 else 0




parseEval :: String -> Env -> Either String Env
parseEval s env = case parse program s of
  Left err -> Left (show err)
  Right (_, p) -> Right (eval p env)
