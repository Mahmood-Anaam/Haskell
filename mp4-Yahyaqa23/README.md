# CS 340 Machine Problem 4

## Overview

This machine problem is in 2 parts. In part A, you will update the `Parser` monad presented in class so that it can provide error messages when parsing fails, and add a few additional parsing combinators. In part B, you will use your updated `Parser` monad to help parse and interpret a simple programming language.


## Signing the "package.yaml" file

As with the last machine problem, `git clone` your starter code, then sign the 
honor pledge in the "package.yaml" file before continuing. 


## Part A

For this section, you will be editing the `src/Parser.hs` file. This file contains an updated definition of the `Parser` type, shown below:

    data Parser a = Parser { parse :: String -> Either String (String, a) }

Note that we have changed a few things about the definition. First of all, to simplify things, `Parser` is no longer defined in terms of `State`; the record accessor has also been renamed `parse` (from `runState`). More importantly, when the parser is run on some input string, it returns an `Either` value. If the parser succeeds, it returns a pair containing the remaining input string and the value that was parsed. If the parser fails, it returns an error message. Recall that success and failure are represented by the `Right` and `Left` constructors, respectively, of the `Either` type.

Your first task is to define the `Functor`, `Applicative`, and `Monad` instances for the new `Parser` type, handling the `Either` values appropriately. We provide an updated `fail` function, which takes a string representing an error message, and returns a parser that always fails with that error message.

Next, you will re-implement the following parser combinators previously presented in class, in terms of the new `Parser` type --- feel free to reuse as much of the code from [Lect11.hs](https://github.com/cs340ppp/lectures/blob/completed/src/Lect11.hs) as you like:

- `item :: Parser Char`
- `sat :: (Char -> Bool) -> Parser Char`
- `char :: Char -> Parser Char`
- `string :: String -> Parser String`
- `oneOrMore :: Parser a -> Parser [a]`
- `zeroOrMore :: Parser a -> Parser [a]`
- `(<|>) :: Parser a -> Parser a -> Parser a`
- `int :: Parser Int`
- `space :: Parser ()`
- `token :: Parser a -> Parser a`
- `symbol :: String -> Parser String`

Note that the above combinators should behave as they did before, except in the following cases:

- `item` should return an error message of the form "unexpected end of input" if the input string is empty.
- `sat` should return an error message of the form "unexpected `c`" if the input character does not satisfy the predicate.

Consider the following interactions in GHCi demonstrating the desired behavior:

    ghci> parse item "abc"
    Right ("bc",'a')

    ghci> parse item ""
    Left "Unexpected end of input"

    ghci> parse (sat isDigit) "123"
    Right ("23",'1')

    ghci> parse (sat isDigit) "abc"
    Left "Unexpected character: a"

Finally, you will implement the following new combinators:

- `oneOf :: [String] -> Parser String`: parses one of the given strings as symbols (i.e., with an arbitrary amount of whitespace on either side), returning the string that was parsed. If none of the strings can be parsed, it should return an error message of the form "Expected one of: s1, s2, s2 ...".
- `identifier :: Parser String`: parses an identifier, which is a non-empty sequence of letters and digits that starts with a letter. If the input string does not start with a letter, it should return the error message "Expected an identifier".

Sample usage:

    ghci> parse (oneOf ["+", "*", "=="]) "== 10"
    Right ("10","==")

    ghci> parse (oneOf ["+", "*", "=="]) "!= 10"
    Left "Expected one of: +,*,=="

    ghci> parse identifier "var2 = 10"
    Right ("= 10","var2")

    ghci> parse identifier "_var2 = 10"
    Left "Expected identifier"

## Part B

For this section, you will be editing the `src/IMP.hs` file to implement a parser and interpreter for a simple imperative programming language named "IMP". The syntax of the language is described by the grammar below:

    <program> ::= <stmt> ; | <stmt> ; <program>
    <stmt> ::= <assignment> | <if> | <while>
    <assignment> ::= <identifier> "=" <expr>
    <if> ::= "if" <expr> "then" <program> "else" <program> "end"
    <while> ::= "while" <expr> "do" <program> "end"
    <expr> ::= <term> <op> <expr> | <term>
    <term> ::= <identifier> | <number>
    <op> ::= "+" | "*" | "<" | ">" | "==" | "!="

Here are some sample programs:

    a = 10; 
    b = 2 * a;

    ---

    tmp = a; 
    a = b; 
    b = tmp;

    ---

    if a > b then 
      max = a; 
    else 
      max = b; 
    end;

    ---

    i = 0; 
    sum = 0; 
    while i < n do 
      sum = sum + i; 
      i = i + 1; 
    end;

We've defined the following structures for you to use in your implementation:

    data Program = Program [Stmt]

    data Stmt = Assign String Expr 
              | If Expr Program Program 
              | While Expr Program

    data Expr = Lit Int 
              | Var String 
              | Bin String Expr Expr

Note that the string in the `Var` contructor represents a variable name (an identifier), and the string in the `Bin` constructor represents the binary operator (one of "+", "*", "<", ">", "==", "!=" --- which correspond to the addition, multiplication, less than, greater than, equality, and inequality operators, respectively). The only literals in the language are integers, represented by the `Lit` constructor.

You are to implement the parser combinators (and any others you find useful):

    program :: Parser Program

    stmt :: Parser Stmt

    expr :: Parser Expr

Given a valid IMP program, `program` should return a `Program` value. If the input string is not a valid program, it should return an error message. E.g.,

    ghci> parse program "a=10; b=2*a;"
    Right ("",Program [ Assign "a" (Lit 10),
                        Assign "b" (Bin "*" (Lit 2) (Var "a")) ])

    parse program "if a > b then max = a; else max = b; end;"
    Right ("",Program [If (Bin ">" (Var "a") (Var "b")) 
                          (Program [Assign "max" (Var "a")]) 
                          (Program [Assign "max" (Var "b")]) ])

    ghci> parse program "a = 10"
    Left "Unexpected end of input"

    ghci> parse program "100 = a ;"
    Left "Unexpected character: 1"


After implementing the IMP parser, you will implement an interpreter. The interpreter will take a `Program` value and an initial state (an association list mapping variable names to integer values) and return the final state. The interpreter can be expressed as follows:


    type Env = [(String, Int)]

    eval :: Program -> Env -> Env
    eval (Program stmts) env = foldl evalStmt env stmts


where `evalStmt` is a helper function that evaluates a single statement in the given environment and returns the resulting environment. You will need to implement `evalStmt` and `evalExpr` (which evaluates an expression in the given environment and returns the resulting integer value), along with any other helper functions you may need.

Some details of evaluation:

- When evaluating an expression, if a variable is not found in the environment, it should be treated as having the value 0. For example, if the environment is `[]`, then expression `a + 1` should evaluate to `1`.

- As IMP does not have Boolean values, we will use the convention that `0` is "False" and any other value is "True" -- this will be important when evaluating `if` and `while` statements. `1` is the default representation for "True". For example, the expression `a > b` should evaluate to `1` if `a` is greater than `b`, and `0` otherwise.

We provide you with the `parseEval` function, shown below, which can be used to parse and evaluate a program in one step. 

    parseEval :: String -> Env -> Either String Env
    parseEval s env = case parse program s of
      Left err -> Left (show err)
      Right (_, p) -> Right (eval p env)

For example:

    ghci> parseEval "a=2*b;" []
    Right [("a",0)]

    ghci> parseEval "a=2*b;" [("b",10)]
    Right [("b",10),("a",20)]

    ghci> parseEval "a=2*b;" [("a",99),("b",10)]
    Right [("a",20),("b",10)]

    ghci> parseEval "if a>b then max=a; else max=b; end;" [("a",10),("b",20)]
    Right [("a",10),("b",20),("max",20)]

You may also run the command `stack exec mp`, which will start an interactive session where you can type in one-line IMP programs for evaluation (press Ctrl-D to exit).


## Testing

We have provided a test suite for you to use to check your work. To run the tests, run the command:

    stack test

The test suite contains tests for both the `Parser` and IMP parser & evaluator. If you make any changes to the IMP types or functions, you will need to change the tests in `test/IMPSpec.hs` accordingly. Note that you should *not* change the IMP test programs themselves.


## Evaluation

### Part A

- Correct implementations of `Parser` functor, applicative, and monad instances are worth a total of 15 points.

- Correct implementations of `item`, `sat`, `oneOf`, and `identifier` are worth a total of 10 points.

### Part 2

- A correct implementation of the IMP parser is worth a total of 25 points
   - 5 points for expression parsing
   - 5 points for assignment statement parsing
   - 5 points for `if` statement parsing
   - 5 points for `while` statement parsing
   - 5 points for program parsing

- A correct implementation of the IMP interpreter is worth a total of 24 points
  - 8 points for evaluating assignment statements (including expressions)
  - 8 points for evaluating `if` statements
  - 8 points for evaluating `while` statements

---  

Maximum points = $15 + 10 + 25 + 24 = 74$.

## Submission

First, make sure you correctly signed and committed the "package.yaml" file.
Then, commit and push your changes to your GitHub repository.
