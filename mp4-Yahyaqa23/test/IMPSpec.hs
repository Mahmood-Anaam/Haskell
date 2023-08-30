module IMPSpec (spec) where

import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Control.Exception
import Parser
import IMP


spec :: Spec
spec = do 
  it "Parses expressions correctly" $ do
    parse expr "10" `shouldBe` Right ("", Lit 10)
    parse expr "x" `shouldBe` Right ("", Var "x")
    parse expr "10 + 2" `shouldBe` Right ("", Bin "+" (Lit 10) (Lit 2))
    parse expr "1 + 2 + 3" `shouldBe` Right ("", Bin "+" (Lit 1) (Bin "+" (Lit 2) (Lit 3)))
    parse expr "a == 2" `shouldBe` Right ("", Bin "==" (Var "a") (Lit 2))
  it "Parses assignment statements correctly" $ do
    parse stmt "x = 10" `shouldBe` Right ("", Assign "x" (Lit 10))
    parse stmt "x = 10 + 2" `shouldBe` Right ("", Assign "x" (Bin "+" (Lit 10) (Lit 2)))
  it "Parses if statements correctly" $ do
    parse stmt "if x then x = 10 ; else x = 20 ; end" `shouldBe` Right ("", If (Var "x") (Program [Assign "x" (Lit 10)]) (Program [Assign "x" (Lit 20)]))
  it "Parses while statements correctly" $ do
    parse stmt "while x do x = 10 ; end" `shouldBe` Right ("", While (Var "x") (Program [Assign "x" (Lit 10)]))
  it "Parses programs correctly" $ do
    parse program p0 `shouldBe` Right ("", e0)
    parse program p1 `shouldBe` Right ("", e1)
    parse program p2 `shouldBe` Right ("", e2)
    parse program p3 `shouldBe` Right ("", e3)
    parse program p4 `shouldBe` Right ("", e4)
    parse program p5 `shouldBe` Right ("", e5)
  it "Evaluates programs correctly" $ do
    eval e0 i0 `shouldMatchList` r0
    eval e1 i1 `shouldMatchList` r1
    eval e2 i2 `shouldMatchList` r2
    eval e3 i3 `shouldMatchList` r3
    eval e4 i4 `shouldMatchList` r4
    eval e5 i5 `shouldMatchList` r5


p0 = "foo = 10 ;"
e0 = Program [Assign "foo" (Lit 10)]
i0 = []
r0 = [("foo", 10)]


p1 = "a = 10 ; b = 2 * a ;"
e1 = Program [Assign "a" (Lit 10), 
              Assign "b" (Bin "*" (Lit 2) (Var "a"))]
i1 = []
r1 = [("a", 10), ("b", 20)]


p2 = "tmp = a ; a = b ; b = tmp ;"
e2 = Program [Assign "tmp" (Var "a"), 
              Assign "a" (Var "b"), 
              Assign "b" (Var "tmp")]
i2 = [("a", 20), ("b", 10)]
r2 = [("a", 10), ("b", 20), ("tmp", 20)]


p3 = "if a > b then max = a ; else max = b ; end ;"
e3 = Program [If (Bin ">" (Var "a") (Var "b")) 
                 (Program [Assign "max" (Var "a")]) 
                 (Program [Assign "max" (Var "b")])]
i3 = [("a", 10), ("b", 20)]
r3 = [("a", 10), ("b", 20), ("max", 20)]


p4 = "i = 0 ; sum = 0 ; while i < n do sum = sum + i ; i = i + 1 ; end ;"
e4 = Program [Assign "i" (Lit 0), 
              Assign "sum" (Lit 0), 
              While (Bin "<" (Var "i") (Var "n")) 
                    (Program [Assign "sum" (Bin "+" (Var "sum") (Var "i")),
                              Assign "i" (Bin "+" (Var "i") (Lit 1))])]
i4 = [("n", 10)]
r4 = [("i", 10), ("sum", 45), ("n", 10)]


p5 = "i = 0 ; f0 = 0 ; f1 = 1 ; \
    \ while i < n do    \
    \   f2 = f0 + f1 ;  \
    \   f0 = f1 ; f1 = f2 ; \
    \   i = i + 1 ;         \
    \ end ;"
e5 = Program [Assign "i" (Lit 0), 
              Assign "f0" (Lit 0), 
              Assign "f1" (Lit 1), 
              While (Bin "<" (Var "i") (Var "n")) 
                    (Program [Assign "f2" (Bin "+" (Var "f0") (Var "f1")),
                              Assign "f0" (Var "f1"), 
                              Assign "f1" (Var "f2"), 
                              Assign "i" (Bin "+" (Var "i") (Lit 1))])]
i5 = [("n", 10)]
r5 = [("i", 10), ("f0", 55), ("f1", 89), ("f2", 89), ("n", 10)]
