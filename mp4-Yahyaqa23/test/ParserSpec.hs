module ParserSpec (spec) where

import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Control.Exception
import Data.Char
import Data.Either
import Parser


spec :: Spec
spec = do 
  describe "item" $ do
    it "works to spec" $ do
      parse item "a" `shouldBe` Right ("", 'a')
      parse item " foo " `shouldBe` Right ("foo ", ' ')
      parse item "123" `shouldBe` Right ("23", '1')
      parse item "" `shouldSatisfy` isLeft

  describe "sat" $ do
    it "works to spec" $ do
      parse (sat isAlpha) "a" `shouldBe` Right ("", 'a')
      parse (sat isDigit) "1" `shouldBe` Right ("", '1')
      parse (sat isAlpha) "123" `shouldSatisfy` isLeft
      parse (sat isDigit) "abc" `shouldSatisfy` isLeft

  describe "oneOf" $ do
    it "works to spec" $ do
      parse (oneOf ["a", "b", "c"]) "a 2 3" `shouldBe` Right ("2 3", "a")
      parse (oneOf ["a", "b", "c"]) "bac" `shouldBe` Right ("ac", "b")
      parse (oneOf ["+", "*", "=="]) "== 10" `shouldBe` Right ("10", "==")
      parse (oneOf ["a", "b", "c"]) "d" `shouldSatisfy` isLeft
      
  describe "identifier" $ do
    it "works to spec" $ do
      parse identifier "a" `shouldBe` Right ("", "a")
      parse identifier " foo " `shouldBe` Right ("", "foo")
      parse identifier "aB2c3 10" `shouldBe` Right ("10", "aB2c3")
      parse identifier "foo123 = x" `shouldBe` Right ("= x", "foo123")
      parse identifier "123" `shouldSatisfy` isLeft
      parse identifier "   " `shouldSatisfy` isLeft
