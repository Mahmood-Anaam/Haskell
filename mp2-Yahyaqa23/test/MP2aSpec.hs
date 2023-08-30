{-# LANGUAGE ImplicitParams #-}

module MP2aSpec (spec) where

import Data.Char
import Test.Hspec
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck
import Control.Exception
import MP2a

spec :: Spec
spec = do
  describe "cycleN" $ do
    it "works for the provided test cases" $ do
      cycleN 3 [1..4] `shouldBe` [1,2,3,4,1,2,3,4,1,2,3,4]
      cycleN 0 "hello?" `shouldBe` ""

  describe "chunksOf" $ do
    it "works for the provided test cases" $ do
      chunksOf 3 "hello world" `shouldBe` ["hel","lo ","wor","ld"]
      chunksOf 5 [1..3] `shouldBe` [[1,2,3]]

  describe "unzip4" $ do
    it "works for the provided test cases" $ do
      unzip4 [(1,2,3,4),(5,6,7,8),(9,10,11,12),(13,14,15,16)]
        `shouldBe` ([1,5,9,13],[2,6,10,14],[3,7,11,15],[4,8,12,16])
      unzip4 [(1,'h',True,3.14), (2,'i',False,2.7), (3,'!',True,9.8)]
        `shouldBe` ([1,2,3],"hi!",[True,False,True],[3.14,2.7,9.8])

  describe "intersperse" $ do
    it "works for the provided test cases" $ do
      intersperse ',' ["tom","dick","harry"] `shouldBe` "tom,dick,harry"
      intersperse '!' ["hi"] `shouldBe` "hi"
      intersperse 0 [[1..5],[6..10]] `shouldBe` [1,2,3,4,5,0,6,7,8,9,10]

  describe "removeAll" $ do
    it "works for the provided test cases" $ do
      removeAll [1..3] [0..10] `shouldBe` [0,4,5,6,7,8,9,10]
      removeAll "aeiou" "supercalifragilisticexpialidocious" 
        `shouldBe` "sprclfrglstcxpldcs"
    it "works when there's nothing to remove" $ do
      removeAll [] [1..10] `shouldBe` [1..10]

  describe "sublist" $ do
    it "works for the provided test cases" $ do
      sublist (2,7) [0..10] `shouldBe` ([2,3,4,5,6],[0,1,7,8,9,10])
      sublist (3,4) [0..10] `shouldBe` ([3],[0,1,2,4,5,6,7,8,9,10])
      sublist (5,5) [0..10] `shouldBe` ([],[0,1,2,3,4,5,6,7,8,9,10])
      sublist (0,12) "hello world!" `shouldBe` ("hello world!","")
      sublist (6,100) "hello world!" `shouldBe` ("world!","hello ")

  describe "luhn" $ do
    it "works for the provided test cases" $ do
      luhn [2,7,5,8] `shouldBe` True
      luhn [4,3,1,7,5,6,8] `shouldBe` False
      luhn [3,9,2,8,6,4,1,7,2,0,5,2] `shouldBe` True
    it "has precisely one checksum value that works for a given value" $
      property prop_luhnOneSol

  describe "runLengthEncode" $ do
    it "works for the provided test cases" $ do
      runLengthEncode "aaaaaaabbb" `shouldBe` [(7,'a'),(3,'b')]
      runLengthEncode "happy daaay" `shouldBe` [(1,'h'),(1,'a'),(2,'p'),(1,'y'),(1,' '),(1,'d'),(3,'a'),(1,'y')]
    it "doesn't encode adjacent duplicates" $
      property prop_runLengthEncodeNoAdjDups
    it "has only positive counts" $
      property prop_runLengthEncodePosCounts

  describe "runLengthDecode" $ do
    it "works for the provided test cases" $ do
      runLengthDecode [(1,'h'), (5,'i')] `shouldBe` "hiiiii"
      runLengthDecode (runLengthEncode "whhhhaaaaat?") `shouldBe` "whhhhaaaaat?"
    it "reverses run-length encoding" $
      property prop_runLengthCodec

  describe "vigenere" $ do
    it "works for the provided test cases" $ do
      vigenere "baz" "foobar" `shouldBe` "GONCAQ"
      vigenere "Yadda" "Hello, world!" `shouldBe` "FEOOO, UOUOD!"
    it "throws an exception for an empty key" $ do
      evaluate (vigenere "" "foo") `shouldThrow` anyErrorCall


genString :: Gen String
genString = listOf $ elements $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']


prop_unzipLength :: [(Int,Int)] -> Bool
prop_unzipLength ts = let (l1,l2) = unzip ts
                   in length l1 == length l2 && length l1 == length ts

                   
genVectorOfPair :: (Arbitrary a, Arbitrary b) => Gen ([a], [b])
genVectorOfPair = sized $ \n -> do
  l1 <- vectorOf n arbitrary
  l2 <- vectorOf n arbitrary
  return (l1, l2)


prop_zipunzip :: Property
prop_zipunzip = forAll genVectorOfPair $ \(l1, l2) -> 
    let l3 = zip l1 l2
        (l4,l5) = unzip l3
        types = (l1 :: [Int], l2 :: [Int])
    in l1 == l4 && l2 == l5


genLuhnCandidates :: Gen [[Int]]
genLuhnCandidates = do 
  l <- listOf1 $ choose (0,9)
  return [l++[check] | check <- [0..9]]  


prop_luhnOneSol :: Property
prop_luhnOneSol = forAll genLuhnCandidates $ \cs ->
  length (filter luhn cs) == 1


prop_runLengthCodec :: Property  
prop_runLengthCodec = forAll genString $ \s -> 
  runLengthDecode (runLengthEncode s) == s


prop_runLengthEncodeNoAdjDups :: Property
prop_runLengthEncodeNoAdjDups = forAll genString $ \s ->
    noAdjDups $ runLengthEncode s
  where noAdjDups [] = True
        noAdjDups [x] = True
        noAdjDups s@((_,c1):(_,c2):cs) = c1 /= c2 && noAdjDups (tail s)


prop_runLengthEncodePosCounts :: Property
prop_runLengthEncodePosCounts = forAll genString $ \s ->
    all ((> 0) . fst) $ runLengthEncode s
