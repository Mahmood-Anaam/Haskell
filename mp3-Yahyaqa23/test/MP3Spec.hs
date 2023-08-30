{-# LANGUAGE ImplicitParams #-}

module MP3Spec (spec) where

import Test.Hspec
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck
import Control.Exception
import MP3a
import MP3b

spec :: Spec
spec = do
  describe "Binary tree" $ do
    describe "treeRepeat" $ do 
      it "returns the correct root node" $ do
        let (Node x _ _) = treeRepeat 10 in x `shouldBe` 10

    describe "treeNats" $ do 
      it "returns the correct root node" $ do
        let (Node x _ _) = treeNats in x `shouldBe` 1
      it "returns the correct level 1 nodes" $ do
        let (Node x (Node l _ _) _) = treeNats in l `shouldBe` 2
        let (Node x _ (Node r _ _)) = treeNats in r `shouldBe` 3

    describe "treeVal" $ do 
      it "returns the correct treeVal node" $ do
        let x=treeVal [L,R] treeNats in x `shouldBe` 5
      it "returns the correct treeVal node empty list" $ do
        let x=treeVal [] treeNats in x `shouldBe` 1 
      it "returns the correct treeVal  node" $ do
        let x=treeVal [L,R,R,L] treeNats in x `shouldBe` 22

    describe "treeToList" $ do 
      it "returns the correct list nodes " $ do
        let x=take 5 $ treeToList treeNats in x `shouldBe` [1,2,3,4,5]
      it "returns the correct list  nodes" $ do
        let x=take 10 $ treeToList treeNats in x `shouldBe` [1,2,3,4,5,6,7,8,9,10]
    
    describe "treeFlip" $ do 
      it "returns the correct list nodes fliping" $ do
        let x=take 5 (treeToList (treeFlip treeNats)) in x `shouldBe` [1,3,2,7,6]
      it "returns the correct list  nodes fliping" $ do
        let x=take 10 (treeToList (treeFlip treeNats)) in x `shouldBe` [1,3,2,7,6,5,4,15,14,13]

    describe "treeFromList" $ do 
      it "returns the correct list nodes treeFromList" $ do
        let x=take 5 $ treeToList $ treeFromList [1..] in x `shouldBe` [1,2,3,4,5]
      it "returns the correct list  nodes treeFromList" $ do
        let x=take 10 $ treeToList $ treeFromList [1..] in x `shouldBe` [1,2,3,4,5,6,7,8,9,10]

    describe "treeIterate" $ do 
      it "returns the correct  node treeIterate by treeVal" $ do
        let x=treeVal [R,R,R] $ treeIterate (2*) 1 in x `shouldBe` 16384
      it "returns the correct list  nodes treeIterate" $ do
        let x=take 15 $ treeToList $ treeFlip $ treeIterate (2*) 1 in x `shouldBe` [1,4,2,64,32,16,8,16384,8192,4096,2048,1024,512,256,128]
    
    describe "instance Functor" $ do 
      it "returns the correct  nodes fmap(1)" $ do
        let x=take 5 $ treeToList $ fmap (+2) treeNats in x `shouldBe` [3,4,5,6,7]
      it "returns the correct  nodes fmap(2)" $ do
        let x=take 5 $ treeToList $ fmap (*2) treeNats in x `shouldBe` [2,4,6,8,10]

    describe "instance Applicative" $ do 
      it "returns the correct  nodes(1)" $ do
        let x=take 20 $ treeToList $ (+) <$> treeNats <*> treeFlip treeNats in x `shouldBe` [2,5,5,11,11,11,11,23,23,23,23,23,23,23,23,47,47,47,47,47]
      it "returns the correct  nodes(1)" $ do
        let x=take 10 $ treeToList $ (*) <$> treeNats <*> treeFlip treeNats in x `shouldBe` [1,6,6,28,30,30,28,120,126,130]

  




  describe "Poker stats" $ do
    describe "deck" $ do      
      it "is the correct length" $ do
        length deck `shouldBe` 52

    describe "hand" $ do      
      it "is the correct hand(1)" $ do
        let x=hand [Card Two Hearts, Card Three Diamonds, Card Ace Hearts, Card Five Diamonds, Card Four Spades] in x `shouldBe` Straight
      it "is the correct hand(2)" $ do
        let x=hand [Card Two Diamonds, Card Three Clubs, Card Two Clubs, Card Three Diamonds, Card Two Hearts] in x `shouldBe` FullHouse
      it "is the correct hand(3)" $ do
        let x=hand [Card Ace Diamonds, Card King Diamonds, Card Queen Diamonds, Card Jack Diamonds, Card Ten Diamonds] in x `shouldBe` RoyalFlush
      it "is the correct hand(4)" $ do
        let x=hand [Card Ace Diamonds, Card Ace Clubs, Card Queen Diamonds, Card Queen Hearts, Card Ten Diamonds] in x `shouldBe` TwoPair
  
    describe "checksameSuits" $ do      
      it "is the correct checksameSuits" $ do
        let x=checksameSuits [Card Ace Diamonds, Card King Diamonds, Card Queen Diamonds, Card Jack Diamonds, Card Ten Diamonds] in x `shouldBe` RoyalFlush
   
    describe "checknotsameSuits" $ do      
      it "is the correct checknotsameSuits" $ do
        let x=hand [Card Two Diamonds, Card Three Clubs, Card Two Clubs, Card Three Diamonds, Card Two Hearts] in x `shouldBe` FullHouse
    
    describe "issquenst" $ do      
      it "is the correct issquenst(1)" $ do
        let x=issquenst $ sortdecs [Two,Three,Two ,Three,Two] in x `shouldBe` False
      it "is the correct issquenst(2)" $ do
        let x=issquenst $ sortdecs [Ace,Two,Three,Four,Five] in x `shouldBe` True
    

    describe "computeStats" $ do      
      it "is the correct computeStats(1)" $ do
        let x=computeStats [[Card Two Diamonds, Card Three Clubs, Card Two Clubs, Card Three Diamonds, Card Two Hearts],[Card Ace Diamonds, Card King Diamonds, Card Queen Diamonds, Card Jack Diamonds, Card Ten Diamonds]] in x `shouldBe` [(1,RoyalFlush),(1,FullHouse)]
      it "is the correct computeStats(2)" $ do
        let x=computeStats [[Card Two Diamonds, Card Three Clubs, Card Two Clubs, Card Three Diamonds, Card Two Hearts],[Card Two Diamonds, Card Three Clubs, Card Two Clubs, Card Three Diamonds, Card Two Hearts],[Card Ace Diamonds, Card King Diamonds, Card Queen Diamonds, Card Jack Diamonds, Card Ten Diamonds]] in x `shouldBe` [(2,FullHouse),(1,RoyalFlush)]
    
    
              
  