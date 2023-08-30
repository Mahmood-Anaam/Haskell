module Main where

import System.Environment  
import Data.List
import Data.List.Split
import Test.QuickCheck
import MP3b


genDeck :: Gen [Card]
genDeck = shuffle deck

genHand :: Gen [Card]
genHand = (take 5) <$> genDeck

genHands :: Int -> Gen [[Card]]
genHands n = (take n . chunksOf 5) <$> genDeck

test :: Int -> IO [(Int, Hand)]
test n = generate $  computeStats <$> (vectorOf n genHand)


main :: IO ()
main = do 
  args <- getArgs
  stats <- test $ (read $ head args :: Int)
  print stats
