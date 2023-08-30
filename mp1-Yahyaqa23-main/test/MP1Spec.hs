{-# LANGUAGE ImplicitParams #-}

module MP1Spec (spec) where

import Test.Hspec
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck
import Control.Exception
import MP1 (p1_1, p1_2, p1_3, p1_4,
            transposeTup, sort3Tup, compoundInterest, collatzLen, newtonsSqrt,
            meanMotion, meanAnomaly, eccentricAnomaly, trueAnomaly, heliocentricDist)


spec :: Spec
spec = describe "MP1" $ do
  describe "p1_1" $ do
    it "works correctly" $ do
      p1_1 1 2 @?= 2
      p1_1 'a' 'b' @?= 'b'
      p1_1 True False @?= False
  
  describe "p1_2" $ do
    it "works correctly" $ do
      p1_2 (+) (1,2) @?= 3
      p1_2 (++) ("hello","world") @?= "helloworld"
      p1_2 (==) (1,1) @?= True
      p1_2 (==) (1,2) @?= False

  describe "p1_3" $ do
    it "works correctly" $ do
      p1_3 (+1) (*2) 3 @?= 8
      p1_3 (*2) (+2) 4 @?= 10
      p1_3 show ("hello"++) 5 @?= "hello5"

  describe "p1_4" $ do
    it "works correctly" $ do
      p1_4 (+) 1 (+1) 3 @?= 5
      p1_4 (++) "hello" show 500 @?= "hello500"

  describe "transposeTup" $ do
    it "works correctly" $ do
      transposeTup ((1,2),(3,4)) @?= ((1,3),(2,4))
      transposeTup ((True, "yo"), (False, "dawg")) 
        @?= ((True, False), ("yo", "dawg"))

  describe "sort3Tup" $ do
    it "works correctly" $ do
      sort3Tup (1,2,3) @?= (1,2,3)
      sort3Tup (1,3,2) @?= (1,2,3)
      sort3Tup (2,1,3) @?= (1,2,3)
      sort3Tup (3,2,1) @?= (1,2,3)

  describe "compoundInterest" $ do
    it "works correctly" $ do
      let ?epsilon = 0.1
      compoundInterest 100 0.2 1 @?~ 20
      compoundInterest 100 0.2 2 @?~ 44
      compoundInterest 500 0.15 10 @?~ 1522.8
      compoundInterest 10000 0.025 50 @?~ 24371.1
  
  describe "collatzLen" $ do
    it "works correctly" $ do
      collatzLen 1 @?= 0
      collatzLen 6 @?= 8
      collatzLen 27 @?= 111
      collatzLen 1161 @?= 181

  describe "newtonsSqrt" $ do 
    it "works correctly" $ do
      property prop_newtonsSqrt

  describe "meanMotion" $ do
    it "works correctly" $ do
      property prop_meanMotion

  describe "meanAnomaly" $ do
    it "works correctly" $ do
      property prop_meanAnomaly

  describe "eccentricAnomaly" $ do
    it "works correctly" $ do
      property prop_eccentricAnomaly

  describe "trueAnomaly" $ do
    it "works correctly" $ do
      property prop_trueAnomaly

  describe "heliocentricDist" $ do
    it "works correctly" $ do
      property prop_heliocentricDist


infix 4 =~=
(=~=) :: (Floating a, Ord a, ?epsilon :: a) =>  a -> a -> Bool
x =~= y = abs (x - y) < ?epsilon


prop_newtonsSqrt :: (NonNegative Double) -> Bool
prop_newtonsSqrt (NonNegative x) = let r = newtonsSqrt x in r*r =~= x
  where ?epsilon = 0.001


prop_meanMotion :: (Positive Double) -> Bool
prop_meanMotion (Positive p) = meanMotion p =~= 2*pi / p
  where ?epsilon = 0.001


prop_meanAnomaly :: (Positive Double) -> (Positive Double) -> Bool
prop_meanAnomaly (Positive n) (Positive t) = meanAnomaly n t =~= n * t
  where ?epsilon = 0.001


prop_eccentricAnomaly :: (Positive Double) -> Property
prop_eccentricAnomaly (Positive ma) = forAll (choose (0.0, 1.0))
    $ \ecc -> let e = eccentricAnomaly ecc ma
              in ma =~= e - ecc * sin e
  where ?epsilon = 0.001


prop_trueAnomaly :: Double -> Property
prop_trueAnomaly ea = forAll (choose (0.0, 1.0))
    $ \ecc -> let ta = trueAnomaly ecc ea
              in (1-ecc)*(tan (ta/2))^2 =~= (1+ecc)*(tan (ea/2))^2
  where ?epsilon = 0.01


prop_heliocentricDist :: (Positive Double) -> Double -> Property
prop_heliocentricDist (Positive a) ea = forAll (choose (0.0, 1.0))
    $ \ecc -> let r = heliocentricDist a ecc ea
              in r =~= a * (1 - ecc * cos ea)
  where ?epsilon = 0.01
