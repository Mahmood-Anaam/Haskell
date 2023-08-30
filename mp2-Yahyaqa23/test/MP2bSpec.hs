{-# LANGUAGE ImplicitParams #-}

module MP2bSpec (spec) where

import Test.Hspec
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck
import Control.Exception
import MP2b



spec :: Spec
spec = do
  describe "makeWorld" $ do
    it "creates worlds of the correct size" $ do
      property prop_worldSize
      
  describe "liveNeighbors" $ do
    it "works correctly" $ do
      liveNeighbors (makeWorld (10,10)) (5,5)  `shouldBe` 6
      liveNeighbors ((5,5),[[True,True,True,True,True],[False,True,False,False,True],[True,False,True,False,True],[True,True,False,True,False],[False,False,False,False,False]]) (2,2)  `shouldBe` 3
      liveNeighbors ((5,5),[[True,True,True,True,True],[False,True,False,False,True],[True,False,True,False,True],[True,True,False,True,False],[False,False,False,False,False]]) (2,4)  `shouldBe` 2

  describe "nextWorld" $ do
    it "works correctly" $ do
      nextWorld (makeWorld (10,10)) `shouldBe` ((10,10),[[False,False,False,False,False,False,False,False,False,False],[True,True,False,True,False,True,False,True,False,True],[True,True,False,True,False,True,False,True,False,True],[True,True,False,True,False,True,False,True,False,True],[True,True,False,True,False,True,False,True,False,True],[True,True,False,True,False,True,False,True,False,True],[True,True,False,True,False,True,False,True,False,True],[True,True,False,True,False,True,False,True,False,True],[True,True,False,True,False,True,False,True,False,True],[False,False,False,False,False,False,False,False,False,False]])

      nextWorld  ((5,5),[[True,True,True,True,True],[False,True,False,False,True],[True,False,True,False,True],[True,True,False,True,False],[False,False,False,False,False]]) `shouldBe` ((5,5),[[False,False,False,False,False],[False,True,True,True,True],[True,False,True,False,True],[True,False,False,False,False],[True,True,True,True,True]])
      


prop_worldSize :: Property
prop_worldSize = forAll dims $ \(w,h) -> 
                   let (d, world) = makeWorld (w,h)
                   in d == (w,h) 
                      && length world == h 
                      && all ((== w) . length) world
  where dims = arbitrary `suchThat` (\(x,y) -> x > 0 && y > 0)
