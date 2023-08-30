module Main where

import Graphics.Gloss
import MP1


main :: IO ()
main = animate (InWindow "MP1" (500,500) (10,10))
               black
               picture
  where picture t = pictures [color azure  $ drawOrbit  150 8 t, 
                              color violet $ drawOrbit' 150 0.6 8 t,
                              color yellow $ circleSolid 10]
