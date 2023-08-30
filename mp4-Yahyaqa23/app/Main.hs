module Main where

import IMP

main :: IO ()
main = interact $ unlines 
                  . map (either id show . (flip parseEval [])) 
                  . lines
