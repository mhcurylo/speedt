module Main where

import Lib

main :: IO ()
main = do
  p <- playsFromFileC "fixtures/large_sample" 
  print $ show p
