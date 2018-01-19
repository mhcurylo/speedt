module Main where

import qualified Data.IntMap.Strict as IM
import Acc

main :: IO ()
main = do
  p <- accIMPFromFileC "fixtures/large_sample" 
  print $ show . fst $ IM.findMax p
