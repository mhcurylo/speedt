module Main (main) where

import Criterion.Main
import Lib 
import BSParse 
import Wc

main = defaultMain [
--    bench "wc  fixtures/sample"  $ nfIO (wc "fixtures/sample")
--    bench "strictWc fixtures/sample"  $ nfIO (strictWc "fixtures/sample")
--  , bench "conduitWc fixtures/sample"  $ nfIO (conduitWc "fixtures/sample")
--  , bench "vecStrictWc fixtures/sample"  $ nfIO (vecStrictWc "fixtures/sample")
--  , bench "vecConduitWc fixtures/sample"  $ nfIO (vecConduitWc "fixtures/sample")
--  , bench "mvStrictWc fixtures/sample"  $ nfIO (mvStrictWc "fixtures/sample")
--  , bench "mvConduitWc fixtures/sample"  $ nfIO (mvConduitWc "fixtures/sample")
--  , bench "paraMvConduitWc fixtures/sample"  $ nfIO (paraMvConduitWc "fixtures/sample")
--  , bench "strictWc fixtures/large_sample"  $ nfIO (strictWc "fixtures/large_sample")
--  , bench "conduitWc fixtures/large_sample"  $ nfIO (conduitWc "fixtures/large_sample")
--  , bench "vecStrictWc fixtures/large_sample"  $ nfIO (vecStrictWc "fixtures/large_sample")
--  , bench "vecConduitWc fixtures/large_sample"  $ nfIO (vecConduitWc "fixtures/large_sample")
--  , bench "mvStrictWc fixtures/large_sample"  $ nfIO (mvStrictWc "fixtures/large_sample")
--  , bench "mvConduitWc fixtures/large_sample"  $ nfIO (mvConduitWc "fixtures/large_sample")
--  , bench "paraMvConduitWc fixtures/large_sample"  $ nfIO (paraMvConduitWc "fixtures/large_sample")
--  , bench "wc  fixtures/large_large_sample"  $ nfIO (wc "fixtures/large_large_sample")
--  , bench "strictWc fixtures/large_sample"  $ nfIO (strictWc "fixtures/large_sample")
--  , bench "conduitWc fixtures/large_sample"  $ nfIO (conduitWc "fixtures/large_sample")
    bench "plays fixtures/sample"  $ nfIO (playsFromFile "fixtures/sample")
  , bench "conduit plays fixtures/sample"  $ nfIO (playsFromFileC "fixtures/sample")
  , bench "strict plays fixtures/sample"  $ nfIO (strictPlaysFromFile "fixtures/sample")
  , bench "plays fixtures/large_sample"  $ nfIO (playsFromFile "fixtures/large_sample")
  , bench "conduit plays fixtures/large_sample"  $ nfIO (playsFromFileC "fixtures/large_sample")
  , bench "strict plays fixtures/large_sample"  $ nfIO (strictPlaysFromFile "fixtures/large_sample")
  ]
