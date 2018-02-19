{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main
import Data.Time.Calendar (fromGregorian)
import Data.ByteString (ByteString)
import Lib 
import BSParse 
import Wc
import Date
import Acc
import PlayerId
import FileData

main = do
  fileDataTest
  playIdTest
  dateTests
  accTests
  playsTests 
  wcTests 

exampleLine :: ByteString
exampleLine = "All,19000,20-11-2012\n"
exdate :: ByteString
exdate = "20-11-2012\n"


fileDataTest = do
  pid <- emptyPid
  v <- emptyVector
  pid1 <- emptyPid
  v1 <- emptyVector
  defaultMain [
     bench "VECTOR Conduit sample" $ whnfIO (accVFromFileC "fixtures/sample" pid v)
   , bench "FileData Conduit sample" $ nfIO (getFileDataC "fixtures/sample")
   , bench "VECTOR Conduit large sample" $ whnfIO (accVFromFileC "fixtures/large_sample" pid1 v1)
   , bench "FileData Conduit large sample" $ nfIO (getFileDataC "fixtures/large_sample")
   ]



playIdTest = do
  mhm <- emptyPidHmM
  mht <- emptyPidHtM
  ht <- emptyPidHt
  let eph' = fst $ insertPidHm "boo" emptyPidHm
  defaultMain [
      bench "pure HashMap"        $ nf (insertPidHm "boo") emptyPidHm
    , bench "pure HashMap lookup" $ nf (insertPidHm "boo") eph'
    , bench "MVar HashMap"        $ nfIO (insertPidHmM mhm "boo")
    , bench "only HashTable"      $ whnfIO (insertPidHt ht "boo")
    , bench "MVar HashTable"      $ whnfIO (insertPidHtM mht "boo")
    ]

dateTests = defaultMain [
    bench "fromGregorian" $ nf (fromGregorian 2012 11) 20
  , bench "parse 1 line" $ nf sParseP exampleLine
  , bench "dparse 1 line" $ nf parseDP exampleLine
  , bench "dparseHM 1 line" $ nf parseDPHM exampleLine
  , bench "parse date day" $ nf parseDate exdate
  , bench "parse date day HashMap" $ nf parseDateHM exdate
  , bench "day" $ nf day (20, 11, 2012)
  , bench "day HM" $ nf dayHM "20-11-2012"
  ]

accTests = do
  ht <- emptyHashTable
  ht1 <- emptyHashTable
  ht2 <- emptyHashTable
  ht3 <- emptyHashTable
  pid <- emptyPid
  v <- emptyVector
  pid1 <- emptyPid
  v1 <- emptyVector
  defaultMain [
      bench "Parse  BS      sample" $ nfIO (dplaysFromFileHM "fixtures/sample")
    , bench "IntMap BS      sample" $ nfIO (accIMPFromFile "fixtures/sample")
    , bench "IntMap Conduit sample" $ nfIO (accIMPFromFileC "fixtures/sample")
    , bench "HashMap BS      sample" $ nfIO (accHMPFromFile "fixtures/sample")
    , bench "HashMap Conduit sample" $ nfIO (accHMPFromFileC "fixtures/sample")
    , bench "HashTable BS      sample" $ whnfIO (accHTFromFile "fixtures/sample" ht)
    , bench "HashTable Conduit sample" $ whnfIO (accHTFromFileC "fixtures/sample" ht1)
    , bench "VECTOR Conduit sample" $ whnfIO (accVFromFileC "fixtures/sample" pid v)
    , bench "Parse  BS      large sample" $ nfIO (dplaysFromFileHM "fixtures/large_sample")
    , bench "IntMap BS      large sample" $ nfIO (accIMPFromFile "fixtures/large_sample")
    , bench "IntMap Conduit large sample" $ nfIO (accIMPFromFileC "fixtures/large_sample")
    , bench "HashMap BS      large sample" $ nfIO (accHMPFromFile "fixtures/large_sample")
    , bench "HashMap Conduit large sample" $ nfIO (accHMPFromFileC "fixtures/large_sample")
    , bench "HashTable BS      large sample" $ whnfIO (accHTFromFile "fixtures/large_sample" ht2)
    , bench "HashTable Conduit large sample" $ whnfIO (accHTFromFileC "fixtures/large_sample" ht3)
    , bench "VECTOR Conduit large sample" $ whnfIO (accVFromFileC "fixtures/large_sample" pid1 v1)
    ]


playsTests = defaultMain [
--    bench "plays fixtures/sample"  $ nfIO (playsFromFile "fixtures/sample")
    bench "conduit plays fixtures/sample"  $ nfIO (playsFromFileC "fixtures/sample")
  , bench "conduit dplays fixture/sample" $ nfIO (dplaysFromFileC "fixtures/sample")
  , bench "conduit dplays HM fixture/sample" $ nfIO (dplaysFromFileCHM "fixtures/sample")
  , bench "strict plays fixtures/sample"  $ nfIO (strictPlaysFromFile "fixtures/sample")
  , bench "strict dplays fixture/sample" $ nfIO (dplaysFromFile "fixtures/sample")
  , bench "strict dplays HM fixture/sample" $ nfIO (dplaysFromFileHM "fixtures/sample")
--  , bench "plays fixtures/large_sample"  $ nfIO (playsFromFile "fixtures/large_sample")
  , bench "conduit plays fixtures/large_sample"  $ nfIO (playsFromFileC "fixtures/large_sample")
  , bench "conduit  dplays fixture/large_sample" $ nfIO (dplaysFromFileC "fixtures/large_sample")
  , bench "conduit  dplays HM fixture/large_sample" $ nfIO (dplaysFromFileCHM "fixtures/large_sample")
  , bench "strict plays fixtures/large_sample"  $ nfIO (strictPlaysFromFile "fixtures/large_sample")
  , bench "strict dplays fixture/large_sample" $ nfIO (dplaysFromFile "fixtures/large_sample")
  , bench "strict dplays hm fixture/large_sample" $ nfIO (dplaysFromFileHM "fixtures/large_sample")
  ] 


wcTests = defaultMain [
    bench "wc  fixtures/sample"  $ nfIO (wc "fixtures/sample")
  , bench "strictWc fixtures/sample"  $ nfIO (strictWc "fixtures/sample")
  , bench "conduitWc fixtures/sample"  $ nfIO (conduitWc "fixtures/sample")
  , bench "vecStrictWc fixtures/sample"  $ nfIO (vecStrictWc "fixtures/sample")
  , bench "vecConduitWc fixtures/sample"  $ nfIO (vecConduitWc "fixtures/sample")
  , bench "mvStrictWc fixtures/sample"  $ nfIO (mvStrictWc "fixtures/sample")
  , bench "mvConduitWc fixtures/sample"  $ nfIO (mvConduitWc "fixtures/sample")
  , bench "paraMvConduitWc fixtures/sample"  $ nfIO (paraMvConduitWc "fixtures/sample")
  , bench "wc  fixtures/large_sample"  $ nfIO (wc "fixtures/large_sample")
  , bench "strictWc fixtures/large_sample"  $ nfIO (strictWc "fixtures/large_sample")
  , bench "conduitWc fixtures/large_sample"  $ nfIO (conduitWc "fixtures/large_sample")
  , bench "vecStrictWc fixtures/large_sample"  $ nfIO (vecStrictWc "fixtures/large_sample")
  , bench "vecConduitWc fixtures/large_sample"  $ nfIO (vecConduitWc "fixtures/large_sample")
  , bench "mvStrictWc fixtures/large_sample"  $ nfIO (mvStrictWc "fixtures/large_sample")
  , bench "mvConduitWc fixtures/large_sample"  $ nfIO (mvConduitWc "fixtures/large_sample")
  , bench "paraMvConduitWc fixtures/large_sample"  $ nfIO (paraMvConduitWc "fixtures/large_sample")
  ]
