{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main
import Data.Time.Calendar (fromGregorian)
import Data.ByteString (ByteString)
import Play
import Id
import Acc
import FileData
import Concurrent

main = allTest

exampleLine :: ByteString
exampleLine = "All,19000,20-11-2012\n"
exdate :: ByteString
exdate = "20-11-2012\n"

fileDataTest = do
  v <- emptyFDV
  v1 <- emptyFDV
  defaultMain [
     bench "FDVECTOR Conduit sample" $ whnfIO (getFileDataVC "fixtures/sample" v)
   , bench "FileData Conduit sample" $ whnfIO (getFileDataC "fixtures/sample")
   , bench "FDVECTOR Conduit large_sample" $ whnfIO (getFileDataVC "fixtures/large_sample" v1)
   , bench "FileData Conduit large_sample" $ whnfIO (getFileDataC "fixtures/large_sample")
   ]

allTest = do
  fv <- emptyFDV
  fv1 <- emptyFDV
  vv <- emptyVector
  pid <- emptyPid
  vv1 <- emptyVector
  pid1 <- emptyPid
  vv2 <- emptyVector
  pid2 <- emptyPid
  vv3 <- emptyVector
  pid3 <- emptyPid
  defaultMain [
     bench "PTSM Conduit sample" $ whnfIO (readFileS "fixtures/sample" pid vv)
   , bench "FDVVVM Conduit sample" $ whnfIO (readFileM "fixtures/sample" fv pid vv)
   , bench "PTSM Conduit large sample" $ whnfIO (readFileS "fixtures/large_sample" pid vv)
   , bench "FDVVVM Conduit large sample" $ whnfIO (readFileM "fixtures/large_sample" fv pid vv)
   ]


