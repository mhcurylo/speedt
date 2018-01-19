{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module FileData where

import GHC.Generics (Generic, Generic1)
import Control.DeepSeq
import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit.Binary as C
import qualified Control.Concurrent.MVar.Strict as M
import qualified Control.Concurrent.Chan.Strict as MC
import Control.Monad (forM_, forever)
import qualified Data.Conduit.List as CL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Mutable as VM
import Data.List (foldl')
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Conduit as CON
import Date
import PlayerId

import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as HT


type FileData = (Int, Int)

initFileData = (99999, 0)

sDate = dayHM "01-01-2016"

laterThan :: Int -> DPlay -> Bool
laterThan cd (DPlay _ _ d) = d > (cd + 99)
  

updateFileData :: FileData -> DPlay -> FileData
updateFileData fd@(f, l) (DPlay _ _ d) 
  | dsf && dll = (d, d)
  | dsf = (d, l)
  | dll = (f, d) 
  | otherwise = fd
  where
  dsf = d < f
  dll = d > l

getFileDataC:: String -> IO FileData
getFileDataC file = runConduitRes $ C.sourceFile file 
                      .| C.lines 
                      .| mapC (fst . parseDPHM) 
                      .| filterC (laterThan sDate)
                      .| foldlC updateFileData initFileData


