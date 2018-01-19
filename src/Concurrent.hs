{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Concurrent where

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

import qualified Data.IntMap.Strict as IM
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as HT

allFromFileC :: String -> Pid -> VV -> IO VV
allFromFileC file pid v = do
  e <- runConduitRes $ (C.sourceFile file .| C.lines .| mapC (fst . parseDPHM) .| CON.mapM_C (\c -> liftIO $ do
      !dps <- getPid pid c
      addDPSV sDate v dps 
    ))
  return v


