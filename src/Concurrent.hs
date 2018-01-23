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
import Data.Conduit.Async

import qualified Data.IntMap.Strict as IM
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as HT
import Control.Concurrent.STM.TBQueue (readTBQueue)

import Id 
import Play
import FileData
import Acc

vvSink pid v = CON.mapM_C (\c -> liftIO $ do
        !dps <- getPid pid c
        addDPSV Acc.sDate v dps 
      )

v2Sink fdv pid v =  CON.mapM_C (\c@(Play f d _ _) -> liftIO $ do
        fdvUpdate f d fdv
        !dps <- getPid pid c
        addDPSV Acc.sDate v dps 
      )

readFileS :: String -> Id -> VV -> IO (VV, FileData)
readFileS file pid v = do
  e <- runConduitRes $ (C.sourceFile file 
      .| C.lines 
      .| mapC (parsePlay 0) 
      .| passthroughSink (vvSink pid v) (\_ -> return ())
      .| fdSink
      )
  return $ (v, e)

readFileM :: String -> FDV -> Id -> VV -> IO (VV, FDV)
readFileM file fdv pid v = do
  runConduitRes $ (C.sourceFile file 
      .| C.lines 
      .| mapC (parsePlay 0) 
      .| v2Sink fdv pid v
      )
  return $ (v, fdv)


