{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Acc where

import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Mutable as VM
import Control.Monad.IO.Class (liftIO)
import Conduit as CON
import Play
import Id 

type VV = VM.IOVector (VUM.IOVector Int)

emptyVector :: IO VV
emptyVector = VM.replicateM 1000 (VUM.replicate 100000 0) 

sDate = dayHM "01-01-2017"

addDPSV :: Int -> VV -> DPS -> IO ()
addDPSV cd v (d, p, s) = if (d < cd)
  then return ()
  else do
    dv <- VM.read v (d - cd)
    VUM.write dv p s

sinkVV pid v = CON.mapM_C (\c -> liftIO $ do
      !dps <- getPid pid c
      addDPSV sDate v dps 
    )
