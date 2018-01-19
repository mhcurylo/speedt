{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module PlayerId where

import GHC.Generics (Generic, Generic1)
import Control.DeepSeq
import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit.Binary as C
import qualified Control.Concurrent.MVar.Strict as M
import qualified Control.Concurrent.MVar as ML
import qualified Control.Concurrent.Chan.Strict as MC
import Control.Monad (forM_, forever)
import qualified Data.Conduit.List as CL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.List (foldl')
import Control.Monad.IO.Class (liftIO)
import Date

import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as HT

import Conduit 
import Date

type DPS = (Int, Int, Int)

data PidHm = PidHm {
    phm :: HM.HashMap B.ByteString Int
  , nhm :: Int 
} deriving (Generic, NFData)

data PidHt = PidHt {
    pht :: HT.BasicHashTable B.ByteString Int
  , nht :: VM.IOVector Int
}  

emptyPidHm = PidHm HM.empty 0
emptyPidHmM = M.newMVar emptyPidHm

emptyPidHt = do
  t <- HT.newSized 110000 
  v <- VM.replicate 1 0
  return $ PidHt t v

emptyPidHtM = emptyPidHt >>= ML.newMVar 

insertPidHm :: B.ByteString -> PidHm -> (PidHm, Int)
insertPidHm bs hm@(PidHm p n) = case HM.lookup bs p of
  (Just v) -> (hm, v) 
  Nothing -> (PidHm (HM.insert bs n p) (n + 1), n)

insertPidHmM :: M.MVar PidHm -> B.ByteString -> IO Int
insertPidHmM mp bs = M.modifyMVar mp $ return . insertPidHm bs

insertPidHt :: PidHt -> B.ByteString -> IO Int
insertPidHt ph@(PidHt ht vn) bs = do
  mv <- HT.lookup ht bs 
  case mv of
    (Just v) -> return v
    Nothing -> do
      n <- VM.read vn 0
      HT.insert ht bs n
      VM.write vn 0 (n + 1)
      return n
  
insertPidHtM :: M.MVar PidHt -> B.ByteString -> IO Int
insertPidHtM mp bs = do
  m <- ML.takeMVar mp
  n <- insertPidHt m bs
  ML.putMVar mp m
  return n

type Pid = PidHt

emptyPid = emptyPidHt
getPid :: Pid -> DPlay -> IO DPS
getPid pid (DPlay p s d)= do
  i <- insertPidHt pid p
  return (d, i, s)

emptyPidM = emptyPidHtM
getPidM :: ML.MVar Pid -> DPlay -> IO DPS
getPidM  pid (DPlay p s d)= do
  i <- insertPidHtM pid p
  return (d, i, s)
