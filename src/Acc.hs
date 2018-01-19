{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Acc where

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

type IMP = IM.IntMap [DPlay]
type HMP = HM.HashMap Int (HM.HashMap B.ByteString Int) 
type HT = HT.BasicHashTable Int (HT.BasicHashTable B.ByteString Int)
type VV = VM.IOVector (VUM.IOVector Int)

emptyIntMap :: IMP
emptyIntMap = IM.empty

emptyHashMap ::HMP
emptyHashMap = HM.empty

emptyHashTable :: IO HT
emptyHashTable = HT.newSized 200

emptyPSHashTable :: IO (HT.BasicHashTable B.ByteString Int)
emptyPSHashTable = HT.newSized 120000

emptyVector:: IO VV
emptyVector = VM.replicateM 1000 (VUM.replicate 100000 0) 

sDate = dayHM "01-01-2017"

addDPlay :: DPlay -> IMP -> IMP
addDPlay dp@(DPlay p s d) = IM.insertWith (++) d [dp] 

addDPlayF = flip addDPlay

addDPlayHM :: HMP -> DPlay -> HMP
addDPlayHM h dp@(DPlay p s d) = HM.alter insertDayScore d h
  where
  insertDayScore mv = case mv of
    (Just v) -> Just $ HM.insertWith (+) p s v
    Nothing -> Just $ HM.singleton p s
    
addDPlayHT :: HT -> DPlay -> IO ()
addDPlayHT h dp@(DPlay p s d) = do
   l <- HT.lookup h d
   case l of
     (Just vh) -> HT.mutate vh p addScore 
       where
       addScore ms = case ms of
         (Just s') -> (Just (s + s'), ()) 
         Nothing -> (Just s, ())
     Nothing -> do
       nvh <- emptyPSHashTable
       HT.insert nvh p s
       HT.insert h d nvh

addDPSV :: Int -> VV -> DPS -> IO ()
addDPSV cd v dp@(d, p, s) = if (d < cd)
  then return ()
  else do
    dv <- VM.read v (d - cd)
    VUM.write dv p s

accHMPFromFile:: String -> IO HMP
accHMPFromFile file = do
  f <- B.readFile file
  return $ foldl' addDPlayHM emptyHashMap  $ parseDPlaysHM f 

accHMPFromFileC :: String -> IO HMP 
accHMPFromFileC file = do
  e <-runConduitRes $ (C.sourceFile file .| C.lines .| mapC (fst . parseDPHM) .| CON.foldlC addDPlayHM emptyHashMap)
  return e 

accIMPFromFile:: String -> IO IMP
accIMPFromFile file = do
  f <- B.readFile file
  return $ foldl' addDPlayF emptyIntMap  $ parseDPlaysHM f 

accIMPFromFileC :: String -> IO IMP 
accIMPFromFileC file = do
  e <-runConduitRes $ (C.sourceFile file .| C.lines .| mapC (fst . parseDPHM) .| CON.foldlC addDPlayF emptyIntMap)
  return e 

accHTFromFile:: String -> HT -> IO HT
accHTFromFile file h = do
  f <- B.readFile file
  h <- emptyHashTable
  forM_ (parseDPlaysHM f) (addDPlayHT h)
  return h

accHTFromFileC :: String -> HT -> IO HT 
accHTFromFileC file h = do
  e <- runConduitRes $ (C.sourceFile file .| C.lines .| mapC (fst . parseDPHM) .| CON.mapM_C (\c -> liftIO $ addDPlayHT h c))
  return h

accVFromFileC :: String -> Pid -> VV -> IO VV
accVFromFileC file pid v = do
  e <- runConduitRes $ (C.sourceFile file .| C.lines .| mapC (fst . parseDPHM) .| CON.mapM_C (\c -> liftIO $ do
      !dps <- getPid pid c
      addDPSV sDate v dps 
    ))
  return v


