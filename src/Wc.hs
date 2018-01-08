{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Wc  where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Conduit.Binary as C
import qualified Control.Concurrent.MVar.Strict as M
import qualified Control.Concurrent.Chan.Strict as MC
import Control.Monad (forM_, forever)
import qualified Data.Conduit.List as CL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Conduit

type VC = VM.IOVector Int
newVCounter :: IO VC
newVCounter = VM.replicate 1 0

incVCounter :: VC -> a -> IO ()
incVCounter vc _ = VM.modify vc (+ 1) 0 

newCounter :: IO (M.MVar Int)
newCounter = M.newMVar 0

incCounter :: (M.MVar Int) -> a -> IO ()
incCounter m _ = M.modifyMVar_ m (return . (+ 1))
  
newLineChan :: IO (MC.Chan B.ByteString)
newLineChan = MC.newChan 

incCounterMany :: (M.MVar Int) -> V.Vector B.ByteString -> IO ()
incCounterMany m vec = do
   v <- M.takeMVar m 
   M.putMVar m (V.foldl' (\a _ -> a + 1) v vec)

writeToCounter :: (M.MVar Int) -> MC.Chan B.ByteString -> IO ()
writeToCounter m c = do
  forkIO . forever $ do
    p <- MC.readChan c
    incCounter m p
  return ()  

wc :: String -> IO Int
wc file = do
  f <- BL.readFile file
  return $ length . BL.lines $ f

strictWc :: String -> IO Int
strictWc file = do
  f <- B.readFile file
  return $ length . B.lines $ f

conduitWc :: String -> IO Int
conduitWc file = runConduitRes $ (C.sourceFile file .| C.lines .| lengthC)

vecStrictWc :: String -> IO Int
vecStrictWc file = do
  m <- newVCounter
  f <- B.readFile file
  forM_ (B.lines f) $ incVCounter m
  v <- VM.read m 0
  return v
 
vecConduitWc :: String -> IO Int
vecConduitWc file = do
  m <- newVCounter
  runConduitRes $ (C.sourceFile file .| C.lines .| (CL.mapM_ (liftIO . incVCounter m)))
  v <- VM.read m 0
  return v
 
mvStrictWc :: String -> IO Int
mvStrictWc file = do
  m <- newCounter
  f <- B.readFile file
  forM_ (B.lines f) $ incCounter m
  v <- M.readMVar m
  return v

mvConduitWc :: String -> IO Int
mvConduitWc file = do
  m <- newCounter
  runConduitRes $ (C.sourceFile file .| C.lines .| (CL.mapM_ (liftIO . incCounter m)))
  v <- M.readMVar m
  return v
 
paraMvConduitWc :: String -> IO Int
paraMvConduitWc file = do
  c <- newLineChan
  m <- newCounter
  writeToCounter m c
  writeToCounter m c
  writeToCounter m c
  writeToCounter m c
  runConduitRes $ (C.sourceFile file .| C.lines .| (CL.mapM_ (liftIO . MC.writeChan c)))
  v <- M.readMVar m
  return v
