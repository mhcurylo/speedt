{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Id where

import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit.List as CL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Mutable as VM
import Data.List (foldl')
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashTable.IO as HT

import Play  

data Id = Id {
    pht :: HT.BasicHashTable B.ByteString Int
  , nht :: VM.IOVector Int
}  

emptyId s = do
  t <- HT.newSized s 
  v <- VM.replicate 1 0
  return $ Id t v

insertId :: Id -> B.ByteString -> IO Int
insertId (Id ht vn) bs = do
  mv <- HT.lookup ht bs 
  case mv of
    (Just v) -> return v
    Nothing -> do
      n <- VM.read vn 0
      HT.insert ht bs n
      VM.write vn 0 (n + 1)
      return n

emptyFid = emptyId 100000

emptyPid = emptyId 110000

type DPS = (Int, Int, Int)

getPid :: Id -> Play -> IO DPS
getPid pid (Play _ d p s)= do
  i <- insertId pid p
  return (d, i, s)

toIdVector :: Id -> IO (V.Vector B.ByteString)
toIdVector (Id ti vi) = do
  i <- VM.read vi 0
  vec <- VM.replicate i "" 
  HT.mapM_ (\(k, v) -> VM.write vec v k) ti
  V.unsafeFreeze vec
