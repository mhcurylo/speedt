{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module BSParse where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Conduit.Binary as C
import GHC.Generics (Generic, Generic1)
import Control.DeepSeq
import Data.Maybe (fromJust)
import Data.List (maximum)
import Conduit

data Play = Play {
    player :: !BL.ByteString
  , score  :: !Int
  , date   :: !BL.ByteString
} deriving (Show, Eq, Ord, Generic, NFData)

data SPlay = SPlay {
    sPlayer :: !B.ByteString
  , sScore  :: !Int
  , sDate   :: !B.ByteString
} deriving (Show, Eq, Ord, Generic, NFData)


parsePlays :: BL.ByteString -> [Play]
parsePlays s = if BL.null r' 
  then p : []
  else p : (parsePlays r')
  where
  !(p,r) = parseP s
  !r' = BL.tail r

parseP :: BL.ByteString -> (Play, BL.ByteString)
parseP s = (Play a b d, ds)
  where
  !(a,as) = BL.break (==',') s
  !as' = BL.tail as
  !(b,bs) = fromJust $ BL.readInt as'
  !(d,ds) = BL.break (=='\n') (BL.tail bs)

sParsePlays :: B.ByteString -> [SPlay]
sParsePlays s = if B.null r' 
  then p : []
  else p : (sParsePlays r')
  where
  !(p,r) = sParseP s
  !r' = B.tail r

sParseP :: B.ByteString -> (SPlay, B.ByteString)
sParseP s = (SPlay a b d, ds)
  where
  !(a,as) = B.break (==',') s
  !as' = B.tail as
  !(b,bs) = fromJust $ B.readInt as'
  !(d,ds) = B.break (=='\n') (B.tail bs)

emptyPlay = Play "" 0 ""
emptyStrictPlay = SPlay "" 0 ""

playsFromFile :: String -> IO [Play]
playsFromFile  file = do
  f <- BL.readFile file
  return $ parsePlays f 

strictPlaysFromFile :: String -> IO [SPlay]
strictPlaysFromFile  file = do
  f <- B.readFile file
  return $ sParsePlays f 
