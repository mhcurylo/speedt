{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Date where

import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit.Binary as C
import qualified Data.Time.Calendar as T
import GHC.Generics (Generic, Generic1)
import Control.DeepSeq
import Data.Maybe (fromJust)
import Data.List (maximum)


data Play = Play {
    player :: !B.ByteString
  , score  :: !Int
  , date   :: !B.ByteString
} deriving (Show, Eq, Ord, Generic, NFData)

data DPlay = DPlay {
    dplayer :: !B.ByteString
  , dscore  :: !Int
  , ddate   :: !Int
} deriving (Show, Eq, Ord, Generic, NFData)


parsePlays :: B.ByteString -> [Play]
parsePlays s = if B.null r' 
  then p : []
  else p : (parsePlays r')
  where
  !(p,r) = parseP s
  !r' = B.tail r

parseP :: B.ByteString -> (Play, B.ByteString)
parseP s = (Play a b d, ds)
  where
  !(a,as) = B.break (==',') s
  !as' = B.tail as
  !(b,bs) = fromJust $ B.readInt as'
  !(d,ds) = B.break (=='\n') (B.tail bs)

emptyPlay = Play "" 0 ""

dplaysFromFile :: String -> IO [Play]
dplaysFromFile  file = do
  f <- B.readFile file
  return $ parsePlays f 
