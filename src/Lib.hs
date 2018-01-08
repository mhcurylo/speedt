{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Lib where

import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit.Binary as C
import GHC.Generics (Generic, Generic1)
import Control.DeepSeq
import Data.Maybe (fromJust)
import Conduit

data Play = Play {
    player :: !B.ByteString
  , score  :: !Int
  , date   :: !B.ByteString
} deriving (Show, Eq, Ord, Generic, NFData)

parsePlay :: B.ByteString -> Play
parsePlay s = Play a b d
  where
  !(a,as) = B.break (==',') s
  !as' = B.tail as
  !(b,bs) = fromJust $ B.readInt as'
  (!d,_) = B.break (=='\n') (B.tail bs)

emptyPlay = Play "" 0 ""

playsFromFileC :: String -> IO [Play]
playsFromFileC file = do
  e <-runConduitRes $ (C.sourceFile file .| C.lines .| mapC parsePlay .| maximumC)
  return $ [fromJust e]

