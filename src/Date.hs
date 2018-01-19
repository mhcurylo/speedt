{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Date where

import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit.Binary as C
import qualified Data.Time.Calendar as T
import qualified Data.HashMap.Strict as HM
import GHC.Generics (Generic, Generic1)
import Control.DeepSeq
import Data.Maybe (fromJust)
import Data.List (maximum, zip3)
import Conduit

data DPlay = DPlay {
    dplayer :: !B.ByteString
  , dscore  :: !Int
  , ddate   :: !Int
} deriving (Show, Eq, Ord, Generic, NFData)

type DayOfYear = (Int, Int, Int)

dayZero = "01-01-2010"

dayZeroT = (1, 1, 2010)

nonLeap = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
leap = 31 : 29 : drop 2 nonLeap

yearLength y = if isLeapYear y 
                then leapYearLength
                else nonLeapYearLength

nonLeapYearLength = sum nonLeap
leapYearLength = nonLeapYearLength - 1

divisableBy x y = y `div` x == 0
isLeapYear x = r
  where 
  divBy y = y `div` x == 0
  !r = (divBy 4 || ((not $ divBy 100) || (divBy 400))) 

monthLength y m = if isLeapYear y
                    then leap !! (m - 1)
                    else nonLeap !! (m - 1)

daysFromDay :: DayOfYear -> DayOfYear -> Int 
daysFromDay (d, m, y) (d', m', y') = dd + dm + dy
  where
  !dy = sum $ map yearLength [2010..(y' - 1)]
  !dm = sum $ map (monthLength y) [1..(m' - 1 )]
  !dd = d' - d

day = daysFromDay dayZeroT

allDays = (1, 1, 2010) : map nextDay allDays
  where
  nextDay (d, m, y)
    | d == 31 && m == 12 = (1, 1, y + 1)
    | d == monthLength y m = (1, m + 1, y)
    | otherwise = (d + 1, m, y)

dayHMap :: HM.HashMap B.ByteString Int
dayHMap = dm
  where
  ensure2 bs = if B.length bs == 1 
                  then B.cons '0' bs
                  else bs
  toKV dat@(d, m, y) = (B.intercalate "-" $ map (ensure2 . B.pack . show) [d,m,y], day dat)
  !dm = HM.fromList $ map toKV $ take 20000 allDays

dayHM d = case HM.lookup d dayHMap of
  (Just r) -> r
  (Nothing) -> fst . parseDate $ d

parseDate :: B.ByteString -> (Int, B.ByteString)
parseDate bs = (i, ys)
  where
  !(d,ds) = fromJust $ B.readInt bs 
  !(m,ms) = fromJust $ B.readInt (B.tail ds) 
  !(y,ys) = fromJust $ B.readInt (B.tail ms) 
  !i = day (d, m, y)

parseDateHM :: B.ByteString -> (Int, B.ByteString)
parseDateHM bs = (i, vs)
  where
  !(v,vs) = B.break (=='\n') bs 
  !i = dayHM v

parseDPlays :: B.ByteString -> [DPlay]
parseDPlays s = if B.null r' 
  then p : []
  else p : (parseDPlays r')
  where
  !(p,r) = parseDP s
  !r' = B.tail r

parseDP :: B.ByteString -> (DPlay, B.ByteString)
parseDP s = (DPlay a b d, ds)
  where
  !(a,as) = B.break (==',') s
  !as' = B.tail as
  !(b,bs) = fromJust $ B.readInt as'
  !(d,ds) = parseDate (B.tail bs)

parseDPlaysHM :: B.ByteString -> [DPlay]
parseDPlaysHM s = if B.null r'
  then p : []
  else p : (parseDPlaysHM r')
  where
  !(p,r) = parseDPHM s
  !r' = B.tail r

parseDPHM :: B.ByteString -> (DPlay, B.ByteString)
parseDPHM s = (DPlay a b d, ds)
  where
  !(a,as) = B.break (==',') s
  !as' = B.tail as
  !(b,bs) = fromJust $ B.readInt as'
  !(d,ds) = parseDateHM (B.tail bs)

emptyPlay = DPlay "" 0 0

dplaysFromFileHM :: String -> IO [DPlay]
dplaysFromFileHM  file = do
  f <- B.readFile file
  return $ parseDPlaysHM f 

dplaysFromFile :: String -> IO [DPlay]
dplaysFromFile  file = do
  f <- B.readFile file
  return $ parseDPlays f 

dplaysFromFileC :: String -> IO [DPlay]
dplaysFromFileC file = do
  e <-runConduitRes $ (C.sourceFile file .| C.lines .| mapC parseDP .| sinkNull)
  return $ [emptyPlay]

dplaysFromFileCHM :: String -> IO [DPlay]
dplaysFromFileCHM file = do
  e <-runConduitRes $ (C.sourceFile file .| C.lines .| mapC parseDPHM .| sinkNull)
  return $ [emptyPlay]
