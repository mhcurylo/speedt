{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Play where

import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit.Binary as C
import qualified Data.Time.Calendar as T
import qualified Data.HashMap.Strict as HM
import GHC.Generics (Generic, Generic1)
import Control.DeepSeq
import Data.Maybe (fromJust)
import Data.List (maximum, zip3)
import Conduit

data Play = Play {
    file   :: !Int
  , date   :: !Int
  , player :: !B.ByteString
  , score  :: !Int
} deriving (Show, Eq, Ord, Generic, NFData)

-- FUNCTIONS FOR CALCULATING DATE AS DAYS FROM

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
  !dm = HM.fromList $ map toKV $ take 30000 allDays

dayHM d = case HM.lookup d dayHMap of
  (Just r) -> r
  (Nothing) -> undefined 

-- END OF DATE CALCULATION

parseDate :: B.ByteString -> Int
parseDate bs = i
  where
  (v,vs) = B.break (=='\n') bs 
  !i = dayHM v

parsePlay :: Int -> B.ByteString -> Play
parsePlay f s = Play f d p sc
  where
  !(p,as) = B.break (==',') s
  !as' = B.tail as
  !(sc,bs) = fromJust $ B.readInt as'
  !d = parseDate (B.tail bs)

emptyPlay = Play 0 0 "" 0
