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


main = print "boo"
