{- Copyright 2019 The eta-ethereum Authors
   This file is part of the ethereum-common library.
  
   The ethereum-common library is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
  
   The ethereum-common library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
   GNU Lesser General Public License for more details.
  
   You should have received a copy of the GNU Lesser General Public License
   along with the ethereum-common library. If not, see <http://www.gnu.org/licenses/>. -}

{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-binds #-}
module Ethereum.Common.Math (mustParseBig256Dec, mustParseBig256Hex, big256Bytes) where

import Numeric (readHex, readDec)
import GHC.Integer.Logarithms (integerLog2#)
import GHC.Types (Int(..))
import GHC.Prim((+#))
import Data.ByteString (ByteString, pack)
import Data.Bits ((.&.), shiftR)

tt255, tt256, tt63, tt256m1, maxBig256, maxBig63 :: Integer
tt255 = 2^255
tt256 = 2^256
tt63 = 2^63
tt256m1 = tt256 - 1
maxBig256 = tt256m1
maxBig63 = tt63 - 1

parseBig256Hex, parseBig256Dec :: String -> Maybe Integer
parseBig256Hex s = parseBig256 readHex (drop 2 s)
parseBig256Dec s = parseBig256 readDec s

mustParseBig256Hex, mustParseBig256Dec :: String -> Integer
mustParseBig256Hex s = mustParseBig256 readHex (drop 2 s)
mustParseBig256Dec s = mustParseBig256 readDec s

parseBig256 :: ReadS Integer -> String -> Maybe Integer
parseBig256 readf s 
  | ((i, rest) : _) <- readf s
  , null rest 
  , bitLen i <= 256
  = Just i
  | otherwise = Nothing

mustParseBig256 :: ReadS Integer -> String -> Integer
mustParseBig256 readf s
  | Just result <- parseBig256 readf s
  = result
  | otherwise
  = error $ "Invalid 256 bit integer: " ++ s

bitLen :: Integer -> Int
bitLen i = I# ((integerLog2# i) +# 1#)

zeroBytes :: ByteString
zeroBytes = pack [0]

big256Bytes :: Integer -> ByteString
big256Bytes int
  | int == 0  = zeroBytes
  | otherwise = pack $ reverse $ go int
  where go i
          | i > 0  = (fromIntegral i .&. 0xFF) : go (i `shiftR` 8)
          | otherwise = []
