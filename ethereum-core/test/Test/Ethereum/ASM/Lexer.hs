{- Copyright 2019 The eta-ethereum Authors
   This file is part of the ethereum-core library.
  
   The ethereum-core library is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
  
   The ethereum-core library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
   GNU Lesser General Public License for more details.
  
   You should have received a copy of the GNU Lesser General Public License
   along with the ethereum-core library. If not, see <http://www.gnu.org/licenses/>. -}

module Test.Ethereum.ASM.Lexer (lexerTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Numeric (readHex)

import Ethereum.ASM.Tokens
import Ethereum.ASM.Lexer

tokens :: [[TokenType]] -> Either String [Token]
tokens tts = Right $ concatMap mapTokens $ zip [1..] tts
  where numLines = length tts
        mapTokens (i, ts) 
          | i == numLines = mapped ++ [mkToken 1 EOFToken]
          | otherwise = mapped
          where ts' | i == 1 = LineStartToken : ts
                    | otherwise = ts
                mapped = map (mkToken i) ts'

fromHex :: String -> Integer
fromHex s 
  | ((i,"") : _) <- readHex (drop 2 s)
  = i
  | otherwise = error $ "bad hex: " <> s

lexerTests :: TestTree
lexerTests = testGroup "ASM Lexer Tests" 
  [ testCase "comment" $
      lexASM ";; this is a comment" @?= tokens [[]]

  , testCase "hexadecimal number" $
      lexASM "0x12345678" @?= tokens [[NumberToken (fromHex "0x12345678")]]

  , testCase "hexadecimal number and element" $
      lexASM "0x123ooo" @?=
        tokens [[NumberToken (fromHex "0x123"), InvalidOpCodeToken "ooo"]]

  , testCase "decimal number" $
      lexASM "12345678" @?=
        tokens [[NumberToken 12345678]]

  , testCase "decimal number and element 1" $
      lexASM "123abc" @?=
        tokens [[NumberToken 123, InvalidOpCodeToken "abc"]]

  , testCase "decimal number and element 2" $
      lexASM "0123abc" @?=
        tokens [[NumberToken 123, InvalidOpCodeToken "abc"]]
  ]