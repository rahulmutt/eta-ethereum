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

import Test.Ethereum.ASM.Lexer (lexerTests)

import Test.Tasty

main :: IO ()
main = defaultMain $ 
  testGroup "ethereum-core Tests" [
      lexerTests
  ]