{

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

{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Ethereum.ASM.Lexer (lexASM) where

import Ethereum.ASM.Tokens
import Ethereum.Common.Math (mustParseBig256Dec, mustParseBig256Hex)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Text.Read (readMaybe)
import Data.Char (toUpper)

}

%wrapper "monad-bytestring"

$nl = [\n\r\f]
$space = [$nl\v\ ]
$space_no_nl = $space # \n
$alpha = [a-z A-Z]
$digit = 0-9
$hexdigit = [$digit a-f A-F]
$alpha_under = [$alpha \_]
$alpha_numeric = [$alpha $digit \_]

easm :-

0 [xX] $hexdigit+               { emit ( NumberToken 
                                       . mustParseBig256Hex 
                                       . BC.unpack ) }
$digit+                         { emit ( NumberToken 
                                       . mustParseBig256Dec 
                                       . BC.unpack ) }
\" .*  \"                       { emit (StringToken . B.init . B.drop 1) }
\@ $alpha_under+                { emit (LabelToken . B.tail) }
$alpha_under $alpha_numeric* \: { emit (LabelDefinitionToken . B.init) }
$alpha_under $alpha_numeric*    { emit lookupOpCode }
$space_no_nl+                   ;
\;\; .*                         ;
\n                              { emit' LineEndToken }

{

lexASM :: BL.ByteString -> Either String [Token]
lexASM input = runAlex input $ 
  fmap (mkToken 1 LineStartToken :) getTokens
  where getTokens = do
          t <- alexMonadScan
          case tokenType t of
             EOFToken -> return [t]
             LineEndToken 
               | let lineStart ts = t : mkToken (tokenLine t + 1) LineStartToken : ts
               -> fmap lineStart getTokens
             _ -> fmap (t :) getTokens

emit' :: TokenType -> AlexAction Token
emit' t = emit (const t)

emit :: (B.ByteString -> TokenType) -> AlexAction Token
emit t (AlexPn _ line _,_,input,_) len = return $
  mkToken line $ t (BL.toStrict (BL.take len input))

alexEOF :: Alex Token
alexEOF = return $ mkToken 1 EOFToken

lookupOpCode :: B.ByteString -> TokenType
lookupOpCode b
  | Just opcode <- readMaybe $ map toUpper $ BC.unpack b
  = OpCodeToken opcode
  | otherwise = InvalidOpCodeToken b
}