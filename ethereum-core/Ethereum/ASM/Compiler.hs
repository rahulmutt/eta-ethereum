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

module Ethereum.ASM.Compiler (compile, CompileError(..)) where

import Ethereum.ASM.Tokens
import Ethereum.VM.OpCode
import Ethereum.Common.Safe (expectJust)
import Ethereum.Common.Math (big256Bytes)

import qualified Data.Map as M
import Data.Word (Word32)
import Control.Monad.State (State, modify, execState, gets, mfix)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder

compile :: [Token] -> Either [CompileError] BL.ByteString
compile tokens
  | null compilerStateErrors =
    Right $ toLazyByteString compilerStateResult
  | otherwise = 
    Left compilerStateErrors
  where CompilerState { compilerStateResult
                      , compilerStateErrors } = 
          execState action initialCompilerState
        action = mfix $ \labels -> do
           compileLines labels tokens
           gets compilerStateLabels

type Label = B.ByteString
type Labels = M.Map Label Word32

data CompileError = 
  CompileError { compileErrorWant :: String
               , compileErrorGot  :: String
               , compileErrorLine :: Int }
  deriving Show

data CompilerState =
  CompilerState { compilerStateErrors :: [CompileError]
                , compilerStateResult :: Builder 
                , compilerStateLabels :: Labels
                , compilerStateBytes  :: Word32 }

initialCompilerState :: CompilerState
initialCompilerState = CompilerState [] mempty mempty 0

type Compiler a = State CompilerState a

addError :: String -> String -> Int -> Compiler ()
addError want got line =
  modify (\s@CompilerState{ compilerStateErrors } ->
           s { compilerStateErrors = err : compilerStateErrors })
  where err = CompileError want got line

addLabel :: B.ByteString -> Compiler ()
addLabel label = do
  modify (\s@CompilerState{ compilerStateLabels,
                            compilerStateBytes } ->
           s { compilerStateLabels = 
                 M.insert label compilerStateBytes compilerStateLabels })

addResult :: Word32 -> Builder -> Compiler ()
addResult newBytes newResult =
  modify (\s@CompilerState{ compilerStateResult, compilerStateBytes } ->
           s { compilerStateResult = compilerStateResult <> newResult 
             , compilerStateBytes  = compilerStateBytes  +  newBytes })

compileLines :: Labels -> [Token] -> Compiler ()
compileLines labels ((Token _ LineStartToken) : tokens) = do
    tokens' <- compileLine labels tokens
    compileLines labels tokens'
compileLines labels ((Token line tt) : tokens) = do
    addError (show LineStartToken) (show tt) line
    compileLines labels tokens
compileLines _ [] = return ()

compileLine :: Labels -> [Token] -> Compiler [Token]
compileLine labels ((Token line lvalue) : tokens) =
  case lvalue of
    EOFToken -> return tokens
    LineEndToken -> return tokens
    OpCodeToken opcode -> do
      (shouldScan, tokens') <- compileElement labels opcode tokens
      if shouldScan
      then checkLineEnd tokens'
      else return tokens'
    LabelDefinitionToken label -> do
      compileLabelDef label 
      checkLineEnd tokens
    _ -> do
      addError "label definition or element" (show lvalue) line
      return tokens
  where checkLineEnd (Token l typ : ts) 
          | typ == LineEndToken = return ts
          | otherwise = do
            addError (show LineEndToken) (show typ) l
            return ts
        checkLineEnd [] = unexpectedEndOfTokenStream

compileLine _ [] = unexpectedEndOfTokenStream

compileElement :: Labels -> OpCode -> [Token] -> Compiler (Bool, [Token])
compileElement labels opcode tokens =
  case opcode of
    JUMP  -> doJump JUMP  tokens
    JUMPI -> doJump JUMPI tokens
    PUSH -> doPush tokens
    _ -> do
      addResult 1 (opCodeResult opcode)
      return (True, tokens)

  where doJump op ((Token line rvalue) : ts) = fmap (, ts) $
          withRValue True rvalue line $ \len result -> do
            addResult (len + 1) (result <> opCodeResult op)
            return True
        doJump _ [] = unexpectedEndOfTokenStream

        doPush ((Token line rvalue) : ts) = fmap (, ts) $
          withRValue False rvalue line $ \len result -> do
            let op = toEnum $ fromEnum PUSH1 - 1 + fromIntegral len
            if len > 32
            then do addError "must be <= 32 bytes" (show rvalue) line
                    return False
            else do addResult (1 + len) (opCodeResult op <> result)
                    return True
        doPush [] = unexpectedEndOfTokenStream
        
        withRValue jump rvalue line f = do
          case getRValue jump rvalue of
            Nothing -> do
              addError "number, string, or label" (show rvalue) line
              return False
            Just (len, result) -> f len result

        getRValue jump rvalue = 
          case rvalue of
            NumberToken i 
              | let s = big256Bytes i -> 
              Just ( fromIntegral (B.length s)
                   , byteStringHex s )
            StringToken s ->
              Just ( fromIntegral (B.length s)
                   , byteStringHex s )
            LabelToken l
              | let offset =
                      expectJust ("Label: " <> show l <> " is missing") 
                                 (M.lookup l labels)
                    offsetResult = word32HexFixed offset
                    result
                      | jump      = (5, opCodeResult PUSH4 <> offsetResult)
                      | otherwise = (4, offsetResult)
              -> Just result 
            _ -> Nothing

compileLabelDef :: Label -> Compiler ()
compileLabelDef label = do
  addLabel label
  addResult 1 (opCodeResult JUMPDEST)

opCodeResult :: OpCode -> Builder
opCodeResult = word8HexFixed . toByte

unexpectedEndOfTokenStream :: a
unexpectedEndOfTokenStream =
  error "Unexpected end of token stream - didn't find EOF token"