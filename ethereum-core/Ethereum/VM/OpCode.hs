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

module Ethereum.VM.OpCode where

import Prelude hiding (Ordering(..))
import Data.Word (Word8)

data OpCode =
    STOP
  | ADD
  | MUL
  | SUB
  | DIV
  | SDIV
  | MOD
  | SMOD
  | EXP
  | NOT
  | LT
  | GT
  | SLT
  | SGT
  | EQ
  | ISZERO
  | SIGNEXTEND
  | AND
  | OR
  | XOR
  | BYTE
  | SHL
  | SHR
  | SAR
  | ADDMOD
  | MULMOD
  | SHA3
  | ADDRESS
  | BALANCE
  | ORIGIN
  | CALLER
  | CALLVALUE
  | CALLDATALOAD
  | CALLDATASIZE
  | CALLDATACOPY
  | CODESIZE
  | CODECOPY
  | GASPRICE
  | EXTCODESIZE
  | EXTCODECOPY
  | RETURNDATASIZE
  | RETURNDATACOPY
  | EXTCODEHASH
  | BLOCKHASH
  | COINBASE
  | TIMESTAMP
  | NUMBER
  | DIFFICULTY
  | GASLIMIT
  | POP
  | MLOAD
  | MSTORE
  | MSTORE8
  | SLOAD
  | SSTORE
  | JUMP
  | JUMPI
  | PC
  | MSIZE
  | GAS
  | JUMPDEST
  | PUSH1
  | PUSH2
  | PUSH3
  | PUSH4
  | PUSH5
  | PUSH6
  | PUSH7
  | PUSH8
  | PUSH9
  | PUSH10
  | PUSH11
  | PUSH12
  | PUSH13
  | PUSH14
  | PUSH15
  | PUSH16
  | PUSH17
  | PUSH18
  | PUSH19
  | PUSH20
  | PUSH21
  | PUSH22
  | PUSH23
  | PUSH24
  | PUSH25
  | PUSH26
  | PUSH27
  | PUSH28
  | PUSH29
  | PUSH30
  | PUSH31
  | PUSH32
  | DUP1
  | DUP2
  | DUP3
  | DUP4
  | DUP5
  | DUP6
  | DUP7
  | DUP8
  | DUP9
  | DUP10
  | DUP11
  | DUP12
  | DUP13
  | DUP14
  | DUP15
  | DUP16
  | SWAP1
  | SWAP2
  | SWAP3
  | SWAP4
  | SWAP5
  | SWAP6
  | SWAP7
  | SWAP8
  | SWAP9
  | SWAP10
  | SWAP11
  | SWAP12
  | SWAP13
  | SWAP14
  | SWAP15
  | SWAP16
  | LOG0
  | LOG1
  | LOG2
  | LOG3
  | LOG4

  -- These are unofficial opcodes used for parsing
  | PUSH
  | DUP
  | SWAP

  | CREATE
  | CALL
  | RETURN
  | CALLCODE
  | DELEGATECALL
  | CREATE2
  | STATICCALL
  | REVERT
  | SELFDESTRUCT

  deriving (Show, Read, Ord, Enum, Eq)

isPush :: OpCode -> Bool
isPush opcode = opcode `elem`
  [ PUSH1
    , PUSH2
    , PUSH3
    , PUSH4
    , PUSH5
    , PUSH6
    , PUSH7
    , PUSH8
    , PUSH9
    , PUSH10
    , PUSH11
    , PUSH12
    , PUSH13
    , PUSH14
    , PUSH15
    , PUSH16
    , PUSH17
    , PUSH18
    , PUSH19
    , PUSH20
    , PUSH21
    , PUSH22
    , PUSH23
    , PUSH24
    , PUSH25
    , PUSH26
    , PUSH27
    , PUSH28
    , PUSH29
    , PUSH30
    , PUSH31
    , PUSH32 ]
     
isStaticJump :: OpCode -> Bool
isStaticJump JUMP = True
isStaticJump _    = False

toByte :: OpCode -> Word8
toByte =
  \case 
    STOP -> 0x0
    ADD -> 0x1
    MUL -> 0x2
    SUB -> 0x3
    DIV -> 0x4
    SDIV -> 0x5
    MOD -> 0x6
    SMOD -> 0x7
    ADDMOD -> 0x8
    MULMOD -> 0x9
    EXP -> 0xA
    SIGNEXTEND -> 0xB

    LT -> 0x10
    GT -> 0x11
    SLT -> 0x12
    SGT -> 0x13
    EQ -> 0x14
    ISZERO -> 0x15
    AND -> 0x16
    OR -> 0x17
    XOR -> 0x18
    NOT -> 0x19
    BYTE -> 0x1A
    SHL -> 0x1B
    SHR -> 0x1C
    SAR -> 0x1D

    SHA3 -> 0x20

    ADDRESS -> 0x30
    BALANCE -> 0x31 
    ORIGIN -> 0x32 
    CALLER -> 0x33 
    CALLVALUE -> 0x34 
    CALLDATALOAD -> 0x35 
    CALLDATASIZE -> 0x36 
    CALLDATACOPY -> 0x37 
    CODESIZE -> 0x38 
    CODECOPY -> 0x39 
    GASPRICE -> 0x3A
    EXTCODESIZE -> 0x3B
    EXTCODECOPY -> 0x3C
    RETURNDATASIZE -> 0x3D
    RETURNDATACOPY -> 0x3E
    EXTCODEHASH -> 0x3F

    BLOCKHASH -> 0x40
    COINBASE -> 0x41
    TIMESTAMP -> 0x42
    NUMBER -> 0x43
    DIFFICULTY -> 0x44
    GASLIMIT -> 0x45

    POP -> 0x50
    MLOAD -> 0x51
    MSTORE -> 0x52
    MSTORE8 -> 0x53
    SLOAD -> 0x54
    SSTORE -> 0x55
    JUMP -> 0x56
    JUMPI -> 0x57
    PC -> 0x58
    MSIZE -> 0x59
    GAS -> 0x5A
    JUMPDEST -> 0x5B

    PUSH1 -> 0x60
    PUSH2 -> 0x61
    PUSH3 -> 0x62
    PUSH4 -> 0x63
    PUSH5 -> 0x64
    PUSH6 -> 0x65
    PUSH7 -> 0x66
    PUSH8 -> 0x67
    PUSH9 -> 0x68
    PUSH10 -> 0x69
    PUSH11 -> 0x6A
    PUSH12 -> 0x6B
    PUSH13 -> 0x6C
    PUSH14 -> 0x6D
    PUSH15 -> 0x6E
    PUSH16 -> 0x6F
    PUSH17 -> 0x70
    PUSH18 -> 0x71
    PUSH19 -> 0x72
    PUSH20 -> 0x73
    PUSH21 -> 0x74
    PUSH22 -> 0x75
    PUSH23 -> 0x76
    PUSH24 -> 0x77
    PUSH25 -> 0x78
    PUSH26 -> 0x79
    PUSH27 -> 0x7A
    PUSH28 -> 0x7B
    PUSH29 -> 0x7C
    PUSH30 -> 0x7D
    PUSH31 -> 0x7E
    PUSH32 -> 0x7F
    DUP1 -> 0x80
    DUP2 -> 0x81
    DUP3 -> 0x82
    DUP4 -> 0x83
    DUP5 -> 0x84
    DUP6 -> 0x85
    DUP7 -> 0x86
    DUP8 -> 0x87
    DUP9 -> 0x88
    DUP10 -> 0x89
    DUP11 -> 0x8A
    DUP12 -> 0x8B
    DUP13 -> 0x8C
    DUP14 -> 0x8D
    DUP15 -> 0x8E
    DUP16 -> 0x8F
    SWAP1 -> 0x90
    SWAP2 -> 0x91
    SWAP3 -> 0x92
    SWAP4 -> 0x93
    SWAP5 -> 0x94
    SWAP6 -> 0x95
    SWAP7 -> 0x96
    SWAP8 -> 0x97
    SWAP9 -> 0x98
    SWAP10 -> 0x99
    SWAP11 -> 0x9A
    SWAP12 -> 0x9B
    SWAP13 -> 0x9C
    SWAP14 -> 0x9D
    SWAP15 -> 0x9E
    SWAP16 -> 0x9F

    LOG0 -> 0xA0
    LOG1 -> 0xA1
    LOG2 -> 0xA2
    LOG3 -> 0xA3
    LOG4 -> 0xA4

    PUSH -> 0xB0
    DUP -> 0xB1
    SWAP -> 0xB2

    CREATE -> 0xF0
    CALL -> 0xF1
    CALLCODE -> 0xF2
    RETURN -> 0xF3
    DELEGATECALL -> 0xF4
    CREATE2 -> 0xF5

    STATICCALL -> 0xFA

    REVERT -> 0xFD

    SELFDESTRUCT -> 0xFF