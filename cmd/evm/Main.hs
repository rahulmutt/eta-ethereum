{- Copyright 2019 The eta-ethereum Authors
   This file is part of eta-ethereum.
  
   eta-ethereum is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
  
   eta-ethereum is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
   GNU General Public License for more details.
  
   You should have received a copy of the GNU General Public License
   along with eta-ethereum. If not, see <http://www.gnu.org/licenses/>. -}

module Main where

import Commands
import Options.Applicative 
  (execParser, info, hsubparser, progDesc)

main :: IO ()
main = do
  res <- execParser cliInfo
  case res of
    CompileCommand {} -> error "compile: not implemented"
    RunCommand {} -> error "run: not implemented"
    StateTestCommand {} -> error "statetest: not implemented"
    DisassembleCommand {} -> error "disasm: not implemented"
  where cliInfo = 
          info (hsubparser (compileCommand 
                         <> disassembleCommand 
                         <> runCommand 
                         <> stateTestCommand))
               (progDesc "the evm command line interface")
