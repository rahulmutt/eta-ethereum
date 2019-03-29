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

module Commands.Disassemble (disassembleCommand) where

import Commands.Types 
  (CommandSpec, Command(DisassembleCommand))

import Options.Applicative 
  (command, info, progDesc, strArgument, metavar, Parser)

disassembleCommand :: CommandSpec
disassembleCommand =
  command "disasm" $ info disassembleParser $ 
    progDesc "disassembles evm binary"

disassembleParser :: Parser Command
disassembleParser = 
  DisassembleCommand <$> strArgument (metavar "<file>") 