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

module Commands.Compile (compileCommand) where

import Commands.Types (Command(CompileCommand), CommandSpec)
import Options.Applicative 
  (command, info, progDesc, strArgument, metavar, Parser, switch,
   long, help)

compileCommand :: CommandSpec
compileCommand =
  command "compile" $ info compileParser $ 
    progDesc "compiles easm source to evm binary"

compileParser :: Parser Command
compileParser = 
  CompileCommand 
    <$> strArgument (metavar "<file>") 
    <*> switch
      ( long "debug"
     <> help "output full trace logs" )