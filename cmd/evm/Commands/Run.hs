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

module Commands.Run (runCommand) where

import Commands.Types (Command(RunCommand), RunFlags(RunFlags), CommandSpec)
import Options.Applicative 
  (command, info, progDesc, strArgument, metavar, Parser, switch,
   long, help, strOption, option, auto, value)
import Control.Applicative (optional)

runCommand :: CommandSpec
runCommand =
  command "run" $ info runParser $ 
    progDesc "run arbitrary evm binary"

runParser :: Parser Command
runParser = 
  RunCommand 
    <$> optional (strArgument (metavar "<code>"))
    <*> runFlagsParser

runFlagsParser :: Parser RunFlags
runFlagsParser = 
  RunFlags
    <$> option auto
      ( long "verbosity"
     <> help "sets the verbosity level" 
     <> value 0 )
    <*> switch
      ( long "nomemory"
     <> help "disable memory output" )
    <*> switch
      ( long "nostack"
     <> help "disable stack output" )
    <*> switch
      ( long "json"
     <> help "output trace logs in machine readable format (json)" )
    <*> switch
      ( long "debug"
     <> help "output full trace logs" )
    <*> switch
      ( long "dump"
     <> help "dump the state after the run" )
    <*> optStrOption
      ( long "prestate"
     <> help "JSON file with prestate (genesis) config" )
    <*> optStrOption
      ( long "sender"
     <> help "The transaction origin" )
    <*> optStrOption
      ( long "reciever"
     <> help "The transaction receiver (execution context)" )
    <*> optStrOption
      ( long "code"
     <> help "EVM code" )
    <*> optStrOption
      ( long "codefile"
     <> help "File containing EVM code. If '-' is specified, code is read from stdin " )
    <*> option auto
      ( long "gas"
     <> help "gas limit for the evm" 
     <> value 10000000000 )
    <*> option auto
      ( long "price"
     <> help "price set for the evm" 
     <> value 0 )
    <*> option auto
      ( long "value"
     <> help "value set for the evm" 
     <> value 0 )
    <*> optStrOption
      ( long "vm.evm"
     <> help "External EVM configuration (default = built-in interpreter)" )
    <*> optStrOption
      ( long "cpuprofile"
     <> help "creates a CPU profile at the given path" )
    <*> switch
      ( long "create"
     <> help "indicates the action should be create rather than call" )
    <*> optStrOption
      ( long "input"
     <> help "input for the EVM" )
    <*> optStrOption
      ( long "memprofile"
     <> help "creates a memory profile at the given path" )
    <*> optStrOption
      ( long "statdump"
     <> help "displays stack and heap memory information" )
  where optStrOption = optional . strOption