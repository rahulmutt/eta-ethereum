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

module Commands.StateTest (stateTestCommand) where

import Commands.Types 
  (StateTestFlags(StateTestFlags), CommandSpec, Command(StateTestCommand))
import Options.Applicative 
  (command, info, progDesc, strArgument, metavar, Parser, switch,
   long, help, option, auto)

stateTestCommand :: CommandSpec
stateTestCommand =
  command "statetest" $ info stateTestParser $ 
    progDesc "executes the given state tests"

stateTestParser :: Parser Command
stateTestParser = 
  StateTestCommand 
    <$> strArgument (metavar "<file>") 
    <*> stateTestFlagsParser

stateTestFlagsParser :: Parser StateTestFlags
stateTestFlagsParser = 
  StateTestFlags
    <$> option auto
      ( long "verbosity"
     <> help "sets the verbosity level" )
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