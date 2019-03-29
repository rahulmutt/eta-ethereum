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

module Commands.Types
  ( Command(..)
  , StateTestFlags(..)
  , RunFlags(..)
  , CommandSpec )
where

import Data.Word (Word64)
import Data.Text (Text)
import Options.Applicative (Mod, CommandFields)

data Command =
    CompileCommand { compileFile :: Text 
                   , compileDebug :: Bool }
  | RunCommand { runFile :: Maybe Text
               , runFlags :: RunFlags }
  | DisassembleCommand { disassembleFile :: Text}
  | StateTestCommand { stateTestFile :: Text
                     , stateTestFlags :: StateTestFlags }

data StateTestFlags = 
  StateTestFlags { stateTestFlagVerbosity :: Int
                 , stateTestFlagDisableMemory :: Bool
                 , stateTestFlagDisableStack :: Bool
                 , stateTestFlagMachine :: Bool
                 , stateTestFlagDebug :: Bool
                 , stateTestFlagDump :: Bool }

data RunFlags =
  RunFlags { runFlagVerbosity :: Int
           , runFlagDisableMemory :: Bool
           , runFlagDisableStack :: Bool
           , runFlagMachine :: Bool
           , runFlagDebug :: Bool
           , runFlagDump :: Bool 
           , runFlagGenesis :: Maybe Text
           , runFlagSender :: Maybe Text
           , runFlagReceiver :: Maybe Text
           , runFlagCode :: Maybe Text
           , runFlagCodeFile :: Maybe Text
           , runFlagGas :: Word64
           , runFlagPrice :: Integer
           , runFlagValue :: Integer
           , runFlagEVMInterpreter :: Maybe Text
           , runFlagCPUProfile :: Maybe Text
           , runFlagCreate :: Bool
           , runFlagInput :: Maybe Text
           , runFlagMemProfile :: Maybe Text
           , runFlagStatDump :: Maybe Text }

type CommandSpec = Mod CommandFields Command