{-# LANGUAGE OverloadedStrings #-}

module Repl where

import Control.Monad
import Control.Monad.Trans (lift)
import Data.Array
import Data.Char
import qualified Data.Text as T
import Data.Void
import System.Console.Haskeline
import Text.Megaparsec
import Text.Megaparsec.Char

import Emulator (Instruction, EmulatorState (..), initEmulatorState, printRegisterBank, registers, showStatusFlags, flags, stepOneInstruction, stepMultipleInstructions, replaceLabels)
import Emulator.State (EmulatorState(EmulatorState))
import Emulator.Utils (toHex4)
import Data.Text.Internal.Builder.Int.Digits (digits)
import Numeric (readInt, readDec)
import Data.Maybe (catMaybes)

type Parser = Parsec Void T.Text

data UserCommand
    = StepOnce
    | StepMultiple Int
    | PrintRegisters
    | PrintFlags
    | PrintCurrentInstruction
    | PrintPc
    | Help
    | Quit

digitParser :: Parser Char
digitParser = do 
    oneOf ['0'..'9']

stepOnceCommandParser :: Parser UserCommand
stepOnceCommandParser = do
    string "step" <|> string "s"
    return StepOnce

stepMultipleCommandParser :: Parser UserCommand
stepMultipleCommandParser = do
    string "step" <|> string "s"
    space
    input <- some digitParser
    let steps = read input :: Int
    return $ StepMultiple steps

printRegistersCommandParser :: Parser UserCommand
printRegistersCommandParser = do
    string "registers" <|> string "r"
    return PrintRegisters

printFlagsCommandParser :: Parser UserCommand
printFlagsCommandParser = do
    string "flags" <|> string "f"
    return PrintFlags

printCurrentInstructionCommandParser :: Parser UserCommand
printCurrentInstructionCommandParser = do
    string "instruction" <|> string "i"
    return PrintCurrentInstruction

printProgramCounterCommandParser :: Parser UserCommand
printProgramCounterCommandParser = do
    string "pc" <|> string "p"
    return PrintPc

helpCommandParser :: Parser UserCommand
helpCommandParser = do
    string "help"
    return Help

quitCommandParser :: Parser UserCommand
quitCommandParser = do
    string "quit" <|> string "q"
    return Quit

commandParser :: Parser UserCommand
commandParser =
    choice [
        try stepMultipleCommandParser,
        try stepOnceCommandParser,
        try printRegistersCommandParser,
        try printFlagsCommandParser,
        try printProgramCounterCommandParser,
        try printCurrentInstructionCommandParser,
        try helpCommandParser,
        try quitCommandParser]

parseCommand :: String -> Either (ParseErrorBundle T.Text Void) UserCommand
parseCommand command = runParser commandParser "" $ T.pack command
    
dispatcher :: UserCommand -> Array Int Instruction -> EmulatorState -> InputT IO (EmulatorState)
dispatcher StepOnce programMemory state = do
    let updatedState = stepOneInstruction programMemory state
    outputStrLn $ "PC: 0x" ++ (toHex4 $ fromIntegral $ programCounter updatedState)
    return updatedState

dispatcher (StepMultiple n) programMemory state = do
    let updatedState = stepMultipleInstructions programMemory state n
    outputStrLn $ "PC: 0x" ++ (toHex4 $ fromIntegral $ programCounter updatedState)
    outputStrLn $ show n
    return updatedState

dispatcher PrintRegisters instructions state = do
    lift $ printRegisterBank $ registers state
    return state

dispatcher PrintFlags instructions state = do
    outputStrLn $ showStatusFlags $ flags state
    return state

dispatcher PrintPc instructions state = do
    outputStrLn $ "0x" ++ (toHex4 $ fromIntegral $ programCounter state)
    return state

dispatcher PrintCurrentInstruction instructions state = do
    outputStr $ "0x" ++ (toHex4 $ fromIntegral $ programCounter state) ++ ": "
    outputStrLn $ show $ instructions ! (fromIntegral $ programCounter state)
    return state

dispatcher Help _ state = do
    outputStrLn helpText
    return state

-- TODO: this currently has no effect on the REPL
-- Modify it to actually quit the REPL
dispatcher Quit instructions state = do
    outputStrLn "Exited."
    return state


replLoop :: [Instruction] -> Int -> IO (EmulatorState)
replLoop instructions memorySize = runInputT defaultSettings (loop initialState)
  where
    initialState = initEmulatorState memorySize
    resolvedInstructions = catMaybes $ replaceLabels instructions
    programMemory = listArray (0, (length resolvedInstructions) - 1) resolvedInstructions
    loop :: EmulatorState -> InputT IO (EmulatorState)
    loop state = do
      rawInput <- getInputLine "avr-emulator> "
      case rawInput of
        Just input -> case parseCommand input of
            Left error -> do
                outputStrLn "Uknown command. Type 'help' to see available commands"
                loop state
                return ()
            Right parsedCommand -> do
                newState <- dispatcher parsedCommand programMemory state
                loop newState
                return ()
        Nothing -> outputStrLn "Exited."

      return (state)

helpText :: String
helpText = unlines
  ["Available commands:"
  , "  step | s           Execute the next instruction in the program"
  , "  step <n> | s <n>   Execute the next n instructions"
  , "  registers | r      Print the current values of the register"
  , "  flags | f          Print the current values of the status flags"
  , "  pc | p             Print the ccurent value of the program counter"
  , "  instruction | i    Print the next instruction to be executed"
  , "  help               Print this message"
  , "  Control-D          Exit the REPL"
  ]