{-# LANGUAGE OverloadedStrings #-}

module Repl where

import Control.Monad
import Control.Monad.Trans (lift)
import Data.Array
import Data.Char
import Text.Printf (printf)
import qualified Data.Text as T
import Data.Void
import System.Console.Haskeline
import Text.Megaparsec
import Text.Megaparsec.Char

import Emulator (Instruction, EmulatorState (..), initEmulatorState, printRegisterBank, registers, showStatusFlags, flags, stepOneInstruction, stepMultipleInstructions, replaceLabels, runUntilProgramEnd, runUntilFunctionEnd)
import Emulator.State (EmulatorState(EmulatorState))
import Emulator.Utils (toHex4)
import Data.Text.Internal.Builder.Int.Digits (digits)
import Numeric (readInt, readDec)
import Data.Maybe (catMaybes)

type Parser = Parsec Void T.Text

data UserCommand
    = StepOnce
    | StepMultiple Int
    | ExecuteUntilProgramEnd
    | ExecuteUntilFunctionEnd
    | Restart
    | PrintRegisters
    | PrintFlags
    | PrintCurrentInstruction
    | PrintPc
    | Help
    | Quit
    | MissingCommand

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

executeUntilProgramEndCommandParser :: Parser UserCommand
executeUntilProgramEndCommandParser = do
    string "e"
    lookAhead eof
    return ExecuteUntilProgramEnd

executeUntilFunctionEndCommandParser :: Parser UserCommand
executeUntilFunctionEndCommandParser = do
    string "f"
    lookAhead eof
    return ExecuteUntilFunctionEnd

restartCommandParser :: Parser UserCommand
restartCommandParser = do
    string "restart" <|> string "re"
    lookAhead eof
    return Restart

printRegistersCommandParser :: Parser UserCommand
printRegistersCommandParser = do
    string "registers" <|> string "r"
    lookAhead eof
    return PrintRegisters

printFlagsCommandParser :: Parser UserCommand
printFlagsCommandParser = do
    string "flags"
    lookAhead eof
    return PrintFlags

printCurrentInstructionCommandParser :: Parser UserCommand
printCurrentInstructionCommandParser = do
    string "instruction" <|> string "i"
    lookAhead eof
    return PrintCurrentInstruction

printProgramCounterCommandParser :: Parser UserCommand
printProgramCounterCommandParser = do
    string "pc" <|> string "p"
    lookAhead eof
    return PrintPc

helpCommandParser :: Parser UserCommand
helpCommandParser = do
    string "help"
    lookAhead eof
    return Help

quitCommandParser :: Parser UserCommand
quitCommandParser = do
    (string "quit" <|> string "q")
    lookAhead eof
    return Quit

missingCommand :: Parser UserCommand
missingCommand = do
    space >> eof
    return MissingCommand

commandParser :: Parser UserCommand
commandParser =
    choice [
        try stepMultipleCommandParser,
        try stepOnceCommandParser,
        try executeUntilProgramEndCommandParser,
        try executeUntilFunctionEndCommandParser,
        try restartCommandParser,
        try printRegistersCommandParser,
        try printFlagsCommandParser,
        try printProgramCounterCommandParser,
        try printCurrentInstructionCommandParser,
        try helpCommandParser,
        try quitCommandParser,
        try missingCommand]

parseCommand :: String -> Either (ParseErrorBundle T.Text Void) UserCommand
parseCommand command = runParser commandParser "" $ T.pack command

-- TODO: Add support for no instruction, in which case do nothing and restart the loop
-- TODO: Move printing of PC, instructions, etc. to their own functions
-- FIX: There is a bug somewhere in this fun that crashes the program if stepped on the last instr 
dispatcher :: UserCommand -> Array Int Instruction -> EmulatorState -> InputT IO (EmulatorState)
dispatcher StepOnce programMemory state = do
    let (isProgramDone, updatedState) = stepOneInstruction programMemory state
    if isProgramDone then
        printMessageIfProgramIsDone isProgramDone
    else
        printPcAndInstruction programMemory updatedState
    return updatedState

dispatcher (StepMultiple n) programMemory state = do
    let (isProgramDone, updatedState) = stepMultipleInstructions programMemory state n
    if isProgramDone then
        printMessageIfProgramIsDone isProgramDone
    else
        printPcAndInstruction programMemory updatedState
    return updatedState

dispatcher (ExecuteUntilProgramEnd) programMemory state = do
    let (isProgramDone, updatedState) = runUntilProgramEnd programMemory state
    outputStrLn $ "PC: 0x" ++ (toHex4 $ fromIntegral $ programCounter updatedState)
    printMessageIfProgramIsDone isProgramDone
    return updatedState

-- TODO: Only stop at the RET of the calling function, e.g. ignore other functions that the current function is calling
dispatcher (ExecuteUntilFunctionEnd) programMemory state = do
    let (isProgramDone, updatedState) = runUntilFunctionEnd programMemory state
    outputStrLn $ "PC: 0x" ++ (toHex4 $ fromIntegral $ programCounter updatedState)
    printMessageIfProgramIsDone isProgramDone
    return updatedState

dispatcher (Restart) programMemory state = do
    let restartState = initEmulatorState (length (memory state))
    outputStrLn "The emulator has been restarted"
    return (restartState)

dispatcher PrintRegisters instructions state = do
    lift $ printRegisterBank $ registers state
    return state

dispatcher PrintFlags instructions state = do
    outputStrLn $ showStatusFlags $ flags state
    return state

-- TODO: Instead of showing a single instruction, show the last 3-4 and the next 3-4 instructions and make it an argument
dispatcher PrintPc instructions state = do
    outputStrLn $ "0x" ++ (toHex4 $ fromIntegral $ programCounter state)
    return state

dispatcher PrintCurrentInstruction instructions state = do
    printPcAndInstructions instructions state 3
    return state

dispatcher Help _ state = do
    outputStrLn helpText
    return state

-- TODO: this currently has no effect on the REPL. Modify it to actually quit the REPL.
dispatcher Quit instructions state = do
    outputStrLn "Exited."
    return state

dispatcher MissingCommand _ state = do
    return state

printMessageIfProgramIsDone :: Bool -> InputT IO ()
printMessageIfProgramIsDone done
    | done = outputStrLn "The program finished execution. You can restart or close this REPL"
    | otherwise = return ()

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
                outputStrLn "Unknown command. Type 'help' to see available commands"
                loop state
                return ()
            Right parsedCommand -> do
                newState <- dispatcher parsedCommand programMemory state
                loop newState
                return ()
        Nothing -> outputStrLn "Exited."

      return (state)

printPcAndInstruction :: Array Int Instruction -> EmulatorState -> InputT IO ()
printPcAndInstruction programMemory state = do
    outputStr $ "0x" ++ (toHex4 $ fromIntegral $ programCounter state) ++ ": "
    outputStrLn $ show $ programMemory ! (fromIntegral $ programCounter state)

printPcAndInstructions :: Array Int Instruction -> EmulatorState -> Int -> InputT IO ()
printPcAndInstructions programMemory state n = do
    outputStrLn $ unlines [printf "0x%04X     %s" i (show (programMemory ! i)) | i <- [startIndex..endIndex]]
    where
        pc = fromIntegral $ programCounter state
        startIndex = max start (pc - n)
            where (start, _) = bounds programMemory
        endIndex = min end (pc + n)
            where (_, end) = bounds programMemory

helpText :: String
helpText = unlines
  ["Available commands:"
  , "  step | s           Execute the next instruction in the program"
  , "  step <n> | s <n>   Execute the next n instructions"
  , "  e                  Run until the end of the program"
  , "  f                  Run until the next RET or RETI instruction"
  , "  restart | re       Restart the emulator with the same parameters"
  , "  registers | r      Print the current values of the register"
  , "  flags | f          Print the current values of the status flags"
  , "  pc | p             Print the ccurent value of the program counter"
  , "  instruction | i    Print the next instruction to be executed"
  , "  help               Print this message"
  , "  Control-D          Exit the REPL"
  ]