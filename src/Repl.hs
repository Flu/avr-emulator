{-# LANGUAGE OverloadedStrings #-}

module Repl where

import Control.Monad.Trans (lift)
import qualified Data.Text as T
import Data.Vector ((!), Vector)
import qualified Data.Vector as V
import Data.Void
import System.Console.Haskeline
import Text.Megaparsec
import Text.Megaparsec.Char

import Emulator (Instruction, EmulatorState (..), initEmulatorState, printRegisterBank, registers, showStatusFlags, flags, stepOneInstruction, stepMultipleInstructions, replaceLabels, runUntilProgramEnd, runUntilFunctionEnd, printInstructionsAroundAddress, prettyPrintMemory)
import Emulator.Utils (prettyPrintMemoryFromStart, prettyPrintMemoryFromStartToEnd, toHex4WithPrefix)
import Data.Maybe (catMaybes)
import Data.List (isPrefixOf)
import System.Console.ANSI (setSGR, ConsoleLayer (Foreground), SGR (SetColor, Reset), Color (Green), ColorIntensity (Vivid))

type Parser = Parsec Void T.Text

-- | Data type for user commands that the REPL can receive
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
    | PrintWholeMemory
    | PrintMemoryFromStart Int
    | PrintMemory Int Int
    | Help
    | Quit
    | MissingCommand

-- | Parser for single digit numbers
digitParser :: Parser Char
digitParser = do 
    oneOf ['0'..'9']

-- | Parser for the step command 
stepOnceCommandParser :: Parser UserCommand
stepOnceCommandParser = do
    string "step" <|> string "s"
    lookAhead (eof <|> (space >> eof))
    return StepOnce

-- | Parser for the step command with the optional number of steps parameter
stepMultipleCommandParser :: Parser UserCommand
stepMultipleCommandParser = do
    string "step" <|> string "s"
    space
    input <- some digitParser
    let steps = read input :: Int
    return $ StepMultiple steps

-- | Parser for the execute until end of program command
executeUntilProgramEndCommandParser :: Parser UserCommand
executeUntilProgramEndCommandParser = do
    string "e"
    lookAhead (eof <|> (space >> eof))
    return ExecuteUntilProgramEnd

-- | Parser for the execute until current function end command
executeUntilFunctionEndCommandParser :: Parser UserCommand
executeUntilFunctionEndCommandParser = do
    string "f"
    lookAhead (eof <|> (space >> eof))
    return ExecuteUntilFunctionEnd

-- | Parser for the restart command
restartCommandParser :: Parser UserCommand
restartCommandParser = do
    string "restart" <|> string "re"
    lookAhead (eof <|> (space >> eof))
    return Restart

-- | Parser for printing the registers command
printRegistersCommandParser :: Parser UserCommand
printRegistersCommandParser = do
    string "registers" <|> string "r"
    lookAhead (eof <|> (space >> eof))
    return PrintRegisters

-- | Parser for printing the flags command
printFlagsCommandParser :: Parser UserCommand
printFlagsCommandParser = do
    string "flags"
    lookAhead (eof <|> (space >> eof))
    return PrintFlags

-- | Parser for printing the current instructions command
printCurrentInstructionCommandParser :: Parser UserCommand
printCurrentInstructionCommandParser = do
    string "instruction" <|> string "i"
    lookAhead (eof <|> (space >> eof))
    return PrintCurrentInstruction

-- | Parser for printing the program counter command
printProgramCounterCommandParser :: Parser UserCommand
printProgramCounterCommandParser = do
    string "pc" <|> string "p"
    lookAhead (eof <|> (space >> eof))
    return PrintPc

-- | Parser for the print memory command with start address
printWholeMemoryCommandParser :: Parser UserCommand
printWholeMemoryCommandParser = do
    string "memory" <|> string "m"
    lookAhead (eof <|> (space >> eof))
    return $ PrintWholeMemory

-- | Parser for the print memory command with start address
printMemoryFromStartCommandParser :: Parser UserCommand
printMemoryFromStartCommandParser = do
    string "memory" <|> string "m"
    space
    input <- some digitParser
    lookAhead (eof <|> (space >> eof))
    let start = read input :: Int
    return $ PrintMemoryFromStart start

-- | Parser for the print memory command with start address
printMemoryCommandParser :: Parser UserCommand
printMemoryCommandParser = do
    string "memory" <|> string "m"
    space
    startString <- some digitParser
    space
    endString <- some digitParser
    lookAhead (eof <|> (space >> eof))
    return $ PrintMemory (read startString :: Int) (read endString :: Int)

-- | Parser for the help command
helpCommandParser :: Parser UserCommand
helpCommandParser = do
    string "help"
    lookAhead (eof <|> (space >> eof))
    return Help

-- | Parser for the quit command
quitCommandParser :: Parser UserCommand
quitCommandParser = do
    (string "quit" <|> string "q")
    lookAhead (eof <|> (space >> eof))
    return Quit

-- | Parser that matches a lack of command (line of whitespace)
missingCommand :: Parser UserCommand
missingCommand = do
    space >> eof
    return MissingCommand

-- | Combinator parser
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
        try printMemoryCommandParser,
        try printMemoryFromStartCommandParser,
        try printWholeMemoryCommandParser,
        try helpCommandParser,
        try quitCommandParser,
        try missingCommand]

-- | Command parser that tries any parser and returns an error if it fails
parseCommand :: String -> Either (ParseErrorBundle T.Text Void) UserCommand
parseCommand command = runParser commandParser "" $ T.pack command

{-- | Dispatcher function that calls the emulator based on the command given and returns an
    IO computation with what should be displayed in the REPL
--}
dispatcher :: UserCommand -> Vector Instruction -> EmulatorState -> InputT IO (EmulatorState)
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
    printMessageIfProgramIsDone isProgramDone
    return updatedState

dispatcher (ExecuteUntilFunctionEnd) programMemory state = do
    let (isProgramDone, updatedState) = runUntilFunctionEnd programMemory state
    printPcAndInstruction programMemory updatedState
    printMessageIfProgramIsDone isProgramDone
    return updatedState

dispatcher (Restart) programMemory state = do
    let restartState = initEmulatorState (V.length (memory state))
    outputStrLn "The emulator has been restarted."
    return (restartState)

dispatcher PrintRegisters instructions state = do
    lift $ printRegisterBank $ registers state
    return state

dispatcher PrintFlags instructions state = do
    outputStrLn $ showStatusFlags $ flags state
    return state

dispatcher PrintPc instructions state = do
    printProgramCounter $ fromIntegral (programCounter state)
    return state

dispatcher (PrintMemory start end) instructions state = do
    if start >= end || start >= V.length (memory state) then
        printMessageIfAddressIsInvalid (V.length (memory state))
    else
        lift $ prettyPrintMemoryFromStartToEnd (memory state) start end
    return state

dispatcher (PrintMemoryFromStart start) instructions state = do
    if start >= V.length (memory state) then
        printMessageIfAddressIsInvalid (V.length (memory state))
    else
        lift $ prettyPrintMemoryFromStart (memory state) start
    return state

dispatcher PrintWholeMemory instructions state = do
    lift $ prettyPrintMemory (memory state)
    return state

dispatcher PrintCurrentInstruction instructions state = do
    lift $ printInstructionsAroundAddress instructions (fromIntegral $ programCounter state) 3
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

{-- | Main REPL loop
    It waits for input from the user, parses the command and calls the dispatcher to deal with the command. Depending
    on the dispatcher's state it either calls itself recursively or it terminates the loop.
--}
replLoop :: [Instruction] -> Int -> IO (EmulatorState)
replLoop instructions memorySize = runInputT defaultReplSettings (loop initialState)
    where
    initialState = initEmulatorState memorySize
    resolvedInstructions = catMaybes $ replaceLabels instructions
    programMemory = V.fromList resolvedInstructions
    loop :: EmulatorState -> InputT IO (EmulatorState)
    loop state = do
        lift printPrompt
        rawInput <- getInputLine ""
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

printPrompt :: IO ()
printPrompt = do
    setSGR [SetColor Foreground Vivid Green]
    putStr "avr-emulator> "
    setSGR [Reset]

printMessageIfProgramIsDone :: Bool -> InputT IO ()
printMessageIfProgramIsDone done
    | done = outputStrLn "The program finished execution. You can inspect variables, restart or close this REPL."
    | otherwise = return ()

printMessageIfAddressIsInvalid :: Int -> InputT IO ()
printMessageIfAddressIsInvalid memoryLength = do
    outputStrLn $ "Invalid addresses"
    outputStrLn $ "You can't print memory greater than " ++ toHex4WithPrefix memoryLength ++ " or have the start address bigger than the end address."

printPcAndInstruction :: Vector Instruction -> EmulatorState -> InputT IO ()
printPcAndInstruction programMemory state = do
    outputStr $ (toHex4WithPrefix (fromIntegral $ programCounter state)) ++ "     "
    outputStrLn $ show $ programMemory ! (fromIntegral $ programCounter state)

printProgramCounter :: Int -> InputT IO ()
printProgramCounter pc = do
    outputStrLn $ toHex4WithPrefix pc

helpText :: String
helpText = unlines
  ["Available commands:"
  , "  step | s                    Execute the next instruction in the program"
  , "  step <n> | s <n>            Execute the next n instructions"
  , "  e                           Run until the end of the program"
  , "  f                           Run until the next RET or RETI instruction"
  , "  restart | re                Restart the emulator with the same parameters"
  , "  registers | r               Print the current values of the registers"
  , "  flags                       Print the current values of the status flags"
  , "  pc | p                      Print the curent value of the program counter"
  , "  m [[<start>] <end>]         Print the memory contents between two addresses"
  , "  instruction | i             Print the next instruction to be executed"
  , "  help                        Print this message"
  , "  Control-D                   Exit the REPL"
  ]

defaultReplSettings :: Settings IO
defaultReplSettings = Settings {
    historyFile = Nothing
    , complete = completeWord Nothing " \t" $ return . searchFunctionForAutocomplete
    , autoAddHistory = True
}

searchFunctionForAutocomplete :: String -> [Completion]
searchFunctionForAutocomplete query = map simpleCompletion $ filter (query `isPrefixOf`) fullWordCommandList

fullWordCommandList :: [String]
fullWordCommandList = ["step", "restart", "registers", "instruction", "pc", "memory", "help", "quit"]