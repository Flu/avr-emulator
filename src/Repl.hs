{-# LANGUAGE OverloadedStrings #-}

module Repl where

import Control.Monad.Trans (lift)
import Data.Array
import qualified Data.Text as T
import Data.Void
import System.Console.Haskeline
import Text.Megaparsec
import Text.Megaparsec.Char

import Emulator (Instruction, EmulatorState (..), initEmulatorState, printRegisterBank, registers, showStatusFlags, flags, stepOneInstruction)
import Emulator.State (EmulatorState(EmulatorState))
import Emulator.Utils (toHex4)

type Parser = Parsec Void T.Text

data UserCommand =
      Step
    | PrintRegisters
    | PrintFlags
    | PrintPc
    | Quit

commandParser :: Parser UserCommand
commandParser =
    choice [
        try (Step <$ string "step"),
        try (PrintRegisters <$ string "registers"),
        try (PrintFlags <$ string "flags"),
        try (PrintPc <$ string "pc"),
        try (Quit <$ string "quit")]

parseCommand :: String -> Either (ParseErrorBundle T.Text Void) UserCommand
parseCommand command = runParser commandParser "" $ T.pack command
    
dispatcher :: UserCommand -> Array Int Instruction -> EmulatorState -> InputT IO (EmulatorState)
dispatcher Step programMemory state = do
    let updatedState = stepOneInstruction programMemory state
    outputStrLn $ "Stepped to " ++ (toHex4 $ fromIntegral $ programCounter updatedState)
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

dispatcher Quit instructions state = do
    outputStrLn "Exited."
    return state


replLoop :: [Instruction] -> Int -> IO (EmulatorState)
replLoop instructions memorySize = runInputT defaultSettings (loop initialState)
  where
    initialState = initEmulatorState memorySize
    programMemory = listArray (0, (length instructions) - 1) instructions
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
