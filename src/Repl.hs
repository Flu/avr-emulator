{-# LANGUAGE OverloadedStrings #-}

module Repl where

import System.Console.Haskeline

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import Data.Void
import Emulator (Instruction, EmulatorState, initEmulatorState)
import Emulator.State (EmulatorState(EmulatorState))

type Parser = Parsec Void T.Text

data UserCommand =
      Step
    | PrintRegisters
    | PrintFlags
    | Quit

commandParser :: Parser UserCommand
commandParser =
    choice [
        try (Step <$ string "step"),
        try (PrintRegisters <$ string "registers"),
        try (PrintFlags <$ string "flags"),
        try (Quit <$ string "quit")]

parseCommand :: String -> Either (ParseErrorBundle T.Text Void) UserCommand
parseCommand command = runParser commandParser "" $ T.pack command
    
dispatcher :: UserCommand -> [Instruction] -> EmulatorState -> InputT IO (EmulatorState)
dispatcher Step instructions state = do
    outputStrLn "You stepped one instruction"
    return state

dispatcher PrintRegisters instructions state = do
    outputStrLn "You just printed the registers"
    return state

dispatcher PrintFlags instructions state = do
    outputStrLn "You just printed the flags"
    return state

dispatcher Quit instructions state = do
    outputStrLn "You just quit"
    return state


replLoop :: [Instruction] -> Int -> IO (EmulatorState)
replLoop instructions memorySize = runInputT defaultSettings (loop initialState)
  where
    initialState = initEmulatorState memorySize
    loop :: EmulatorState -> InputT IO (EmulatorState)
    loop state = do
      rawInput <- getInputLine "avr-emulator> "
      case rawInput of
        Just input -> case parseCommand input of
            Left error -> do
                outputStr "Uknown command. Type 'help' to see available commands"
                loop state
                return ()
            Right parsedCommand -> do
                newState <- dispatcher parsedCommand instructions state
                loop newState
                return ()
        Nothing -> outputStrLn "Unknown command. Type 'help' to see available commands"

      return (state)
