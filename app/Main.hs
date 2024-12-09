module Main where

import System.Environment
import Options.Applicative
import Emulator
import Parser
import Options

main :: IO ()
main = entryFunction =<< execParser opts
    where
        opts = info (options <**> helper)
            (fullDesc
            <> progDesc "Compile an AVR assembly file and execute it"
            <> header "avr-emulator - an emulator for the AVR instruction set, v0.0.1")


assembleProgramFromFile :: FilePath -> IO (Maybe [Instruction])
assembleProgramFromFile filename = do
    contents <- readFile filename
    case parseAssembly contents of
        Left err -> do
            print err
            return Nothing
        Right instructions -> return (Just instructions)

compileFromFile :: FilePath -> Bool -> Int -> IO (Either String EmulatorState)
compileFromFile input False memorySize = do
    maybeInstructions <- assembleProgramFromFile input
    case maybeInstructions of
        Just instructions -> do
            let finalState = run instructions memorySize
            (return (Right finalState))
        Nothing -> do
            (return (Left "Error assembling the program"))

compileFromFile input True memorySize = do
    maybeInstructions <- assembleProgramFromFile input
    case maybeInstructions of
        Just instructions -> do
            mapM_ print (replaceLabels instructions)
            let finalState = run instructions memorySize
            (return (Right finalState))
        Nothing -> do
            (return (Left "Error assembling the program"))

entryFunction :: Options -> IO () 
entryFunction (Options file False dmpIR memorySize) = do
    finalState <- compileFromFile file dmpIR memorySize
    case finalState of
        Right state -> do
            putStrLn (showRegisters $ registers state)  -- Print the final register values
            putStrLn (showStatusFlags $ flags state)    -- Print the final status flags
        Left errorMessage -> do
            print errorMessage

entryFunction (Options file True dmpIR memorySize) = do
    finalState <- compileFromFile file dmpIR memorySize
    case finalState of
        Right state -> do
            print (memory state)
            putStrLn (showRegisters $ registers state)  -- Print the final register values
            putStrLn (showStatusFlags $ flags state)    -- Print the final status flags
        Left errorMessage -> do
            print errorMessage