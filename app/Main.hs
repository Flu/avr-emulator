module Main where

import System.Environment
import Options.Applicative
import Emulator
import Parser
import Options

import Data.Version ( showVersion )
import Paths_avr_emulator ( version )
import Repl (replLoop)

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
            return (Right finalState)
        Nothing -> (return (Left "Error assembling the program"))

compileFromFile input True memorySize = do
    maybeInstructions <- assembleProgramFromFile input
    case maybeInstructions of
        Just instructions -> do
            mapM_ print (replaceLabels instructions)
            let finalState = run instructions memorySize
            return (Right finalState)
        Nothing -> (return (Left "Error assembling the program"))

getVersion :: String
getVersion = showVersion version

entryFunction :: Options -> IO ()
entryFunction (Options False dmpIR memorySize False False file) = do
    finalState <- compileFromFile file dmpIR memorySize
    case finalState of
        Right state -> do
            printRegisterBank $ registers state         -- Pretty print the register banks
            putStrLn (showStatusFlags $ flags state)    -- Print the final status flags
        Left errorMessage -> print errorMessage

entryFunction (Options True dmpIR memorySize False False file) = do
    finalState <- compileFromFile file dmpIR memorySize
    case finalState of
        Right state -> do
            prettyPrintMemory (memory state)            -- Pretty print the memory
            putStrLn ""
            printRegisterBank $ registers state         -- Pretty print the register banks
            putStrLn (showStatusFlags $ flags state)    -- Print the final status flags
        Left errorMessage -> print errorMessage

entryFunction (Options _ _ memorySize True False file) = do
    maybeInstructions <- assembleProgramFromFile file
    case maybeInstructions of
        Just instructions -> do
            state <- replLoop instructions memorySize
            putStrLn "geagre"
        Nothing -> putStrLn "Nothing ever worked right, idk"
    

entryFunction (Options _ _ _ _ True _) = putStrLn ("avr-emulator v" ++ getVersion)