module Main where

import Options.Applicative
import Text.Megaparsec.Error
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
            putStrLn $ errorBundlePretty err
            return Nothing
        Right instructions -> return (Just instructions)

compileFromFile :: FilePath -> Bool -> Int -> IO (Maybe EmulatorState)
compileFromFile input False memorySize = do
    maybeInstructions <- assembleProgramFromFile input
    case maybeInstructions of
        Just instructions -> do
            let finalState = run instructions memorySize
            return (Just finalState)
        Nothing -> return Nothing

compileFromFile input True memorySize = do
    maybeInstructions <- assembleProgramFromFile input
    case maybeInstructions of
        Just instructions -> do
            mapM_ print (replaceLabels instructions)
            let finalState = run instructions memorySize
            return (Just finalState)
        Nothing -> return Nothing

getVersion :: String
getVersion = showVersion version

entryFunction :: Options -> IO ()
-- | Has opion "-v" but no file was given
entryFunction (Options _ _ _ _ True Nothing) = putStrLn ("avr-emulator v" ++ getVersion)

-- | Has option "-v" but a file was given, ignore the file and just print the version
entryFunction (Options _ _ _ _ True (Just _)) = putStrLn ("avr-emulator v" ++ getVersion)

-- | Supplied arguments but did not provide a file, return error
entryFunction (Options _ _ _ _ False Nothing) = error "You did not supply a file. Exiting."

-- | Supplied file, does not dump memory to stdout
entryFunction (Options False dmpIR memorySize False _ (Just filepath)) = do
    finalState <- compileFromFile filepath dmpIR memorySize
    case finalState of
        Just state -> do
            printRegisterBank $ registers state         -- Pretty print the register banks
            putStrLn (showStatusFlags $ flags state)    -- Print the final status flags
        Nothing -> return ()

-- | Supplied file, will dump SRAM to stdout
entryFunction (Options True dmpIR memorySize False _ (Just filepath)) = do
    finalState <- compileFromFile filepath dmpIR memorySize
    case finalState of
        Just state -> do
            prettyPrintMemory (memory state)            -- Pretty print the memory
            putStrLn ""
            printRegisterBank $ registers state         -- Pretty print the register banks
            putStrLn (showStatusFlags $ flags state)    -- Print the final status flags
        Nothing -> return ()

-- | Ignore dump memory and dump IR flags, start a REPL for an interactive session
entryFunction (Options _ _ memorySize True False (Just filepath)) = do
    maybeInstructions <- assembleProgramFromFile filepath
    case maybeInstructions of
        Just instructions -> do
            _ <- replLoop instructions memorySize
            return ()
        Nothing -> putStrLn "Assembler error"
