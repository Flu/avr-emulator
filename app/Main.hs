module Main where

import System.Environment
import Emulator
import Parser

assembleProgramFromFile :: FilePath -> IO (Maybe [Instruction])
assembleProgramFromFile filename = do
    contents <- readFile filename
    case parseAssembly contents of
        Left err -> do
            print err
            return Nothing
        Right instructions -> return (Just instructions)

main :: IO ()
main = do
    args <- getArgs  -- Get command-line arguments
    case args of
        [filename] -> do  -- Expect exactly one argument (the filename)
            maybeInstructions <- assembleProgramFromFile filename
            print maybeInstructions
            case maybeInstructions of
                Just instructions -> do
                    mapM_ print (replaceLabels instructions)
                    let finalState = run instructions
                    print (memory finalState)
                    putStrLn (showRegisters $ registers finalState)  -- Print the final register values
                    putStrLn (showStatusFlags $ flags finalState)    -- Print the final status flags
                    print (programCounter finalState)
                Nothing -> do
                    putStrLn "Error assembling the program."
        _ -> putStrLn "Usage: ./main <filename>"  -- Handle incorrect number of arguments
