module Options(Options(..), options) where

import Options.Applicative

data Options = Options
    { dumpMemory :: Bool
    , dumpIntermediaryRepresentation :: Bool
    , memorySize :: Int
    , interactive :: Bool
    , displayVersion :: Bool
    , file :: String }

options :: Parser Options
options = Options
    <$> switch
        (long "dumpMemory"
        <> short 'd'
        <> help "Dump all the memory")
    <*> switch
        (long "dumpIntermediaryRepresentation"
        <> short 'D'
        <> internal
        <> hidden)
    <*> option auto
        (long "memorySize"
        <> short 'm'
        <> help "How much available memory to allocate for the virtual machine in bytes"
        <> showDefault
        <> value 2000
        <> metavar "INT")
    <*> switch
        (long "interactive"
        <> short 'i'
        <> help "Start up an interactive session for step-by-step execution and debugging")
    <*> switch
        (long "version"
        <> short 'v'
        <> help "Print version of program")
    <*> strOption
        (metavar "TARGET"
        <> short 'f'
        <> long "file"
        <> help "Target file for assembling")