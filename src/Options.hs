module Options(Options(..), options) where

import Options.Applicative

data Options = Options
    { dumpMemory :: Bool
    , dumpIntermediaryRepresentation :: Bool
    , memorySize :: Int
    , displayVersion :: Bool
    , file :: Maybe FilePath }

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
        (long "version"
        <> short 'v'
        <> help "Print version of program")
    <*> optional (strArgument
        (metavar "TARGET"
        <> help "Target file for assembling"))