module Options(Options(..), options) where

import Options.Applicative

data Options = Options
    { file :: String
    , dumpMemory :: Bool
    , dumpIntermediaryRepresentation :: Bool
    , memorySize :: Int }

options :: Parser Options
options = Options
    <$> strOption
        (metavar "TARGET"
        <> short 'f'
        <> long "file"
        <> help "Target file for assembling")
    <*> switch
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