module Emulator.State where

import Data.Array
import Data.Binary (Word8, Word16)

type Register = Word8               -- ^ Registers are 1 byte in AVR processors
type Label = String                 -- ^ Labels as they come from parsing
type ProgramCounter = Word16        -- ^ The PC is 2 bytes in AVR processors
type Registers = Array Int Register -- ^ The register bank is an array of 32 Word8
type Memory = Array Int Word8       -- ^ Memory is an array of Word8 as well, but the exact size depends on the SRAM of the device
type StackPointer = Word16          -- ^ The stack pointer is also 2 bytes, since it points to a location in SRAM

-- Data type for holding everything about the current state of the emulator
-- TODO: registers should be part of memory, mapped to the first 32 bytes of memory
data EmulatorState = EmulatorState {
    registers :: Registers,             -- ^ General purpose registers R0 through R31
    flags :: StatusFlags,               -- ^ SREG or status flags for keeping track of certain conditions 
    programCounter :: ProgramCounter,   -- ^ Program counter for keeping track of the next instruction to execute
    sp :: StackPointer,                 -- ^ Stack pointer - it always points to the top of the stack, in memory
    memory :: Memory                    -- ^ Memory array for SRAM (LD or ST operations write to memory)
} deriving (Show)

-- | Data type for holding the status flags. Otherwise called the SREG
data StatusFlags = StatusFlags {
    interruptFlag :: Bool,  -- ^ Global interrupt flag
    tFlag :: Bool,          -- ^ T flag (custom flag to be used by the user)
    halfCarryFlag :: Bool,  -- ^ Half-carry flag
    signFlag :: Bool,       -- ^ Sign flag
    overflowFlag :: Bool,   -- ^ Overflow flag
    negativeFlag :: Bool,   -- ^ Negative flag
    zeroFlag :: Bool,       -- ^ Zero flag
    carryFlag :: Bool       -- ^ Carry flag
} deriving (Show)