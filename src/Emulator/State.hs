module Emulator.State
    ( Register
    , Label
    , ProgramCounter
    , Registers
    , Memory
    , StackPointer
    , EmulatorState(..)
    , StatusFlags(..)
    , getMemory
    , setMemory
    , setMemoryValues
    , getRegister
    , setRegister
    , setRegisters) where

import Control.Monad.ST (runST)
import Data.Binary (Word8, Word16)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Vector ((!), Vector, thaw, freeze)
import qualified Data.Vector.Mutable as MV

type Register = Word8               -- ^ Registers are 1 byte in AVR processors
type Label = String                 -- ^ Labels as they come from parsing
type ProgramCounter = Word16        -- ^ The PC is 2 bytes in AVR processors
type Registers = Vector Register    -- ^ The register bank is an array of 32 Word8
type Memory = Vector Word8          -- ^ Memory is an array of Word8 as well, but the exact size depends on the SRAM of the device
type StackPointer = Word16          -- ^ The stack pointer is also 2 bytes, since it points to a location in SRAM

-- | Data type for holding everything about the current state of the emulator
data EmulatorState = EmulatorState {
    registers :: !Registers,             -- ^ General purpose registers R0 through R31
    flags :: !StatusFlags,               -- ^ SREG or status flags for keeping track of certain conditions 
    programCounter :: !ProgramCounter,   -- ^ Program counter for keeping track of the next instruction to execute
    sp :: !StackPointer,                 -- ^ Stack pointer - it always points to the top of the stack, in memory
    memory :: !Memory                    -- ^ Memory array for SRAM (LD or ST operations write to memory)
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

getMemory :: Memory -> Int -> Word8
getMemory memory memoryAddress =  memory ! memoryAddress

setMemory :: Memory -> Registers -> Int -> Word8 -> (Memory, Registers)
setMemory memory registers memoryAddress value
  | memoryAddress >= 0 && memoryAddress < 32  = (mutateVector memory memoryAddress value, mutateVector registers memoryAddress value)
  | otherwise = (mutateVector memory memoryAddress value, registers)

setMemoryValues :: Memory -> Registers -> [(Int,Word8)] -> (Memory, Registers)
setMemoryValues memory registers (x:xs) = loop (memory, registers) (x :| xs)
    where
        loop :: (Memory, Registers) -> NonEmpty (Int,Word8) -> (Memory, Registers)
        loop (memory, registers) (x :| []) = setMemory memory registers (fst x) (snd x)
        loop (memory, registers) (x :| y:ys) = loop (setMemory memory registers (fst x) (snd x)) (y :| ys)

getRegister :: Registers -> Int -> Word8
getRegister registers registerIndex = registers ! registerIndex

setRegister :: Memory -> Registers -> Int -> Word8 -> (Memory, Registers)
setRegister memory registers registerIndex value
    | 0 <= registerIndex && registerIndex < 32 = (mutateVector memory registerIndex value, mutateVector registers registerIndex value)
    | otherwise = error "something has gone terribly wrong, register out of bounds"

setRegisters :: Memory -> Registers -> [(Int,Word8)] -> (Memory, Registers)
setRegisters memory registers (x:xs) = loop (memory, registers) (x :| xs)
    where
        loop :: (Memory, Registers) -> NonEmpty (Int,Word8) -> (Memory, Registers)
        loop (memory, registers) (x :| []) = setRegister memory registers (fst x) (snd x)
        loop (memory, registers) (x :| y:ys) = loop (setRegister memory registers (fst x) (snd x)) (y :| ys)

mutateVector :: Vector Word8 -> Int -> Word8 -> Vector Word8
mutateVector vec index newValue = runST $ do
    mutableVector <- thaw vec

    MV.write mutableVector index newValue

    updatedVector <- freeze mutableVector
    return updatedVector
