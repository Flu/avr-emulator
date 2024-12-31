module Emulator.State
    ( Register
    , Label
    , ProgramCounter
    , Registers
    , Memory
    , StackPointer
    , MutRegisters
    , MutMemory
    , EmulatorState(..)
    , StatusFlags(..)
    , getMemory
    , setMemory
    , setMemoryValues
    , getRegister
    , setRegister
    , setRegisters) where

import Control.Monad.ST (ST)
import Data.Binary (Word8, Word16)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Vector (Vector)
import qualified Data.Vector.Mutable as MV

type Register = Word8                       -- ^ Registers are 1 byte in AVR processors
type Label = String                         -- ^ Labels as they come from parsing
type ProgramCounter = Word16                -- ^ The PC is 2 bytes in AVR processors
type Registers = Vector Register            -- ^ The register bank is an array of 32 Word8
type Memory = Vector Word8                  -- ^ Memory is an array of Word8 as well, but the exact size depends on the SRAM of the device
type StackPointer = Word16                  -- ^ The stack pointer is also 2 bytes, since it points to a location in SRAM
type MutRegisters s = MV.MVector s Register -- ^ Type alias for a mutable vector of registers
type MutMemory s = MV.MVector s Word8       -- ^ Type alias for a mutable memory vector 

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
    interruptFlag :: !Bool,  -- ^ Global interrupt flag
    tFlag :: Bool,           -- ^ T flag (custom flag to be used by the user)
    halfCarryFlag :: Bool,   -- ^ Half-carry flag
    signFlag :: Bool,        -- ^ Sign flag
    overflowFlag :: Bool,    -- ^ Overflow flag
    negativeFlag :: Bool,    -- ^ Negative flag
    zeroFlag :: Bool,        -- ^ Zero flag
    carryFlag :: Bool        -- ^ Carry flag
} deriving (Show)

{-|
Reads a value from the specified memory address. 
This function retrieves the value stored at the given address in the mutable memory vector.
* memory: The mutable vector representing memory.
* memoryAddress: The address from which to read the value.
-}
getMemory :: MutMemory s -> Int -> ST s (Word8)
getMemory memory memoryAddress = MV.read memory memoryAddress

{-|
Writes a value to the specified memory address and updates the corresponding register 
if the address falls within the register range (0-31). If the address is outside this range, 
only the memory is updated.
* registers: The mutable vector representing registers.
* memory: The mutable vector representing memory.
* memoryAddress: The address to update.
* value: The value to write to the specified address.
-}
setMemory :: MutRegisters s -> MutMemory s -> Int -> Word8 -> ST s ()
setMemory registers memory memoryAddress value
    | memoryAddress >= 0 && memoryAddress < 32 = do 
        mutateVector memory memoryAddress value
        mutateVector registers memoryAddress value
    | otherwise = mutateVector memory memoryAddress value

{-|
Writes multiple values to memory and updates corresponding registers if the addresses 
fall within the register range (0-31). It processes each address-value pair sequentially.
* registers: The mutable vector representing registers.
* memory: The mutable vector representing memory.
* [(Int, Word8)]: A list of (address, value) pairs to update.

-}
setMemoryValues :: MutRegisters s -> MutMemory s -> [(Int,Word8)] -> ST s ()
setMemoryValues memory registers (x:xs) = loop (memory, registers) (x :| xs)
    where
        loop :: (MutMemory s, MutRegisters s) -> NonEmpty (Int,Word8) -> ST s ()
        loop (memory, registers) (x :| []) = setMemory memory registers (fst x) (snd x)
        loop (memory, registers) (x :| y:ys) = do
            setMemory memory registers (fst x) (snd x)
            loop (memory, registers) (y :| ys)

{-|
Reads a value from the specified register. This function retrieves the value 
stored at the given index in the mutable registers vector.
* registers: The mutable vector representing registers.
* registerIndex: The index of the register to read.
-}
getRegister :: MutRegisters s -> Int -> ST s (Word8)
getRegister registers registerIndex = MV.read registers registerIndex

{-|
Writes a value to the specified register and updates the corresponding memory 
address if the register index falls within the memory-mapped range (0-31). 
Raises an error if the register index is out of bounds.
* memory: The mutable vector representing memory.
* registers: The mutable vector representing registers.
* registerIndex: The index of the register to update.
* value: The value to write to the specified register.
-}
setRegister :: MutRegisters s -> MutMemory s -> Int -> Word8 -> ST s ()
setRegister memory registers registerIndex value
    | 0 <= registerIndex && registerIndex < 32 = do
        mutateVector memory registerIndex value
        mutateVector registers registerIndex value
    | otherwise = error "something has gone terribly wrong, register out of bounds"

{-|
Writes multiple values to registers and updates corresponding memory addresses if 
the register indices fall within the memory-mapped range (0-31). It processes each 
index-value pair sequentially.
* memory: The mutable vector representing memory.
* registers: The mutable vector representing registers.
* [(Int, Word8)]: A list of (index, value) pairs specifying the registers to update.
-}
setRegisters :: MutRegisters s -> MutMemory s -> [(Int,Word8)] -> ST s ()
setRegisters memory registers (x:xs) = loop (memory, registers) (x :| xs)
    where
        loop :: (MutMemory s, MutRegisters s) -> NonEmpty (Int,Word8) -> ST s ()
        loop (memory, registers) (x :| []) = setRegister memory registers (fst x) (snd x)
        loop (memory, registers) (x :| y:ys) = do
            setRegister memory registers (fst x) (snd x)
            loop (memory, registers) (y :| ys)

mutateVector :: MV.MVector s Word8 -> Int -> Word8 -> ST s ()
mutateVector vec index newValue = do
    MV.write vec index newValue
