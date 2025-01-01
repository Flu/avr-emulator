module Emulator
    ( run
    , Instruction(..)
    , Memory
    , Register
    , EmulatorState(..)
    , StatusFlags(..)
    , printRegisterBank
    , registersToString
    , showStatusFlags
    , prettyPrintMemory
    , replaceLabels
    , initEmulatorState
    , stepOneInstruction
    , stepMultipleInstructions
    , runUntilProgramEnd
    , runUntilFunctionEnd
    , printInstructionsAroundAddress) where

import Emulator.Core
import Emulator.Instructions
import Emulator.Utils
import Emulator.State

import Control.Monad.ST
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import Data.Vector ((!), Vector)

{- | After resolving the labels, this is the heart of the emulator. It fetches the instruction from memory with the PC,
    loads the state from the previous instruction and call executeInstruction. After the execution is done, it calls itself
    again with either PC + 1 (if it was a normal instruction or a branch that didn't happen) or PC + relAddress (if it was 
    a branch, jump or call). This is the 'fetch -> decode -> execute' cycle in a processor.

    If there are no more instructions in the list (PC has surpassed the upper bound of the instruction list), execution is considered
    done and the function returns the most recent EmulatorState.
-}
runProgram :: Vector Instruction -> EmulatorState -> EmulatorState
runProgram initialInstructions state = runST $ do
    mutMemory <- V.thaw (memory state) :: ST s (MutMemory s)
    mutRegisters <- V.thaw (registers state) :: ST s (MutRegisters s)
    (flags, pc, sp) <- go mutRegisters mutMemory (flags state) (programCounter state) (sp state)
    finalMemory <- V.freeze mutMemory
    finalRegisters <- V.freeze mutRegisters
    return EmulatorState {
        registers = finalRegisters,
        flags = flags,
        programCounter = pc,
        memory = finalMemory,
        sp = sp
    }
    where
        go :: MutRegisters s -> MutMemory s -> StatusFlags -> ProgramCounter -> StackPointer -> ST s (StatusFlags, ProgramCounter, StackPointer)
        go mutRegisters mutMemory currFlags currProgramCounter currSp = do
            let pc = fromIntegral currProgramCounter
            if pc >= length initialInstructions then
                return (currFlags, currProgramCounter, currSp)
            else do
                (newFlags, newPc, newSp) <- executeInstruction mutRegisters mutMemory currFlags currProgramCounter currSp (initialInstructions ! pc)
                go mutRegisters mutMemory newFlags newPc newSp

stepOneInstruction :: Vector Instruction -> EmulatorState -> (Bool, EmulatorState)
stepOneInstruction programMemory lastState@(EmulatorState regs flags pcReg sp memory)
    | (fromIntegral $ programCounter lastState) >= (length programMemory) - 1 = (True, lastState)
    | otherwise = runST $ do
        let pc = fromIntegral pcReg
        let currentInstruction = programMemory ! pc
        mutRegisters <- V.thaw regs :: ST s (MutRegisters s)
        mutMemory <- V.thaw memory :: ST s (MutMemory s)
        (newFlags, newPc, newSp) <- executeInstruction mutRegisters mutMemory flags pcReg sp currentInstruction
        updatedRegisters <- V.freeze mutRegisters
        updatedMemory <- V.freeze mutMemory
        return (False, EmulatorState {
            registers = updatedRegisters,
            flags = newFlags,
            programCounter = newPc,
            memory = updatedMemory,
            sp = newSp
        })

stepMultipleInstructions :: Vector Instruction -> EmulatorState -> Int -> (Bool, EmulatorState)
stepMultipleInstructions programMemory lastState@(EmulatorState regs flags pcReg sp memory) steps = runST $ do
    mutRegisters <- V.thaw regs :: ST s (MutRegisters s)
    mutMemory <- V.thaw memory :: ST s (MutMemory s)
    (isDone, newFlags, newPc, newSp) <- loop mutRegisters mutMemory flags pcReg sp steps
    updatedRegisters <- V.freeze mutRegisters
    updatedMemory <- V.freeze mutMemory
    return (isDone, EmulatorState {
        registers = updatedRegisters,
        flags = newFlags,
        programCounter = newPc,
        memory = updatedMemory,
        sp = newSp
    })
    where
        loop :: MutRegisters s -> MutMemory s -> StatusFlags -> ProgramCounter -> StackPointer -> Int -> ST s (Bool, StatusFlags, ProgramCounter, StackPointer)
        loop _ _ f p s 0 = return (False, f, p, s)
        loop mutRegs mutMem f p s n
            | (fromIntegral p) >= length programMemory = return (True, f, p, s)
            | otherwise = do
                (updatedFlags, updatedPc, updatedSp) <- executeInstruction mutRegs mutMem f p s (programMemory ! (fromIntegral p))
                loop mutRegs mutMem updatedFlags updatedPc updatedSp (n-1)

runUntilProgramEnd :: Vector Instruction -> EmulatorState -> (Bool, EmulatorState)
runUntilProgramEnd programMemory lastState@(EmulatorState regs flags pcReg sp memory) = runST $ do
    mutRegisters <- V.thaw regs :: ST s (MutRegisters s)
    mutMemory <- V.thaw memory :: ST s (MutMemory s)
    (isDone, newFlags, newPc, newSp) <- loop mutRegisters mutMemory flags pcReg sp
    updatedRegisters <- V.freeze mutRegisters
    updatedMemory <- V.freeze mutMemory
    return (isDone, EmulatorState {
        registers = updatedRegisters,
        flags = newFlags,
        programCounter = newPc,
        memory = updatedMemory,
        sp = newSp
    })
    where
        loop :: MutRegisters s -> MutMemory s -> StatusFlags -> ProgramCounter -> StackPointer -> ST s (Bool, StatusFlags, ProgramCounter, StackPointer)
        loop mutRegs mutMem f p s
            | fromIntegral p >= length programMemory = return (True, f, p, s)
            | otherwise = do
                (updatedFlags, updatedPc, updatedSp) <- executeInstruction mutRegs mutMem f p s (programMemory ! (fromIntegral p))
                loop mutRegs mutMem updatedFlags updatedPc updatedSp

runUntilFunctionEnd :: Vector Instruction -> EmulatorState -> (Bool, EmulatorState)
runUntilFunctionEnd programMemory lastState@(EmulatorState regs flags pcReg sp memory) = runST $ do
    mutRegisters <- V.thaw regs :: ST s (MutRegisters s)
    mutMemory <- V.thaw memory :: ST s (MutMemory s)
    (isDone, newFlags, newPc, newSp) <- loop mutRegisters mutMemory flags pcReg sp 0
    updatedRegisters <- V.freeze mutRegisters
    updatedMemory <- V.freeze mutMemory
    return (isDone, EmulatorState {
        registers = updatedRegisters,
        flags = newFlags,
        programCounter = newPc,
        memory = updatedMemory,
        sp = newSp
    })
    where
        loop :: MutRegisters s -> MutMemory s -> StatusFlags -> ProgramCounter -> StackPointer -> Int -> ST s (Bool, StatusFlags, ProgramCounter, StackPointer)
        loop mutRegs mutMem f p s depth
            | pc >= length programMemory = return (True, f, p, s)
            | checkIfEndOfFunctionInstruction (programMemory ! pc) && depth == 0 = return (False, f, p, s)
            | checkIfEndOfFunctionInstruction (programMemory ! pc) && depth /= 0 = do
                (updatedFlags, updatedPc, updatedSp) <- executeInstruction mutRegs mutMem f p s (programMemory ! pc)
                loop mutRegs mutMem updatedFlags updatedPc updatedSp (depth - 1)
            | checkIfFunctionCallInstruction (programMemory ! pc) = do
                (updatedFlags, updatedPc, updatedSp) <- executeInstruction mutRegs mutMem f p s (programMemory ! pc)
                loop mutRegs mutMem updatedFlags updatedPc updatedSp (depth + 1)
            | otherwise = do
                (updatedFlags, updatedPc, updatedSp) <- executeInstruction mutRegs mutMem f p s (programMemory ! pc)
                loop mutRegs mutMem updatedFlags updatedPc updatedSp depth
            where pc = fromIntegral p

-- | Gets the list of instructions from the parser and the size of the SRAM as configured by the user.
-- @returns the final emulator state after finishing execution.
run :: [Instruction] -> Int -> EmulatorState
run instructions memorySize =
    let initialState = initEmulatorState memorySize
        instructionsWithAddresses = catMaybes $ replaceLabels instructions -- Resolve labels and filter out Nothings from the list
        instructionVector = V.fromList instructionsWithAddresses
    in
        runProgram instructionVector initialState -- Start the 'fetch -> decode -> execute' cycle by calling this function with the initial state
