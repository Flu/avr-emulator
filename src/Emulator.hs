module Emulator(run, Instruction(..), Memory, Register, Registers(..), EmulatorState(..), StatusFlags(..),
printRegisterBank, registersToString, showStatusFlags, prettyPrintMemory, replaceLabels, initEmulatorState, stepOneInstruction, stepMultipleInstructions) where

import Emulator.Core
import Emulator.Instructions
import Emulator.Utils
import Emulator.State

import Data.Binary (Word16)
import Data.Maybe (catMaybes)
import Data.Array

{- | After resolving the labels, this is the heart of the emulator. It fetches the instruction from memory with the PC,
    loads the state from the previous instruction and call executeInstruction. After the execution is done, it calls itself
    again with either PC + 1 (if it was a normal instruction or a branch that didn't happen) or PC + relAddress (if it was 
    a branch, jump or call). This is the 'fetch -> decode -> execute' cycle in a processor.

    If there are no more instructions in the list (PC has surpassed the upper bound of the instruction list), execution is considered
    done and the function returns the most recent EmulatorState.
-}
runProgram :: Array Int Instruction -> EmulatorState -> EmulatorState
runProgram initialInstructions = go -- Call recursive helper function go
  where
    go state =
      let pc = fromIntegral (programCounter state)
      in if pc >= length initialInstructions -- If PC bigger than the list, it means we got to the end of execution
         then state -- So return the state
         else
           let currentInstruction = initialInstructions ! pc -- Fetch next instruction to be executed from where the PC points to
               newState = executeInstruction currentInstruction state -- Decode and execute it, then get the updated emulator state 
           in go newState -- Call recursively with the new state

stepOneInstruction :: Array Int Instruction -> EmulatorState -> EmulatorState
stepOneInstruction programMemory lastState =
    let pc = fromIntegral $ programCounter lastState
        currentInstruction = programMemory ! pc
        updatedState = executeInstruction currentInstruction lastState
    in updatedState

stepMultipleInstructions :: Array Int Instruction -> EmulatorState -> Int -> EmulatorState
stepMultipleInstructions programMemory lastState steps = loop lastState steps
    where
        loop :: EmulatorState -> Int -> EmulatorState
        loop s 0 = s
        loop s n = loop (executeInstruction (programMemory ! (fromIntegral $ programCounter s)) s) (n-1)
    

initEmulatorState :: Int -> EmulatorState
initEmulatorState memorySize = EmulatorState {
        registers = listArray (0,31) (replicate 32 0),                        -- Initialize all registers to 0
        flags = StatusFlags False False False False False False False False,  -- Initialize all status flags to False
        programCounter = 0,                                                   -- Program counter starts executing from 0x0000
        memory = listArray (0, memorySize - 1) (replicate memorySize 0),      -- Initialize the memory with the requested size, set to 0
        sp = fromIntegral (memorySize - 1) :: Word16                          -- The stack pointer should point to the last memory address
        }

-- | Gets the list of instructions from the parser and the size of the SRAM as configured by the user.
-- @returns the final emulator state after finishing execution.
run :: [Instruction] -> Int -> EmulatorState
run instructions memorySize =
    let initialState = initEmulatorState memorySize
        instructionsWithAddresses = catMaybes $ replaceLabels instructions -- Resolve labels and filter out Nothings from the list
        instructionArray = listArray (0, (length instructionsWithAddresses) - 1) instructionsWithAddresses
    in
        runProgram instructionArray initialState -- Start the 'fetch -> decode -> execute' cycle by calling this function with the initial state
