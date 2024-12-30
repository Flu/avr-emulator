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

stepOneInstruction :: Vector Instruction -> EmulatorState -> (Bool, EmulatorState)
stepOneInstruction programMemory lastState
    | (fromIntegral $ programCounter lastState) >= (length programMemory) - 1 = (True, lastState)
    | otherwise = let
        pc = fromIntegral $ programCounter lastState
        currentInstruction = programMemory ! pc
        updatedState = executeInstruction currentInstruction lastState
        in (False, updatedState)

stepMultipleInstructions :: Vector Instruction -> EmulatorState -> Int -> (Bool, EmulatorState)
stepMultipleInstructions programMemory lastState steps = loop lastState steps
    where
        loop :: EmulatorState -> Int -> (Bool, EmulatorState)
        loop s 0 = (False, s)
        loop s n
            | (fromIntegral $ programCounter s) >= length programMemory = (True, s)
            | otherwise = loop (executeInstruction (programMemory ! (fromIntegral $ programCounter s)) s) (n-1)

runUntilProgramEnd :: Vector Instruction -> EmulatorState -> (Bool, EmulatorState)
runUntilProgramEnd programMemory lastState = loop lastState
    where
        pc state = (fromIntegral $ programCounter state)
        loop :: EmulatorState -> (Bool, EmulatorState)
        loop s
            | pc s >= length programMemory = (True, s)
            | otherwise = loop (executeInstruction (programMemory ! (pc s)) s)

runUntilFunctionEnd :: Vector Instruction -> EmulatorState -> (Bool, EmulatorState)
runUntilFunctionEnd programMemory lastState = loop lastState 0
    where
        pc state = (fromIntegral $ programCounter state)
        loop :: EmulatorState -> Int -> (Bool, EmulatorState)
        loop s depth
            | pc s >= length programMemory = (True, s)
            | checkIfEndOfFunctionInstruction (programMemory ! (pc s)) && depth == 0 = (False, s)
            | checkIfEndOfFunctionInstruction (programMemory ! (pc s)) && depth /= 0 = loop (executeInstruction (programMemory ! (pc s)) s ) (depth - 1)
            | checkIfFunctionCallInstruction $ programMemory ! (pc s) = loop (executeInstruction (programMemory ! (pc s)) s ) (depth + 1)
            | otherwise = loop (executeInstruction (programMemory ! (pc s)) s) depth

-- | Gets the list of instructions from the parser and the size of the SRAM as configured by the user.
-- @returns the final emulator state after finishing execution.
run :: [Instruction] -> Int -> EmulatorState
run instructions memorySize =
    let initialState = initEmulatorState memorySize
        instructionsWithAddresses = catMaybes $ replaceLabels instructions -- Resolve labels and filter out Nothings from the list
        instructionVector = V.fromList instructionsWithAddresses
    in
        runProgram instructionVector initialState -- Start the 'fetch -> decode -> execute' cycle by calling this function with the initial state
