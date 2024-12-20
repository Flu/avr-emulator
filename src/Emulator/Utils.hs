module Emulator.Utils where

import Emulator.State
import Emulator.Instructions

import Data.Array
import Data.Binary (Word8, Word16)
import Data.Char (intToDigit)
import qualified Data.Map as Map
import Numeric (showHex, showIntAtBase)
import System.Console.ANSI
import Text.Printf


-- | Constructs a hash table with all the labels and their addresses to refer to later when resolving labels for branch instructions
constructJumpTable :: [Instruction] -> Int -> Map.Map Label Int -> (Map.Map Label Int, Int)
constructJumpTable [] currentAddress labelMap = (labelMap, currentAddress)
constructJumpTable ((LABEL label):rest) currentAddress labelMap = constructJumpTable rest (currentAddress+1) (Map.insert label currentAddress labelMap)
constructJumpTable (_:rest) currentAddress labelMap = constructJumpTable rest (currentAddress + 1) labelMap

{- | Iterate through all instructions. If the instruction is a branch/jump/call, resolve the label using the jump table to get a
    relative address to the current instruction. For example, if the instruction was 'CALL label1' and label1 was 20 instructions behind,
    the new instruction would be 'CALLR -21'.
-}
resolveLabels :: Map.Map Label Int -> (Int, Instruction) -> Maybe Instruction
resolveLabels labelMap (address, JMP label)
    | relAddress > 0 = Just (JMPR (relAddress - 1))
    | otherwise = Just (JMPR relAddress)
    where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRCC label)
    | relAddress > 0 = Just (BRCCR (relAddress - 1))
    | otherwise = Just (BRCCR relAddress)
    where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRCS label)
    | relAddress > 0 = Just (BRCSR (relAddress - 1))
    | otherwise = Just (BRCSR relAddress)
    where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BREQ label)
    | relAddress > 0 = Just (BREQR (relAddress - 1))
    | otherwise = Just (BREQR relAddress)
    where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRGE label)
    | relAddress > 0 = Just (BRGER (relAddress - 1))
    | otherwise = Just (BRGER relAddress)
    where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRHC label)
    | relAddress > 0 = Just (BRHCR (relAddress - 1))
    | otherwise = Just (BRHCR relAddress)
    where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRHS label)
    | relAddress > 0 = Just (BRHSR (relAddress - 1))
    | otherwise = Just (BRHSR relAddress)
    where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRID label)
    | relAddress > 0 = Just (BRIDR (relAddress - 1))
    | otherwise = Just (BRIDR relAddress)
    where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRIE label)
    | relAddress > 0 = Just (BRIER (relAddress - 1))
    | otherwise = Just (BRIER relAddress)
    where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRLO label)
    | relAddress > 0 = Just (BRLOR (relAddress - 1))
    | otherwise = Just (BRLOR relAddress)
    where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRLT label)
    | relAddress > 0 = Just (BRLTR (relAddress - 1))
    | otherwise = Just (BRLTR relAddress)
    where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRMI label)
   | relAddress > 0 = Just (BRMIR (relAddress -1))
   | otherwise = Just (BRMIR relAddress)
   where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRNE label)
    | relAddress > 0 = Just (BRNER (relAddress - 1))
    | otherwise = Just (BRNER relAddress)
    where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRPL label)
   | relAddress > 0 = Just (BRPLR (relAddress -1))
   | otherwise = Just (BRPLR relAddress)
   where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRSH label)
   | relAddress > 0 = Just (BRSHR (relAddress -1))
   | otherwise = Just (BRSHR relAddress)
   where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRTC label)
   | relAddress > 0 = Just (BRTCR (relAddress -1))
   | otherwise = Just (BRTCR relAddress)
   where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRTS label)
   | relAddress > 0 = Just (BRTSR (relAddress -1))
   | otherwise = Just (BRTSR relAddress)
   where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRVC label)
   | relAddress > 0 = Just (BRVCR (relAddress -1))
   | otherwise = Just (BRVCR relAddress)
   where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRVS label)
   | relAddress > 0 = Just (BRVSR (relAddress -1))
   | otherwise = Just (BRVSR relAddress)
   where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, CALL label)
   | relAddress > 0 = Just (CALLR (relAddress -1))
   | otherwise = Just (CALLR relAddress)
   where relAddress = Map.findWithDefault 0 label labelMap - address

-- | If the current Instruction is a label or any other instruction, skip
resolveLabels _labelMap (_, LABEL label) = Just (LABEL label)
resolveLabels _labelMap (address, otherInstr) = Just otherInstr

-- | Constructs the jump table and then resolves the labels with it
replaceLabels :: [Instruction] -> [Maybe Instruction]
replaceLabels instructions =
  let (labelMap, _) = constructJumpTable instructions 0 Map.empty
   in zipWith (curry (resolveLabels labelMap)) [0 .. ] instructions

-- | Pretty prints the register bank to stdout and colors non-zero values so they can be easier to see
printRegisterBank :: Registers -> IO ()
printRegisterBank regs = go (assocs regs)
    where
        go::[(Int, Register)] -> IO ()
        go [] = return ()
        go ((i, r):rs)
            | r /= 0 = do
                putStr $ printf "R%-5s " (show i)
                setSGR [SetColor Foreground Vivid Red]
                putStr $ printf "0x%02x" r
                setSGR [Reset]
                putStr "  "
                setSGR [SetColor Foreground Vivid Red]
                putStr $ printf "%08s" (showIntAtBase 2 intToDigit r "")
                setSGR [Reset]
                putStrLn ""
                go rs
            | r == 0 = do
                putStr $ printf "R%-5s " (show i)
                setSGR [Reset]
                putStrLn $ printf "0x%02x  %08s" r (showIntAtBase 2 intToDigit r "")
                setSGR [Reset]
                go rs

-- | Returns a String representation of the register bank in hexadecimal and binary
registersToString :: Registers -> String
registersToString regs =
    let
        registers = assocs regs         -- Create a list of tuples of index and register value
        go::[(Int, Register)] -> String -- unction for iterating through the register bank array
        go regs = case regs of
            -- If arrived at the end of the array, return an empty String
            [] -> ""
            -- Take the current index and register value, construct the String representation in the form "R12  0x01 00000001",
            -- add a newline and call the function recursively on the next element
            ((i, r):rs) -> printf "R%-5s 0x%02x  %08s" (show i) r (showIntAtBase 2 intToDigit r "") ++ "\n" ++ go rs
    in
        go registers

-- | Returns a String representation of the SREG/status flags
showStatusFlags :: StatusFlags -> String
showStatusFlags sreg =
    " I: " ++ show (interruptFlag sreg) ++
    " T: " ++ show (tFlag sreg) ++
    " H: " ++ show (halfCarryFlag sreg) ++
    " S: " ++ show (signFlag sreg) ++
    " V: " ++ show (overflowFlag sreg) ++
    " N: " ++ show (negativeFlag sreg) ++
    " Z: " ++ show (zeroFlag sreg) ++
    " C: " ++ show (carryFlag sreg)

-- | Converts an Int to a zero-padded hex string of length 4 (e.g., "0000")
toHex4 :: Int -> String
toHex4 x = let h = showHex x "" in replicate (4 - length h) '0' ++ h

-- | Converts a Word8 to a zero-padded hex string of length 2 (e.g., "00")
toHex2 :: Word8 -> String
toHex2 x = let h = showHex x "" in replicate (2 - length h) '0' ++ h

{- | Prints a single row of memory, starting with the address of the first byte to be printed and then a number of bytes
depending on the values given. The bytes are grouped 2-by-2 and colored if non-zero 
-}
printRow :: Int -> [Word8] -> IO ()
printRow addr values = do
    putStr $ "0x" ++ toHex4 addr ++ "    " -- Print the address
    mapM_ printPair (groupPairs values)    -- Print the byte pair for every pair after groupinh
    putStrLn ""
  where
    -- | Groups the list of bytes into pairs
    groupPairs :: [Word8] -> [[Word8]]
    groupPairs []       = []
    groupPairs (x:y:xs) = [x, y] : groupPairs xs
    groupPairs [x]      = [[x]] -- Handle odd-sized memory gracefully

    -- | Prints a single pair of bytes, with non-zero highlighting
    printPair :: [Word8] -> IO ()
    printPair [a, b] = do
        if a /= 0 || b /= 0 -- If either of the bytes is not zero, color the whole group
            then do
                setSGR [SetColor Foreground Vivid Red]
                putStr $ toHex2 a ++ toHex2 b
                setSGR [Reset]
            else            -- Else don't color them
                putStr $ toHex2 a ++ toHex2 b
        putStr " "
    printPair [a] = do -- Handle the last unpaired byte if any
        let pair = toHex2 a ++ "00"
        if a /= 0   -- If either of the bytes is not zero, color the whole group
            then do
                setSGR [SetColor Foreground Vivid Red]
                putStr pair
                setSGR [Reset]
            else    -- Else don't color them
                putStr pair
        putStr " " -- Add some space before the next byte pair

-- | Pretty-prints the entire memory
prettyPrintMemory :: Memory -> IO ()
prettyPrintMemory mem = do
    let (_, end) = bounds mem -- Get the length of the memory in bytes
        -- Get every 16th address and pair it with the next 16 bytes of memory
        rows = [(addr, [mem ! i | i <- [addr .. min (addr + 15) end]]) | addr <- [0, 16 .. end]]
    -- Because printRow needs two arguments, we need to uncurry it so it can receive a tuple instead
    -- Then we map over all elements of rows (the tuples) and give them one by one to printRow, which formattes them
    -- and prints them
    mapM_ (uncurry printRow) rows
