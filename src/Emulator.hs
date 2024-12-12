module Emulator(run, Instruction(..), Memory, Register, Registers(..), EmulatorState, StatusFlags(..), add, ldi, mov, flags, registers,
printRegisterBank, registersToString, showStatusFlags, prettyPrintMemory, programCounter, replaceLabels, memory, sp) where

import Data.Binary (Word8, Word16)
import Data.Bits
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Array
import Debug.Trace (trace)
import Text.Printf
import System.Console.ANSI

-- Data type for holding everything about the current state of the emulator
-- TODO: registers should be part of memory, mapped to the first 32 bytes of memory
data EmulatorState = EmulatorState {
    registers :: Registers,             -- ^ General purpose registers R0 through R31
    flags :: StatusFlags,               -- ^ SREG or status flags for keeping track of certain conditions 
    programCounter :: ProgramCounter,   -- ^ Program counter for keeping track of the next instruction to execute
    sp :: StackPointer,                 -- ^ Stack pointer - it always points to the top of the stack, in memory
    memory :: Memory                    -- ^ Memory array for SRAM (LD or ST operations write to memory)
} deriving (Show)

type Register = Word8               -- ^ Registers are 1 byte in AVR processors
type Label = String                 -- ^ Labels as they come from parsing
type ProgramCounter = Word16        -- ^ The PC is 2 bytes in AVR processors
type Registers = Array Int Register -- ^ The register bank is an array of 32 Word8
type Memory = Array Int Word8       -- ^ Memory is an array of Word8 as well, but the exact size depends on the SRAM of the device
type StackPointer = Word16          -- ^ The stack pointer is also 2 bytes, since it points to a location in SRAM

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

{- | Data type for holding the intermediary representation of the code.
    In this emulator, code, after being parsed, is transformed into IR (intermediary representation), which is just a list
    of Instruction objects. Each Instruction can be a different instruction, as well as pseudo-instructions, like `LABEL String`,
    which isn't really an instruction, but rather a way to signal that a label was there in the original program. This is useful
    for resolving labels with actual addresses later, as this is not done during the parsing process.

    Every instruction has an equivalent in this Instruction datatype. Sometimes even 2 if it is a branch instruction that uses labels.
    Because labels are resolved later, the parser returns `BRCC Label` for example. After resolving labels to addresses in memory,
    we wouldn't know what `Label` refers to anymore, so we need a BRCCR (the last R is for relative) which holds an Int instead,
    which signifies how many instructions ahead/behind we should jump. This is the instruction that gets interpreted by the VM.

    Find the complete reference for the instructions here:
    https://ww1.microchip.com/downloads/en/DeviceDoc/AVR-InstructionSet-Manual-DS40002198.pdf 
-}
data Instruction
    = ADC Register Register
    | ADD Register Register
    | ADIW Register Register Word8
    | AND Register Register
    | ANDI Register Word8
    | ASR Register
    | BCLR Int
    | BLD Register Int
    | BRCC Label
    | BRCCR Int
    | BRCS Label
    | BRCSR Int
    | BREQ Label
    | BREQR Int
    | BRGE Label
    | BRGER Int
    | BRHC Label
    | BRHCR Int
    | BRHS Label
    | BRHSR Int
    | BRID Label
    | BRIDR Int
    | BRIE Label
    | BRIER Int
    | BRLO Label
    | BRLOR Int
    | BRLT Label
    | BRLTR Int
    | BRMI Label
    | BRMIR Int
    | BRNE Label
    | BRNER Int
    | BRPL Label
    | BRPLR Int
    | BRSH Label
    | BRSHR Int
    | BRTC Label
    | BRTCR Int
    | BRTS Label
    | BRTSR Int
    | BRVC Label
    | BRVCR Int
    | BRVS Label
    | BRVSR Int
    | CALL Label
    | CALLR Int
    | CBR Register Word8
    | CLC
    | CLH
    | CLI
    | CLN
    | CLR Register
    | CLS
    | CLT
    | CLV
    | CLZ
    | COM Register
    | CP Register Register
    | CPC Register Register
    | CPI Register Word8
    | CPSE Register Register
    | DEC Register
    | EOR Register Register
    | INC Register
    | JMP Label
    | JMPR Int
    | LD Register String
    | LABEL Label
    | LDI Register Word8
    | LDS Register Word16
    | LSL Register
    | LSR Register
    | MOV Register Register
    | MOVW Register Register Register Register
    | MUL Register Register
    | MULS Register Register
    | NEG Register
    | NOP
    | OR Register Register
    | ORI Register Word8
    | POP Register
    | PUSH Register
    | RET
    | ROL Register
    | ROR Register
    | SBC Register Register
    | SBRC Register Word8
    | SBRS Register Word8
    | SEC
    | SEH
    | SEI
    | SEN
    | SER Register
    | SES
    | SET
    | SEV
    | SEZ
    | ST String Register
    | STS Word16 Register
    | SUB Register Register
    | SUBI Register Word8
    | SWAP Register
    | TST Register
    deriving (Show)

-- /////////////////////////////////////////////////////
-- Instruction implementations
-- Study the reference for what every instruction does, what flags it sets and what registers it modifies:
-- https://ww1.microchip.com/downloads/en/DeviceDoc/AVR-InstructionSet-Manual-DS40002198.pdf
-- /////////////////////////////////////////////////////

{-
    The arguments for these functions may vary, as the decoder function takes care to assign the arguments, but the
    return type should be the same for all of them. So if you want your new instruction to return something else,
    you need to change the return type for all the functions.
-}

adc :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
adc oldStatus registers sp memory rd rs =
    let
        rdIndex = fromIntegral rd
        rsIndex = fromIntegral rs
        op1 = registers ! rdIndex
        op2 = registers ! rsIndex
        result = op1 + op2 + (if carryFlag oldStatus then 1 else 0)
        updatedRegisters = registers // [(rdIndex,result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = testBit op1 3 && testBit op2 3 || testBit op1 3 && not (testBit result 3) || not (testBit result 3) && testBit op1 3,
            overflowFlag = testBit op1 7 && testBit op2 7 && not (testBit result 7) || not (testBit op1 7) && not (testBit op2 7) && testBit result 7,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = testBit op1 7 && testBit op2 7 || testBit op1 7 && not (testBit result 7) || not (testBit result 7) && testBit op1 7,
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (updatedRegisters, updatedFlags, 0, sp, memory)

add :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
add oldStatus registers sp memory rd rs =
    let
        rdIndex = fromIntegral rd
        rsIndex = fromIntegral rs
        op1 = registers ! rdIndex
        op2 = registers ! rsIndex
        result = op1 + op2
        updatedRegisters = registers // [(rdIndex,result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = testBit op1 3 && testBit op2 3 || testBit op1 3 && not (testBit result 3) || not (testBit result 3) && testBit op1 3,
            overflowFlag = testBit op1 7 && testBit op2 7 && not (testBit result 7) || not (testBit op1 7) && not (testBit op2 7) && testBit result 7,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = testBit op1 7 && testBit op2 7 || testBit op1 7 && not (testBit result 7) || not (testBit result 7) && testBit op1 7,
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (updatedRegisters, updatedFlags, 0, sp, memory)

adiw :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> Word8 -> (Registers, StatusFlags, Int, StackPointer, Memory)
adiw oldStatus registers sp memory op1 op2 k =
    let rd1Index = fromIntegral op1
        rdIndex = fromIntegral op2
        rd1 = registers ! rd1Index
        rd = registers ! rdIndex
        result = (fromIntegral rd1 :: Word16) `shiftL` 8 + (fromIntegral rd :: Word16) + (fromIntegral k :: Word16)
        resultH = fromIntegral (result `shiftR` 8) :: Word8
        resultL = fromIntegral result :: Word8
        updatedRegisters = registers // [(rd1Index, resultH), (rdIndex, resultL)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = not (testBit rd1 7) && testBit result 15,
            negativeFlag = testBit result 15,
            zeroFlag = result == 0,
            carryFlag = not (testBit result 15) && testBit rd1 7,
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
        in (updatedRegisters, updatedFlags, 0, sp, memory)


andInstr :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
andInstr oldStatus registers sp memory op1 op2 =
    let
        rdIndex = fromIntegral op1
        rrIndex = fromIntegral op2
        rd = registers ! rdIndex
        rr = registers ! rrIndex
        result = rd .&. rr
        updatedRegisters = registers // [(rdIndex,result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = False,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = carryFlag oldStatus,
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (updatedRegisters, updatedFlags, 0, sp, memory)

andi :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, StackPointer, Memory)
andi oldStatus registers sp memory op1 immediate =
    let
        rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        k = immediate
        result = rd .&. k
        updatedRegisters = registers // [(rdIndex,result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = False,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = carryFlag oldStatus,
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (updatedRegisters, updatedFlags, 0, sp, memory)

asr :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
asr oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        lsb = testBit rd 0
        msb = testBit rd 7
        result = if msb then rd `shiftR` 1 .|. 0x80 else rd `shiftR` 1 .&. 0xBF
        updatedRegisters = registers // [(rdIndex,result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = lsb,
            overflowFlag = xor (negativeFlag updatedFlags) (carryFlag updatedFlags),
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (updatedRegisters, updatedFlags, 0, sp, memory)

-- Clears a single flag.
bclr :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
bclr oldStatus registers sp memory flagNumber =
  let updatedFlags =
        StatusFlags {
            interruptFlag = interruptFlag oldStatus && (flagNumber /= 7),
            tFlag = tFlag oldStatus && (flagNumber /= 6),
            halfCarryFlag = halfCarryFlag oldStatus && (flagNumber /= 5),
            signFlag = signFlag oldStatus && (flagNumber /= 4),
            overflowFlag = overflowFlag oldStatus && (flagNumber /= 3),
            negativeFlag = negativeFlag oldStatus && (flagNumber /= 2),
            zeroFlag = zeroFlag oldStatus && (flagNumber /= 1),
            carryFlag = halfCarryFlag oldStatus && (flagNumber /= 0)
        }
   in (registers, updatedFlags, 0, sp, memory)

-- Sets byte b in register Rd equal to the T bit.
bld :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
bld flags registers sp memory rd b = 
    let rdIndex = fromIntegral rd
        t = tFlag flags
        value = registers ! rdIndex
        updatedValue
            | t = setBit value b
            | otherwise = clearBit value b
        newRegisters = registers // [(rdIndex, updatedValue)]
    in (newRegisters, flags, 0, sp, memory)

brcc :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
brcc oldStatus registers sp memory relAddress =
    let shouldJump = not (carryFlag oldStatus)
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, sp, memory)

brcs :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
brcs oldStatus registers sp memory relAddress =
    let shouldJump = carryFlag oldStatus
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, sp, memory)

breq :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
breq oldStatus registers sp memory relAddress =
    let shouldJump = zeroFlag oldStatus
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, sp, memory)

brge :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
brge oldStatus registers sp memory relAddress =
    let shouldJump = not (signFlag oldStatus)
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, sp, memory)

brhc :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
brhc oldStatus registers sp memory relAddress =
    let shouldJump = not (halfCarryFlag oldStatus)
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, sp, memory)

brhs :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
brhs oldStatus registers sp memory relAddress =
    let shouldJump = halfCarryFlag oldStatus
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, sp, memory)

brid :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
brid oldStatus registers sp memory relAddress =
    let shouldJump = interruptFlag oldStatus
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, sp, memory)

brie :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
brie oldStatus registers sp memory relAddress =
    let shouldJump = not (interruptFlag oldStatus)
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, sp, memory)

brlo :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
brlo oldStatus registers sp memory relAddress =
    let shouldJump = carryFlag oldStatus
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, sp, memory)

brlt :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
brlt oldStatus registers sp memory relAddress =
    let shouldJump = signFlag oldStatus
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, sp, memory)

brmi :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
brmi oldStatus registers sp memory relAddress =
    let shouldJump = negativeFlag oldStatus
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, sp, memory)

brne :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
brne oldStatus registers sp memory relAddress =
    let shouldJump = zeroFlag oldStatus
        jumpAddress = if shouldJump then 0 else relAddress
    in (registers, oldStatus, jumpAddress, sp, memory)

brpl :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
brpl oldStatus registers sp memory relAddress =
    let shouldJump = not (negativeFlag oldStatus)
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, sp, memory)

brsh :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
brsh oldStatus registers sp memory relAddress =
    let shouldJump = not (carryFlag oldStatus)
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, sp, memory)

brtc :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
brtc oldStatus registers sp memory relAddress =
    let shouldJump = not (tFlag oldStatus)
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, sp, memory)

brts :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
brts oldStatus registers sp memory relAddress =
    let shouldJump = tFlag oldStatus
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, sp, memory)

brvc :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
brvc oldStatus registers sp memory relAddress =
    let shouldJump = not (overflowFlag oldStatus)
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, sp, memory)

brvs :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
brvs oldStatus registers sp memory relAddress =
    let shouldJump = overflowFlag oldStatus
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, sp, memory)

call :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> Word16 -> (Registers, StatusFlags, Int, StackPointer, Memory)
call oldStatus registers sp memory relAddress returnAddress =
    let (high, low) = (fromIntegral (returnAddress `shiftR` 8), fromIntegral (returnAddress .&. 0xFF))
        updatedMemory = memory // [(fromIntegral sp, high),(fromIntegral (sp - 1), low)]
        newSp = sp - 2
        in (registers, oldStatus, relAddress, newSp, updatedMemory)

cbr :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, StackPointer, Memory)
cbr status registers sp mem rb k = andi status registers sp mem rb (0xFF - k)

clc :: StatusFlags -> Registers -> StackPointer -> Memory -> (Registers, StatusFlags, Int, StackPointer, Memory)
clc oldStatus registers sp memory =
    let updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = overflowFlag oldStatus,
            negativeFlag = negativeFlag oldStatus,
            zeroFlag = zeroFlag oldStatus,
            carryFlag = False,
            signFlag = signFlag oldStatus
    }
    in (registers, updatedFlags, 0, sp, memory)

clh :: StatusFlags -> Registers -> StackPointer -> Memory -> (Registers, StatusFlags, Int, StackPointer, Memory)
clh oldStatus registers sp memory =
    let updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = False,
            overflowFlag = overflowFlag oldStatus,
            negativeFlag = negativeFlag oldStatus,
            zeroFlag = zeroFlag oldStatus,
            carryFlag = carryFlag oldStatus,
            signFlag = signFlag oldStatus
    }
    in (registers, updatedFlags, 0, sp, memory)

cli :: StatusFlags -> Registers -> StackPointer -> Memory -> (Registers, StatusFlags, Int, StackPointer, Memory)
cli oldStatus registers sp memory =
    let updatedFlags = StatusFlags {
            interruptFlag = False,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = overflowFlag oldStatus,
            negativeFlag = negativeFlag oldStatus,
            zeroFlag = zeroFlag oldStatus,
            carryFlag = carryFlag oldStatus,
            signFlag = signFlag oldStatus
    }
    in (registers, updatedFlags, 0, sp, memory)

cln :: StatusFlags -> Registers -> StackPointer -> Memory -> (Registers, StatusFlags, Int, StackPointer, Memory)
cln oldStatus registers sp memory =
    let updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = overflowFlag oldStatus,
            negativeFlag = False,
            zeroFlag = zeroFlag oldStatus,
            carryFlag = carryFlag oldStatus,
            signFlag = signFlag oldStatus
    }
    in (registers, updatedFlags, 0, sp, memory)

clr :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
clr oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        result = 0
        updatedRegisters = registers // [(rdIndex, result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = False,
            negativeFlag = False,
            zeroFlag = True,
            carryFlag = carryFlag oldStatus,
            signFlag = False
    }
    in (updatedRegisters, updatedFlags, 0, sp, memory)

cls :: StatusFlags -> Registers -> StackPointer -> Memory -> (Registers, StatusFlags, Int, StackPointer, Memory)
cls oldStatus registers sp memory =
    let updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = overflowFlag oldStatus,
            negativeFlag = negativeFlag oldStatus,
            zeroFlag = zeroFlag oldStatus,
            carryFlag = carryFlag oldStatus,
            signFlag = False
    }
    in (registers, updatedFlags, 0, sp, memory)

clt :: StatusFlags -> Registers -> StackPointer -> Memory -> (Registers, StatusFlags, Int, StackPointer, Memory)
clt oldStatus registers sp memory =
    let updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = False,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = overflowFlag oldStatus,
            negativeFlag = negativeFlag oldStatus,
            zeroFlag = zeroFlag oldStatus,
            carryFlag = carryFlag oldStatus,
            signFlag = signFlag oldStatus
    }
    in (registers, updatedFlags, 0, sp, memory)

clv :: StatusFlags -> Registers -> StackPointer -> Memory -> (Registers, StatusFlags, Int, StackPointer, Memory)
clv oldStatus registers sp memory =
    let updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = False,
            negativeFlag = negativeFlag oldStatus,
            zeroFlag = zeroFlag oldStatus,
            carryFlag = carryFlag oldStatus,
            signFlag = signFlag oldStatus
    }
    in (registers, updatedFlags, 0, sp, memory)

clz :: StatusFlags -> Registers -> StackPointer -> Memory -> (Registers, StatusFlags, Int, StackPointer, Memory)
clz oldStatus registers sp memory =
    let updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = overflowFlag oldStatus,
            negativeFlag = negativeFlag oldStatus,
            zeroFlag = False,
            carryFlag = carryFlag oldStatus,
            signFlag = signFlag oldStatus
    }
    in (registers, updatedFlags, 0, sp, memory)

com :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
com oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        result = 255 - rd
        updatedRegisters = registers // [(rdIndex, result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = False,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = True,
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
        in (updatedRegisters, updatedFlags, 0, sp, memory)

cp :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
cp oldStatus registers sp memory op1 op2 =
    let rdIndex = fromIntegral op1
        rrIndex = fromIntegral op2
        rd = registers ! rdIndex
        rr = registers ! rrIndex
        result = rd - rr
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = not (testBit rd 3) && testBit rr 3 || testBit rr 3 && testBit result 3 || testBit result 3 && not (testBit rd 3),
            overflowFlag = testBit rd 7 && not (testBit rr 7) && not (testBit result 7) || not (testBit rd 7) && testBit rr 7 && testBit result 7,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = not (testBit rd 7) && testBit rr 7 || testBit rr 7 && testBit result 7 || testBit result 7 && not (testBit rd 7),
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (registers, updatedFlags, 0, sp, memory)

cpc :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
cpc oldStatus registers sp memory op1 op2 =
    let rdIndex = fromIntegral op1
        rrIndex = fromIntegral op2
        rd = registers ! rdIndex
        rr = registers ! rrIndex
        carry = if carryFlag oldStatus then 1 else 0
        result = rd - rr - carry
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = not (testBit rd 3) && testBit rr 3 || testBit rr 3 && testBit result 3 || testBit result 3 && not (testBit rd 3),
            overflowFlag = testBit rd 7 && not (testBit rr 7) && not (testBit result 7) || not (testBit rd 7) && testBit rr 7 && testBit result 7,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0 && zeroFlag oldStatus,
            carryFlag = not (testBit rd 7) && testBit rr 7 || testBit rr 7 && testBit result 7 || testBit result 7 && not (testBit rd 7),
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (registers, updatedFlags, 0, sp, memory)

cpi :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, StackPointer, Memory)
cpi oldStatus registers sp memory op1 immediate =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        k = immediate
        result = rd - k
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = not (testBit rd 3) && testBit k 3 || testBit k 3 && testBit result 3 || testBit result 3 && not (testBit rd 3),
            overflowFlag = testBit rd 7 && not (testBit k 7) && not (testBit result 7) || not (testBit rd 7) && testBit k 7 && testBit result 7,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = not (testBit rd 7) && testBit k 7 || testBit k 7 && testBit result 7 || testBit result 7 && not (testBit rd 7),
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (registers, updatedFlags, 0, sp, memory)

cpse :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
cpse oldStatus registers sp memory op1 op2 =
    let rdIndex = fromIntegral op1
        rrIndex = fromIntegral op2
        rd = registers ! rdIndex
        rr = registers ! rrIndex
        shouldJump = rd - rr == 0
        relativeJump = if shouldJump then 1 else 0
    in (registers, oldStatus, relativeJump, sp, memory)

dec :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
dec oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        result = rd - 1
        updatedRegisters = registers // [(rdIndex, result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = rd == 128,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = carryFlag oldStatus,
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (updatedRegisters, updatedFlags, 0, sp, memory)

eor :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
eor oldStatus registers sp memory op1 op2 =
    let
        rdIndex = fromIntegral op1
        rrIndex = fromIntegral op2
        rd = registers ! rdIndex
        rr = registers ! rrIndex
        result = xor rd rr
        updatedRegisters = registers // [(rdIndex,result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = False,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = carryFlag oldStatus,
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (updatedRegisters, updatedFlags, 0, sp, memory)

inc :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
inc oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        result = rd + 1
        updatedRegisters = registers // [(rdIndex, result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = rd == 127,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = carryFlag oldStatus,
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (updatedRegisters, updatedFlags, 0, sp, memory)

jmp :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
jmp oldStatus registers sp memory relAddress = (registers, oldStatus, relAddress, sp, memory)

ld :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> String -> (Registers, StatusFlags, Int, StackPointer, Memory)
ld oldStatus registers sp memory op1 "X" =
    let rdIndex = fromIntegral op1
        address16b = (fromIntegral (registers ! 27) :: Word16) `shiftL` 8 + (fromIntegral (registers ! 26) :: Word16)
        updatedRegisters = registers // [(rdIndex, memory ! fromIntegral address16b)]
    in (updatedRegisters, oldStatus, 0, sp, memory)

ld oldStatus registers sp memory op1 "X+" =
    let rdIndex = fromIntegral op1
        address16b = (fromIntegral (registers ! 27) :: Word16) `shiftL` 8 + (fromIntegral (registers ! 26) :: Word16)
        newXRegister = address16b + 1
        xHigh = fromIntegral (newXRegister `shiftR` 8) :: Word8
        xLow = fromIntegral newXRegister :: Word8
        updatedRegisters = registers // [(rdIndex, memory ! fromIntegral address16b), (27, xHigh), (26, xLow)]
    in (updatedRegisters, oldStatus, 0, sp, memory)

ld oldStatus registers sp memory op1 "-X" =
    let rdIndex = fromIntegral op1
        address16b = (fromIntegral (registers ! 27) :: Word16) `shiftL` 8 + (fromIntegral (registers ! 26) :: Word16)
        newXRegister = address16b - 1
        xHigh = fromIntegral (newXRegister `shiftR` 8) :: Word8
        xLow = fromIntegral newXRegister :: Word8
        updatedRegisters = registers // [(rdIndex, memory ! fromIntegral newXRegister), (27, xHigh), (26, xLow)]
    in (updatedRegisters, oldStatus, 0, sp, memory)

ldi :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, StackPointer, Memory)
ldi oldStatus registers sp memory rd immediate =
    let rdIndex = fromIntegral rd
        updatedRegisters = registers // [(rdIndex,immediate)]
        updatedFlags = oldStatus
    in (updatedRegisters, updatedFlags, 0, sp, memory)

lds :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Word16 -> (Registers, StatusFlags, Int, StackPointer, Memory)
lds oldStatus registers sp memory op1 immediate =
    let rdIndex = fromIntegral op1
        rd = fromIntegral (registers ! rdIndex)
        k = fromIntegral immediate
        updatedRegisters = registers // [(rdIndex, memory ! k)]
    in (updatedRegisters, oldStatus, 0, sp, memory)

lsl :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
lsl oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        msb = testBit rd 7
        result = rd `shiftL` 1
        updatedRegisters = registers // [(rdIndex,result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = testBit rd 3,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = msb,
            overflowFlag = xor (negativeFlag updatedFlags) (carryFlag updatedFlags),
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (updatedRegisters, updatedFlags, 0, sp, memory)

lsr :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
lsr oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        lsb = testBit rd 0
        result = rd `shiftR` 1
        updatedRegisters = registers // [(rdIndex,result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            negativeFlag = False,
            zeroFlag = result == 0,
            carryFlag = lsb,
            overflowFlag = xor (negativeFlag updatedFlags) (carryFlag updatedFlags),
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (updatedRegisters, updatedFlags, 0, sp, memory)

mov :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
mov oldStatus registers sp memory rd rs =
    let rdIndex = fromIntegral rd
        rsIndex = fromIntegral rs
        value = registers ! rsIndex
        updatedRegisters = registers // [(rdIndex,value)]
        updatedFlags = oldStatus
    in (updatedRegisters, updatedFlags, 0, sp, memory)

movw :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
movw oldStatus registers sp memory rd1 rd rr1 rr =
    let rd1Index = fromIntegral rd1
        rdIndex = fromIntegral rd
        rr1Index = fromIntegral rr1
        rrIndex = fromIntegral rr
        valueH = registers ! rr1Index
        valueL = registers ! rrIndex
        updatedRegisters = registers // [(rd1Index,valueH), (rdIndex, valueL)]
        updatedFlags = oldStatus
    in (updatedRegisters, updatedFlags, 0, sp, memory)

mul :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
mul oldStatus registers sp memory op1 op2 =
    let rdIndex = fromIntegral op1
        rrIndex = fromIntegral op2
        rd = registers ! rdIndex
        rr = registers ! rrIndex
        result = (fromIntegral rd :: Word16)*(fromIntegral rr :: Word16)
        resultH = fromIntegral (result `shiftR` 8) :: Word8
        resultL = fromIntegral result :: Word8
        updatedRegisters = registers // [(1,resultH),(0,resultL)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            signFlag = signFlag oldStatus,
            overflowFlag = overflowFlag oldStatus,
            negativeFlag = negativeFlag oldStatus,
            zeroFlag = result == 0,
            carryFlag = testBit resultH 7
        }
    in (updatedRegisters, updatedFlags, 0, sp, memory)

muls :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
muls oldStatus registers sp memory op1 op2 =
    let rdIndex = fromIntegral op1
        rrIndex = fromIntegral op2
        rd = registers ! rdIndex
        rr = registers ! rrIndex

        multiplySigned :: Word8 -> Word8 -> Word16
        multiplySigned _ 0 = 0
        multiplySigned 0 _ = 0
        multiplySigned m1 m2
            | testBit m1 7 && not (testBit m2 7) = negate ((fromIntegral (negate m1) :: Word16) * fromIntegral m2 :: Word16)
            | not (testBit m1 7) && testBit m2 7 = negate ((fromIntegral m1 ::Word16)*(fromIntegral (negate m2) ::Word16))
            | not (testBit m1 7) && not (testBit m2 7) = (fromIntegral m1 ::Word16)*(fromIntegral m2 ::Word16)
            | testBit m1 7 && testBit m2 7 = (fromIntegral (negate m1) ::Word16)*(fromIntegral (negate m2) ::Word16)
        result = multiplySigned rd rr
        resultH = fromIntegral (result `shiftR` 8) :: Word8
        resultL = fromIntegral result :: Word8
        updatedRegisters = registers // [(1,resultH),(0,resultL)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            signFlag = signFlag oldStatus,
            overflowFlag = overflowFlag oldStatus,
            negativeFlag = negativeFlag oldStatus,
            zeroFlag = result == 0,
            carryFlag = testBit resultH 7
        }
    in (updatedRegisters, updatedFlags, 0, sp, memory)

neg :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
neg oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        result = negate rd
        updatedRegisters = registers // [(rdIndex, result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = testBit result 3 || not (testBit rd 3),
            overflowFlag = testBit result 7 && not (testBit result 6) && not (testBit result 5) && not (testBit result 4) && not (testBit result 3) && not (testBit result 2) && not (testBit result 1) && not (testBit result 0),
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = testBit result 7 || testBit result 6 || testBit result 5 || testBit result 4 || testBit result 3 || testBit result 2 || testBit result 1 || testBit result 0,
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
        in (updatedRegisters, updatedFlags, 0, sp, memory)

orInstr :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
orInstr oldStatus registers sp memory op1 op2 =
    let rdIndex = fromIntegral op1
        rsIndex = fromIntegral op2
        rd = registers ! rdIndex
        rs = registers ! rsIndex
        result = rd .|. rs
        updateRegisters = registers // [(rdIndex, result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = False,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = carryFlag oldStatus,
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (updateRegisters, updatedFlags, 0, sp, memory)

ori :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, StackPointer, Memory)
ori oldStatus registers sp memory op1 immediate =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        k = immediate
        result = rd .|. k
        updateRegisters = registers // [(rdIndex, result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = False,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = carryFlag oldStatus,
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (updateRegisters, updatedFlags, 0, sp, memory)

pop :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
pop oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        newSp = sp + 1
        poppedValue = memory ! fromIntegral newSp
        updatedRegisters = registers // [(rdIndex, poppedValue)]
    in (updatedRegisters, oldStatus, 0, newSp, memory)

push :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
push oldStatus registers sp memory op1 =
    let rrIndex = fromIntegral op1
        rr = registers ! rrIndex
        updatedMemory = memory // [(fromIntegral sp, rr)]
        newSp = sp - 1
    in (registers, oldStatus, 0, newSp, updatedMemory)

ret ::  StatusFlags -> Registers -> StackPointer -> Memory -> ProgramCounter -> (Registers, StatusFlags, Int, StackPointer, Memory)
ret oldStatus registers sp memory pc =
    let
        newSp = sp + 2
        returnAddressHigh = memory ! fromIntegral newSp
        returnAddressLow = memory ! (fromIntegral newSp - 1)
        returnAddress = (fromIntegral returnAddressHigh :: Word16) `shiftL` 8 .|. (fromIntegral returnAddressLow :: Word16)
        relativeAddress = (fromIntegral returnAddress :: Int) - (fromIntegral pc :: Int)
    in (registers, oldStatus, relativeAddress, newSp, memory)

rol :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
rol oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        msb = testBit rd 7
        result = if carryFlag oldStatus then rd `shiftL` 1 .|. 0x01 else rd `shiftL` 1
        updatedRegisters = registers // [(rdIndex,result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = testBit rd 3,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = msb,
            overflowFlag = xor (negativeFlag updatedFlags) (carryFlag updatedFlags),
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (updatedRegisters, updatedFlags, 0, sp, memory)

ror :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
ror oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        lsb = testBit rd 0
        result = if carryFlag oldStatus then rd `shiftR` 1 .|. 0x80 else rd `shiftR` 1
        updatedRegisters = registers // [(rdIndex,result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            negativeFlag = False,
            zeroFlag = result == 0,
            carryFlag = lsb,
            overflowFlag = xor (negativeFlag updatedFlags) (carryFlag updatedFlags),
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (updatedRegisters, updatedFlags, 0, sp, memory)

sbc :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
sbc oldStatus registers sp memory op1 op2 =
    let
        rdIndex = fromIntegral op1
        rsIndex = fromIntegral op2
        rd = registers ! rdIndex
        rr = registers ! rsIndex
        carry = if carryFlag oldStatus then 1 else 0
        result = rd - rr - carry
        updatedRegisters = registers // [(rdIndex,result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = not (testBit rd 3) && testBit rr 3 || testBit rr 3 && testBit result 3 || testBit result 3 && not (testBit rd 3),
            overflowFlag = testBit rd 7 && not (testBit rr 7) && not (testBit result 7) || not (testBit rd 7) && testBit rr 7 && testBit result 7,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0 && zeroFlag oldStatus,
            carryFlag = not (testBit rd 7) && testBit rr 7 || testBit rr 7 && testBit result 7 || testBit result 7 && not (testBit rd 7),
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (updatedRegisters, updatedFlags, 0, sp, memory)

sbrc :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, StackPointer, Memory)
sbrc oldStatus registers sp memory op1 immediate =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        b = fromIntegral immediate
        shouldJump = not (testBit rd b)
        relativeJump = if shouldJump then 1 else 0
    in (registers, oldStatus, relativeJump, sp, memory)

sbrs :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, StackPointer, Memory)
sbrs oldStatus registers sp memory op1 immediate =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        b = fromIntegral immediate
        shouldJump = testBit rd b
        relativeJump = if shouldJump then 1 else 0
    in (registers, oldStatus, relativeJump, sp, memory)

sec :: StatusFlags -> Registers -> StackPointer -> Memory -> (Registers, StatusFlags, Int, StackPointer, Memory)
sec oldStatus registers sp memory =
    let updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = overflowFlag oldStatus,
            negativeFlag = negativeFlag oldStatus,
            zeroFlag = zeroFlag oldStatus,
            carryFlag = True,
            signFlag = signFlag oldStatus
    }
    in (registers, updatedFlags, 0, sp, memory)

seh :: StatusFlags -> Registers -> StackPointer -> Memory -> (Registers, StatusFlags, Int, StackPointer, Memory)
seh oldStatus registers sp memory =
    let updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = True,
            overflowFlag = overflowFlag oldStatus,
            negativeFlag = negativeFlag oldStatus,
            zeroFlag = zeroFlag oldStatus,
            carryFlag = carryFlag oldStatus,
            signFlag = signFlag oldStatus
    }
    in (registers, updatedFlags, 0, sp, memory)

sei :: StatusFlags -> Registers -> StackPointer -> Memory -> (Registers, StatusFlags, Int, StackPointer, Memory)
sei oldStatus registers sp memory =
    let updatedFlags = StatusFlags {
            interruptFlag = True,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = overflowFlag oldStatus,
            negativeFlag = negativeFlag oldStatus,
            zeroFlag = zeroFlag oldStatus,
            carryFlag = carryFlag oldStatus,
            signFlag = signFlag oldStatus
    }
    in (registers, updatedFlags, 0, sp, memory)

sen :: StatusFlags -> Registers -> StackPointer -> Memory -> (Registers, StatusFlags, Int, StackPointer, Memory)
sen oldStatus registers sp memory =
    let updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = overflowFlag oldStatus,
            negativeFlag = True,
            zeroFlag = zeroFlag oldStatus,
            carryFlag = carryFlag oldStatus,
            signFlag = signFlag oldStatus
    }
    in (registers, updatedFlags, 0, sp, memory)

ser :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
ser oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        result = 255
        updatedRegisters = registers // [(rdIndex, result)]
    in (updatedRegisters, oldStatus, 0, sp, memory)

ses :: StatusFlags -> Registers -> StackPointer -> Memory -> (Registers, StatusFlags, Int, StackPointer, Memory)
ses oldStatus registers sp memory =
    let updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = overflowFlag oldStatus,
            negativeFlag = negativeFlag oldStatus,
            zeroFlag = zeroFlag oldStatus,
            carryFlag = carryFlag oldStatus,
            signFlag = True
    }
    in (registers, updatedFlags, 0, sp, memory)

set :: StatusFlags -> Registers -> StackPointer -> Memory -> (Registers, StatusFlags, Int, StackPointer, Memory)
set oldStatus registers sp memory =
    let updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = True,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = overflowFlag oldStatus,
            negativeFlag = negativeFlag oldStatus,
            zeroFlag = zeroFlag oldStatus,
            carryFlag = carryFlag oldStatus,
            signFlag = signFlag oldStatus
    }
    in (registers, updatedFlags, 0, sp, memory)

sev :: StatusFlags -> Registers -> StackPointer -> Memory -> (Registers, StatusFlags, Int, StackPointer, Memory)
sev oldStatus registers sp memory =
    let updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = True,
            negativeFlag = negativeFlag oldStatus,
            zeroFlag = zeroFlag oldStatus,
            carryFlag = carryFlag oldStatus,
            signFlag = signFlag oldStatus
    }
    in (registers, updatedFlags, 0, sp, memory)

sez :: StatusFlags -> Registers -> StackPointer -> Memory -> (Registers, StatusFlags, Int, StackPointer, Memory)
sez oldStatus registers sp memory =
    let updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = overflowFlag oldStatus,
            negativeFlag = negativeFlag oldStatus,
            zeroFlag = True,
            carryFlag = carryFlag oldStatus,
            signFlag = signFlag oldStatus
    }
    in (registers, updatedFlags, 0, sp, memory)

st :: StatusFlags -> Registers -> StackPointer -> Memory -> String -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
st oldStatus registers sp memory "X" op2 =
    let rrIndex = fromIntegral op2
        rr = registers ! rrIndex
        address16b = (fromIntegral (registers ! 27) :: Word16) `shiftL` 8 + (fromIntegral (registers ! 26) :: Word16)
        updatedMemory = memory // [(fromIntegral address16b, rr)]
    in (registers, oldStatus, 0, sp, updatedMemory)

st oldStatus registers sp memory "X+" op2 =
    let rrIndex = fromIntegral op2
        rr = registers ! rrIndex
        address16b = (fromIntegral (registers ! 27) :: Word16) `shiftL` 8 + (fromIntegral (registers ! 26) :: Word16)
        updatedMemory = memory // [(fromIntegral address16b, rr)]
        newXRegister = address16b + 1
        xHigh = fromIntegral (newXRegister `shiftR` 8) :: Word8
        xLow = fromIntegral newXRegister :: Word8
        updatedRegisters = registers // [(27, xHigh), (26, xLow)]
    in (updatedRegisters, oldStatus, 0, sp, updatedMemory)

st oldStatus registers sp memory "-X" op2 =
    let rrIndex = fromIntegral op2
        rr = registers ! rrIndex
        address16b = (fromIntegral (registers ! 27) :: Word16) `shiftL` 8 + (fromIntegral (registers ! 26) :: Word16)
        newXRegister = address16b - 1
        updatedMemory = memory // [(fromIntegral newXRegister, rr)]
        xHigh = fromIntegral (newXRegister `shiftR` 8) :: Word8
        xLow = fromIntegral newXRegister :: Word8
        updatedRegisters = registers // [(27, xHigh), (26, xLow)]
    in (updatedRegisters, oldStatus, 0, sp, updatedMemory)

sts :: StatusFlags -> Registers -> StackPointer -> Memory -> Word16 -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
sts oldStatus registers sp memory immediate op2 =
    let rrIndex = fromIntegral op2
        rr = registers ! rrIndex
        k = fromIntegral immediate
        updatedMemory = memory // [(k, rr)]
    in (registers, oldStatus, 0, sp, updatedMemory)

sub :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
sub oldStatus registers sp memory op1 op2 =
    let
        rdIndex = fromIntegral op1
        rsIndex = fromIntegral op2
        rd = registers ! rdIndex
        rr = registers ! rsIndex
        result = rd - rr
        updatedRegisters = registers // [(rdIndex,result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = not (testBit rd 3) && testBit rr 3 || testBit rr 3 && testBit result 3 || testBit result 3 && not (testBit rd 3),
            overflowFlag = testBit rd 7 && not (testBit rr 7) && not (testBit result 7) || not (testBit rd 7) && testBit rr 7 && testBit result 7,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = not (testBit rd 7) && testBit rr 7 || testBit rr 7 && testBit result 7 || testBit result 7 && not (testBit rd 7),
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (updatedRegisters, updatedFlags, 0, sp, memory)

subi :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, StackPointer, Memory)
subi oldStatus registers sp memory op1 immediate =
    let
        rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        k = immediate
        result = rd - k
        updatedRegisters = registers // [(rdIndex,result)]
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = not (testBit rd 3) && testBit k 3 || testBit k 3 && testBit result 3 || testBit result 3 && not (testBit rd 3),
            overflowFlag = testBit rd 7 && not (testBit k 7) && not (testBit result 7) || not (testBit rd 7) && testBit k 7 && testBit result 7,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = not (testBit rd 7) && testBit k 7 || testBit k 7 && testBit result 7 || testBit result 7 && not (testBit rd 7),
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (updatedRegisters, updatedFlags, 0, sp, memory)

swap :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
swap oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        result = rd `shiftR` 4 .|. rd `shiftL` 4
        updatedRegisters = registers // [(rdIndex, result)]
        in (updatedRegisters, oldStatus, 0, sp, memory)

tst :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
tst oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = False,
            negativeFlag = testBit rd 7,
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags),
            zeroFlag = rd == 0,
            carryFlag = carryFlag oldStatus
        }
        in (registers, updatedFlags, 0, sp, memory)

-- /////////////////////////////////////////////////////
-- End instruction implementations 
-- /////////////////////////////////////////////////////

{- | Decodes the current instruction and calls its respective function with the current emulator state,
    then records the updated emulator state for the next instruction. For instructions that may jump, it also
    sends the relative address to the current PC to jump to, if needed.
-}
executeInstruction :: Instruction -> EmulatorState -> EmulatorState
executeInstruction instruction state =
    let (updatedRegisters, updatedFlags, relativeJump, updatedSp, updatedMemory) = case instruction of
            ADC rd rs -> adc (flags state) (registers state) (sp state) (memory state) rd rs
            ADD rd rs -> add (flags state) (registers state) (sp state) (memory state) rd rs
            ADIW rdh rdl immediate -> adiw (flags state) (registers state) (sp state) (memory state) rdh rdl immediate
            AND rd rr -> andInstr (flags state) (registers state) (sp state) (memory state) rd rr
            ANDI rd k -> andi (flags state) (registers state) (sp state) (memory state) rd k
            ASR rd -> asr (flags state) (registers state) (sp state) (memory state) rd
            BCLR s -> bclr (flags state) (registers state) (sp state) (memory state) s
            BLD rd b -> bld (flags state) (registers state) (sp state) (memory state) rd b
            BRCCR relativeAddress -> brcc (flags state) (registers state) (sp state) (memory state) relativeAddress
            BRCSR relativeAddress -> brcs (flags state) (registers state) (sp state) (memory state) relativeAddress
            BREQR relativeAddress -> breq (flags state) (registers state) (sp state) (memory state) relativeAddress
            BRGER relativeAddress -> brge (flags state) (registers state) (sp state) (memory state) relativeAddress
            BRHCR relativeAddress -> brhc (flags state) (registers state) (sp state) (memory state) relativeAddress
            BRHSR relativeAddress -> brhs (flags state) (registers state) (sp state) (memory state) relativeAddress
            BRIDR relativeAddress -> brid (flags state) (registers state) (sp state) (memory state) relativeAddress
            BRIER relativeAddress -> brie (flags state) (registers state) (sp state) (memory state) relativeAddress
            BRLOR relativeAddress -> brlo (flags state) (registers state) (sp state) (memory state) relativeAddress
            BRLTR relativeAddress -> brlt (flags state) (registers state) (sp state) (memory state) relativeAddress
            BRMIR relativeAddress -> brmi (flags state) (registers state) (sp state) (memory state) relativeAddress
            BRNER relativeAddress -> brne (flags state) (registers state) (sp state) (memory state) relativeAddress
            BRPLR relativeAddress -> brpl (flags state) (registers state) (sp state) (memory state) relativeAddress
            BRSHR relativeAddress -> brsh (flags state) (registers state) (sp state) (memory state) relativeAddress
            BRTCR relativeAddress -> brtc (flags state) (registers state) (sp state) (memory state) relativeAddress
            BRTSR relativeAddress -> brts (flags state) (registers state) (sp state) (memory state) relativeAddress
            BRVCR relativeAddress -> brvc (flags state) (registers state) (sp state) (memory state) relativeAddress
            BRVSR relativeAddress -> brvs (flags state) (registers state) (sp state) (memory state) relativeAddress
            CALLR relativeAddress -> call (flags state) (registers state) (sp state) (memory state) relativeAddress (programCounter state)
            CBR rd k -> cbr (flags state) (registers state) (sp state) (memory state) rd k
            CLC -> clc (flags state) (registers state) (sp state) (memory state)
            CLH -> clh (flags state) (registers state) (sp state) (memory state)
            CLI -> cli (flags state) (registers state) (sp state) (memory state)
            CLN -> cln (flags state) (registers state) (sp state) (memory state)
            CLR rd -> clr (flags state) (registers state) (sp state) (memory state) rd
            CLS -> cls (flags state) (registers state) (sp state) (memory state)
            CLT -> clt (flags state) (registers state) (sp state) (memory state)
            CLV -> clv (flags state) (registers state) (sp state) (memory state)
            CLZ -> clz (flags state) (registers state) (sp state) (memory state)
            COM rd -> com (flags state) (registers state) (sp state) (memory state) rd
            CP rd rr -> cp (flags state) (registers state) (sp state) (memory state) rd rr
            CPC rd rr -> cpc (flags state) (registers state) (sp state) (memory state) rd rr
            CPI rd k -> cpi (flags state) (registers state) (sp state) (memory state) rd k
            CPSE rd rr -> cpse (flags state) (registers state) (sp state) (memory state) rd rr
            DEC rd -> dec (flags state) (registers state) (sp state) (memory state) rd
            EOR rd rr -> eor (flags state) (registers state) (sp state) (memory state) rd rr
            INC rd -> inc (flags state) (registers state) (sp state) (memory state) rd
            JMPR relativeAddress -> jmp (flags state) (registers state) (sp state) (memory state) relativeAddress
            LD rd xregister -> ld (flags state) (registers state) (sp state) (memory state) rd xregister
            LABEL label -> (registers state, flags state, 0, sp state, memory state)
            LDI rd immediate -> ldi (flags state) (registers state) (sp state) (memory state) rd immediate
            LDS rd k -> lds (flags state) (registers state) (sp state) (memory state) rd k
            LSL rd -> lsl (flags state) (registers state) (sp state) (memory state) rd
            LSR rd -> lsr (flags state) (registers state) (sp state) (memory state) rd
            MOV rd rs -> mov (flags state) (registers state) (sp state) (memory state) rd rs
            MOVW rdh rdl rrh rrl -> movw (flags state) (registers state) (sp state) (memory state) rdh rdl rrh rrl
            MUL rd rs -> mul (flags state) (registers state) (sp state) (memory state) rd rs
            MULS rd rs -> muls (flags state) (registers state) (sp state) (memory state) rd rs
            NEG rd -> neg (flags state) (registers state) (sp state) (memory state) rd
            NOP -> (registers state, flags state, 0, sp state, memory state)
            OR rd rr -> orInstr (flags state) (registers state) (sp state) (memory state) rd rr
            ORI rd k -> ori (flags state) (registers state) (sp state) (memory state) rd k
            POP rd -> pop (flags state) (registers state) (sp state) (memory state) rd
            PUSH rr -> push (flags state) (registers state) (sp state) (memory state) rr
            RET -> ret (flags state) (registers state) (sp state) (memory state) (programCounter state)
            ROL rd -> rol (flags state) (registers state) (sp state) (memory state) rd
            ROR rd -> ror (flags state) (registers state) (sp state) (memory state) rd
            SBC rd rr -> sbc (flags state) (registers state) (sp state) (memory state) rd rr
            SBRC rd b -> sbrc (flags state) (registers state) (sp state) (memory state) rd b
            SBRS rd b -> sbrs (flags state) (registers state) (sp state) (memory state) rd b
            SEC -> sec (flags state) (registers state) (sp state) (memory state)
            SEH -> seh (flags state) (registers state) (sp state) (memory state)
            SEI -> sei (flags state) (registers state) (sp state) (memory state)
            SEN -> sen (flags state) (registers state) (sp state) (memory state)
            SER rd -> ser (flags state) (registers state) (sp state) (memory state) rd
            SES -> ses (flags state) (registers state) (sp state) (memory state)
            SET -> set (flags state) (registers state) (sp state) (memory state)
            SEV -> sev (flags state) (registers state) (sp state) (memory state)
            SEZ -> sez (flags state) (registers state) (sp state) (memory state)
            ST xregister rr -> st (flags state) (registers state) (sp state) (memory state) xregister rr
            STS k rr -> sts (flags state) (registers state) (sp state) (memory state) k rr
            SUB rd rr -> sub (flags state) (registers state) (sp state) (memory state) rd rr
            SUBI rd k -> subi (flags state) (registers state) (sp state) (memory state) rd k
            SWAP rd -> swap (flags state) (registers state) (sp state) (memory state) rd
            TST rd -> tst (flags state) (registers state) (sp state) (memory state) rd
    in state {
        registers = updatedRegisters,
        flags = updatedFlags,
        -- This is where the PC is either set to PC + 1 if there was no jump, or to PC + relAddress + 1 if the instruction jumped.
        -- This is why most instructions return a 0 as a relative address, and this is also why, when resolving labels, a label that's
        -- 20 instructions behind is actually getting resolved to a relative address of -21, because of this '+ 1' that guarantees that if
        -- the instruction doesn't jump, we continue normally to the next one.
        programCounter = programCounter state + fromIntegral relativeJump + 1,
        sp = updatedSp,
        memory = updatedMemory
    }

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

{- | After resolving the labels, this is the heart of the emulator. It fetches the instruction from memory with the PC,
    loads the state from the previous instruction and call executeInstruction. After the execution is done, it calls itself
    again with either PC + 1 (if it was a normal instruction or a branch that didn't happen) or PC + relAddress (if it was 
    a branch, jump or call). This is the 'fetch -> decode -> execute' cycle in a processor.

    If there are no more instructions in the list (PC has surpassed the upper bound of the instruction list), execution is considered
    done and the function returns the most recent EmulatorState.
-}
runProgram :: [Instruction] -> EmulatorState -> EmulatorState
runProgram initialInstructions = go -- Call recursive helper function go
  where
    go state =
      let pc = fromIntegral (programCounter state)
      in if pc >= length initialInstructions -- If PC bigger than the list, it means we got to the end of execution
         then state -- So return the state
         else
           let currentInstruction = initialInstructions !! pc -- Fetch next instruction to be executed from where the PC points to
               newState = executeInstruction currentInstruction state -- Decode and execute it, then get the updated emulator state 
           in go newState -- Call recursively with the new state

-- | Gets the list of instructions from the parser and the size of the SRAM as configured by the user.
-- @returns the final emulator state after finishing execution.
run :: [Instruction] -> Int -> EmulatorState
run instructions memorySize =
    let initialState = EmulatorState {
        registers = listArray (0,31) (replicate 32 0),                        -- Initialize all registers to 0
        flags = StatusFlags False False False False False False False False,  -- Initialize all status flags to False
        programCounter = 0,                                                   -- Program counter starts executing from 0x0000
        memory = listArray (0, memorySize - 1) (replicate memorySize 0),      -- Initialize the memory with the requested size, set to 0
        sp = fromIntegral (memorySize - 1) :: Word16                          -- The stack pointer should point to the last memory address
        }
        instructionsWithAddresses = catMaybes $ replaceLabels instructions -- Resolve labels and filter out Nothings from the list
    in
        runProgram instructionsWithAddresses initialState -- Start the 'fetch -> decode -> execute' cycle by calling this function with the initial state
