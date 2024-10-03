module Emulator(run, Instruction(..), Register, Registers, EmulatorState, StatusFlags, add, ldi, mov, flags, registers,
showRegisters, showStatusFlags, programCounter, replaceLabels, memory) where

import Data.Binary (Word8, Word16)
import Data.Bits
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Array

data EmulatorState = EmulatorState {
    registers :: Registers,
    flags :: StatusFlags,
    programCounter :: ProgramCounter,
    sp :: StackPointer,
    memory :: Memory
} deriving (Show)

type Register = Word8
type Label = String
type ProgramCounter = Word16
type Registers = Array Int Register
type Memory = Array Int Word8
type StackPointer = Word16

showRegisters :: Registers -> String
showRegisters regs =
    let
        registers = assocs regs
        go::[(Int, Register)] -> String
        go regs = case regs of
            [] -> ""
            ((i, r):rs) -> "R" ++ show i ++ ": 0x" ++ showHex r "" ++ " (" ++ showIntAtBase 2 intToDigit r "" ++ ")" ++ "\n" ++ go rs
    in
        go registers

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
    
data StatusFlags = StatusFlags {
    interruptFlag :: Bool,  -- I flag
    tFlag :: Bool,          -- T flag
    halfCarryFlag :: Bool,  -- H flag
    signFlag :: Bool,       -- S flag
    overflowFlag :: Bool,   -- V flag
    negativeFlag :: Bool,   -- N flag
    zeroFlag :: Bool,       -- Z flag
    carryFlag :: Bool       -- C flag
} deriving (Show)

data Instruction
    = ADC Register Register
    | ADD Register Register  -- Add two registers
    | ADIW Register Register Word8
    | AND Register Register
    | ANDI Register Word8
    | BREQ Label
    | BREQR Int
    | BRLO Label
    | BRLOR Int
    | BRNE Label
    | BRNER Int
    | BRMI Label
    | BRMIR Int
    | CALL Label
    | CALLR Int
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
    | LDI Register Word8     -- Load immediate value into a register
    | LDS Register Word16
    | LSL Register
    | LSR Register
    | MOV Register Register  -- Move value between registers
    | MUL Register Register
    | MULS Register Register
    | NOP
    | OR Register Register
    | ORI Register Word8
    | POP Register
    | PUSH Register
    | RET
    | SBRC Register Word8
    | SBRS Register Word8
    | ST String Register
    | STS Word16 Register
    | SUB Register Register
    | SUBI Register Word8
    deriving (Show)

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
            halfCarryFlag = testBit op1 3 && testBit op2 3 || testBit op1 3 && not(testBit result 3) || not(testBit result 3) && testBit op1 3,
            overflowFlag = testBit op1 7 && testBit op2 7 && not(testBit result 7) || not(testBit op1 7) && not(testBit op2 7) && testBit result 7,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = testBit op1 7 && testBit op2 7 || testBit op1 7 && not(testBit result 7) || not(testBit result 7) && testBit op1 7,
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
            halfCarryFlag = testBit op1 3 && testBit op2 3 || testBit op1 3 && not(testBit result 3) || not(testBit result 3) && testBit op1 3,
            overflowFlag = testBit op1 7 && testBit op2 7 && not(testBit result 7) || not(testBit op1 7) && not(testBit op2 7) && testBit result 7,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = testBit op1 7 && testBit op2 7 || testBit op1 7 && not(testBit result 7) || not(testBit result 7) && testBit op1 7,
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

breq :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
breq oldStatus registers sp memory relAddress =
    let shouldJump = zeroFlag oldStatus
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, sp, memory)

brlo :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
brlo oldStatus registers sp memory relAddress =
    let shouldJump = carryFlag oldStatus
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

call :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> Word16 -> (Registers, StatusFlags, Int, StackPointer, Memory)
call oldStatus registers sp memory relAddress returnAddress =
    let (high, low) = (fromIntegral (returnAddress `shiftR` 8), fromIntegral (returnAddress .&. 0xFF))
        updatedMemory = memory // [(fromIntegral sp, high),(fromIntegral (sp - 1), low)]
        newSp = sp - 2
        in (registers, oldStatus, relAddress, newSp, updatedMemory)

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
            halfCarryFlag = not(testBit rd 3) && testBit rr 3 || testBit rr 3 && testBit result 3 || testBit result 3 && not(testBit rd 3),
            overflowFlag = testBit rd 7 && not (testBit rr 7) && not(testBit result 7) || not(testBit rd 7) && testBit rr 7 && testBit result 7,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = not (testBit rd 7) && testBit rr 7 || testBit rr 7 && testBit result 7 || testBit result 7 && not(testBit rd 7),
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
            halfCarryFlag = not(testBit rd 3) && testBit rr 3 || testBit rr 3 && testBit result 3 || testBit result 3 && not(testBit rd 3),
            overflowFlag = testBit rd 7 && not (testBit rr 7) && not(testBit result 7) || not(testBit rd 7) && testBit rr 7 && testBit result 7,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0 && zeroFlag oldStatus,
            carryFlag = not (testBit rd 7) && testBit rr 7 || testBit rr 7 && testBit result 7 || testBit result 7 && not(testBit rd 7),
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
            halfCarryFlag = not(testBit rd 3) && testBit k 3 || testBit k 3 && testBit result 3 || testBit result 3 && not(testBit rd 3),
            overflowFlag = testBit rd 7 && not (testBit k 7) && not(testBit result 7) || not(testBit rd 7) && testBit k 7 && testBit result 7,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = not (testBit rd 7) && testBit k 7 || testBit k 7 && testBit result 7 || testBit result 7 && not(testBit rd 7),
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
        address16b = ((fromIntegral (registers ! 27) :: Word16) `shiftL` 8) + (fromIntegral (registers ! 26) :: Word16)
        updatedRegisters = registers // [(rdIndex, memory ! (fromIntegral address16b))]
    in (updatedRegisters, oldStatus, 0, sp, memory)

ld oldStatus registers sp memory op1 "X+" =
    let rdIndex = fromIntegral op1
        address16b = ((fromIntegral (registers ! 27) :: Word16) `shiftL` 8) + (fromIntegral (registers ! 26) :: Word16)
        newXRegister = address16b + 1
        xHigh = fromIntegral (newXRegister `shiftR` 8) :: Word8
        xLow = fromIntegral newXRegister :: Word8
        updatedRegisters = registers // [(rdIndex, memory ! (fromIntegral address16b)), (27, xHigh), (26, xLow)]
    in (updatedRegisters, oldStatus, 0, sp, memory)

ld oldStatus registers sp memory op1 "-X" =
    let rdIndex = fromIntegral op1
        address16b = ((fromIntegral (registers ! 27) :: Word16) `shiftL` 8) + (fromIntegral (registers ! 26) :: Word16)
        newXRegister = address16b - 1
        xHigh = fromIntegral (newXRegister `shiftR` 8) :: Word8
        xLow = fromIntegral newXRegister :: Word8
        updatedRegisters = registers // [(rdIndex, memory ! (fromIntegral newXRegister)), (27, xHigh), (26, xLow)]
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
            | testBit m1 7 && not(testBit m2 7) = negate ((fromIntegral (negate m1) :: Word16) * fromIntegral m2 :: Word16)
            | not(testBit m1 7) && testBit m2 7 = negate ((fromIntegral m1 ::Word16)*(fromIntegral (negate m2) ::Word16))
            | not(testBit m1 7) && not(testBit m2 7) = (fromIntegral m1 ::Word16)*(fromIntegral m2 ::Word16)
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
        poppedValue = memory ! (fromIntegral newSp)
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
        returnAddress = ((memory ! (fromIntegral newSp)) `shiftL` 8) .|. (memory ! ((fromIntegral newSp) - 1))
        relativeAddress = (fromIntegral returnAddress :: Int) - (fromIntegral pc :: Int)
    in (registers, oldStatus, relativeAddress, newSp, memory)


sbrc :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, StackPointer, Memory)
sbrc oldStatus registers sp memory op1 immediate =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        b = fromIntegral immediate
        shouldJump = not(testBit rd b)
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

st :: StatusFlags -> Registers -> StackPointer -> Memory -> String -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
st oldStatus registers sp memory "X" op2 =
    let rrIndex = fromIntegral op2
        rr = registers ! rrIndex
        address16b = ((fromIntegral (registers ! 27) :: Word16) `shiftL` 8) + (fromIntegral (registers ! 26) :: Word16)
        updatedMemory = memory // [(fromIntegral address16b, rr)]
    in (registers, oldStatus, 0, sp, updatedMemory)

st oldStatus registers sp memory "X+" op2 =
    let rrIndex = fromIntegral op2
        rr = registers ! rrIndex
        address16b = ((fromIntegral (registers ! 27) :: Word16) `shiftL` 8) + (fromIntegral (registers ! 26) :: Word16)
        updatedMemory = memory // [(fromIntegral address16b, rr)]
        newXRegister = address16b + 1
        xHigh = fromIntegral (newXRegister `shiftR` 8) :: Word8
        xLow = fromIntegral newXRegister :: Word8
        updatedRegisters = registers // [(27, xHigh), (26, xLow)]
    in (updatedRegisters, oldStatus, 0, sp, updatedMemory)

st oldStatus registers sp memory "-X" op2 =
    let rrIndex = fromIntegral op2
        rr = registers ! rrIndex
        address16b = ((fromIntegral (registers ! 27) :: Word16) `shiftL` 8) + (fromIntegral (registers ! 26) :: Word16)
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
            halfCarryFlag = not (testBit rd 3) && testBit rr 3 || testBit rr 3 && testBit result 3 || testBit result 3 && not(testBit rd 3),
            overflowFlag = testBit rd 7 && not (testBit rr 7) && not(testBit result 7) || not(testBit rd 7) && testBit rr 7 && testBit result 7,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = not (testBit rd 7) && testBit rr 7 || testBit rr 7 && testBit result 7 || testBit result 7 && not(testBit rd 7),
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
            halfCarryFlag = not (testBit rd 3) && testBit k 3 || testBit k 3 && testBit result 3 || testBit result 3 && not(testBit rd 3),
            overflowFlag = testBit rd 7 && not (testBit k 7) && not(testBit result 7) || not(testBit rd 7) && testBit k 7 && testBit result 7,
            negativeFlag = testBit result 7,
            zeroFlag = result == 0,
            carryFlag = not (testBit rd 7) && testBit k 7 || testBit k 7 && testBit result 7 || testBit result 7 && not(testBit rd 7),
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
    in (updatedRegisters, updatedFlags, 0, sp, memory)

executeInstruction :: Instruction -> EmulatorState -> EmulatorState
executeInstruction instruction state = 
    let (updatedRegisters, updatedFlags, relativeJump, updatedSp, updatedMemory) = case instruction of
            ADC rd rs -> adc (flags state) (registers state) (sp state) (memory state) rd rs
            ADD rd rs -> add (flags state) (registers state) (sp state) (memory state) rd rs
            ADIW rdh rdl immediate -> (registers state, flags state, 0, sp state, memory state)
            AND rd rr -> andInstr (flags state) (registers state) (sp state) (memory state) rd rr
            ANDI rd k -> andi (flags state) (registers state) (sp state) (memory state) rd k
            BREQR relativeAddress -> breq (flags state) (registers state) (sp state) (memory state) relativeAddress
            BRLOR relativeAddress -> brlo (flags state) (registers state) (sp state) (memory state) relativeAddress
            BRMIR relativeAddress -> brmi (flags state) (registers state) (sp state) (memory state) relativeAddress
            BRNER relativeAddress -> brne (flags state) (registers state) (sp state) (memory state) relativeAddress
            CALLR relativeAddress -> call (flags state) (registers state) (sp state) (memory state) relativeAddress (programCounter state)
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
            MUL rd rs -> mul (flags state) (registers state) (sp state) (memory state) rd rs
            MULS rd rs -> muls (flags state) (registers state) (sp state) (memory state) rd rs
            NOP -> (registers state, flags state, 0, sp state, memory state)
            OR rd rr -> orInstr (flags state) (registers state) (sp state) (memory state) rd rr
            ORI rd k -> ori (flags state) (registers state) (sp state) (memory state) rd k
            POP rd -> pop (flags state) (registers state) (sp state) (memory state) rd
            PUSH rr -> push (flags state) (registers state) (sp state) (memory state) rr
            RET -> ret (flags state) (registers state) (sp state) (memory state) (programCounter state)
            SBRC rd b -> sbrc (flags state) (registers state) (sp state) (memory state) rd b
            SBRS rd b -> sbrs (flags state) (registers state) (sp state) (memory state) rd b
            ST xregister rr -> st (flags state) (registers state) (sp state) (memory state) xregister rr
            STS k rr -> sts (flags state) (registers state) (sp state) (memory state) k rr
            SUB rd rr -> sub (flags state) (registers state) (sp state) (memory state) rd rr
            SUBI rd k -> subi (flags state) (registers state) (sp state) (memory state) rd k

    in state {
        registers = updatedRegisters,
        flags = updatedFlags,
        programCounter = programCounter state + fromIntegral relativeJump + 1,
        sp = updatedSp,
        memory = updatedMemory
    }

constructJumpTable :: [Instruction] -> Int -> Map.Map Label Int -> (Map.Map Label Int, Int)
constructJumpTable [] currentAddress labelMap = (labelMap, currentAddress)
constructJumpTable ((LABEL label):rest) currentAddress labelMap = constructJumpTable rest (currentAddress+1) (Map.insert label currentAddress labelMap)
constructJumpTable (_:rest) currentAddress labelMap = constructJumpTable rest (currentAddress + 1) labelMap

resolveLabels :: Map.Map Label Int -> (Int, Instruction) -> Maybe Instruction
resolveLabels labelMap (address, JMP label)
    | relAddress > 0 = Just (JMPR (relAddress - 1))
    | otherwise = Just (JMPR relAddress)
    where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BREQ label)
    | relAddress > 0 = Just (BREQR (relAddress - 1))
    | otherwise = Just (BREQR relAddress)
    where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRLO label)
    | relAddress > 0 = Just (BRLOR (relAddress - 1))
    | otherwise = Just (BRLOR relAddress)
    where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRNE label)
    | relAddress > 0 = Just (BRNER (relAddress - 1))
    | otherwise = Just (BRNER relAddress)
    where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, BRMI label)
   | relAddress > 0 = Just (BRMIR (relAddress -1))
   | otherwise = Just (BRMIR relAddress)
   where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels labelMap (address, CALL label)
   | relAddress > 0 = Just (CALLR (relAddress -1))
   | otherwise = Just (CALLR relAddress)
   where relAddress = Map.findWithDefault 0 label labelMap - address

resolveLabels _labelMap (_, LABEL label) = Just (LABEL label)
resolveLabels _labelMap (address, otherInstr) = Just otherInstr

replaceLabels :: [Instruction] -> [Maybe Instruction]
replaceLabels instructions = 
  let (labelMap, _) = constructJumpTable instructions 0 Map.empty
   in zipWith (curry (resolveLabels labelMap)) [0 .. ] instructions

runProgram :: [Instruction] -> EmulatorState -> EmulatorState
runProgram initialInstructions = go
  where
    go state =
      let pc = fromIntegral (programCounter state)
      in if pc >= length initialInstructions 
         then state
         else 
           let currentInstruction = initialInstructions !! pc
               newState = executeInstruction currentInstruction state
           in go newState 

memorySize = 2000

run :: [Instruction] -> EmulatorState
run instructions =
    let initialState = EmulatorState {
        registers = listArray (0,31) (replicate 32 0),  -- Initialize all registers to 0
        flags = StatusFlags False False False False False False False False,
        programCounter = 0,
        memory = listArray (0, memorySize - 1) (replicate memorySize 0),
        sp = fromIntegral (memorySize - 1) :: Word16
        }
        instructionsWithAddresses = catMaybes $ replaceLabels instructions
    in
        runProgram instructionsWithAddresses initialState
