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
    memory :: Memory
} deriving (Show)

type Register = Word8
type Label = String
type ProgramCounter = Word16
type Registers = Array Int Register
type Memory = Array Int Word8

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
    | SBRC Register Word8
    | SBRS Register Word8
    | ST String Register
    | STS Word16 Register
    | SUB Register Register
    | SUBI Register Word8
    deriving (Show)

adc :: StatusFlags -> Registers -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, Memory)
adc oldStatus registers memory rd rs =
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
    in (updatedRegisters, updatedFlags, 0, memory)

add :: StatusFlags -> Registers -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, Memory)
add oldStatus registers memory rd rs =
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
    in (updatedRegisters, updatedFlags, 0, memory)

andInstr :: StatusFlags -> Registers -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, Memory)
andInstr oldStatus registers memory op1 op2 =
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
    in (updatedRegisters, updatedFlags, 0, memory)

andi :: StatusFlags -> Registers -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, Memory)
andi oldStatus registers memory op1 immediate =
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
    in (updatedRegisters, updatedFlags, 0, memory)

breq :: StatusFlags -> Registers -> Memory -> Int -> (Registers, StatusFlags, Int, Memory)
breq oldStatus registers memory relAddress =
    let shouldJump = zeroFlag oldStatus
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, memory)

brlo :: StatusFlags -> Registers -> Memory -> Int -> (Registers, StatusFlags, Int, Memory)
brlo oldStatus registers memory relAddress =
    let shouldJump = carryFlag oldStatus
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress, memory)

brne :: StatusFlags -> Registers -> Memory -> Int -> (Registers, StatusFlags, Int, Memory)
brne oldStatus registers memory relAddress =
    let shouldJump = zeroFlag oldStatus
        jumpAddress = if shouldJump then 0 else relAddress
    in (registers, oldStatus, jumpAddress, memory)

cp :: StatusFlags -> Registers -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, Memory)
cp oldStatus registers memory op1 op2 =
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
    in (registers, updatedFlags, 0, memory)

cpc :: StatusFlags -> Registers -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, Memory)
cpc oldStatus registers memory op1 op2 =
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
    in (registers, updatedFlags, 0, memory)

cpi :: StatusFlags -> Registers -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, Memory)
cpi oldStatus registers memory op1 immediate =
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
    in (registers, updatedFlags, 0, memory)

cpse :: StatusFlags -> Registers -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, Memory)
cpse oldStatus registers memory op1 op2 =
    let rdIndex = fromIntegral op1
        rrIndex = fromIntegral op2
        rd = registers ! rdIndex
        rr = registers ! rrIndex
        shouldJump = rd - rr == 0
        relativeJump = if shouldJump then 1 else 0
    in (registers, oldStatus, relativeJump, memory)

dec :: StatusFlags -> Registers -> Memory -> Register -> (Registers, StatusFlags, Int, Memory)
dec oldStatus registers memory op1 =
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
    in (updatedRegisters, updatedFlags, 0, memory)

eor :: StatusFlags -> Registers -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, Memory)
eor oldStatus registers memory op1 op2 =
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
    in (updatedRegisters, updatedFlags, 0, memory)

inc :: StatusFlags -> Registers -> Memory -> Register -> (Registers, StatusFlags, Int, Memory)
inc oldStatus registers memory op1 =
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
    in (updatedRegisters, updatedFlags, 0, memory)

jmp :: StatusFlags -> Registers -> Memory -> Int -> (Registers, StatusFlags, Int, Memory)
jmp oldStatus registers memory relAddress = (registers, oldStatus, relAddress, memory)

ld :: StatusFlags -> Registers -> Memory -> Register -> String -> (Registers, StatusFlags, Int, Memory)
ld oldStatus registers memory op1 "X" =
    let rdIndex = fromIntegral op1
        address16b = ((fromIntegral (registers ! 27) :: Word16) `shiftL` 8) + (fromIntegral (registers ! 26) :: Word16)
        updatedRegisters = registers // [(rdIndex, memory ! (fromIntegral address16b))]
    in (updatedRegisters, oldStatus, 0, memory)

ld oldStatus registers memory op1 "X+" =
    let rdIndex = fromIntegral op1
        address16b = ((fromIntegral (registers ! 27) :: Word16) `shiftL` 8) + (fromIntegral (registers ! 26) :: Word16)
        newXRegister = address16b + 1
        xHigh = fromIntegral (newXRegister `shiftR` 8) :: Word8
        xLow = fromIntegral newXRegister :: Word8
        updatedRegisters = registers // [(rdIndex, memory ! (fromIntegral address16b)), (27, xHigh), (26, xLow)]
    in (updatedRegisters, oldStatus, 0, memory)

ld oldStatus registers memory op1 "-X" =
    let rdIndex = fromIntegral op1
        address16b = ((fromIntegral (registers ! 27) :: Word16) `shiftL` 8) + (fromIntegral (registers ! 26) :: Word16)
        newXRegister = address16b - 1
        xHigh = fromIntegral (newXRegister `shiftR` 8) :: Word8
        xLow = fromIntegral newXRegister :: Word8
        updatedRegisters = registers // [(rdIndex, memory ! (fromIntegral newXRegister)), (27, xHigh), (26, xLow)]
    in (updatedRegisters, oldStatus, 0, memory)

ldi :: StatusFlags -> Registers -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, Memory)
ldi oldStatus registers memory rd immediate = 
    let rdIndex = fromIntegral rd
        updatedRegisters = registers // [(rdIndex,immediate)]
        updatedFlags = oldStatus
    in (updatedRegisters, updatedFlags, 0, memory)

lds :: StatusFlags -> Registers -> Memory -> Register -> Word16 -> (Registers, StatusFlags, Int, Memory)
lds oldStatus registers memory op1 immediate =
    let rdIndex = fromIntegral op1
        rd = fromIntegral (registers ! rdIndex)
        k = fromIntegral immediate
        updatedRegisters = registers // [(rdIndex, memory ! k)]
    in (updatedRegisters, oldStatus, 0, memory)

lsl :: StatusFlags -> Registers -> Memory -> Register -> (Registers, StatusFlags, Int, Memory)
lsl oldStatus registers memory op1 = 
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
    in (updatedRegisters, updatedFlags, 0, memory)

lsr :: StatusFlags -> Registers -> Memory -> Register -> (Registers, StatusFlags, Int, Memory)
lsr oldStatus registers memory op1 = 
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
    in (updatedRegisters, updatedFlags, 0, memory)

mov :: StatusFlags -> Registers -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, Memory)
mov oldStatus registers memory rd rs =
    let rdIndex = fromIntegral rd
        rsIndex = fromIntegral rs
        value = registers ! rsIndex
        updatedRegisters = registers // [(rdIndex,value)]
        updatedFlags = oldStatus
    in (updatedRegisters, updatedFlags, 0, memory)

mul :: StatusFlags -> Registers -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, Memory)
mul oldStatus registers memory op1 op2 =
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
    in (updatedRegisters, updatedFlags, 0, memory)

muls :: StatusFlags -> Registers -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, Memory)
muls oldStatus registers memory op1 op2 =
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
    in (updatedRegisters, updatedFlags, 0, memory)


orInstr :: StatusFlags -> Registers -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, Memory)
orInstr oldStatus registers memory op1 op2 =
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
    in (updateRegisters, updatedFlags, 0, memory)

ori :: StatusFlags -> Registers -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, Memory)
ori oldStatus registers memory op1 immediate =
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
    in (updateRegisters, updatedFlags, 0, memory)

sbrc :: StatusFlags -> Registers -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, Memory)
sbrc oldStatus registers memory op1 immediate =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        b = fromIntegral immediate
        shouldJump = not(testBit rd b)
        relativeJump = if shouldJump then 1 else 0
    in (registers, oldStatus, relativeJump, memory)

sbrs :: StatusFlags -> Registers -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, Memory)
sbrs oldStatus registers memory op1 immediate =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        b = fromIntegral immediate
        shouldJump = testBit rd b
        relativeJump = if shouldJump then 1 else 0
    in (registers, oldStatus, relativeJump, memory)

st :: StatusFlags -> Registers -> Memory -> String -> Register -> (Registers, StatusFlags, Int, Memory)
st oldStatus registers memory "X" op2 =
    let rrIndex = fromIntegral op2
        rr = registers ! rrIndex
        address16b = ((fromIntegral (registers ! 27) :: Word16) `shiftL` 8) + (fromIntegral (registers ! 26) :: Word16)
        updatedMemory = memory // [(fromIntegral address16b, rr)]
    in (registers, oldStatus, 0, updatedMemory)

st oldStatus registers memory "X+" op2 =
    let rrIndex = fromIntegral op2
        rr = registers ! rrIndex
        address16b = ((fromIntegral (registers ! 27) :: Word16) `shiftL` 8) + (fromIntegral (registers ! 26) :: Word16)
        updatedMemory = memory // [(fromIntegral address16b, rr)]
        newXRegister = address16b + 1
        xHigh = fromIntegral (newXRegister `shiftR` 8) :: Word8
        xLow = fromIntegral newXRegister :: Word8
        updatedRegisters = registers // [(27, xHigh), (26, xLow)]
    in (updatedRegisters, oldStatus, 0, updatedMemory)

st oldStatus registers memory "-X" op2 =
    let rrIndex = fromIntegral op2
        rr = registers ! rrIndex
        address16b = ((fromIntegral (registers ! 27) :: Word16) `shiftL` 8) + (fromIntegral (registers ! 26) :: Word16)
        newXRegister = address16b - 1
        updatedMemory = memory // [(fromIntegral newXRegister, rr)]
        xHigh = fromIntegral (newXRegister `shiftR` 8) :: Word8
        xLow = fromIntegral newXRegister :: Word8
        updatedRegisters = registers // [(27, xHigh), (26, xLow)]
    in (updatedRegisters, oldStatus, 0, updatedMemory)

sts :: StatusFlags -> Registers -> Memory -> Word16 -> Register -> (Registers, StatusFlags, Int, Memory)
sts oldStatus registers memory immediate op2 =
    let rrIndex = fromIntegral op2
        rr = registers ! rrIndex
        k = fromIntegral immediate
        updatedMemory = memory // [(k, rr)]
    in (registers, oldStatus, 0, updatedMemory)

sub :: StatusFlags -> Registers -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, Memory)
sub oldStatus registers memory op1 op2 =
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
    in (updatedRegisters, updatedFlags, 0, memory)

subi :: StatusFlags -> Registers -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, Memory)
subi oldStatus registers memory op1 immediate =
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
    in (updatedRegisters, updatedFlags, 0, memory)

executeInstruction :: Instruction -> EmulatorState -> EmulatorState
executeInstruction instruction state = 
    let (updatedRegisters, updatedFlags, relativeJump, updatedMemory) = case instruction of
            ADC rd rs -> adc (flags state) (registers state) (memory state) rd rs
            ADD rd rs -> add (flags state) (registers state) (memory state) rd rs
            ADIW rdh rdl immediate -> (registers state, flags state, 0, memory state)
            AND rd rr -> andInstr (flags state) (registers state) (memory state) rd rr
            ANDI rd k -> andi (flags state) (registers state) (memory state) rd k
            BREQR relativeAddress -> breq (flags state) (registers state) (memory state) relativeAddress
            BRLOR relativeAddress -> brlo (flags state) (registers state) (memory state) relativeAddress 
            BRNER relativeAddress -> brne (flags state) (registers state) (memory state) relativeAddress
            CP rd rr -> cp (flags state) (registers state) (memory state) rd rr
            CPC rd rr -> cpc (flags state) (registers state) (memory state) rd rr
            CPI rd k -> cpi (flags state) (registers state) (memory state) rd k
            CPSE rd rr -> cpse (flags state) (registers state) (memory state) rd rr
            DEC rd -> dec (flags state) (registers state) (memory state) rd
            EOR rd rr -> eor (flags state) (registers state) (memory state) rd rr
            INC rd -> inc (flags state) (registers state) (memory state) rd
            JMPR relativeAddress -> jmp (flags state) (registers state) (memory state) relativeAddress
            LD rd xregister -> ld (flags state) (registers state) (memory state) rd xregister
            LABEL label -> (registers state, flags state, 0, memory state)
            LDI rd immediate -> ldi (flags state) (registers state) (memory state) rd immediate
            LDS rd k -> lds (flags state) (registers state) (memory state) rd k
            LSL rd -> lsl (flags state) (registers state) (memory state) rd
            LSR rd -> lsr (flags state) (registers state) (memory state) rd
            MOV rd rs -> mov (flags state) (registers state) (memory state) rd rs
            MUL rd rs -> mul (flags state) (registers state) (memory state) rd rs
            MULS rd rs -> muls (flags state) (registers state) (memory state) rd rs
            NOP -> (registers state, flags state, 0, memory state)
            OR rd rr -> orInstr (flags state) (registers state) (memory state) rd rr
            ORI rd k -> ori (flags state) (registers state) (memory state) rd k
            SBRC rd b -> sbrc (flags state) (registers state) (memory state) rd b
            SBRS rd b -> sbrs (flags state) (registers state) (memory state) rd b
            ST xregister rr -> st (flags state) (registers state) (memory state) xregister rr
            STS k rr -> sts (flags state) (registers state) (memory state) k rr
            SUB rd rr -> sub (flags state) (registers state) (memory state) rd rr
            SUBI rd k -> subi (flags state) (registers state) (memory state) rd k

    in state {
        registers = updatedRegisters,
        flags = updatedFlags,
        programCounter = programCounter state + fromIntegral relativeJump + 1,
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

run :: [Instruction] -> EmulatorState
run instructions =
    let initialState = EmulatorState {
        registers = listArray (0,31) (replicate 32 0),  -- Initialize all registers to 0
        flags = StatusFlags False False False False False False False False,
        programCounter = 0,
        memory = listArray (0,1999) (replicate 2000 0)
        }
        instructionsWithAddresses = catMaybes $ replaceLabels instructions
    in
        runProgram instructionsWithAddresses initialState
