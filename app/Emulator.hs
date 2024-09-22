module Emulator(run, Instruction(..), Register, Registers, EmulatorState, StatusFlags, add, ldi, mov, flags, registers,
showRegisters, showStatusFlags, programCounter, replaceLabels) where

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
    programCounter :: ProgramCounter
} deriving (Show)

type Register = Word8
type Label = String
type ProgramCounter = Word16
type Registers = Array Int Register

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
    | LABEL Label
    | LDI Register Word8     -- Load immediate value into a register
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
    | SUB Register Register
    | SUBI Register Word8
    deriving (Show)

adc :: StatusFlags -> Registers -> Register -> Register -> (Registers, StatusFlags, Int)
adc oldStatus registers rd rs =
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
    in (updatedRegisters, updatedFlags, 0)

add :: StatusFlags -> Registers -> Register -> Register -> (Registers, StatusFlags, Int)
add oldStatus registers rd rs =
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
    in (updatedRegisters, updatedFlags, 0)

andInstr :: StatusFlags -> Registers -> Register -> Register -> (Registers, StatusFlags, Int)
andInstr oldStatus registers op1 op2 =
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
    in (updatedRegisters, updatedFlags, 0)

andi :: StatusFlags -> Registers -> Register -> Word8 -> (Registers, StatusFlags, Int)
andi oldStatus registers op1 immediate =
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
    in (updatedRegisters, updatedFlags, 0)

breq :: StatusFlags -> Registers -> Int -> (Registers, StatusFlags, Int)
breq oldStatus registers relAddress =
    let shouldJump = zeroFlag oldStatus
        jumpAddress = if shouldJump then relAddress else 0
    in (registers, oldStatus, jumpAddress)

brne :: StatusFlags -> Registers -> Int -> (Registers, StatusFlags, Int)
brne oldStatus registers relAddress =
    let shouldJump = zeroFlag oldStatus
        jumpAddress = if shouldJump then 0 else relAddress
    in (registers, oldStatus, jumpAddress)

cp :: StatusFlags -> Registers -> Register -> Register -> (Registers, StatusFlags, Int)
cp oldStatus registers op1 op2 =
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
    in (registers, updatedFlags, 0)

cpc :: StatusFlags -> Registers -> Register -> Register -> (Registers, StatusFlags, Int)
cpc oldStatus registers op1 op2 =
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
    in (registers, updatedFlags, 0)

cpi :: StatusFlags -> Registers -> Register -> Word8 -> (Registers, StatusFlags, Int)
cpi oldStatus registers op1 immediate =
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
    in (registers, updatedFlags, 0)

cpse :: StatusFlags -> Registers -> Register -> Register -> (Registers, StatusFlags, Int)
cpse oldStatus registers op1 op2 =
    let rdIndex = fromIntegral op1
        rrIndex = fromIntegral op2
        rd = registers ! rdIndex
        rr = registers ! rrIndex
        shouldJump = rd - rr == 0
        relativeJump = if shouldJump then 1 else 0
    in (registers, oldStatus, relativeJump)

dec :: StatusFlags -> Registers -> Register -> (Registers, StatusFlags, Int)
dec oldStatus registers op1 =
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
    in (updatedRegisters, updatedFlags, 0)

eor :: StatusFlags -> Registers -> Register -> Register -> (Registers, StatusFlags, Int)
eor oldStatus registers op1 op2 =
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
    in (updatedRegisters, updatedFlags, 0)

inc :: StatusFlags -> Registers -> Register -> (Registers, StatusFlags, Int)
inc oldStatus registers op1 =
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
    in (updatedRegisters, updatedFlags, 0)

jmp :: StatusFlags -> Registers -> Int -> (Registers, StatusFlags, Int)
jmp oldStatus registers relAddress = (registers, oldStatus, relAddress)

ldi :: StatusFlags -> Registers -> Register -> Word8 -> (Registers, StatusFlags, Int)
ldi oldStatus registers rd immediate = 
    let rdIndex = fromIntegral rd
        updatedRegisters = registers // [(rdIndex,immediate)]
        updatedFlags = oldStatus
    in (updatedRegisters, updatedFlags, 0)

lsl :: StatusFlags -> Registers -> Register -> (Registers, StatusFlags, Int)
lsl oldStatus registers op1 = 
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
    in (updatedRegisters, updatedFlags, 0)

lsr :: StatusFlags -> Registers -> Register -> (Registers, StatusFlags, Int)
lsr oldStatus registers op1 = 
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
    in (updatedRegisters, updatedFlags, 0)

mov :: StatusFlags -> Registers -> Register -> Register -> (Registers, StatusFlags, Int)
mov oldStatus registers rd rs =
    let rdIndex = fromIntegral rd
        rsIndex = fromIntegral rs
        value = registers ! rsIndex
        updatedRegisters = registers // [(rdIndex,value)]
        updatedFlags = oldStatus
    in (updatedRegisters, updatedFlags, 0)

mul :: StatusFlags -> Registers -> Register -> Register -> (Registers, StatusFlags, Int)
mul oldStatus registers op1 op2 =
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
    in (updatedRegisters, updatedFlags, 0)

muls :: StatusFlags -> Registers -> Register -> Register -> (Registers, StatusFlags, Int)
muls oldStatus registers op1 op2 =
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
    in (updatedRegisters, updatedFlags, 0)


orInstr :: StatusFlags -> Registers -> Register -> Register -> (Registers, StatusFlags, Int)
orInstr oldStatus registers op1 op2 =
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
    in (updateRegisters, updatedFlags, 0)

ori :: StatusFlags -> Registers -> Register -> Word8 -> (Registers, StatusFlags, Int)
ori oldStatus registers op1 immediate =
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
    in (updateRegisters, updatedFlags, 0)

sbrc :: StatusFlags -> Registers -> Register -> Word8 -> (Registers, StatusFlags, Int)
sbrc oldStatus registers op1 immediate =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        b = fromIntegral immediate
        shouldJump = not(testBit rd b)
        relativeJump = if shouldJump then 1 else 0
    in (registers, oldStatus, relativeJump)

sbrs :: StatusFlags -> Registers -> Register -> Word8 -> (Registers, StatusFlags, Int)
sbrs oldStatus registers op1 immediate =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        b = fromIntegral immediate
        shouldJump = testBit rd b
        relativeJump = if shouldJump then 1 else 0
    in (registers, oldStatus, relativeJump)

sub :: StatusFlags -> Registers -> Register -> Register -> (Registers, StatusFlags, Int)
sub oldStatus registers op1 op2 =
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
    in (updatedRegisters, updatedFlags, 0)

subi :: StatusFlags -> Registers -> Register -> Word8 -> (Registers, StatusFlags, Int)
subi oldStatus registers op1 immediate =
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
    in (updatedRegisters, updatedFlags, 0)

-- Helper function to replace an element at a specific index in a list
replaceAtIndex :: [a] -> Int -> a -> [a]
replaceAtIndex xs i newValue = take i xs ++ [newValue] ++ drop (i + 1) xs

executeInstruction :: Instruction -> EmulatorState -> EmulatorState
executeInstruction instruction state = 
    let (updatedRegisters, updatedFlags, relativeJump) = case instruction of
            ADC rd rs -> adc (flags state) (registers state) rd rs
            ADD rd rs -> add (flags state) (registers state) rd rs
            ADIW rdh rdl immediate -> (registers state, flags state, 0)
            AND rd rr -> andInstr (flags state) (registers state) rd rr
            ANDI rd k -> andi (flags state) (registers state) rd k
            BREQR relativeAddress -> breq (flags state) (registers state) relativeAddress
            BRNER relativeAddress -> brne (flags state) (registers state) relativeAddress
            CP rd rr -> cp (flags state) (registers state) rd rr
            CPC rd rr -> cpc (flags state) (registers state) rd rr
            CPI rd k -> cpi (flags state) (registers state) rd k
            CPSE rd rr -> cpse (flags state) (registers state) rd rr
            DEC rd -> dec (flags state) (registers state) rd
            EOR rd rr -> eor (flags state) (registers state) rd rr
            INC rd -> inc (flags state) (registers state) rd
            JMPR relativeAddress -> jmp (flags state) (registers state) relativeAddress
            LABEL label -> (registers state, flags state, 0)
            LDI rd immediate -> ldi (flags state) (registers state) rd immediate
            LSL rd -> lsl (flags state) (registers state) rd
            LSR rd -> lsr (flags state) (registers state) rd
            MOV rd rs -> mov (flags state) (registers state) rd rs
            MUL rd rs -> mul (flags state) (registers state) rd rs
            MULS rd rs -> muls (flags state) (registers state) rd rs
            NOP -> (registers state, flags state, 0)
            OR rd rr -> orInstr (flags state) (registers state) rd rr
            ORI rd k -> ori (flags state) (registers state) rd k
            SBRC rd b -> sbrc (flags state) (registers state) rd b
            SBRS rd b -> sbrs (flags state) (registers state) rd b
            SUB rd rr -> sub (flags state) (registers state) rd rr
            SUBI rd k -> subi (flags state) (registers state) rd k

    in state {
        registers = updatedRegisters,
        flags = updatedFlags,
        programCounter = programCounter state + fromIntegral relativeJump + 1}

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
        programCounter = 0
        }
        instructionsWithAddresses = catMaybes $ replaceLabels instructions
    in
        runProgram instructionsWithAddresses initialState
