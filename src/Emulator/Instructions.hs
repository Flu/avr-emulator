module Emulator.Instructions where

import Emulator.State

import Control.Monad.ST
import Data.Binary (Word8, Word16)
import Data.Bits

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
    | Comment
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
    | XCH Register
    deriving (Show, Eq)

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

adc :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> Register -> ST s (StatusFlags, Int, StackPointer)
adc registers memory oldFlags sp rd rs = do
    let rdIndex = fromIntegral rd
    let rsIndex = fromIntegral rs
    op1 <- getRegister registers rdIndex
    op2 <- getRegister registers rsIndex
    let result = op1 + op2 + (if carryFlag oldFlags then 1 else 0)
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        halfCarryFlag = testBit op1 3 && testBit op2 3 || testBit op1 3 && not (testBit result 3) || not (testBit result 3) && testBit op1 3,
        overflowFlag = testBit op1 7 && testBit op2 7 && not (testBit result 7) || not (testBit op1 7) && not (testBit op2 7) && testBit result 7,
        negativeFlag = testBit result 7,
        zeroFlag = result == 0,
        carryFlag = testBit op1 7 && testBit op2 7 || testBit op2 7 && not (testBit result 7) || not (testBit result 7) && testBit op1 7,
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

add :: MutRegisters s ->  MutMemory s -> StatusFlags -> StackPointer -> Register -> Register -> ST s (StatusFlags, Int, StackPointer)
add registers memory oldFlags sp rd rs = do
    let rdIndex = fromIntegral rd
    let rsIndex = fromIntegral rs
    op1 <- getRegister registers rdIndex
    op2 <- getRegister registers rsIndex
    let result = op1 + op2
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        halfCarryFlag = testBit op1 3 && testBit op2 3 || testBit op1 3 && not (testBit result 3) || not (testBit result 3) && testBit op1 3,
        overflowFlag = testBit op1 7 && testBit op2 7 && not (testBit result 7) || not (testBit op1 7) && not (testBit op2 7) && testBit result 7,
        negativeFlag = testBit result 7,
        zeroFlag = result == 0,
        carryFlag = testBit op1 7 && testBit op2 7 || testBit op2 7 && not (testBit result 7) || not (testBit result 7) && testBit op1 7,
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

adiw :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> Register -> Word8 -> ST s (StatusFlags, Int, StackPointer)
adiw  registers memory oldFlags sp oph opl k = do
    let rhIndex = fromIntegral oph
    let rlIndex = fromIntegral opl
    rh <- getRegister registers rhIndex
    rl <- getRegister registers rlIndex
    let result = (fromIntegral rh :: Word16) `shiftL` 8 + (fromIntegral rl :: Word16) + (fromIntegral k :: Word16)
    let resultH = fromIntegral (result `shiftR` 8) :: Word8
    let resultL = fromIntegral result :: Word8
    setRegisters registers memory [(rhIndex, resultH), (rlIndex, resultL)]
    let updatedFlags = oldFlags {
        overflowFlag = not (testBit rh 7) && testBit result 15,
        negativeFlag = testBit result 15,
        zeroFlag = result == 0,
        carryFlag = not (testBit result 15) && testBit rh 7,
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

andInstr :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> Register -> ST s (StatusFlags, Int, StackPointer)
andInstr registers memory oldFlags sp op1 op2 = do
    let rdIndex = fromIntegral op1
    let rrIndex = fromIntegral op2
    rd <- getRegister registers rdIndex
    rr <- getRegister registers rrIndex
    let result = rd .&. rr
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        overflowFlag = False,
        negativeFlag = testBit result 7,
        zeroFlag = result == 0,
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

andi :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> Word8 -> ST s (StatusFlags, Int, StackPointer)
andi registers memory oldFlags sp op1 immediate = do
    let rdIndex = fromIntegral op1
    rd <- getRegister registers rdIndex
    let k = immediate
    let result = rd .&. k
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        overflowFlag = False,
        negativeFlag = testBit result 7,
        zeroFlag = result == 0,
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

asr :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> ST s (StatusFlags, Int, StackPointer)
asr registers memory oldFlags sp op1 = do
    let rdIndex = fromIntegral op1
    rd <- getRegister registers rdIndex
    let lsb = testBit rd 0
    let msb = testBit rd 7
    let result = if msb then rd `shiftR` 1 .|. 0x80 else rd `shiftR` 1 .&. 0xBF
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        negativeFlag = testBit result 7,
        zeroFlag = result == 0,
        carryFlag = lsb,
        overflowFlag = xor (negativeFlag updatedFlags) (carryFlag updatedFlags),
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

bclr :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
bclr oldFlags sp flagNumber = do
    let updatedFlags = StatusFlags {
        interruptFlag = interruptFlag oldFlags && (flagNumber /= 7),
        tFlag = tFlag oldFlags && (flagNumber /= 6),
        halfCarryFlag = halfCarryFlag oldFlags && (flagNumber /= 5),
        signFlag = signFlag oldFlags && (flagNumber /= 4),
        overflowFlag = overflowFlag oldFlags && (flagNumber /= 3),
        negativeFlag = negativeFlag oldFlags && (flagNumber /= 2),
        zeroFlag = zeroFlag oldFlags && (flagNumber /= 1),
        carryFlag = halfCarryFlag oldFlags && (flagNumber /= 0)
    }
    return (updatedFlags, 0, sp)

bld :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> Int -> ST s (StatusFlags, Int, StackPointer)
bld registers memory flags sp rd b = do
    let rdIndex = fromIntegral rd
    let t = tFlag flags
    value <- getRegister registers rdIndex
    let updatedValue = if t then setBit value b else clearBit value b
    setRegister registers memory rdIndex updatedValue
    return (flags, 0, sp)

brcc :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
brcc oldFlags sp relAddress = do
    let shouldJump = not (carryFlag oldFlags)
    let jumpAddress = if shouldJump then relAddress else 0
    return (oldFlags, jumpAddress, sp)

brcs :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
brcs oldFlags sp relAddress = do
    let shouldJump = carryFlag oldFlags
    let jumpAddress = if shouldJump then relAddress else 0
    return (oldFlags, jumpAddress, sp)

breq :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
breq oldFlags sp relAddress = do
    let shouldJump = zeroFlag oldFlags
    let jumpAddress = if shouldJump then relAddress else 0
    return (oldFlags, jumpAddress, sp)

brge :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
brge oldFlags sp relAddress = do
    let shouldJump = not (signFlag oldFlags)
    let jumpAddress = if shouldJump then relAddress else 0
    return (oldFlags, jumpAddress, sp)

brhc :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
brhc oldFlags sp relAddress = do
    let shouldJump = not (halfCarryFlag oldFlags)
    let jumpAddress = if shouldJump then relAddress else 0
    return (oldFlags, jumpAddress, sp)

brhs :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
brhs oldFlags sp relAddress = do
    let shouldJump = halfCarryFlag oldFlags
    let jumpAddress = if shouldJump then relAddress else 0
    return (oldFlags, jumpAddress, sp)

brid :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
brid oldFlags sp relAddress = do
    let shouldJump = interruptFlag oldFlags
    let jumpAddress = if shouldJump then relAddress else 0
    return (oldFlags, jumpAddress, sp)

brie :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
brie oldFlags sp relAddress = do
    let shouldJump = not (interruptFlag oldFlags)
    let jumpAddress = if shouldJump then relAddress else 0
    return (oldFlags, jumpAddress, sp)

brlo :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
brlo oldFlags sp relAddress = do
    let shouldJump = carryFlag oldFlags
    let jumpAddress = if shouldJump then relAddress else 0
    return (oldFlags, jumpAddress, sp)

brlt :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
brlt oldFlags sp relAddress = do
    let shouldJump = signFlag oldFlags
    let jumpAddress = if shouldJump then relAddress else 0
    return (oldFlags, jumpAddress, sp)

brmi :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
brmi oldFlags sp relAddress = do
    let shouldJump = negativeFlag oldFlags
    let jumpAddress = if shouldJump then relAddress else 0
    return (oldFlags, jumpAddress, sp)

brne :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
brne oldFlags sp relAddress = do
    let shouldJump = zeroFlag oldFlags
    let jumpAddress = if shouldJump then 0 else relAddress
    return (oldFlags, jumpAddress, sp)

brpl :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
brpl oldFlags sp relAddress = do
    let shouldJump = not (negativeFlag oldFlags)
    let jumpAddress = if shouldJump then relAddress else 0
    return (oldFlags, jumpAddress, sp)

brsh :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
brsh oldFlags sp relAddress = do
    let shouldJump = not (carryFlag oldFlags)
    let jumpAddress = if shouldJump then relAddress else 0
    return (oldFlags, jumpAddress, sp)

brtc :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
brtc oldFlags sp relAddress = do
    let shouldJump = not (tFlag oldFlags)
    let jumpAddress = if shouldJump then relAddress else 0
    return (oldFlags, jumpAddress, sp)

brts :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
brts oldFlags sp relAddress = do
    let shouldJump = tFlag oldFlags
    let jumpAddress = if shouldJump then relAddress else 0
    return (oldFlags, jumpAddress, sp)

brvc :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
brvc oldFlags sp relAddress = do
    let shouldJump = not (overflowFlag oldFlags)
    let jumpAddress = if shouldJump then relAddress else 0
    return (oldFlags, jumpAddress, sp)

brvs :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
brvs oldFlags sp relAddress = do
    let shouldJump = overflowFlag oldFlags
    let jumpAddress = if shouldJump then relAddress else 0
    return (oldFlags, jumpAddress, sp)

call :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Int -> Word16 -> ST s (StatusFlags, Int, StackPointer)
call registers memory oldFlags sp relAddress returnAddress = do
    let (high, low) = (fromIntegral (returnAddress `shiftR` 8), fromIntegral (returnAddress .&. 0xFF))
    setMemoryValues registers memory [((fromIntegral sp), high), ((fromIntegral (sp - 1)), low)]
    let newSp = sp - 2
    return (oldFlags, relAddress, newSp)

cbr :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> Word8 -> ST s (StatusFlags, Int, StackPointer)
cbr registers mem status sp rb k = andi registers mem status sp rb (0xFF - k)

clc :: StatusFlags -> StackPointer -> ST s (StatusFlags, Int, StackPointer)
clc oldFlags sp = do
    let updatedFlags = oldFlags { carryFlag = False }
    return (updatedFlags, 0, sp)

clh :: StatusFlags -> StackPointer -> ST s (StatusFlags, Int, StackPointer)
clh oldFlags sp = do
    let updatedFlags = oldFlags { halfCarryFlag = False }
    return (updatedFlags, 0, sp)

cli :: StatusFlags -> StackPointer -> ST s (StatusFlags, Int, StackPointer)
cli oldFlags sp = do
    let updatedFlags = oldFlags { interruptFlag = False }
    return (updatedFlags, 0, sp)

cln :: StatusFlags -> StackPointer -> ST s (StatusFlags, Int, StackPointer)
cln oldFlags sp = do
    let updatedFlags = oldFlags { negativeFlag = False }
    return (updatedFlags, 0, sp)

clr :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> ST s (StatusFlags, Int, StackPointer)
clr registers memory oldFlags sp op1 = do
    let rdIndex = fromIntegral op1
    let result = 0
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        overflowFlag = False,
        negativeFlag = False,
        zeroFlag = True,
        signFlag = False
    }
    return (updatedFlags, 0, sp)

cls :: StatusFlags -> StackPointer -> ST s (StatusFlags, Int, StackPointer)
cls oldFlags sp = do
    let updatedFlags = oldFlags { signFlag = False }
    return (updatedFlags, 0, sp)

clt :: StatusFlags -> StackPointer -> ST s (StatusFlags, Int, StackPointer)
clt oldFlags sp = do
    let updatedFlags = oldFlags { tFlag = False }
    return (updatedFlags, 0, sp)

clv :: StatusFlags -> StackPointer -> ST s (StatusFlags, Int, StackPointer)
clv oldFlags sp = do
    let updatedFlags = oldFlags { overflowFlag = False }
    return (updatedFlags, 0, sp)

clz :: StatusFlags -> StackPointer -> ST s (StatusFlags, Int, StackPointer)
clz oldFlags sp = do
    let updatedFlags = oldFlags { zeroFlag = False }
    return (updatedFlags, 0, sp)

com :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> ST s (StatusFlags, Int, StackPointer)
com registers memory oldFlags sp op1 = do
    let rdIndex = fromIntegral op1
    rd <- getRegister registers rdIndex
    let result = 255 - rd
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        overflowFlag = False,
        negativeFlag = testBit result 7,
        zeroFlag = result == 0,
        carryFlag = True,
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

cp :: MutRegisters s -> StatusFlags -> StackPointer -> Register -> Register -> ST s (StatusFlags, Int, StackPointer)
cp registers oldFlags sp op1 op2 = do
    let rdIndex = fromIntegral op1
    let rrIndex = fromIntegral op2
    rd <- getRegister registers rdIndex
    rr <- getRegister registers rrIndex
    let result = rd - rr
    let updatedFlags = oldFlags {
        halfCarryFlag = not (testBit rd 3) && testBit rr 3 || testBit rr 3 && testBit result 3 || testBit result 3 && not (testBit rd 3),
        overflowFlag = testBit rd 7 && not (testBit rr 7) && not (testBit result 7) || not (testBit rd 7) && testBit rr 7 && testBit result 7,
        negativeFlag = testBit result 7,
        zeroFlag = result == 0,
        carryFlag = not (testBit rd 7) && testBit rr 7 || testBit rr 7 && testBit result 7 || testBit result 7 && not (testBit rd 7),
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

cpc :: MutRegisters s -> StatusFlags -> StackPointer -> Register -> Register -> ST s (StatusFlags, Int, StackPointer)
cpc registers oldFlags sp op1 op2 = do
    let rdIndex = fromIntegral op1
    let rrIndex = fromIntegral op2
    rd <- getRegister registers rdIndex
    rr <- getRegister registers rrIndex
    let carry = if carryFlag oldFlags then 1 else 0
    let result = rd - rr - carry
    let updatedFlags = oldFlags {
        halfCarryFlag = not (testBit rd 3) && testBit rr 3 || testBit rr 3 && testBit result 3 || testBit result 3 && not (testBit rd 3),
        overflowFlag = testBit rd 7 && not (testBit rr 7) && not (testBit result 7) || not (testBit rd 7) && testBit rr 7 && testBit result 7,
        negativeFlag = testBit result 7,
        zeroFlag = result == 0 && zeroFlag oldFlags,
        carryFlag = not (testBit rd 7) && testBit rr 7 || testBit rr 7 && testBit result 7 || testBit result 7 && not (testBit rd 7),
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

cpi :: MutRegisters s -> StatusFlags -> StackPointer -> Register -> Word8 -> ST s (StatusFlags, Int, StackPointer)
cpi registers oldFlags sp op1 immediate = do
    let rdIndex = fromIntegral op1
    rd <- getRegister registers rdIndex
    let k = immediate
    let result = rd - k
    let updatedFlags = oldFlags {
        halfCarryFlag = not (testBit rd 3) && testBit k 3 || testBit k 3 && testBit result 3 || testBit result 3 && not (testBit rd 3),
        overflowFlag = testBit rd 7 && not (testBit k 7) && not (testBit result 7) || not (testBit rd 7) && testBit k 7 && testBit result 7,
        negativeFlag = testBit result 7,
        zeroFlag = result == 0,
        carryFlag = not (testBit rd 7) && testBit k 7 || testBit k 7 && testBit result 7 || testBit result 7 && not (testBit rd 7),
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

cpse :: MutRegisters s -> StatusFlags -> StackPointer -> Register -> Register -> ST s (StatusFlags, Int, StackPointer)
cpse registers oldFlags sp op1 op2 = do
    let rdIndex = fromIntegral op1
    let rrIndex = fromIntegral op2
    rd <- getRegister registers rdIndex
    rr <- getRegister registers rrIndex
    let shouldJump = rd - rr == 0
    let relativeJump = if shouldJump then 1 else 0
    return (oldFlags, relativeJump, sp)

dec :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> ST s (StatusFlags, Int, StackPointer)
dec registers memory oldFlags sp op1 = do
    let rdIndex = fromIntegral op1
    rd <- getRegister registers rdIndex
    let result = rd - 1
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        overflowFlag = rd == 128,
        negativeFlag = testBit result 7,
        zeroFlag = result == 0,
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

eor :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> Register -> ST s (StatusFlags, Int, StackPointer)
eor registers memory oldFlags sp op1 op2 = do
    let rdIndex = fromIntegral op1
    let rrIndex = fromIntegral op2
    rd <- getRegister registers rdIndex
    rr <- getRegister registers rrIndex
    let result = xor rd rr
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        overflowFlag = False,
        negativeFlag = testBit result 7,
        zeroFlag = result == 0,
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

inc :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> ST s (StatusFlags, Int, StackPointer)
inc registers memory oldFlags sp op1 = do
    let rdIndex = fromIntegral op1
    rd <- getRegister registers rdIndex
    let result = rd + 1
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        overflowFlag = rd == 127,
        negativeFlag = testBit result 7,
        zeroFlag = result == 0,
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

jmp :: StatusFlags -> StackPointer -> Int -> ST s (StatusFlags, Int, StackPointer)
jmp oldFlags sp relAddress = return (oldFlags, relAddress, sp)

ld :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> String -> ST s (StatusFlags, Int, StackPointer)
ld registers memory oldFlags sp op1 "X" = do
    let rdIndex = fromIntegral op1
    r27 <- getRegister registers 27
    r26 <- getRegister registers 26
    let address16b = (fromIntegral r27 :: Word16) `shiftL` 8 + (fromIntegral r26 :: Word16)
    loadValue <- (getMemory memory (fromIntegral address16b))
    setRegister registers memory rdIndex loadValue
    return (oldFlags, 0, sp)

ld registers memory oldFlags sp op1 "X+" = do
    let rdIndex = fromIntegral op1
    r27 <- getRegister registers 27
    r26 <- getRegister registers 26
    let address16b = (fromIntegral r27 :: Word16) `shiftL` 8 + (fromIntegral r26 :: Word16)
    let newXRegister = address16b + 1
    let xHigh = fromIntegral (newXRegister `shiftR` 8) :: Word8
    let xLow = fromIntegral newXRegister :: Word8
    loadValue <- getMemory memory (fromIntegral address16b)
    setRegister registers memory rdIndex loadValue
    setRegisters registers memory [(27, xHigh),(26, xLow)]
    return (oldFlags, 0, sp)

ld registers memory oldFlags sp op1 "-X" = do
    let rdIndex = fromIntegral op1
    r27 <- getRegister registers 27
    r26 <- getRegister registers 26
    let address16b = (fromIntegral r27 :: Word16) `shiftL` 8 + (fromIntegral r26 :: Word16)
    let newXRegister = address16b - 1
    let xHigh = fromIntegral (newXRegister `shiftR` 8) :: Word8
    let xLow = fromIntegral newXRegister :: Word8
    value <- getMemory memory (fromIntegral newXRegister)
    setRegister registers memory rdIndex value
    setRegisters registers memory [(27, xHigh), (26, xLow)]
    return (oldFlags, 0, sp)

ld registers memory oldFlags sp op1 "Y" = do
    let rdIndex = fromIntegral op1
    r29 <- getRegister registers 29
    r28 <- getRegister registers 28
    let address16b = (fromIntegral r29 :: Word16) `shiftL` 8 + (fromIntegral r28 :: Word16)
    value <- getMemory memory (fromIntegral address16b)
    setRegister registers memory rdIndex value
    return (oldFlags, 0, sp)

ld registers memory oldFlags sp op1 "Y+" = do
    let rdIndex = fromIntegral op1
    r29 <- getRegister registers 29
    r28 <- getRegister registers 28
    let address16b = (fromIntegral r29 :: Word16) `shiftL` 8 + (fromIntegral r28 :: Word16)
    let newYRegister = address16b + 1
    let yHigh = fromIntegral (newYRegister `shiftR` 8) :: Word8
    let yLow = fromIntegral newYRegister :: Word8
    value <- getMemory memory (fromIntegral address16b)
    setRegister registers memory rdIndex value
    setRegisters registers memory [(29, yHigh), (28, yLow)]
    return (oldFlags, 0, sp)

ld registers memory oldFlags sp op1 "-Y" = do
    let rdIndex = fromIntegral op1
    r29 <- getRegister registers 29
    r28 <- getRegister registers 28
    let address16b = (fromIntegral r29 :: Word16) `shiftL` 8 + (fromIntegral r28 :: Word16)
    let newYRegister = address16b - 1
    let yHigh = fromIntegral (newYRegister `shiftR` 8) :: Word8
    let yLow = fromIntegral newYRegister :: Word8
    yRegisterValue <- getMemory memory (fromIntegral newYRegister)
    setRegister registers memory rdIndex yRegisterValue
    setRegisters registers memory [(29, yHigh), (28, yLow)]
    return (oldFlags, 0, sp)

ld registers memory oldFlags sp op1 "Z" = do
    let rdIndex = fromIntegral op1
    r31 <- getRegister registers 31
    r30 <- getRegister registers 30
    let address16b = (fromIntegral r31 :: Word16) `shiftL` 8 + (fromIntegral r30 :: Word16)
    value <- getMemory memory (fromIntegral address16b)
    setRegister registers memory rdIndex value 
    return (oldFlags, 0, sp)

ld registers memory oldFlags sp op1 "Z+" = do
    let rdIndex = fromIntegral op1
    r31 <- getRegister registers 31
    r30 <- getRegister registers 30
    let address16b = (fromIntegral r31 :: Word16) `shiftL` 8 + (fromIntegral r30 :: Word16)
    let newZRegister = address16b + 1
    let zHigh = fromIntegral (newZRegister `shiftR` 8) :: Word8
    let zLow = fromIntegral newZRegister :: Word8
    value <- getMemory memory (fromIntegral address16b)
    setRegister registers memory rdIndex value
    setRegisters registers memory [(31, zHigh), (30, zLow)]
    return (oldFlags, 0, sp)

ld registers memory oldFlags sp op1 "-Z" = do
    let rdIndex = fromIntegral op1
    r31 <- getRegister registers 31
    r30 <- getRegister registers 30
    let address16b = (fromIntegral r31 :: Word16) `shiftL` 8 + (fromIntegral r30 :: Word16)
    let newZRegister = address16b - 1
    let zHigh = fromIntegral (newZRegister `shiftR` 8) :: Word8
    let zLow = fromIntegral newZRegister :: Word8
    zRegisterValue <- getMemory memory (fromIntegral newZRegister)
    setRegister registers memory rdIndex zRegisterValue
    setRegisters registers memory [(31, zHigh), (30, zLow)]
    return (oldFlags, 0, sp)

ldi :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> Word8 -> ST s (StatusFlags, Int, StackPointer)
ldi registers memory oldFlags sp rd immediate = do
    let rdIndex = fromIntegral rd
    setRegister registers memory rdIndex immediate
    return (oldFlags, 0, sp)

lds :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> Word16 -> ST s (StatusFlags, Int, StackPointer)
lds registers memory oldFlags sp op1 immediate = do
    let rdIndex = fromIntegral op1
    let k = fromIntegral immediate
    value <- getMemory memory k
    setRegister registers memory rdIndex value
    return (oldFlags, 0, sp)

lsl :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> ST s (StatusFlags, Int, StackPointer)
lsl registers memory oldFlags sp op1 = do
    let rdIndex = fromIntegral op1
    rd <- getRegister registers rdIndex
    let msb = testBit rd 7
    let result = rd `shiftL` 1
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        halfCarryFlag = testBit rd 3,
        negativeFlag = testBit result 7,
        zeroFlag = result == 0,
        carryFlag = msb,
        overflowFlag = xor (negativeFlag updatedFlags) (carryFlag updatedFlags),
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

lsr :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> ST s (StatusFlags, Int, StackPointer)
lsr registers memory oldFlags sp op1 = do
    let rdIndex = fromIntegral op1
    rd <- getRegister registers rdIndex
    let lsb = testBit rd 0
    let result = rd `shiftR` 1
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        negativeFlag = False,
        zeroFlag = result == 0,
        carryFlag = lsb,
        overflowFlag = xor (negativeFlag updatedFlags) (carryFlag updatedFlags),
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

mov :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> Register -> ST s (StatusFlags, Int, StackPointer)
mov registers memory oldFlags sp rd rs = do
    let rdIndex = fromIntegral rd
    let rsIndex = fromIntegral rs
    value <- getRegister registers rsIndex
    setRegister registers memory rdIndex value
    return (oldFlags, 0, sp)

movw :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> Register -> Register -> Register -> ST s (StatusFlags, Int, StackPointer)
movw registers memory oldFlags sp rd1 rd rr1 rr = do
    let rd1Index = fromIntegral rd1
    let rdIndex = fromIntegral rd
    let rr1Index = fromIntegral rr1
    let rrIndex = fromIntegral rr
    valueH <- getRegister registers rr1Index
    valueL <- getRegister registers rrIndex
    setRegisters registers memory [(rd1Index, valueH), (rdIndex, valueL)]
    return (oldFlags, 0, sp)

mul :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> Register -> ST s (StatusFlags, Int, StackPointer)
mul registers memory oldFlags sp op1 op2 = do
    let rdIndex = fromIntegral op1
    let rrIndex = fromIntegral op2
    rd <- getRegister registers rdIndex
    rr <- getRegister registers rrIndex
    let result = (fromIntegral rd :: Word16)*(fromIntegral rr :: Word16)
    let resultH = fromIntegral (result `shiftR` 8) :: Word8
    let resultL = fromIntegral result :: Word8
    setRegisters registers memory [(1, resultH), (0, resultL)]
    let updatedFlags = oldFlags {
        signFlag = signFlag oldFlags,
        overflowFlag = overflowFlag oldFlags,
        negativeFlag = negativeFlag oldFlags,
        zeroFlag = result == 0,
        carryFlag = testBit resultH 7
    }
    return (updatedFlags, 0, sp)

muls :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> Register -> ST s (StatusFlags, Int, StackPointer)
muls registers memory oldFlags sp op1 op2 = do
    let rdIndex = fromIntegral op1
    let rrIndex = fromIntegral op2
    rd <- getRegister registers rdIndex
    rr <- getRegister registers rrIndex

    let result = multiplySigned rd rr
    let resultH = fromIntegral (result `shiftR` 8) :: Word8
    let resultL = fromIntegral result :: Word8
    setRegisters registers memory [(1, resultH), (0, resultL)]
    let updatedFlags = oldFlags {
        signFlag = signFlag oldFlags,
        overflowFlag = overflowFlag oldFlags,
        negativeFlag = negativeFlag oldFlags,
        zeroFlag = result == 0,
        carryFlag = testBit resultH 7
    }
    return (updatedFlags, 0, sp)
    where
        multiplySigned :: Word8 -> Word8 -> Word16
        multiplySigned _ 0 = 0
        multiplySigned 0 _ = 0
        multiplySigned m1 m2
            | testBit m1 7 && not (testBit m2 7) = negate ((fromIntegral (negate m1) :: Word16) * fromIntegral m2 :: Word16)
            | not (testBit m1 7) && testBit m2 7 = negate ((fromIntegral m1 ::Word16)*(fromIntegral (negate m2) ::Word16))
            | not (testBit m1 7) && not (testBit m2 7) = (fromIntegral m1 ::Word16)*(fromIntegral m2 ::Word16)
            | testBit m1 7 && testBit m2 7 = (fromIntegral (negate m1) ::Word16)*(fromIntegral (negate m2) ::Word16)

neg :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> ST s (StatusFlags, Int, StackPointer)
neg registers memory oldFlags sp op1 = do
    let rdIndex = fromIntegral op1
    rd <- getRegister registers rdIndex
    let result = negate rd
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        halfCarryFlag = testBit result 3 || not (testBit rd 3),
        overflowFlag = testBit result 7 && not (testBit result 6) && not (testBit result 5) && not (testBit result 4) && not (testBit result 3) && not (testBit result 2) && not (testBit result 1) && not (testBit result 0),
        negativeFlag = testBit result 7,
        zeroFlag = result == 0,
        carryFlag = testBit result 7 || testBit result 6 || testBit result 5 || testBit result 4 || testBit result 3 || testBit result 2 || testBit result 1 || testBit result 0,
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

orInstr :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> Register -> ST s (StatusFlags, Int, StackPointer)
orInstr registers memory oldFlags sp op1 op2 = do
    let rdIndex = fromIntegral op1
    let rsIndex = fromIntegral op2
    rd <- getRegister registers rdIndex
    rs <- getRegister registers rsIndex
    let result = rd .|. rs
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        overflowFlag = False,
        negativeFlag = testBit result 7,
        zeroFlag = result == 0,
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

ori :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> Word8 -> ST s (StatusFlags, Int, StackPointer)
ori registers memory oldFlags sp op1 immediate = do
    let rdIndex = fromIntegral op1
    rd <- getRegister registers rdIndex
    let k = immediate
    let result = rd .|. k
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        overflowFlag = False,
        negativeFlag = testBit result 7,
        zeroFlag = result == 0,
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

pop :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> ST s (StatusFlags, Int, StackPointer)
pop registers memory oldFlags sp op1 = do
    let rdIndex = fromIntegral op1
    let newSp = sp + 1
    poppedValue <- getMemory memory (fromIntegral newSp)
    setRegister registers memory rdIndex poppedValue
    return (oldFlags, 0, newSp)

push :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> ST s (StatusFlags, Int, StackPointer)
push registers memory oldFlags sp op1 = do
    let rrIndex = fromIntegral op1
    rr <- getRegister registers rrIndex
    setMemory registers memory (fromIntegral sp) rr
    return (oldFlags, 0, sp - 1)

ret ::  MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> ProgramCounter -> ST s (StatusFlags, Int, StackPointer)
ret registers memory oldFlags sp pc = do
    let newSp = sp + 2
    returnAddressHigh <- getMemory memory (fromIntegral newSp)
    returnAddressLow <- getMemory memory (fromIntegral newSp - 1)
    let returnAddress = (fromIntegral returnAddressHigh :: Word16) `shiftL` 8 .|. (fromIntegral returnAddressLow :: Word16)
    let relativeAddress = (fromIntegral returnAddress :: Int) - (fromIntegral pc :: Int)
    return (oldFlags, relativeAddress, newSp)

rol :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> ST s (StatusFlags, Int, StackPointer)
rol registers memory oldFlags sp op1 = do
    let rdIndex = fromIntegral op1
    rd <- getRegister registers rdIndex
    let msb = testBit rd 7
    let result = if carryFlag oldFlags then rd `shiftL` 1 .|. 0x01 else rd `shiftL` 1
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        halfCarryFlag = testBit rd 3,
        negativeFlag = testBit result 7,
        zeroFlag = result == 0,
        carryFlag = msb,
        overflowFlag = xor (negativeFlag updatedFlags) (carryFlag updatedFlags),
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

ror :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> ST s (StatusFlags, Int, StackPointer)
ror registers memory oldFlags sp op1 = do
    let rdIndex = fromIntegral op1
    rd <- getRegister registers rdIndex
    let lsb = testBit rd 0
    let result = if carryFlag oldFlags then rd `shiftR` 1 .|. 0x80 else rd `shiftR` 1
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        negativeFlag = False,
        zeroFlag = result == 0,
        carryFlag = lsb,
        overflowFlag = xor (negativeFlag updatedFlags) (carryFlag updatedFlags),
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

sbc :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> Register -> ST s (StatusFlags, Int, StackPointer)
sbc registers memory oldFlags sp op1 op2 = do
    let rdIndex = fromIntegral op1
    let rsIndex = fromIntegral op2
    rd <- getRegister registers rdIndex
    rr <- getRegister registers rsIndex
    let carry = if carryFlag oldFlags then 1 else 0
    let result = rd - rr - carry
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        halfCarryFlag = not (testBit rd 3) && testBit rr 3 || testBit rr 3 && testBit result 3 || testBit result 3 && not (testBit rd 3),
        overflowFlag = testBit rd 7 && not (testBit rr 7) && not (testBit result 7) || not (testBit rd 7) && testBit rr 7 && testBit result 7,
        negativeFlag = testBit result 7,
        zeroFlag = result == 0 && zeroFlag oldFlags,
        carryFlag = not (testBit rd 7) && testBit rr 7 || testBit rr 7 && testBit result 7 || testBit result 7 && not (testBit rd 7),
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

sbrc :: MutRegisters s -> StatusFlags -> StackPointer -> Register -> Word8 -> ST s (StatusFlags, Int, StackPointer)
sbrc registers oldFlags sp op1 immediate = do
    let rdIndex = fromIntegral op1
    rd <- getRegister registers rdIndex
    let b = fromIntegral immediate
    let shouldJump = not (testBit rd b)
    let relativeJump = if shouldJump then 1 else 0
    return (oldFlags, relativeJump, sp)

sbrs :: MutRegisters s -> StatusFlags -> StackPointer -> Register -> Word8 -> ST s (StatusFlags, Int, StackPointer)
sbrs registers oldFlags sp op1 immediate = do
    let rdIndex = fromIntegral op1
    rd <- getRegister registers rdIndex
    let b = fromIntegral immediate
    let shouldJump = testBit rd b
    let relativeJump = if shouldJump then 1 else 0
    return (oldFlags, relativeJump, sp)

sec :: StatusFlags -> StackPointer -> ST s (StatusFlags, Int, StackPointer)
sec oldFlags sp = do
    let updatedFlags = oldFlags { carryFlag = True }
    return (updatedFlags, 0, sp)

seh :: StatusFlags -> StackPointer -> ST s (StatusFlags, Int, StackPointer)
seh oldFlags sp = do
    let updatedFlags = oldFlags { halfCarryFlag = True }
    return (updatedFlags, 0, sp)

sei :: StatusFlags -> StackPointer -> ST s (StatusFlags, Int, StackPointer)
sei oldFlags sp = do
    let updatedFlags = oldFlags { interruptFlag = True }
    return (updatedFlags, 0, sp)

sen :: StatusFlags -> StackPointer -> ST s (StatusFlags, Int, StackPointer)
sen oldFlags sp = do
    let updatedFlags = oldFlags { negativeFlag = True }
    return (updatedFlags, 0, sp)

ser :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> ST s (StatusFlags, Int, StackPointer)
ser registers memory oldFlags sp op1 = do
    let rdIndex = fromIntegral op1
    setRegister registers memory rdIndex 255
    return (oldFlags, 0, sp)

ses :: StatusFlags -> StackPointer -> ST s (StatusFlags, Int, StackPointer)
ses oldFlags sp = do
    let updatedFlags = oldFlags { signFlag = True }
    return (updatedFlags, 0, sp)

set :: StatusFlags -> StackPointer -> ST s (StatusFlags, Int, StackPointer)
set oldFlags sp = do
    let updatedFlags = oldFlags { tFlag = True }
    return (updatedFlags, 0, sp)

sev :: StatusFlags -> StackPointer -> ST s (StatusFlags, Int, StackPointer)
sev oldFlags sp = do
    let updatedFlags = oldFlags { overflowFlag = True }
    return (updatedFlags, 0, sp)

sez :: StatusFlags -> StackPointer -> ST s (StatusFlags, Int, StackPointer)
sez oldFlags sp = do
    let updatedFlags = oldFlags { zeroFlag = True }
    return (updatedFlags, 0, sp)

st :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> String -> Register -> ST s (StatusFlags, Int, StackPointer)
st registers memory oldFlags sp "X" op2 = do
    let rrIndex = fromIntegral op2
    rr <- getRegister registers rrIndex
    r27 <- getRegister registers 27
    r26 <- getRegister registers 26
    let address16b = (fromIntegral r27 :: Word16) `shiftL` 8 + (fromIntegral r26 :: Word16)
    setMemory registers memory (fromIntegral address16b) rr
    return (oldFlags, 0, sp)

st registers memory oldFlags sp "X+" op2 = do
    let rrIndex = fromIntegral op2
    rr <- getRegister registers rrIndex
    r27 <- getRegister registers 27
    r26 <- getRegister registers 26
    let address16b = (fromIntegral r27 :: Word16) `shiftL` 8 + (fromIntegral r26 :: Word16)
    setMemory registers memory (fromIntegral address16b) rr
    let newXRegister = address16b + 1
    let xHigh = fromIntegral (newXRegister `shiftR` 8) :: Word8
    let xLow = fromIntegral newXRegister :: Word8
    setRegisters registers memory [(27, xHigh), (26, xLow)]
    return (oldFlags, 0, sp)

st registers memory oldFlags sp "-X" op2 = do
    let rrIndex = fromIntegral op2
    rr <- getRegister registers rrIndex
    r27 <- getRegister registers 27
    r26 <- getRegister registers 26
    let address16b = (fromIntegral r27 :: Word16) `shiftL` 8 + (fromIntegral r26 :: Word16)
    let newXRegister = address16b - 1
    setMemory registers memory (fromIntegral newXRegister) rr
    let xHigh = fromIntegral (newXRegister `shiftR` 8) :: Word8
    let xLow = fromIntegral newXRegister :: Word8
    setRegisters registers memory [(27, xHigh), (26,xLow)]
    return (oldFlags, 0, sp)

st registers memory oldFlags sp "Y" op2 = do
    let rrIndex = fromIntegral op2
    rr <- getRegister registers rrIndex
    r29 <- getRegister registers 29
    r28 <- getRegister registers 28
    let address16b = (fromIntegral r29 :: Word16) `shiftL` 8 + (fromIntegral r28 :: Word16)
    setMemory registers memory (fromIntegral address16b) rr
    return (oldFlags, 0, sp)

st registers memory oldFlags sp "Y+" op2 = do
    let rrIndex = fromIntegral op2
    rr <- getRegister registers rrIndex
    r29 <- getRegister registers 29
    r28 <- getRegister registers 28
    let address16b = (fromIntegral r29 :: Word16) `shiftL` 8 + (fromIntegral r28 :: Word16)
    setMemory registers memory (fromIntegral address16b) rr
    let newYRegister = address16b + 1
    let yHigh = fromIntegral (newYRegister `shiftR` 8) :: Word8
    let yLow = fromIntegral newYRegister :: Word8
    setRegisters registers memory [(29, yHigh), (28, yLow)]
    return (oldFlags, 0, sp)

st registers memory oldFlags sp "-Y" op2 = do
    let rrIndex = fromIntegral op2
    rr <- getRegister registers rrIndex
    r29 <- getRegister registers 29
    r28 <- getRegister registers 28
    let address16b = (fromIntegral r29 :: Word16) `shiftL` 8 + (fromIntegral r28 :: Word16)
    let newYRegister = address16b - 1
    setMemory registers memory (fromIntegral newYRegister) rr
    let yHigh = fromIntegral (newYRegister `shiftR` 8) :: Word8
    let yLow = fromIntegral newYRegister :: Word8
    setRegisters registers memory [(29, yHigh), (28, yLow)]
    return (oldFlags, 0, sp)

st registers memory oldFlags sp "Z" op2 = do
    let rrIndex = fromIntegral op2
    rr <- getRegister registers rrIndex
    r31 <- getRegister registers 31
    r30 <- getRegister registers 30
    let address16b = (fromIntegral r31 :: Word16) `shiftL` 8 + (fromIntegral r30 :: Word16)
    setMemory registers memory (fromIntegral address16b) rr
    return (oldFlags, 0, sp)

st registers memory oldFlags sp "Z+" op2 = do
    let rrIndex = fromIntegral op2
    rr <- getRegister registers rrIndex
    r31 <- getRegister registers 31
    r30 <- getRegister registers 30
    let address16b = (fromIntegral r31 :: Word16) `shiftL` 8 + (fromIntegral r30 :: Word16)
    setMemory registers memory (fromIntegral address16b) rr
    let newZRegister = address16b + 1
    let zHigh = fromIntegral (newZRegister `shiftR` 8) :: Word8
    let zLow = fromIntegral newZRegister :: Word8
    setRegisters registers memory [(31, zHigh), (30, zLow)]
    return (oldFlags, 0, sp)

st registers memory oldFlags sp "-Z" op2 = do
    let rrIndex = fromIntegral op2
    rr <- getRegister registers rrIndex
    r31 <- getRegister registers 31
    r30 <- getRegister registers 30
    let address16b = (fromIntegral r31 :: Word16) `shiftL` 8 + (fromIntegral r30 :: Word16)
    let newZRegister = address16b - 1
    setMemory registers memory (fromIntegral newZRegister) rr
    let zHigh = fromIntegral (newZRegister `shiftR` 8) :: Word8
    let zLow = fromIntegral newZRegister :: Word8
    setRegisters registers memory [(31, zHigh), (30, zLow)]
    return (oldFlags, 0, sp)

sts :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Word16 -> Register -> ST s (StatusFlags, Int, StackPointer)
sts registers memory oldFlags sp immediate op2 = do
    let rrIndex = fromIntegral op2
    rr <- getRegister registers rrIndex
    let k = fromIntegral immediate
    setMemory registers memory k rr
    return (oldFlags, 0, sp)

sub :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> Register -> ST s (StatusFlags, Int, StackPointer)
sub registers memory oldFlags sp op1 op2 = do
    let rdIndex = fromIntegral op1
    let rsIndex = fromIntegral op2
    rd <- getRegister registers rdIndex
    rr <- getRegister registers rsIndex
    let result = rd - rr
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        halfCarryFlag = not (testBit rd 3) && testBit rr 3 || testBit rr 3 && testBit result 3 || testBit result 3 && not (testBit rd 3),
        overflowFlag = testBit rd 7 && not (testBit rr 7) && not (testBit result 7) || not (testBit rd 7) && testBit rr 7 && testBit result 7,
        negativeFlag = testBit result 7,
        zeroFlag = result == 0,
        carryFlag = not (testBit rd 7) && testBit rr 7 || testBit rr 7 && testBit result 7 || testBit result 7 && not (testBit rd 7),
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

subi :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> Word8 -> ST s (StatusFlags, Int, StackPointer)
subi registers memory oldFlags sp op1 k = do
    let rdIndex = fromIntegral op1
    rd <- getRegister registers rdIndex
    let result = rd - k
    setRegister registers memory rdIndex result
    let updatedFlags = oldFlags {
        halfCarryFlag = not (testBit rd 3) && testBit k 3 || testBit k 3 && testBit result 3 || testBit result 3 && not (testBit rd 3),
        overflowFlag = testBit rd 7 && not (testBit k 7) && not (testBit result 7) || not (testBit rd 7) && testBit k 7 && testBit result 7,
        negativeFlag = testBit result 7,
        zeroFlag = result == 0,
        carryFlag = not (testBit rd 7) && testBit k 7 || testBit k 7 && testBit result 7 || testBit result 7 && not (testBit rd 7),
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
    }
    return (updatedFlags, 0, sp)

swap :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> ST s (StatusFlags, Int, StackPointer)
swap registers memory oldFlags sp op1 = do
    let rdIndex = fromIntegral op1
    rd <- getRegister registers rdIndex
    let result = rd `shiftR` 4 .|. rd `shiftL` 4
    setRegister registers memory rdIndex result
    return (oldFlags, 0, sp)

tst :: MutRegisters s -> StatusFlags -> StackPointer -> Register -> ST s (StatusFlags, Int, StackPointer)
tst registers oldFlags sp op1 = do
    let rdIndex = fromIntegral op1
    rd <- getRegister registers rdIndex
    let updatedFlags = oldFlags {
        overflowFlag = False,
        negativeFlag = testBit rd 7,
        signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags),
        zeroFlag = rd == 0,
        carryFlag = carryFlag oldFlags
    }
    return (updatedFlags, 0, sp)

xch :: MutRegisters s -> MutMemory s -> StatusFlags -> StackPointer -> Register -> ST s (StatusFlags, Int, StackPointer)
xch registers memory oldFlags sp op1 = do
    let rdIndex = fromIntegral op1
    rd <- getRegister registers rdIndex
    r31 <- getRegister registers 31
    r30 <- getRegister registers 30
    let address16b = (fromIntegral r31 :: Word16) `shiftL` 8 + (fromIntegral r30 :: Word16)
    memoryValue <- getMemory memory (fromIntegral address16b)
    setMemory registers memory (fromIntegral address16b) rd
    setRegister registers memory rdIndex memoryValue
    return (oldFlags, 0, sp)

-- /////////////////////////////////////////////////////
-- End instruction implementations 
-- /////////////////////////////////////////////////////

checkIfEndOfFunctionInstruction :: Instruction -> Bool
checkIfEndOfFunctionInstruction (RET) = True
checkIfEndOfFunctionInstruction _ = False

checkIfFunctionCallInstruction :: Instruction -> Bool
checkIfFunctionCallInstruction (CALLR _) = True
checkIfFunctionCallInstruction _ = False
