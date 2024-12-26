module Emulator.Instructions where

import Emulator.State

import Data.Array
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

adc :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
adc oldStatus registers sp memory rd rs =
    let
        rdIndex = fromIntegral rd
        rsIndex = fromIntegral rs
        op1 = getRegister registers rdIndex
        op2 = getRegister registers rsIndex
        result = op1 + op2 + (if carryFlag oldStatus then 1 else 0)
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
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
    in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)

add :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
add oldStatus registers sp memory rd rs =
    let
        rdIndex = fromIntegral rd
        rsIndex = fromIntegral rs
        op1 = getRegister registers rdIndex
        op2 = getRegister registers rsIndex
        result = op1 + op2
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
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
    in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)

adiw :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> Word8 -> (Registers, StatusFlags, Int, StackPointer, Memory)
adiw oldStatus registers sp memory oph opl k =
    let rhIndex = fromIntegral oph
        rlIndex = fromIntegral opl
        rh = getRegister registers rhIndex
        rl = getRegister registers rlIndex
        result = (fromIntegral rh :: Word16) `shiftL` 8 + (fromIntegral rl :: Word16) + (fromIntegral k :: Word16)
        resultH = fromIntegral (result `shiftR` 8) :: Word8
        resultL = fromIntegral result :: Word8
        (updatedMemory, updatedRegisters) = setRegister memory registers rhIndex resultH
        (updatedMemory1, updatedRegisters1) = setRegister updatedMemory updatedRegisters rlIndex resultL
        updatedFlags = StatusFlags {
            interruptFlag = interruptFlag oldStatus,
            tFlag = tFlag oldStatus,
            halfCarryFlag = halfCarryFlag oldStatus,
            overflowFlag = not (testBit rh 7) && testBit result 15,
            negativeFlag = testBit result 15,
            zeroFlag = result == 0,
            carryFlag = not (testBit result 15) && testBit rh 7,
            signFlag = xor (negativeFlag updatedFlags) (overflowFlag updatedFlags)
        }
        in (updatedRegisters1, updatedFlags, 0, sp, updatedMemory1)


andInstr :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
andInstr oldStatus registers sp memory op1 op2 =
    let
        rdIndex = fromIntegral op1
        rrIndex = fromIntegral op2
        rd = getRegister registers rdIndex
        rr = getRegister registers rrIndex
        result = rd .&. rr
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
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
    in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)

andi :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, StackPointer, Memory)
andi oldStatus registers sp memory op1 immediate =
    let
        rdIndex = fromIntegral op1
        rd = getRegister registers rdIndex
        k = immediate
        result = rd .&. k
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
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
    in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)

asr :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
asr oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = getRegister registers rdIndex
        lsb = testBit rd 0
        msb = testBit rd 7
        result = if msb then rd `shiftR` 1 .|. 0x80 else rd `shiftR` 1 .&. 0xBF
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
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
    in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)

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
        value = getRegister registers rdIndex
        updatedValue
            | t = setBit value b
            | otherwise = clearBit value b
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex updatedValue
    in (updatedRegisters, flags, 0, sp, updatedMemory)

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
        (updatedMemory, updatedRegisters) = setMemoryValues memory registers [((fromIntegral sp), high), ((fromIntegral (sp - 1)), low)]
        newSp = sp - 2
        in (updatedRegisters, oldStatus, relAddress, newSp, updatedMemory)

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
        result = 0
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
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
    in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)

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
        rd = getRegister registers rdIndex
        result = 255 - rd
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
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
        in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)

cp :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
cp oldStatus registers sp memory op1 op2 =
    let rdIndex = fromIntegral op1
        rrIndex = fromIntegral op2
        rd = getRegister registers rdIndex
        rr = getRegister registers rrIndex
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
        rd = getRegister registers rdIndex
        rr = getRegister registers rrIndex
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
        rd = getRegister registers rdIndex
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
        rd = getRegister registers rdIndex
        rr = getRegister registers rrIndex
        shouldJump = rd - rr == 0
        relativeJump = if shouldJump then 1 else 0
    in (registers, oldStatus, relativeJump, sp, memory)

dec :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
dec oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = registers ! rdIndex
        result = rd - 1
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
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
    in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)

eor :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
eor oldStatus registers sp memory op1 op2 =
    let
        rdIndex = fromIntegral op1
        rrIndex = fromIntegral op2
        rd = getRegister registers rdIndex
        rr = getRegister registers rrIndex
        result = xor rd rr
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
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
    in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)

inc :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
inc oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = getRegister registers rdIndex
        result = rd + 1
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
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
    in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)

jmp :: StatusFlags -> Registers -> StackPointer -> Memory -> Int -> (Registers, StatusFlags, Int, StackPointer, Memory)
jmp oldStatus registers sp memory relAddress = (registers, oldStatus, relAddress, sp, memory)

ld :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> String -> (Registers, StatusFlags, Int, StackPointer, Memory)
ld oldStatus registers sp memory op1 "X" =
    let rdIndex = fromIntegral op1
        address16b = (fromIntegral (getRegister registers 27) :: Word16) `shiftL` 8 + (fromIntegral (getRegister registers 26) :: Word16)
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex (getMemory memory (fromIntegral address16b))
    in (updatedRegisters, oldStatus, 0, sp, updatedMemory)
--TODO: fix these later
ld oldStatus registers sp memory op1 "X+" =
    let rdIndex = fromIntegral op1
        address16b = (fromIntegral (getRegister registers 27) :: Word16) `shiftL` 8 + (fromIntegral (getRegister registers 26) :: Word16)
        newXRegister = address16b + 1
        xHigh = fromIntegral (newXRegister `shiftR` 8) :: Word8
        xLow = fromIntegral newXRegister :: Word8
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex (getMemory memory (fromIntegral address16b))
        (updatedMemory1, updatedRegisters1) = setRegister updatedMemory updatedRegisters 27 xHigh
        (updatedMemory2, updatedRegisters2) = setRegister updatedMemory1 updatedRegisters1 26 xLow
    in (updatedRegisters2, oldStatus, 0, sp, updatedMemory2)

ld oldStatus registers sp memory op1 "-X" =
    let rdIndex = fromIntegral op1
        address16b = (fromIntegral (getRegister registers 27) :: Word16) `shiftL` 8 + (fromIntegral (getRegister registers 26) :: Word16)
        newXRegister = address16b - 1
        xHigh = fromIntegral (newXRegister `shiftR` 8) :: Word8
        xLow = fromIntegral newXRegister :: Word8
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex (getMemory memory (fromIntegral newXRegister))
        (updatedMemory1, updatedRegisters1) = setRegister updatedMemory updatedRegisters 27 xHigh
        (updatedMemory2, updatedRegisters2) = setRegister updatedMemory1 updatedRegisters1 26 xLow
    in (updatedRegisters2, oldStatus, 0, sp, updatedMemory2)

ld oldStatus registers sp memory op1 "Y" =
    let rdIndex = fromIntegral op1
        address16b = (fromIntegral (getRegister registers  29) :: Word16) `shiftL` 8 + (fromIntegral (getRegister registers 28) :: Word16)
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex (getMemory memory (fromIntegral address16b))
    in (updatedRegisters, oldStatus, 0, sp, updatedMemory)

ld oldStatus registers sp memory op1 "Y+" =
    let rdIndex = fromIntegral op1
        address16b = (fromIntegral (getRegister registers 29) :: Word16) `shiftL` 8 + (fromIntegral (getRegister registers 28) :: Word16)
        newYRegister = address16b + 1
        yHigh = fromIntegral (newYRegister `shiftR` 8) :: Word8
        yLow = fromIntegral newYRegister :: Word8
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex (getMemory memory (fromIntegral address16b))
        (updatedMemory1, updatedRegisters1) = setRegister updatedMemory updatedRegisters 29 yHigh
        (updatedMemory2, updatedRegisters2) = setRegister updatedMemory1 updatedRegisters1 28 yLow
    in (updatedRegisters2, oldStatus, 0, sp, updatedMemory2)

ld oldStatus registers sp memory op1 "-Y" =
    let rdIndex = fromIntegral op1
        address16b = (fromIntegral (registers ! 29) :: Word16) `shiftL` 8 + (fromIntegral (registers ! 28) :: Word16)
        newYRegister = address16b - 1
        yHigh = fromIntegral (newYRegister `shiftR` 8) :: Word8
        yLow = fromIntegral newYRegister :: Word8
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex (getMemory memory (fromIntegral newYRegister))
        (updatedMemory1, updatedRegisters1) = setRegister updatedMemory updatedRegisters 29 yHigh
        (updatedMemory2, updatedRegisters2) = setRegister updatedMemory1 updatedRegisters1 28 yLow
    in (updatedRegisters2, oldStatus, 0, sp, updatedMemory2)

ld oldStatus registers sp memory op1 "Z" =
    let rdIndex = fromIntegral op1
        address16b = (fromIntegral (registers ! 31) :: Word16) `shiftL` 8 + (fromIntegral (registers ! 30) :: Word16)
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex (getMemory memory (fromIntegral address16b))
    in (updatedRegisters, oldStatus, 0, sp, updatedMemory)

ld oldStatus registers sp memory op1 "Z+" =
    let rdIndex = fromIntegral op1
        address16b = (fromIntegral (registers ! 31) :: Word16) `shiftL` 8 + (fromIntegral (registers ! 30) :: Word16)
        newZRegister = address16b + 1
        zHigh = fromIntegral (newZRegister `shiftR` 8) :: Word8
        zLow = fromIntegral newZRegister :: Word8
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex (getMemory memory (fromIntegral address16b))
        (updatedMemory1, updatedRegisters1) = setRegister updatedMemory updatedRegisters 31 zHigh
        (updatedMemory2, updatedRegisters2) = setRegister updatedMemory1 updatedRegisters 30 zLow
   in (updatedRegisters2, oldStatus, 0, sp, updatedMemory2)

ld oldStatus registers sp memory op1 "-Z" =
    let rdIndex = fromIntegral op1
        address16b = (fromIntegral (registers ! 31) :: Word16) `shiftL` 8 + (fromIntegral (registers ! 30) :: Word16)
        newZRegister = address16b - 1
        zHigh = fromIntegral (newZRegister `shiftR` 8) :: Word8
        zLow = fromIntegral newZRegister :: Word8
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex (getMemory memory (fromIntegral newZRegister))
        (updatedMemory1, updatedRegisters1) = setRegister updatedMemory updatedRegisters 31 zHigh
        (updatedMemory2, updatedRegisters2) = setRegister updatedMemory1 updatedRegisters1 30 zLow
    in (updatedRegisters2, oldStatus, 0, sp, updatedMemory2)

ldi :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, StackPointer, Memory)
ldi oldStatus registers sp memory rd immediate =
    let rdIndex = fromIntegral rd
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex immediate
        updatedFlags = oldStatus
    in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)

lds :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Word16 -> (Registers, StatusFlags, Int, StackPointer, Memory)
lds oldStatus registers sp memory op1 immediate =
    let rdIndex = fromIntegral op1
        k = fromIntegral immediate
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex (getMemory memory k)
    in (updatedRegisters, oldStatus, 0, sp, updatedMemory)

lsl :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
lsl oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = getRegister registers rdIndex
        msb = testBit rd 7
        result = rd `shiftL` 1
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
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
    in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)

lsr :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
lsr oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = getRegister registers rdIndex
        lsb = testBit rd 0
        result = rd `shiftR` 1
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
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
    in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)

mov :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
mov oldStatus registers sp memory rd rs =
    let rdIndex = fromIntegral rd
        rsIndex = fromIntegral rs
        value = getRegister registers  rsIndex
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex value
        updatedFlags = oldStatus
    in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)

movw :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
movw oldStatus registers sp memory rd1 rd rr1 rr =
    let rd1Index = fromIntegral rd1
        rdIndex = fromIntegral rd
        rr1Index = fromIntegral rr1
        rrIndex = fromIntegral rr
        valueH = getRegister registers rr1Index
        valueL = getRegister registers rrIndex
        (updatedMemory, updatedRegisters) = setRegister memory registers rd1Index valueH
        (updatedMemory1, updatedRegisters1) = setRegister updatedMemory updatedRegisters rdIndex valueL
        updatedFlags = oldStatus
    in (updatedRegisters1, updatedFlags, 0, sp, updatedMemory1)

mul :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
mul oldStatus registers sp memory op1 op2 =
    let rdIndex = fromIntegral op1
        rrIndex = fromIntegral op2
        rd = getRegister registers rdIndex
        rr = getRegister registers rrIndex
        result = (fromIntegral rd :: Word16)*(fromIntegral rr :: Word16)
        resultH = fromIntegral (result `shiftR` 8) :: Word8
        resultL = fromIntegral result :: Word8
        (updatedMemory, updatedRegisters) = setRegister memory registers 1 resultH
        (updatedMemory1, updatedRegisters1) = setRegister updatedMemory updatedRegisters 0 resultL
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
    in (updatedRegisters1, updatedFlags, 0, sp, updatedMemory1)

muls :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
muls oldStatus registers sp memory op1 op2 =
    let rdIndex = fromIntegral op1
        rrIndex = fromIntegral op2
        rd = getRegister registers rdIndex
        rr = getRegister registers rrIndex

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
        (updatedMemory, updatedRegisters) = setRegister memory registers 1 resultH
        (updatedMemory1, updatedRegisters1) = setRegister updatedMemory updatedRegisters 0 resultL
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
    in (updatedRegisters1, updatedFlags, 0, sp, updatedMemory1)

neg :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
neg oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = getRegister registers rdIndex
        result = negate rd
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
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
        in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)

orInstr :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
orInstr oldStatus registers sp memory op1 op2 =
    let rdIndex = fromIntegral op1
        rsIndex = fromIntegral op2
        rd = getRegister registers rdIndex
        rs = getRegister registers rsIndex
        result = rd .|. rs
        (updatedMemory, updateRegisters) = setRegister memory registers rdIndex result
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
    in (updateRegisters, updatedFlags, 0, sp, updatedMemory)

ori :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, StackPointer, Memory)
ori oldStatus registers sp memory op1 immediate =
    let rdIndex = fromIntegral op1
        rd = getRegister registers rdIndex
        k = immediate
        result = rd .|. k
        (updatedMemory, updateRegisters) = setRegister memory registers rdIndex result
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
    in (updateRegisters, updatedFlags, 0, sp, updatedMemory)

pop :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
pop oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        newSp = sp + 1
        poppedValue = getMemory memory (fromIntegral newSp)
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex poppedValue
    in (updatedRegisters, oldStatus, 0, newSp, updatedMemory)

push :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
push oldStatus registers sp memory op1 =
    let rrIndex = fromIntegral op1
        rr = getRegister registers rrIndex
        (updatedMemory, updatedRegisters) = setMemory memory registers (fromIntegral sp) rr
        newSp = sp - 1
    in (updatedRegisters, oldStatus, 0, newSp, updatedMemory)

ret ::  StatusFlags -> Registers -> StackPointer -> Memory -> ProgramCounter -> (Registers, StatusFlags, Int, StackPointer, Memory)
ret oldStatus registers sp memory pc =
    let
        newSp = sp + 2
        returnAddressHigh = getMemory memory (fromIntegral newSp)
        returnAddressLow = getMemory memory (fromIntegral newSp - 1)
        returnAddress = (fromIntegral returnAddressHigh :: Word16) `shiftL` 8 .|. (fromIntegral returnAddressLow :: Word16)
        relativeAddress = (fromIntegral returnAddress :: Int) - (fromIntegral pc :: Int)
    in (registers, oldStatus, relativeAddress, newSp, memory)

rol :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
rol oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = getRegister registers rdIndex
        msb = testBit rd 7
        result = if carryFlag oldStatus then rd `shiftL` 1 .|. 0x01 else rd `shiftL` 1
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
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
    in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)
ror :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
ror oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = getRegister registers rdIndex
        lsb = testBit rd 0
        result = if carryFlag oldStatus then rd `shiftR` 1 .|. 0x80 else rd `shiftR` 1
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
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
    in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)

sbc :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
sbc oldStatus registers sp memory op1 op2 =
    let
        rdIndex = fromIntegral op1
        rsIndex = fromIntegral op2
        rd = getRegister registers rdIndex
        rr = getRegister registers rsIndex
        carry = if carryFlag oldStatus then 1 else 0
        result = rd - rr - carry
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
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
    in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)

sbrc :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, StackPointer, Memory)
sbrc oldStatus registers sp memory op1 immediate =
    let rdIndex = fromIntegral op1
        rd = getRegister registers rdIndex
        b = fromIntegral immediate
        shouldJump = not (testBit rd b)
        relativeJump = if shouldJump then 1 else 0
    in (registers, oldStatus, relativeJump, sp, memory)

sbrs :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, StackPointer, Memory)
sbrs oldStatus registers sp memory op1 immediate =
    let rdIndex = fromIntegral op1
        rd = getRegister registers rdIndex
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
        result = 255
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
    in (updatedRegisters, oldStatus, 0, sp, updatedMemory)

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
        rr = getRegister registers rrIndex
        address16b = (fromIntegral (getRegister registers 27) :: Word16) `shiftL` 8 + (fromIntegral (getRegister registers 26) :: Word16)
        (updatedMemory, updatedRegisters) = setMemory memory registers (fromIntegral address16b) rr
    in (updatedRegisters, oldStatus, 0, sp, updatedMemory)

st oldStatus registers sp memory "X+" op2 =
    let rrIndex = fromIntegral op2
        rr = registers ! rrIndex
        address16b = (fromIntegral (getRegister registers 27) :: Word16) `shiftL` 8 + (fromIntegral (getRegister registers 26) :: Word16)
        (updatedMemory, updatedRegisters) = setMemory memory registers (fromIntegral address16b) rr
        newXRegister = address16b + 1
        xHigh = fromIntegral (newXRegister `shiftR` 8) :: Word8
        xLow = fromIntegral newXRegister :: Word8
        (updatedMemory1, updatedRegisters1) = setRegisters updatedMemory updatedRegisters [(27, xHigh), (26, xLow)]
    in (updatedRegisters1, oldStatus, 0, sp, updatedMemory1)

st oldStatus registers sp memory "-X" op2 =
    let rrIndex = fromIntegral op2
        rr = registers ! rrIndex
        address16b = (fromIntegral (getRegister registers 27) :: Word16) `shiftL` 8 + (fromIntegral (getRegister registers 26) :: Word16)
        newXRegister = address16b - 1
        (updatedMemory, updatedRegister) = setMemory memory registers (fromIntegral newXRegister) rr
        xHigh = fromIntegral (newXRegister `shiftR` 8) :: Word8
        xLow = fromIntegral newXRegister :: Word8
        updatedRegisters = registers // [(27, xHigh), (26, xLow)]
        (updatedMemory1, updatedRegisters1) = setRegisters updatedMemory updatedRegisters [(27, xHigh), (26,xLow)] 
    in (updatedRegisters1, oldStatus, 0, sp, updatedMemory1)
st oldStatus registers sp memory "Y" op2 =
    let rrIndex = fromIntegral op2
        rr = getRegister registers rrIndex
        address16b = (fromIntegral (getRegister registers 29) :: Word16) `shiftL` 8 + (fromIntegral (getRegister registers 28) :: Word16)
        (updatedMemory, updatedRegisters) = setMemory memory registers (fromIntegral address16b) rr
    in (updatedRegisters, oldStatus, 0, sp, updatedMemory)

st oldStatus registers sp memory "Y+" op2 =
    let rrIndex = fromIntegral op2
        rr = getRegister registers rrIndex
        address16b = (fromIntegral (getRegister registers 29) :: Word16) `shiftL` 8 + (fromIntegral (getRegister registers 28) :: Word16)
        (updatedMemory, updatedRegisters) = setMemory memory registers (fromIntegral address16b) rr
        newYRegister = address16b + 1
        yHigh = fromIntegral (newYRegister `shiftR` 8) :: Word8
        yLow = fromIntegral newYRegister :: Word8
        (updatedMemory1, updatedRegisters1) = setRegisters updatedMemory updatedRegisters [(29, yHigh), (28, yLow)]
    in (updatedRegisters1, oldStatus, 0, sp, updatedMemory1)

st oldStatus registers sp memory "-Y" op2 =
    let rrIndex = fromIntegral op2
        rr = getRegister registers rrIndex
        address16b = (fromIntegral (getRegister registers 29) :: Word16) `shiftL` 8 + (fromIntegral (getRegister registers 28) :: Word16)
        newYRegister = address16b - 1
        (updatedMemory, updatedRegisters) = setMemory memory registers (fromIntegral newYRegister) rr
        yHigh = fromIntegral (newYRegister `shiftR` 8) :: Word8
        yLow = fromIntegral newYRegister :: Word8
        (updatedMemory1, updatedRegisters1) = setRegister updatedMemory updatedRegisters 29 yHigh
        (updatedMemory2, updatedRegisters2) = setRegister updatedMemory1 updatedRegisters1 28 yLow
    in (updatedRegisters2, oldStatus, 0, sp, updatedMemory2)

st oldStatus registers sp memory "Z" op2 =
    let rrIndex = fromIntegral op2
        rr = getRegister registers rrIndex
        address16b = (fromIntegral (getRegister registers 31) :: Word16) `shiftL` 8 + (fromIntegral (getRegister registers 30) :: Word16)
        (updatedMemory, updatedRegisters) = setMemory memory registers (fromIntegral address16b) rr
    in (updatedRegisters, oldStatus, 0, sp, updatedMemory)

st oldStatus registers sp memory "Z+" op2 =
    let rrIndex = fromIntegral op2
        rr = getRegister registers rrIndex
        address16b = (fromIntegral (getRegister registers 31) :: Word16) `shiftL` 8 + (fromIntegral (getRegister registers 30) :: Word16)
        (updatedMemory, updatedRegisters) = setMemory memory registers (fromIntegral address16b) rr
        newZRegister = address16b + 1
        zHigh = fromIntegral (newZRegister `shiftR` 8) :: Word8
        zLow = fromIntegral newZRegister :: Word8
        (updatedMemory1, updatedRegisters1) = setRegister updatedMemory updatedRegisters 31 zHigh
        (updatedMemory2, updatedRegisters2) = setRegister updatedMemory1 updatedRegisters1 30 zLow
    in (updatedRegisters2, oldStatus, 0, sp, updatedMemory2)

st oldStatus registers sp memory "-Z" op2 =
    let rrIndex = fromIntegral op2
        rr = getRegister registers rrIndex
        address16b = (fromIntegral (getRegister registers 31) :: Word16) `shiftL` 8 + (fromIntegral (getRegister registers 30) :: Word16)
        newZRegister = address16b - 1
        (updatedMemory, updatedRegisters) = setMemory memory registers (fromIntegral newZRegister) rr
        zHigh = fromIntegral (newZRegister `shiftR` 8) :: Word8
        zLow = fromIntegral newZRegister :: Word8
        (updatedMemory1, updatedRegisters1) = setRegister updatedMemory updatedRegisters 31 zHigh
        (updatedMemory2, updatedRegisters2) = setRegister updatedMemory1 updatedRegisters1 30 zLow
    in (updatedRegisters2, oldStatus, 0, sp, updatedMemory2)

sts :: StatusFlags -> Registers -> StackPointer -> Memory -> Word16 -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
sts oldStatus registers sp memory immediate op2 =
    let rrIndex = fromIntegral op2
        rr = getRegister registers rrIndex
        k = fromIntegral immediate
        (updatedMemory, updatedRegisters) = setMemory memory registers k rr
    in (updatedRegisters, oldStatus, 0, sp, updatedMemory)

sub :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
sub oldStatus registers sp memory op1 op2 =
    let
        rdIndex = fromIntegral op1
        rsIndex = fromIntegral op2
        rd = getRegister registers rdIndex
        rr = getRegister registers rsIndex
        result = rd - rr
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
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
    in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)

subi :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> Word8 -> (Registers, StatusFlags, Int, StackPointer, Memory)
subi oldStatus registers sp memory op1 immediate =
    let
        rdIndex = fromIntegral op1
        rd = getRegister registers rdIndex
        k = immediate
        result = rd - k
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
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
    in (updatedRegisters, updatedFlags, 0, sp, updatedMemory)

swap :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
swap oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = getRegister registers rdIndex
        result = rd `shiftR` 4 .|. rd `shiftL` 4
        (updatedMemory, updatedRegisters) = setRegister memory registers rdIndex result
        in (updatedRegisters, oldStatus, 0, sp, updatedMemory)

tst :: StatusFlags -> Registers -> StackPointer -> Memory -> Register -> (Registers, StatusFlags, Int, StackPointer, Memory)
tst oldStatus registers sp memory op1 =
    let rdIndex = fromIntegral op1
        rd = getRegister registers rdIndex
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

checkIfEndOfFunctionInstruction :: Instruction -> Bool
checkIfEndOfFunctionInstruction (RET) = True
checkIfEndOfFunctionInstruction _ = False

checkIfFunctionCallInstruction :: Instruction -> Bool
checkIfFunctionCallInstruction (CALLR _) = True
checkIfFunctionCallInstruction _ = False
