module Emulator.Core where

import Emulator.Instructions
import Emulator.State

import Data.Binary (Word16)
import qualified Data.Vector as V
import Control.Monad.ST (ST)

{- | Decodes the current instruction and calls its respective function with the current emulator state,
    then records the updated emulator state for the next instruction. For instructions that may jump, it also
    sends the relative address to the current PC to jump to, if needed.
-}
executeInstruction :: MutRegisters s -> MutMemory s -> StatusFlags -> ProgramCounter -> StackPointer -> Instruction -> ST s (StatusFlags, ProgramCounter, StackPointer)
executeInstruction mutRegisters mutMemory flags pc sp instruction = do
    (updatedFlags, relativeJump, updatedSp) <- case instruction of
        ADC rd rs -> adc flags mutRegisters sp mutMemory rd rs
        ADD rd rs -> add flags mutRegisters sp mutMemory rd rs
        ADIW rdh rdl immediate -> adiw flags mutRegisters sp mutMemory rdh rdl immediate
        AND rd rr -> andInstr flags mutRegisters sp mutMemory rd rr
        ANDI rd k -> andi flags mutRegisters sp mutMemory rd k
        ASR rd -> asr flags mutRegisters sp mutMemory rd
        BCLR s -> bclr flags mutRegisters sp mutMemory s
        BLD rd b -> bld flags mutRegisters sp mutMemory rd b
        BRCCR relativeAddress -> brcc flags mutRegisters sp mutMemory relativeAddress
        BRCSR relativeAddress -> brcs flags mutRegisters sp mutMemory relativeAddress
        BREQR relativeAddress -> breq flags mutRegisters sp mutMemory relativeAddress
        BRGER relativeAddress -> brge flags mutRegisters sp mutMemory relativeAddress
        BRHCR relativeAddress -> brhc flags mutRegisters sp mutMemory relativeAddress
        BRHSR relativeAddress -> brhs flags mutRegisters sp mutMemory relativeAddress
        BRIDR relativeAddress -> brid flags mutRegisters sp mutMemory relativeAddress
        BRIER relativeAddress -> brie flags mutRegisters sp mutMemory relativeAddress
        BRLOR relativeAddress -> brlo flags mutRegisters sp mutMemory relativeAddress
        BRLTR relativeAddress -> brlt flags mutRegisters sp mutMemory relativeAddress
        BRMIR relativeAddress -> brmi flags mutRegisters sp mutMemory relativeAddress
        BRNER relativeAddress -> brne flags mutRegisters sp mutMemory relativeAddress
        BRPLR relativeAddress -> brpl flags mutRegisters sp mutMemory relativeAddress
        BRSHR relativeAddress -> brsh flags mutRegisters sp mutMemory relativeAddress
        BRTCR relativeAddress -> brtc flags mutRegisters sp mutMemory relativeAddress
        BRTSR relativeAddress -> brts flags mutRegisters sp mutMemory relativeAddress
        BRVCR relativeAddress -> brvc flags mutRegisters sp mutMemory relativeAddress
        BRVSR relativeAddress -> brvs flags mutRegisters sp mutMemory relativeAddress
        CALLR relativeAddress -> call flags mutRegisters sp mutMemory relativeAddress pc
        CBR rd k -> cbr flags mutRegisters sp mutMemory rd k
        CLC -> clc flags mutRegisters sp mutMemory
        CLH -> clh flags mutRegisters sp mutMemory
        CLI -> cli flags mutRegisters sp mutMemory
        CLN -> cln flags mutRegisters sp mutMemory
        CLR rd -> clr flags mutRegisters sp mutMemory rd
        CLS -> cls flags mutRegisters sp mutMemory
        CLT -> clt flags mutRegisters sp mutMemory
        CLV -> clv flags mutRegisters sp mutMemory
        CLZ -> clz flags mutRegisters sp mutMemory
        COM rd -> com flags mutRegisters sp mutMemory rd
        CP rd rr -> cp flags mutRegisters sp mutMemory rd rr
        CPC rd rr -> cpc flags mutRegisters sp mutMemory rd rr
        CPI rd k -> cpi flags mutRegisters sp mutMemory rd k
        CPSE rd rr -> cpse flags mutRegisters sp mutMemory rd rr
        DEC rd -> dec flags mutRegisters sp mutMemory rd
        EOR rd rr -> eor flags mutRegisters sp mutMemory rd rr
        INC rd -> inc flags mutRegisters sp mutMemory rd
        JMPR relativeAddress -> jmp flags mutRegisters sp mutMemory relativeAddress
        LD rd xregister -> ld flags mutRegisters sp mutMemory rd xregister
        LABEL label -> return (flags, 0, sp)
        LDI rd immediate -> ldi flags mutRegisters sp mutMemory rd immediate
        LDS rd k -> lds flags mutRegisters sp mutMemory rd k
        LSL rd -> lsl flags mutRegisters sp mutMemory rd
        LSR rd -> lsr flags mutRegisters sp mutMemory rd
        MOV rd rs -> mov flags mutRegisters sp mutMemory rd rs
        MOVW rdh rdl rrh rrl -> movw flags mutRegisters sp mutMemory rdh rdl rrh rrl
        MUL rd rs -> mul flags mutRegisters sp mutMemory rd rs
        MULS rd rs -> muls flags mutRegisters sp mutMemory rd rs
        NEG rd -> neg flags mutRegisters sp mutMemory rd
        NOP -> return (flags, 0, sp)
        OR rd rr -> orInstr flags mutRegisters sp mutMemory rd rr
        ORI rd k -> ori flags mutRegisters sp mutMemory rd k
        POP rd -> pop flags mutRegisters sp mutMemory rd
        PUSH rr -> push flags mutRegisters sp mutMemory rr
        RET -> ret flags mutRegisters sp mutMemory pc
        ROL rd -> rol flags mutRegisters sp mutMemory rd
        ROR rd -> ror flags mutRegisters sp mutMemory rd
        SBC rd rr -> sbc flags mutRegisters sp mutMemory rd rr
        SBRC rd b -> sbrc flags mutRegisters sp mutMemory rd b
        SBRS rd b -> sbrs flags mutRegisters sp mutMemory rd b
        SEC -> sec flags mutRegisters sp mutMemory
        SEH -> seh flags mutRegisters sp mutMemory
        SEI -> sei flags mutRegisters sp mutMemory
        SEN -> sen flags mutRegisters sp mutMemory
        SER rd -> ser flags mutRegisters sp mutMemory rd
        SES -> ses flags mutRegisters sp mutMemory
        SET -> set flags mutRegisters sp mutMemory
        SEV -> sev flags mutRegisters sp mutMemory
        SEZ -> sez flags mutRegisters sp mutMemory
        ST xregister rr -> st flags mutRegisters sp mutMemory xregister rr
        STS k rr -> sts flags mutRegisters sp mutMemory k rr
        SUB rd rr -> sub flags mutRegisters sp mutMemory rd rr
        SUBI rd k -> subi flags mutRegisters sp mutMemory rd k
        SWAP rd -> swap flags mutRegisters sp mutMemory rd
        TST rd -> tst flags mutRegisters sp mutMemory rd
    return (updatedFlags, pc + fromIntegral relativeJump + 1, updatedSp)

initEmulatorState :: Int -> EmulatorState
initEmulatorState memorySize = EmulatorState {
        registers = V.replicate 32 0,                        -- Initialize all registers to 0
        flags = StatusFlags False False False False False False False False,  -- Initialize all status flags to False
        programCounter = 0,                                                   -- Program counter starts executing from 0x0000
        memory = V.replicate (memorySize) 0,      -- Initialize the memory with the requested size, set to 0
        sp = fromIntegral (memorySize - 1) :: Word16                          -- The stack pointer should point to the last memory address
    }