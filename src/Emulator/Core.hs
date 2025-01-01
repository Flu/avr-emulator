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
        ADC rd rs -> adc mutRegisters mutMemory flags sp rd rs
        ADD rd rs -> add mutRegisters mutMemory flags sp rd rs
        ADIW rdh rdl immediate -> adiw mutRegisters mutMemory flags sp rdh rdl immediate
        AND rd rr -> andInstr mutRegisters mutMemory flags sp rd rr
        ANDI rd k -> andi mutRegisters mutMemory flags sp rd k
        ASR rd -> asr mutRegisters mutMemory flags sp rd
        BCLR s -> bclr flags sp s
        BLD rd b -> bld mutRegisters mutMemory flags sp rd b
        BRCCR relativeAddress -> brcc flags sp relativeAddress
        BRCSR relativeAddress -> brcs flags sp relativeAddress
        BREQR relativeAddress -> breq flags sp relativeAddress
        BRGER relativeAddress -> brge flags sp relativeAddress
        BRHCR relativeAddress -> brhc flags sp relativeAddress
        BRHSR relativeAddress -> brhs flags sp relativeAddress
        BRIDR relativeAddress -> brid flags sp relativeAddress
        BRIER relativeAddress -> brie flags sp relativeAddress
        BRLOR relativeAddress -> brlo flags sp relativeAddress
        BRLTR relativeAddress -> brlt flags sp relativeAddress
        BRMIR relativeAddress -> brmi flags sp relativeAddress
        BRNER relativeAddress -> brne flags sp relativeAddress
        BRPLR relativeAddress -> brpl flags sp relativeAddress
        BRSHR relativeAddress -> brsh flags sp relativeAddress
        BRTCR relativeAddress -> brtc flags sp relativeAddress
        BRTSR relativeAddress -> brts flags sp relativeAddress
        BRVCR relativeAddress -> brvc flags sp relativeAddress
        BRVSR relativeAddress -> brvs flags sp relativeAddress
        CALLR relativeAddress -> call mutRegisters mutMemory flags sp relativeAddress pc
        CBR rd k -> cbr mutRegisters mutMemory flags sp rd k
        CLC -> clc flags sp
        CLH -> clh flags sp
        CLI -> cli flags sp
        CLN -> cln flags sp
        CLR rd -> clr mutRegisters mutMemory flags sp rd
        CLS -> cls flags sp
        CLT -> clt flags sp
        CLV -> clv flags sp
        CLZ -> clz flags sp
        COM rd -> com mutRegisters mutMemory flags sp rd
        CP rd rr -> cp mutRegisters flags sp rd rr
        CPC rd rr -> cpc mutRegisters flags sp rd rr
        CPI rd k -> cpi mutRegisters flags sp rd k
        CPSE rd rr -> cpse mutRegisters flags sp rd rr
        DEC rd -> dec mutRegisters mutMemory flags sp rd
        EOR rd rr -> eor mutRegisters mutMemory flags sp rd rr
        INC rd -> inc mutRegisters mutMemory flags sp rd
        JMPR relativeAddress -> jmp flags sp relativeAddress
        LD rd xregister -> ld mutRegisters mutMemory flags sp rd xregister
        LABEL label -> return (flags, 0, sp)
        LDI rd immediate -> ldi mutRegisters mutMemory flags sp rd immediate
        LDS rd k -> lds mutRegisters mutMemory flags sp rd k
        LSL rd -> lsl mutRegisters mutMemory flags sp rd
        LSR rd -> lsr mutRegisters mutMemory flags sp rd
        MOV rd rs -> mov mutRegisters mutMemory flags sp rd rs
        MOVW rdh rdl rrh rrl -> movw mutRegisters mutMemory flags sp rdh rdl rrh rrl
        MUL rd rs -> mul mutRegisters mutMemory flags sp rd rs
        MULS rd rs -> muls mutRegisters mutMemory flags sp rd rs
        NEG rd -> neg mutRegisters mutMemory flags sp rd
        NOP -> return (flags, 0, sp)
        OR rd rr -> orInstr mutRegisters mutMemory flags sp rd rr
        ORI rd k -> ori mutRegisters mutMemory flags sp rd k
        POP rd -> pop mutRegisters mutMemory flags sp rd
        PUSH rr -> push mutRegisters mutMemory flags sp rr
        RET -> ret mutRegisters mutMemory flags sp pc
        ROL rd -> rol mutRegisters mutMemory flags sp rd
        ROR rd -> ror mutRegisters mutMemory flags sp rd
        SBC rd rr -> sbc mutRegisters mutMemory flags sp rd rr
        SBRC rd b -> sbrc mutRegisters flags sp rd b
        SBRS rd b -> sbrs mutRegisters flags sp rd b
        SEC -> sec flags sp
        SEH -> seh flags sp
        SEI -> sei flags sp
        SEN -> sen flags sp
        SER rd -> ser mutRegisters mutMemory flags sp rd
        SES -> ses flags sp
        SET -> set flags sp
        SEV -> sev flags sp
        SEZ -> sez flags sp
        ST xregister rr -> st mutRegisters mutMemory flags sp xregister rr
        STS k rr -> sts mutRegisters mutMemory flags sp k rr
        SUB rd rr -> sub mutRegisters mutMemory flags sp rd rr
        SUBI rd k -> subi mutRegisters mutMemory flags sp rd k
        SWAP rd -> swap mutRegisters mutMemory flags sp rd
        TST rd -> tst mutRegisters flags sp rd
    return (updatedFlags, pc + fromIntegral relativeJump + 1, updatedSp)

initEmulatorState :: Int -> EmulatorState
initEmulatorState memorySize = EmulatorState {
        registers = V.replicate 32 0,                        -- Initialize all registers to 0
        flags = StatusFlags False False False False False False False False,  -- Initialize all status flags to False
        programCounter = 0,                                                   -- Program counter starts executing from 0x0000
        memory = V.replicate (memorySize) 0,      -- Initialize the memory with the requested size, set to 0
        sp = fromIntegral (memorySize - 1) :: Word16                          -- The stack pointer should point to the last memory address
    }