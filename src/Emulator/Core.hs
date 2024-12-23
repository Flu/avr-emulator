module Emulator.Core where

import Emulator.Instructions
import Emulator.State

import Data.Array
import Data.Binary (Word8, Word16)

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

initEmulatorState :: Int -> EmulatorState
initEmulatorState memorySize = EmulatorState {
        registers = listArray (0,31) (replicate 32 0),                        -- Initialize all registers to 0
        flags = StatusFlags False False False False False False False False,  -- Initialize all status flags to False
        programCounter = 0,                                                   -- Program counter starts executing from 0x0000
        memory = listArray (0, memorySize - 1) (replicate memorySize 0),      -- Initialize the memory with the requested size, set to 0
        sp = fromIntegral (memorySize - 1) :: Word16                          -- The stack pointer should point to the last memory address
        }