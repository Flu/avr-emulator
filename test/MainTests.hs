import Test.Hspec
import Parser
import Data.Array
import qualified Data.Vector as V
import Emulator

-- Takes the path of a program and parses it into an intermediary form that the Emulator can understand
-- If parsing fails, returns Nothing
assembleProgramFromFile :: FilePath -> IO (Maybe [Instruction])
assembleProgramFromFile filename = do
    contents <- readFile filename
    case parseAssembly contents of
        Left err -> do
            print err
            return Nothing
        Right instructions -> return (Just instructions)

-- Takes the path of a file, parses, assembles and executes the file
-- It also takes a checking function that checks the emulator state after execution
emulateProgramFromFile :: FilePath -> (EmulatorState -> IO ()) -> IO ()
emulateProgramFromFile filepath checkingFunction = do
    -- Parse assembly file
    maybeInstructions <- assembleProgramFromFile filepath
    case maybeInstructions of
        Just instructions -> do
            -- Emulate the parsed program
            let finalState = run instructions 2000
            prettyPrintMemory $ memory finalState            -- Pretty print the memory
            printRegisterBank $ registers finalState         -- Pretty print the register banks
            putStrLn (showStatusFlags $ flags finalState)    -- Print the final status flags
            -- Check that the emulator returned the expected state
            checkingFunction finalState
        Nothing -> putStrLn "Error assembling the program."

-- Main function for testing
main :: IO ()
main = hspec $ describe "AVR Emulator E2E tests" $ do
    -- Test 0: test_adiw.asm
    it "should correctly emulate test_adiw.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_adiw.asm"
        emulateProgramFromFile assemblyFilePath testAdiw

    -- Test 1: test_fibonacci.asm
    it "should correctly emulate fibonacci.asm and match expected state" $ do
        let assemblyFilePath = "test_files/fibonacci.asm"
        emulateProgramFromFile assemblyFilePath testFibonacci

    -- Test 2: test_asr.asm
    it "should correctly emulate test_asr.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_asr.asm"
        emulateProgramFromFile assemblyFilePath testAsr

    -- Test 3: test_brlo.asm
    it "should correctly emulate test_brlo.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_brlo.asm"
        emulateProgramFromFile assemblyFilePath testBrlo

    -- Test 4: test_brmi.asm
    it "should correctly emulate test_brmi.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_brmi.asm"
        emulateProgramFromFile assemblyFilePath testBrmi

    -- Test 5: test_call.asm
    it "should correctly emulate test_call.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_call.asm"
        emulateProgramFromFile assemblyFilePath testCall

    -- Test 6: test_clc.asm
    it "should correctly emulate test_clc.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_clc.asm"
        emulateProgramFromFile assemblyFilePath testClc

    -- Test 7: test_clr.asm
    it "should correctly emulate test_clr.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_clr.asm"
        emulateProgramFromFile assemblyFilePath testClr

    -- Test 8: test_clearing.asm
    it "should correctly emulate test_clearing.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_clearing.asm"
        emulateProgramFromFile assemblyFilePath testClearingFlags

    -- Test 9: test_clr.asm
    it "should correctly emulate test_eor.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_eor.asm"
        emulateProgramFromFile assemblyFilePath testEor

    -- Test 10: test_movw.asm
    it "should correctly emulate test_movw.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_movw.asm"
        emulateProgramFromFile assemblyFilePath testMovw

    -- Test 11: test_mul.asm
    it "should correctly emulate test_mul.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_mul.asm"
        emulateProgramFromFile assemblyFilePath testMul

    -- Test 12: test_or.asm
    it "should correctly emulate test_or.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_or.asm"
        emulateProgramFromFile assemblyFilePath testOr

    -- Test 13: test_push.asm
    it "should correctly emulate test_push.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_push.asm"
        emulateProgramFromFile assemblyFilePath testPush

    -- Test 14: test_rol_ror.asm
    it "should correctly emulate test_rol_ror.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_rol_ror.asm"
        emulateProgramFromFile assemblyFilePath testRolRor

    -- Test 15: test_sbc.asm
    it "should correctly emulate test_sbc.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_sbc.asm"
        emulateProgramFromFile assemblyFilePath testSbc

    -- Test 16: test_sbrs.asm
    it "should correctly emulate test_sbrs.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_sbrs.asm"
        emulateProgramFromFile assemblyFilePath testSbrs

    -- Test 17: test_setting.asm
    it "should correctly emulate test_setting.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_setting.asm"
        emulateProgramFromFile assemblyFilePath testSettingFlags

    -- Test 18: test_subi.asm
    it "should correctly emulate test_subi.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_subi.asm"
        emulateProgramFromFile assemblyFilePath testSubi

    -- Test 19: test_swap.asm
    it "should correctly emulate test_swap.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_swap.asm"
        emulateProgramFromFile assemblyFilePath testSwap

    -- Test 20: test_tst.asm
    it "should correctly emulate test_tst.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_tst.asm"
        emulateProgramFromFile assemblyFilePath testTst

    -- Test 22: bubblesort.asm
    it "should correctly emulate bubblesort.asm and match expected state" $ do
        let assemblyFilePath = "test_files/bubblesort.asm"
        emulateProgramFromFile assemblyFilePath testBubblesort

    -- Test 23: test_eof.asm
    it "should correctly emulate test_eof.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_eof.asm"
        emulateProgramFromFile assemblyFilePath testEof

    -- Test 24: test_bclr.asm
    it "should correctly emulate test_bclr.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_bclr.asm"
        emulateProgramFromFile assemblyFilePath testBclr

    -- Test 25: test_bld.asm
    it "should correctly emulate test_bld.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_bld.asm"
        emulateProgramFromFile assemblyFilePath testBld

    -- Test 26: test_ld.asm
    it "should correctly emulate test_ld.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_ld.asm"
        emulateProgramFromFile assemblyFilePath testLd

    -- Test 27: test_labeled_registers.asm
    it "Should correctly emulate test_labeled_registers.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_labelled_registers.asm"
        emulateProgramFromFile assemblyFilePath testLabeledRegisters

    -- Test 27: test_ld.asm
    it "Should correctly emulate test_ld.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_ld.asm"
        emulateProgramFromFile assemblyFilePath testLd

-- Checking EmulatorState for the "test_adiw.asm"
testAdiw :: EmulatorState -> IO ()
testAdiw state = do
    -- Registers
    let regValue = registers state ! 26
    regValue `shouldBe` 0xff

    let regValue = registers state ! 27
    regValue `shouldBe` 0xbe

    let regValue = registers state ! 30
    regValue `shouldBe` 0x10

    let regValue = registers state ! 31
    regValue `shouldBe` 0x00

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` True
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` True
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False

testBld:: EmulatorState -> IO()
testBld state = do
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` True
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False

    let regValue = registers state ! 31
    regValue `shouldBe` 0xF0

testBclr :: EmulatorState -> IO ()
testBclr state = do
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "fibonacci.asm"
testFibonacci :: EmulatorState -> IO ()
testFibonacci state = do
    -- Registers
    let regValue = registers state ! 15
    regValue `shouldBe` 0xe

    let regValue = registers state ! 16
    regValue `shouldBe` 0xe9

    let regValue = registers state ! 17
    regValue `shouldBe` 0x90

    let regValue = registers state ! 18
    regValue `shouldBe` 0xe9

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` True
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "test_asr.asm"
testAsr :: EmulatorState -> IO ()
testAsr state = do
    -- Registers
    let regValue = registers state ! 16
    regValue `shouldBe` 0x4

    let regValue = registers state ! 17
    regValue `shouldBe` 0xfe

    let regValue = registers state ! 18
    regValue `shouldBe` 0xfc

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` True
    negativeFlag (flags state) `shouldBe` True
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False


-- Checking EmulatorState for the "test_brlo.asm"
testBrlo :: EmulatorState -> IO ()
testBrlo state = do
    -- Registers
    let regValue = registers state ! 16
    regValue `shouldBe` 0x00

    let regValue = registers state ! 17
    regValue `shouldBe` 0x00

    let regValue = registers state ! 19
    regValue `shouldBe` 0x10

    -- Program counter
    programCounter state `shouldBe` 6

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` True
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "test_brmi.asm"
testBrmi :: EmulatorState -> IO ()
testBrmi state = do
    -- Registers
    let regValue = registers state ! 16
    regValue `shouldBe` 0x1

    let regValue = registers state ! 17
    regValue `shouldBe` 0x1

    let regValue = registers state ! 18
    regValue `shouldBe` 0xf

    -- Program counter
    programCounter state `shouldBe` 7

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "test_call.asm"
testCall :: EmulatorState -> IO ()
testCall state = do
    -- Registers
    let regValue = registers state ! 0
    regValue `shouldBe` 0x1

    let regValue = registers state ! 16
    regValue `shouldBe` 0x1

    let regValue = registers state ! 31
    regValue `shouldBe` 0x1

    -- Program counter
    programCounter state `shouldBe` 29

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` True
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "test_clc.asm"
testClc :: EmulatorState -> IO ()
testClc state = do
    -- Registers
    let regValue = registers state ! 16
    regValue `shouldBe` 0xfe

    -- Program counter
    programCounter state `shouldBe` 4

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` True
    signFlag (flags state) `shouldBe` True
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` True
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "test_clearing.asm"
testClearingFlags :: EmulatorState -> IO ()
testClearingFlags state = do
    -- Registers
    let regValue = registers state ! 18
    regValue `shouldBe` 0x00

    -- Program counter
    programCounter state `shouldBe` 18

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "test_clr.asm"
testClr :: EmulatorState -> IO ()
testClr state = do
    -- Registers
    let regValue = registers state ! 16
    regValue `shouldBe` 0x0

    -- Program counter
    programCounter state `shouldBe` 3

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` True
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "test_eor.asm"
testEor :: EmulatorState -> IO ()
testEor state = do
    -- Registers
    let regValue = registers state ! 16
    regValue `shouldBe` 0x63

    let regValue = registers state ! 17
    regValue `shouldBe` 0x1b

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "test_labeled_registers.asm"
testLabeledRegisters :: EmulatorState -> IO ()
testLabeledRegisters state = do
    -- Registers
    let regValue = registers state ! 26
    regValue `shouldBe` 0x01

    let regValue = registers state ! 27
    regValue `shouldBe` 0x02

    let regValue = registers state ! 28
    regValue `shouldBe` 0x03

    let regValue = registers state ! 29
    regValue `shouldBe` 0x04

    let regValue = registers state ! 30
    regValue `shouldBe` 0x05

    let regValue = registers state ! 31
    regValue `shouldBe` 0x06

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "test_ld.asm"
testLd :: EmulatorState -> IO ()
testLd state = do
    -- Registers
    let regValue = registers state ! 0
    regValue `shouldBe`  0x05

    let regValue = registers state ! 1
    regValue `shouldBe` 0x05

    let regValue = registers state ! 2
    regValue `shouldBe` 0x06

    let regValue = registers state ! 4
    regValue `shouldBe`  0x15

    let regValue = registers state ! 5
    regValue `shouldBe` 0x15

    let regValue = registers state ! 6
    regValue `shouldBe` 0x16

    let regValue = registers state ! 8
    regValue `shouldBe`  0x25

    let regValue = registers state ! 9
    regValue `shouldBe` 0x25

    let regValue = registers state ! 10
    regValue `shouldBe` 0x26

    -- Memory
    let memValue = memory state ! 5
    memValue `shouldBe` 0x05

    let memValue = memory state ! 6
    memValue `shouldBe` 0x06

    let memValue = memory state ! 21
    memValue `shouldBe` 0x15

    let memValue = memory state ! 22
    memValue `shouldBe` 0x16

    let memValue = memory state ! 37
    memValue `shouldBe` 0x25

    let memValue = memory state ! 38
    memValue `shouldBe` 0x26

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` True
    carryFlag (flags state) `shouldBe` False


-- Checking EmulatorState for the "test_movw.asm"
testMovw :: EmulatorState -> IO ()
testMovw state = do
    -- Registers
    let regValue = registers state ! 16
    regValue `shouldBe` 0xbe

    let regValue = registers state ! 17
    regValue `shouldBe` 0xef

    let regValue = registers state ! 25
    regValue `shouldBe` 0xbe

    let regValue = registers state ! 26
    regValue `shouldBe` 0xef

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "test_mul.asm"
testMul :: EmulatorState -> IO ()
testMul state = do
    -- Registers
    let regValue = registers state ! 0
    regValue `shouldBe` 0xc

    let regValue = registers state ! 5
    regValue `shouldBe` 0x2

    let regValue = registers state ! 6
    regValue `shouldBe` 0x6

    let regValue = registers state ! 16
    regValue `shouldBe` 0x15

    let regValue = registers state ! 17
    regValue `shouldBe` 0x1c

    let regValue = registers state ! 25
    regValue `shouldBe` 0x00

    let regValue = registers state ! 26
    regValue `shouldBe` 0x00

    -- Program counter
    programCounter state `shouldBe` 12

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` True
    signFlag (flags state) `shouldBe` True
    overflowFlag (flags state) `shouldBe` True
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "test_or.asm"
testOr :: EmulatorState -> IO ()
testOr state = do
    -- Registers
    let regValue = registers state ! 16
    regValue `shouldBe` 0xff

    let regValue = registers state ! 17
    regValue `shouldBe` 0xef

    let regValue = registers state ! 18
    regValue `shouldBe` 0xff

    -- Program counter
    programCounter state `shouldBe` 5

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` True
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` True
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "test_push.asm"
testPush :: EmulatorState -> IO ()
testPush state = do
    -- Registers
    let regValue = registers state ! 16
    regValue `shouldBe` 0x1

    let regValue = registers state ! 17
    regValue `shouldBe` 0x2

    let regValue = registers state ! 18
    regValue `shouldBe` 0x2

    -- Program counter
    programCounter state `shouldBe` 6

    -- Memory
    let topOfStack = memory state ! fromIntegral (sp state)
    topOfStack `shouldBe` 0x2

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "test_rol_ror.asm"
testRolRor :: EmulatorState -> IO ()
testRolRor state = do
    -- Registers
    let regValue = registers state ! 16
    regValue `shouldBe` 0x4

    let regValue = registers state ! 17
    regValue `shouldBe` 0x66

    let regValue = registers state ! 18
    regValue `shouldBe` 0xd5

    let regValue = registers state ! 19
    regValue `shouldBe` 0x7

    -- Program counter
    programCounter state `shouldBe` 8

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` True
    overflowFlag (flags state) `shouldBe` True
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` True

-- Checking EmulatorState for the "test_sbc.asm"
testSbc :: EmulatorState -> IO ()
testSbc state = do
    -- Registers
    let regValue = registers state ! 0
    regValue `shouldBe` 0x3

    let regValue = registers state ! 1
    regValue `shouldBe` 0x11

    let regValue = registers state ! 2
    regValue `shouldBe` 0x38

    let regValue = registers state ! 3
    regValue `shouldBe` 0xcc

    -- Program counter
    programCounter state `shouldBe` 6

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` True
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` True
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "test_sbrs.asm"
testSbrs :: EmulatorState -> IO ()
testSbrs state = do
    -- Registers
    let regValue = registers state ! 16
    regValue `shouldBe` 0x33

    let regValue = registers state ! 17
    regValue `shouldBe` 0x7

    let regValue = registers state ! 18
    regValue `shouldBe` 0xf0

    -- Program counter
    programCounter state `shouldBe` 6

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "test_setting.asm"
testSettingFlags :: EmulatorState -> IO ()
testSettingFlags state = do
    -- Registers
    let regValue = registers state ! 18
    regValue `shouldBe` 0xff

    -- Program counter
    programCounter state `shouldBe` 9

    -- Status flags
    interruptFlag (flags state) `shouldBe` True
    tFlag (flags state) `shouldBe` True
    halfCarryFlag (flags state) `shouldBe` True
    signFlag (flags state) `shouldBe` True
    overflowFlag (flags state) `shouldBe` True
    negativeFlag (flags state) `shouldBe` True
    zeroFlag (flags state) `shouldBe` True
    carryFlag (flags state) `shouldBe` True

-- Checking EmulatorState for the "test_subi.asm"
testSubi :: EmulatorState -> IO ()
testSubi state = do
    -- Registers
    let regValue = registers state ! 17
    regValue `shouldBe` 0x45

    -- Program counter
    programCounter state `shouldBe` 7

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` True
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "test_swap.asm"
testSwap :: EmulatorState -> IO ()
testSwap state = do
    let regValue = registers state ! 16
    regValue `shouldBe` 0xeb

    let regValue = registers state ! 17
    regValue `shouldBe` 0x41

    let regValue = registers state ! 18
    regValue `shouldBe` 0xff

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` True
    signFlag (flags state) `shouldBe` True
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` True
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` True

-- Checking EmulatorState for the "test_tst.asm"
testTst :: EmulatorState -> IO ()
testTst state = do
    -- Registers
    let regValue = registers state ! 16
    regValue `shouldBe` 0xfc

    -- Program counter
    programCounter state `shouldBe` 2

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` True
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` True
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "bubblesort.asm"
testBubblesort :: EmulatorState -> IO ()
testBubblesort state = do
    -- Memory
    -- Define the expected array
    let sliceOfMemory = V.slice 0 256 $ V.fromList $ elems (memory state)
    let expectedMemory = array (0,255) [(0,0),(1,2),(2,2),(3,2),(4,2),(5,3),(6,3),(7,3),(8,3),(9,4),(10,4),(11,4),(12,4),(13,4),(14,5),(15,5),(16,5),(17,5),(18,7),(19,7),(20,7),(21,7),(22,7),(23,8),(24,8),(25,8),(26,8),(27,9),(28,9),(29,9),(30,9),(31,10),(32,10),(33,10),(34,10),(35,13),(36,13),(37,13),(38,13),(39,13),(40,15),(41,15),(42,15),(43,15),(44,17),(45,17),(46,17),(47,17),(48,19),(49,19),(50,19),(51,19),(52,22),(53,24),(54,24),(55,24),(56,24),(57,25),(58,25),(59,25),(60,25),(61,25),(62,26),(63,26),(64,26),(65,26),(66,26),(67,28),(68,28),(69,28),(70,28),(71,28),(72,29),(73,29),(74,29),(75,29),(76,33),(77,33),(78,33),(79,33),(80,36),(81,36),(82,36),(83,36),(84,36),(85,37),(86,37),(87,37),(88,37),(89,38),(90,38),(91,38),(92,38),(93,38),(94,42),(95,42),(96,42),(97,42),(98,42),(99,44),(100,44),(101,44),(102,44),(103,44),(104,47),(105,47),(106,47),(107,47),(108,48),(109,48),(110,48),(111,48),(112,49),(113,49),(114,49),(115,49),(116,49),(117,51),(118,51),(119,51),(120,51),(121,51),(122,52),(123,52),(124,52),(125,52),(126,55),(127,55),(128,55),(129,55),(130,55),(131,57),(132,57),(133,57),(134,57),(135,62),(136,62),(137,62),(138,62),(139,62),(140,64),(141,64),(142,64),(143,64),(144,64),(145,65),(146,65),(147,65),(148,65),(149,68),(150,68),(151,68),(152,68),(153,71),(154,71),(155,71),(156,71),(157,71),(158,72),(159,72),(160,72),(161,72),(162,72),(163,73),(164,73),(165,73),(166,73),(167,75),(168,75),(169,75),(170,75),(171,75),(172,78),(173,80),(174,80),(175,80),(176,80),(177,80),(178,83),(179,83),(180,83),(181,83),(182,83),(183,86),(184,86),(185,86),(186,86),(187,86),(188,87),(189,87),(190,87),(191,87),(192,87),(193,90),(194,90),(195,90),(196,90),(197,90),(198,93),(199,93),(200,93),(201,93),(202,95),(203,95),(204,95),(205,95),(206,97),(207,97),(208,97),(209,97),(210,97),(211,98),(212,101),(213,101),(214,101),(215,101),(216,101),(217,103),(218,103),(219,103),(220,103),(221,104),(222,104),(223,104),(224,104),(225,109),(226,109),(227,109),(228,109),(229,109),(230,110),(231,110),(232,110),(233,110),(234,110),(235,113),(236,113),(237,113),(238,113),(239,114),(240,114),(241,114),(242,114),(243,114),(244,116),(245,123),(246,123),(247,123),(248,123),(249,123),(250,124),(251,124),(252,124),(253,124),(254,124),(255,127)]

    let expectedVector = V.fromList $ elems expectedMemory
    -- Compare the elements
    sliceOfMemory `shouldBe` expectedVector

    -- Registers
    let regValue = registers state ! 8
    regValue `shouldBe` 0xff

    let regValue = registers state ! 9
    regValue `shouldBe` 0x00

    let regValue = registers state ! 16
    regValue `shouldBe` 0x5b

    let regValue = registers state ! 17
    regValue `shouldBe` 0x45

    let regValue = registers state ! 18
    regValue `shouldBe` 0x1

    -- Program counter
    programCounter state `shouldBe` 93

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` True
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "test_eof.asm"
testEof :: EmulatorState -> IO ()
testEof state = do
    -- Program counter
    programCounter state `shouldBe` 12

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` True
    signFlag (flags state) `shouldBe` True
    overflowFlag (flags state) `shouldBe` True
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False