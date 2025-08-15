import Test.Hspec
import Parser
import Data.Array (array, Array)
import qualified Data.Vector as V
import Data.Vector((!))
import Emulator
import Data.Bits (shiftL)
import Data.Binary (Word8)

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
    maybeInstructions `shouldNotBe` Nothing
    case maybeInstructions of
        Just instructions -> do
            -- Emulate the parsed program
            let finalState = run instructions 600            -- Restrict memory to 600 bytes to make tests faster and less resource intensive
            prettyPrintMemory $ memory finalState            -- Pretty print the memory
            printRegisterBank $ registers finalState         -- Pretty print the register banks
            putStrLn (showStatusFlags $ flags finalState)    -- Print the final status flags
            -- Check that the emulator returned the expected state
            checkingFunction finalState
        Nothing -> putStrLn "Error assembling the program."

-- Main function for testing
main :: IO ()
main = hspec $ describe "AVR Emulator E2E tests" $ do
    -- array_merge.asm
    it "should correctly merge two arrays and match the expected final state" $ do
        let assemblyFilePath = "test_files/array_merge.asm"
        emulateProgramFromFile assemblyFilePath testArrayMerge

    -- collatz.asm
    it "should correctly calculate how many steps it takes when applying the collatz function to a given number" $ do
        let assemblyFilePath = "test_files/collatz.asm"
        emulateProgramFromFile assemblyFilePath testCollatz

    -- factorial.asm
    it "should correctly calculate the factorial to a given number" $ do
        let assemblyFilePath = "test_files/factorial.asm"
        emulateProgramFromFile assemblyFilePath testFactorial

    -- find_maximum.asm
    it "should correctly find the maximum in an array" $ do
        let assemblyFilePath = "test_files/find_maximum.asm"
        emulateProgramFromFile assemblyFilePath testFindMaximum

    -- test_adiw.asm
    it "should correctly emulate test_adiw.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_adiw.asm"
        emulateProgramFromFile assemblyFilePath testAdiw

    -- test_fibonacci.asm
    it "should correctly emulate fibonacci.asm and match expected state" $ do
        let assemblyFilePath = "test_files/fibonacci.asm"
        emulateProgramFromFile assemblyFilePath testFibonacci

    -- test_asr.asm
    it "should correctly emulate test_asr.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_asr.asm"
        emulateProgramFromFile assemblyFilePath testAsr

    -- test_brlo.asm
    it "should correctly emulate test_brlo.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_brlo.asm"
        emulateProgramFromFile assemblyFilePath testBrlo

    -- test_brmi.asm
    it "should correctly emulate test_brmi.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_brmi.asm"
        emulateProgramFromFile assemblyFilePath testBrmi

    -- test_call.asm
    it "should correctly emulate test_call.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_call.asm"
        emulateProgramFromFile assemblyFilePath testCall

    -- test_clc.asm
    it "should correctly emulate test_clc.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_clc.asm"
        emulateProgramFromFile assemblyFilePath testClc

    -- test_clr.asm
    it "should correctly emulate test_clr.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_clr.asm"
        emulateProgramFromFile assemblyFilePath testClr

    -- test_com.asm
    it "should correctly emulate test_com.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_com.asm"
        emulateProgramFromFile assemblyFilePath testCom

    -- CP instruction
    it "should correctly emulate the CP instruction and match expected state" $ do
        let assemblyFilePath = "test_files/test_cp1.asm"
        emulateProgramFromFile assemblyFilePath testCp1

        let assemblyFilePath = "test_files/test_cp2.asm"
        emulateProgramFromFile assemblyFilePath testCp2

        let assemblyFilePath = "test_files/test_cp3.asm"
        emulateProgramFromFile assemblyFilePath testCp3

    -- CPC instruction
    it "should correctly emulate the CPC instruction and match expected state" $ do
        let assemblyFilePath = "test_files/test_cpc1.asm"
        emulateProgramFromFile assemblyFilePath testCpc1

        let assemblyFilePath = "test_files/test_cpc2.asm"
        emulateProgramFromFile assemblyFilePath testCpc2

        let assemblyFilePath = "test_files/test_cpc3.asm"
        emulateProgramFromFile assemblyFilePath testCpc3

    -- CPI instruction
    it "should correctly emulate the CPI instruction and match expected state" $ do
        let assemblyFilePath = "test_files/test_cpi1.asm"
        emulateProgramFromFile assemblyFilePath testCpi1

        let assemblyFilePath = "test_files/test_cpi2.asm"
        emulateProgramFromFile assemblyFilePath testCpi2

        let assemblyFilePath = "test_files/test_cpi3.asm"
        emulateProgramFromFile assemblyFilePath testCpi3

    -- CPSE instruction
    it "should correctly emulate the CPSE instruction and match expected state" $ do
        let assemblyFilePath = "test_files/test_cpse1.asm"
        emulateProgramFromFile assemblyFilePath testCpse1

        let assemblyFilePath = "test_files/test_cpse2.asm"
        emulateProgramFromFile assemblyFilePath testCpse2

    -- test_clearing.asm
    it "should correctly emulate test_clearing.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_clearing.asm"
        emulateProgramFromFile assemblyFilePath testClearingFlags

    -- test_clr.asm
    it "should correctly emulate test_eor.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_eor.asm"
        emulateProgramFromFile assemblyFilePath testEor

    -- test_movw.asm
    it "should correctly emulate test_movw.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_movw.asm"
        emulateProgramFromFile assemblyFilePath testMovw

    -- test_mul.asm
    it "should correctly emulate test_mul.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_mul.asm"
        emulateProgramFromFile assemblyFilePath testMul

    -- test_or.asm
    it "should correctly emulate test_or.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_or.asm"
        emulateProgramFromFile assemblyFilePath testOr

    -- test_push.asm
    it "should correctly emulate test_push.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_push.asm"
        emulateProgramFromFile assemblyFilePath testPush

    -- test_rol_ror.asm
    it "should correctly emulate test_rol_ror.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_rol_ror.asm"
        emulateProgramFromFile assemblyFilePath testRolRor

    -- test_sbc.asm
    it "should correctly emulate test_sbc.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_sbc.asm"
        emulateProgramFromFile assemblyFilePath testSbc

    -- test_sbrs.asm
    it "should correctly emulate test_sbrs.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_sbrs.asm"
        emulateProgramFromFile assemblyFilePath testSbrs

    -- test_setting.asm
    it "should correctly emulate test_setting.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_setting.asm"
        emulateProgramFromFile assemblyFilePath testSettingFlags

    -- test_subi.asm
    it "should correctly emulate test_subi.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_subi.asm"
        emulateProgramFromFile assemblyFilePath testSubi

    -- test_swap.asm
    it "should correctly emulate test_swap.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_swap.asm"
        emulateProgramFromFile assemblyFilePath testSwap

    -- test_tst.asm
    it "should correctly emulate test_tst.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_tst.asm"
        emulateProgramFromFile assemblyFilePath testTst

    -- bubblesort.asm
    it "should correctly emulate bubblesort.asm and match expected state" $ do
        let assemblyFilePath = "test_files/bubblesort.asm"
        emulateProgramFromFile assemblyFilePath testBubblesort

    -- test_eof.asm
    it "should correctly emulate test_eof.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_eof.asm"
        emulateProgramFromFile assemblyFilePath testEof

    -- test_bclr.asm
    it "should correctly emulate test_bclr.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_bclr.asm"
        emulateProgramFromFile assemblyFilePath testBclr

    -- test_bld.asm
    it "should correctly emulate test_bld.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_bld.asm"
        emulateProgramFromFile assemblyFilePath testBld

    -- test_ld.asm
    it "should correctly emulate test_ld.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_ld.asm"
        emulateProgramFromFile assemblyFilePath testLd

    -- test_labeled_registers.asm
    it "Should correctly emulate test_labeled_registers.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_labeled_registers.asm"
        emulateProgramFromFile assemblyFilePath testLabeledRegisters

    -- test_ld.asm
    it "Should correctly emulate test_ld.asm and match expected state" $ do
        let assemblyFilePath = "test_files/test_ld.asm"
        emulateProgramFromFile assemblyFilePath testLd

    -- test_xch.asm
    it "Should correctly emulate test_xch.asm and match expected state" $ do
      let assemblyFilePath = "test_files/test_xch.asm"
      emulateProgramFromFile assemblyFilePath testXch

    -- test_bset.asm
    it "Should correctly emulate test_bset.asm and match expected state" $ do
      let assemblyFilePath = "test_files/test_bset.asm"
      emulateProgramFromFile assemblyFilePath testBset

    -- test_sbr.asm
    it "Should correctly emulate test_sbr.asm and match expected state" $ do
      let assemblyFilePath = "test_files/test_sbr.asm"
      emulateProgramFromFile assemblyFilePath testSbr

-- Checking emulator state for array_merge.asm
testArrayMerge :: EmulatorState -> IO ()
testArrayMerge state = do
    -- Memory
    let firstArray = V.slice 256 17 (memory state)
    let firstArrayExpected = V.fromList [0x01, 0x14, 0x20, 0x23, 0x24, 0x25, 0x3b, 0x44, 0x5c, 0x7b, 0x82, 0x84, 0xa9, 0xaf, 0xb1, 0xb2, 0xb3]
    firstArray `shouldBe` firstArrayExpected

    let secondArray = V.slice 336 21 (memory state)
    let secondArrayExpected = V.fromList [0x02, 0x0a, 0x2e, 0x2f, 0x31, 0x37, 0x4f, 0x54, 0x65, 0x78, 0x88, 0x89, 0x93, 0x9f, 0xb9, 0xba, 0xca, 0xcd, 0xce, 0xed, 0xff]
    secondArray `shouldBe` secondArrayExpected

    let mergedArrayExpected = V.fromList $ merge (V.toList firstArray) (V.toList secondArray)
    let mergedArray = V.slice 416 38 (memory state)

    mergedArray `shouldBe` mergedArrayExpected

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` True
    carryFlag (flags state) `shouldBe` False
    where
        merge :: (Ord a) => [a] -> [a] -> [a]
        merge x [] = x
        merge [] x = x
        merge allX@(x:xs) allY@(y:ys)
            | x < y = x:(merge xs allY)
            | otherwise = y:(merge allX ys)

testCollatz :: EmulatorState -> IO ()
testCollatz state = do
    -- Registers
    let n = (fromIntegral ((registers state) ! 1) `shiftL` 8 :: Int) + (fromIntegral ((registers state) ! 0) :: Int)
    let stepsExpected = collatz n 0
    let steps = (fromIntegral ((registers state) ! 31) `shiftL` 8 :: Int) + (fromIntegral ((registers state) ! 30) :: Int)
    steps `shouldBe` stepsExpected

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` True
    carryFlag (flags state) `shouldBe` False
    where
        collatz :: Int -> Int -> Int
        collatz 1 steps = steps
        collatz n steps
            | even n = collatz (n `div` 2) (steps + 1)
            | odd n = collatz (3*n + 1) (steps + 1)

testFactorial :: EmulatorState -> IO ()
testFactorial state = do
    -- Registers
    ((registers state) ! 16) `shouldBe` 0x05
    ((registers state) ! 24) `shouldBe` 0x78

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` True
    carryFlag (flags state) `shouldBe` False

testFindMaximum :: EmulatorState -> IO ()
testFindMaximum state = do
    -- Memory
    let array = V.slice 160 9 (memory state)
    let maxNumber = findMaxInList (V.toList array)

    maxNumber `shouldBe` (registers state) ! 18
    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` True
    carryFlag (flags state) `shouldBe` False
    where
        findMaxInList = foldr (\x y -> if x > y then x else y) 0

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

-- Checking EmulatorState for the "test_bld.asm"
testBld:: EmulatorState -> IO()
testBld state = do
    -- Registers
    let regValue = registers state ! 31
    regValue `shouldBe` 0xf0

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` True
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the "test_bclr.asm"
testBclr :: EmulatorState -> IO ()
testBclr state = do
    -- Status flags
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

-- Checking EmulatorState for the "test_adiw.asm"
testCom :: EmulatorState -> IO ()
testCom state = do
    -- Registers
    let regValue = registers state ! 16
    regValue `shouldBe` 0xba

    let regValue = registers state ! 17
    regValue `shouldBe` 0x7f

    let regValue = registers state ! 18
    regValue `shouldBe` 0x0c

    let regValue = registers state ! 19
    regValue `shouldBe` 0xff

    -- Status flags
    interruptFlag (flags state) `shouldBe` False
    tFlag (flags state) `shouldBe` False
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` True
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` True
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` True

-- Checking EmulatorState for the CP instruction
testCp1 :: EmulatorState -> IO ()
testCp1 state = do
    -- Registers
    let regValue = registers state ! 16
    regValue `shouldBe` 0x8f

    let regValue = registers state ! 17
    regValue `shouldBe` 0x70

    -- Status flags
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` True
    overflowFlag (flags state) `shouldBe` True
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False
    
-- Checking EmulatorState for the CP instruction
testCp2 :: EmulatorState -> IO ()
testCp2 state = do
    -- Registers
    let regValue = registers state ! 16
    regValue `shouldBe` 0x70

    let regValue = registers state ! 17
    regValue `shouldBe` 0x8f

    -- Status flags
    halfCarryFlag (flags state) `shouldBe` True
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` True
    negativeFlag (flags state) `shouldBe` True
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` True

-- Checking EmulatorState for the CP instruction
testCp3 :: EmulatorState -> IO ()
testCp3 state = do
    -- Registers
    let regValue = registers state ! 16
    regValue `shouldBe` 0x70

    let regValue = registers state ! 17
    regValue `shouldBe` 0x70

    -- Status flags
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` True
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the CPC instruction
testCpc1 :: EmulatorState -> IO ()
testCpc1 state = do
    -- Registers
    let regValue = registers state ! 19
    regValue `shouldBe` 0x7f

    let regValue = registers state ! 20
    regValue `shouldBe` 0x80

    -- Status flags
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` True
    negativeFlag (flags state) `shouldBe` True
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` True
    
-- Checking EmulatorState for the CPC instruction
testCpc2 :: EmulatorState -> IO ()
testCpc2 state = do
    -- Registers
    let regValue = registers state ! 19
    regValue `shouldBe` 0x80

    let regValue = registers state ! 20
    regValue `shouldBe` 0x7f

    -- Status flags
    halfCarryFlag (flags state) `shouldBe` True
    signFlag (flags state) `shouldBe` True
    overflowFlag (flags state) `shouldBe` True
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the CPC instruction
testCpc3 :: EmulatorState -> IO ()
testCpc3 state = do
    -- Registers
    let regValue = registers state ! 19
    regValue `shouldBe` 0x7f

    let regValue = registers state ! 20
    regValue `shouldBe` 0x7f

    -- Status flags
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` True
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the CP instruction
testCpi1 :: EmulatorState -> IO ()
testCpi1 state = do
    -- Registers
    let regValue = registers state ! 18
    regValue `shouldBe` 0xa3

    -- Status flags
    halfCarryFlag (flags state) `shouldBe` True
    signFlag (flags state) `shouldBe` True
    overflowFlag (flags state) `shouldBe` True
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False
    
-- Checking EmulatorState for the CP instruction
testCpi2 :: EmulatorState -> IO ()
testCpi2 state = do
    -- Registers
    let regValue = registers state ! 18
    regValue `shouldBe` 0x5c

    -- Status flags
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` True
    negativeFlag (flags state) `shouldBe` True
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` True

-- Checking EmulatorState for the CP instruction
testCpi3 :: EmulatorState -> IO ()
testCpi3 state = do
    -- Registers
    let regValue = registers state ! 18
    regValue `shouldBe` 0xa3

    -- Status flags
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` True
    carryFlag (flags state) `shouldBe` False

-- Checking EmulatorState for the CPSE instruction
testCpse1 :: EmulatorState -> IO ()
testCpse1 state = do
    -- Registers
    let regValue = registers state ! 16
    regValue `shouldBe` 0x00

    let regValue = registers state ! 21
    regValue `shouldBe` 0x55

    let regValue = registers state ! 22
    regValue `shouldBe` 0xaa

    -- Status flags
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` False
    carryFlag (flags state) `shouldBe` False
    
-- Checking EmulatorState for the CPSE instruction
testCpse2 :: EmulatorState -> IO ()
testCpse2 state = do
    -- Registers
    let regValue = registers state ! 16
    regValue `shouldBe` 0x00

    let regValue = registers state ! 21
    regValue `shouldBe` 0xaa

    let regValue = registers state ! 22
    regValue `shouldBe` 0xaa

    -- Status flags
    halfCarryFlag (flags state) `shouldBe` False
    signFlag (flags state) `shouldBe` False
    overflowFlag (flags state) `shouldBe` False
    negativeFlag (flags state) `shouldBe` False
    zeroFlag (flags state) `shouldBe` False
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
    regValue `shouldBe` 0x05

    let regValue = registers state ! 1
    regValue `shouldBe` 0x05

    let regValue = registers state ! 2
    regValue `shouldBe` 0x06

    let regValue = registers state ! 4
    regValue `shouldBe` 0x15

    let regValue = registers state ! 5
    regValue `shouldBe` 0x15

    let regValue = registers state ! 6
    regValue `shouldBe` 0x16

    let regValue = registers state ! 8
    regValue `shouldBe` 0x25

    let regValue = registers state ! 9
    regValue `shouldBe` 0x25

    let regValue = registers state ! 10
    regValue `shouldBe` 0x26

    -- Memory
    let memValue = memory state ! 128
    memValue `shouldBe` 0x05

    let memValue = memory state ! 129
    memValue `shouldBe` 0x06

    let memValue = memory state ! 144
    memValue `shouldBe` 0x15

    let memValue = memory state ! 145
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

    let regValue = registers state ! 26
    regValue `shouldBe` 0xbe

    let regValue = registers state ! 27
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

    let regValue = registers state ! 16
    regValue `shouldBe` 0x15

    let regValue = registers state ! 20
    regValue `shouldBe` 0x2

    let regValue = registers state ! 21
    regValue `shouldBe` 0x6

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
    let regValue = registers state ! 16
    regValue `shouldBe` 0x3

    let regValue = registers state ! 17
    regValue `shouldBe` 0x11

    let regValue = registers state ! 18
    regValue `shouldBe` 0x38

    let regValue = registers state ! 19
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
    let sliceOfMemory = V.slice 256 256 (memory state)
    let expectedVector = V.fromList [0,2,2,2,2,3,3,3,3,4,4,4,4,4,5,5,5,5,7,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,13,13,13,13,13,15,15,15,15,17,17,17,17,19,19,19,19,22,24,24,24,25,25,25,25,25,26,26,26,26,28,28,28,28,28,29,29,29,29,33,33,33,33,36,36,36,36,36,37,37,37,37,38,38,38,38,38,42,42,42,42,42,44,44,44,44,44,47,47,47,47,48,48,48,48,49,49,49,49,49,51,51,51,51,51,52,52,52,52,55,55,55,55,55,57,57,57,57,62,62,62,62,62,64,64,64,64,64,65,65,65,65,68,68,68,68,71,71,71,71,71,72,72,72,72,72,73,73,73,73,75,75,75,75,75,78,80,80,80,80,80,83,83,83,83,83,86,86,86,86,86,87,87,87,87,87,90,90,90,90,90,93,93,93,93,95,95,95,95,97,97,97,97,97,98,101,101,101,101,101,103,103,103,103,104,104,104,104,109,109,109,109,109,110,110,110,110,110,113,113,113,113,114,114,114,114,114,116,123,123,123,123,123,124,124,124,124,124,127]
    -- Compare the elements
    --sliceOfMemory `shouldBe` expectedVector

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
    --programCounter state `shouldBe` 93

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

-- Checking EmulatorState for the "test_xch.asm"
testXch :: EmulatorState -> IO ()
testXch state = do
    -- Memory
    let memValue = memory state ! 273
    memValue `shouldBe` 37

    let memValue = memory state ! 274
    memValue `shouldBe` 0

    -- Registers
    let regValue = registers state ! 19
    regValue `shouldBe` 14

    let regValue = registers state ! 25
    regValue `shouldBe` 5

    let regValue = registers state ! 30
    regValue `shouldBe` 18

    let regValue = registers state ! 31
    regValue `shouldBe` 1

-- Checking EmulatorState for the "test_bset.asm"
testBset :: EmulatorState -> IO ()
testBset state = do
    -- Status flags
    interruptFlag (flags state) `shouldBe` True
    tFlag (flags state) `shouldBe` True
    halfCarryFlag (flags state) `shouldBe` True
    signFlag (flags state) `shouldBe` True
    overflowFlag (flags state) `shouldBe` True
    negativeFlag (flags state) `shouldBe` True
    zeroFlag (flags state) `shouldBe` True
    carryFlag (flags state) `shouldBe` True

-- Checking EmulatorState for the "test_sbr.asm"
testSbr :: EmulatorState -> IO ()
testSbr state = do
    -- Registers
    let regValue = registers state ! 16
    regValue `shouldBe` 0xAF

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