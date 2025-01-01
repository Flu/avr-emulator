-- Enable the OverloadedStrings language extension so we can use string literals as Text values
{-# LANGUAGE OverloadedStrings #-}


module Parser(parseAssembly, prettyPrintErrorBundle) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import Data.Void
import Data.Binary
import Data.Char
import Numeric (readHex)

import Emulator

-- Alias for the parser type
type Parser = Parsec Void T.Text

-- Basic components parsers

-- Match the lowercase or uppercase form of 'c'
cichar :: (Token s ~ Char, MonadParsec e s f) => Char -> f Char
cichar c = char (toLower c) <|> char (toUpper c)

-- Match the string 's', accepting either lowercase or uppercase form of each character 
cistring :: (Token s ~ Char, MonadParsec e s m) => [Char] -> m [Char]
cistring s = try (mapM cichar s) <?> "\"" ++ s ++ "\""

pLabelledRegister :: Parser Register
pLabelledRegister = do
  reg <- some alphaNumChar
  case reg of
    "XL" -> return 26
    "XH" -> return 27
    "YL" -> return 28
    "YH" -> return 29
    "ZL" -> return 30
    "ZH" -> return 31
    _ -> fail "Invalid register label"

pNumberedRegister :: Parser Register
pNumberedRegister = do
    cichar 'R'
    reg <- some digitChar
    return (read reg)

pRegister :: Parser Register
pRegister = do
  reg <- choice [try pNumberedRegister , try pLabelledRegister] <?> "register"
  if reg < 0 || reg >= 32 then
    fail "Register number can't be outside the range [0..31]"
  else
    return reg

pRegisterPair :: Parser (Register, Register)
pRegisterPair = do
    reg <- pRegister
    return (reg+1, reg)

pHexDigit :: Parser Char
pHexDigit = oneOf ['0'..'9'] <|> oneOf ['a'..'f'] <|> oneOf ['A'..'F']

pFlagDigit :: Parser Int
pFlagDigit = digitToInt <$> oneOf ['0'..'7']

pHexWord8 :: Parser Int
pHexWord8 = do
    string "0x" <|> string "$"
    hexDigits <- some pHexDigit
    case readHex hexDigits of
        [(value, "")] -> return (fromInteger value)
        _ -> fail "Invalid hexadecimal number format"

pDecWord8 :: Parser Int
pDecWord8 = do
    digits <- some digitChar
    return $ read digits

pWord8 :: Parser Word8
pWord8 = do
    byte <- pHexWord8 <|> pDecWord8 <?> "number in hexadecimal or decimal format"
    if 0 <= byte && byte <= 0xFF then
        return (fromIntegral byte)
    else
        fail "Value out of range for a byte"

pWord16 :: Parser Word16
pWord16 = do
    string "0x" <|> string "$"
    hexDigits <- some pHexDigit
    case readHex hexDigits of
        [(value, "")] ->
            if 0 <= value && value <= 0xFFFF
                then return (fromInteger value)
            else fail "Hexadecimal value out of range for Word16"
        _ -> fail "Invalid hexadecimal format"

pXRegister :: Parser String
pXRegister = do
    choice [ try (cistring "-X"), try (cistring "X+"), try (cistring "X") ]

pYRegister :: Parser String
pYRegister = do
    choice [ try (cistring "-Y"), try (cistring "Y+"), try (cistring "Y") ]

pZRegister :: Parser String
pZRegister = do
    choice [ try (cistring "-Z"), try (cistring "Z+"), try (cistring "Z") ]

pDecimal :: Parser Word8
pDecimal = do
    digits <- some digitChar
    return (read digits)

pLabelValue :: Parser String
pLabelValue = some (alphaNumChar <|> char '_')

pLabel :: Parser Instruction
pLabel = do
    label <- pLabelValue
    char ':'
    return (LABEL label)

pComma :: Parser ()
pComma = char ',' >> space

-- Parsers for instructions

pADC :: Parser Instruction
pADC = do
    cistring "ADC" >> space1
    rd <- pRegister
    pComma
    ADC rd <$> pRegister

pADD :: Parser Instruction
pADD = do
    cistring "ADD" >> space1
    rd <- pRegister
    pComma
    ADD rd <$> pRegister

pADIW :: Parser Instruction
pADIW = do
    cistring "ADIW" >> space1
    (reg1, reg2) <- pRegisterPair
    pComma
    if isValidRegister reg2 then
        ADIW reg1 reg2 <$> pWord8
    else
        fail "Register must be R24, R26, R28 or R30."
    where
    isValidRegister r
        | r == 24 || r == 26 || r == 28 || r == 30 = True
        | otherwise = False

pAND :: Parser Instruction
pAND = do
    cistring "AND" >> space1
    rd <- pRegister
    pComma
    AND rd <$> pRegister

pANDI :: Parser Instruction
pANDI = do
    cistring "ANDI" >> space1
    rd <- pRegister
    pComma
    ANDI rd <$> pWord8

pASR :: Parser Instruction
pASR = do
    cistring "ASR" >> space1
    ASR <$> pRegister

pBCLR :: Parser Instruction
pBCLR = do
    cistring "BCLR" >> space1
    BCLR <$> pFlagDigit

pBLD :: Parser Instruction
pBLD = do
    cistring "BLD" >> space1
    rd <- pRegister
    pComma
    BLD rd <$> pFlagDigit

pBRCC :: Parser Instruction
pBRCC = do
    cistring "BRCC" >> space1
    label <- pLabelValue
    return (BRCC label)

pBRCS :: Parser Instruction
pBRCS = do
    cistring "BRCS" >> space1
    label <- pLabelValue
    return (BRCS label)

pBREQ :: Parser Instruction
pBREQ = do
    cistring "BREQ" >> space1
    label <- pLabelValue
    return (BREQ label)

pBRGE :: Parser Instruction
pBRGE = do
    cistring "BRGE" >> space1
    label <- pLabelValue
    return (BRGE label)

pBRHC :: Parser Instruction
pBRHC = do
    cistring "BRHC" >> space1
    label <- pLabelValue
    return (BRHC label)

pBRHS :: Parser Instruction
pBRHS = do
    cistring "BRHS" >> space1
    label <- pLabelValue
    return (BRHS label)

pBRID :: Parser Instruction
pBRID = do
    cistring "BRID" >> space1
    label <- pLabelValue
    return (BRID label)

pBRIE :: Parser Instruction
pBRIE = do
    cistring "BRIE" >> space1
    label <- pLabelValue
    return (BRIE label)

pBRLO :: Parser Instruction
pBRLO = do
    cistring "BRLO" >> space1
    label <- pLabelValue
    return (BRLO label)

pBRLT :: Parser Instruction
pBRLT = do
    cistring "BRLT" >> space1
    label <- pLabelValue
    return (BRLT label)

pBRMI :: Parser Instruction
pBRMI = do
    cistring "BRMI" >> space1
    label <- pLabelValue
    return (BRMI label)

pBRNE :: Parser Instruction
pBRNE = do
    cistring "BRNE" >> space1
    label <- pLabelValue
    return (BRNE label)

pBRPL :: Parser Instruction
pBRPL = do
    cistring "BRPL" >> space1
    label <- pLabelValue
    return (BRPL label)

pBRSH :: Parser Instruction
pBRSH = do
    cistring "BRSH" >> space1
    label <- pLabelValue
    return (BRSH label)

pBRTC :: Parser Instruction
pBRTC = do
    cistring "BRTC" >> space1
    label <- pLabelValue
    return (BRTC label)

pBRTS :: Parser Instruction
pBRTS = do
    cistring "BRTS" >> space1
    label <- pLabelValue
    return (BRTS label)

pBRVC :: Parser Instruction
pBRVC = do
    cistring "BRVC" >> space1
    label <- pLabelValue
    return (BRVC label)

pBRVS :: Parser Instruction
pBRVS = do
    cistring "BRVS" >> space1
    label <- pLabelValue
    return (BRVS label)

pCALL :: Parser Instruction
pCALL = do
    cistring "CALL" >> space1
    label <- pLabelValue
    return (CALL label)

pCBR :: Parser Instruction
pCBR = do
    cistring "CBR" >> space1
    rd <- pRegister
    pComma
    CBR rd <$> pWord8

pCLC :: Parser Instruction
pCLC = do
    cistring "CLC" >> (space1 <|> eof)
    return CLC

pCLH :: Parser Instruction
pCLH = do
    cistring "CLH" >> (space1 <|> eof)
    return CLH

pCLI :: Parser Instruction
pCLI = do
    cistring "CLI" >> (space1 <|> eof)
    return CLI

pCLN :: Parser Instruction
pCLN = do
    cistring "CLN" >> (space1 <|> eof)
    return CLN

pCLR :: Parser Instruction
pCLR = do
    cistring "CLR" >> (space1 <|> eof)
    CLR <$> pRegister

pCLS :: Parser Instruction
pCLS = do
    cistring "CLS" >> (space1 <|> eof)
    return CLS

pCLT :: Parser Instruction
pCLT = do
    cistring "CLT" >> (space1 <|> eof)
    return CLT

pCLV :: Parser Instruction
pCLV = do
    cistring "CLV" >> (space1 <|> eof)
    return CLV

pCLZ :: Parser Instruction
pCLZ = do
    cistring "CLZ" >> (space1 <|> eof)
    return CLZ

pCOM :: Parser Instruction
pCOM = do
    cistring "COM" >> space1
    COM <$> pRegister

pCP :: Parser Instruction
pCP = do
    cistring "CP" >> space1
    rd <- pRegister
    pComma
    CP rd <$> pRegister

pCPC :: Parser Instruction
pCPC = do
    cistring "CPC" >> space1
    rd <- pRegister
    pComma
    CPC rd <$> pRegister

pCPI :: Parser Instruction
pCPI = do
    cistring "CPI" >> space1
    rd <- pRegister
    pComma
    CPI rd <$> pWord8

pCPSE :: Parser Instruction
pCPSE = do
    cistring "CPSE" >> space1
    rd <- pRegister
    pComma
    CPSE rd <$> pRegister

pDEC :: Parser Instruction
pDEC = do
    cistring "DEC" >> space1
    DEC <$> pRegister

pEOR :: Parser Instruction
pEOR = do
    cistring "EOR" >> space1
    rd <- pRegister
    pComma
    EOR rd <$> pRegister

pINC :: Parser Instruction
pINC = do
    cistring "INC" >> space1
    INC <$> pRegister

pJMP :: Parser Instruction
pJMP = do
    cistring "JMP" >> space1
    label <- pLabelValue
    return (JMP label)

pLD :: Parser Instruction
pLD = do
    cistring "LD" >> space1
    rd <- pRegister
    pComma
    LD rd <$> (pXRegister <|> pYRegister <|> pZRegister)

pLDI :: Parser Instruction
pLDI = do
    cistring "LDI" >> space1
    rd <- pRegister
    pComma
    LDI rd <$> pWord8

pLDS :: Parser Instruction
pLDS = do
    cistring "LDS" >> space1
    rd <- pRegister
    pComma
    LDS rd <$> pWord16

pLSL :: Parser Instruction
pLSL = do
    cistring "LSL" >> space1
    LSL <$> pRegister

pLSR :: Parser Instruction
pLSR = do
    cistring "LSR" >> space1
    LSR <$> pRegister

pMOV :: Parser Instruction
pMOV = do
    cistring "MOV" >> space1
    rd <- pRegister
    pComma
    MOV rd <$> pRegister

pMOVW :: Parser Instruction
pMOVW = do
    cistring "MOVW" >> space1
    (reg1, reg2) <- pRegisterPair
    pComma
    (reg3, reg4) <- pRegisterPair
    if isValidRegister reg2 && isValidRegister reg4 then
        return (MOVW reg1 reg2 reg3 reg4)
    else
        fail "Register has to be R{0,2,..,30}"
    where
    isValidRegister r
        | r `mod` 2 == 0 && r >= 0 && r < 32 = True
        | otherwise = False

pMUL :: Parser Instruction
pMUL = do
    cistring "MUL" >> space1
    rd <- pRegister
    pComma
    MUL rd <$> pRegister

pMULS :: Parser Instruction
pMULS = do
    cistring "MULS" >> space1
    rd <- pRegister
    pComma
    MULS rd <$> pRegister

pNEG :: Parser Instruction
pNEG = do
    cistring "NEG" >> space1
    NEG <$> pRegister

pNOP :: Parser Instruction
pNOP = do
    cistring "NOP" >> (space1 <|> eof)
    return NOP

pOR :: Parser Instruction
pOR = do
    cistring "OR" >> space1
    rd <- pRegister
    pComma
    OR rd <$> pRegister

pORI :: Parser Instruction
pORI = do
    cistring "ORI" >> space1
    rd <- pRegister
    pComma
    ORI rd <$> pWord8

pPOP :: Parser Instruction
pPOP = do
    cistring "POP" >> space1
    POP <$> pRegister

pPUSH :: Parser Instruction
pPUSH = do
    cistring "PUSH" >> space1
    PUSH <$> pRegister

pRET :: Parser Instruction
pRET = do
    cistring "RET" >> space1
    return RET

pROL :: Parser Instruction
pROL = do
    cistring "ROL" >> space1
    ROL <$> pRegister

pROR :: Parser Instruction
pROR = do
    cistring "ROR" >> space1
    ROR <$> pRegister

pSBC :: Parser Instruction
pSBC = do
    cistring "SBC" >> space1
    rd <- pRegister
    pComma
    SBC rd <$> pRegister

pSBRC :: Parser Instruction
pSBRC = do
    cistring "SBRC" >> space1
    rd <- pRegister
    pComma
    SBRC rd <$> pDecimal

pSBRS :: Parser Instruction
pSBRS = do
    cistring "SBRS" >> space1
    rd <- pRegister
    pComma
    SBRS rd <$> pDecimal

pSEC :: Parser Instruction
pSEC = do
    cistring "SEC" >> (space1 <|> eof)
    return SEC

pSEH :: Parser Instruction
pSEH = do
    cistring "SEH" >> (space1 <|> eof)
    return SEH

pSEI :: Parser Instruction
pSEI = do
    cistring "SEI" >> (space1 <|> eof)
    return SEI

pSEN :: Parser Instruction
pSEN = do
    cistring "SEN" >> (space1 <|> eof)
    return SEN

pSER :: Parser Instruction
pSER = do
    cistring "SER" >> (space1 <|> eof)
    SER <$> pRegister

pSES :: Parser Instruction
pSES = do
    cistring "SES" >> (space1 <|> eof)
    return SES

pSET :: Parser Instruction
pSET = do
    cistring "SET" >> (space1 <|> eof)
    return SET

pSEV :: Parser Instruction
pSEV = do
    cistring "SEV" >> (space1 <|> eof)
    return SEV

pSEZ :: Parser Instruction
pSEZ = do
    cistring "SEZ" >> (space1 <|> eof)
    return SEZ

pST :: Parser Instruction
pST = do
    cistring "ST" >> space1
    x <- (pXRegister <|> pYRegister <|> pZRegister)
    pComma
    ST x <$> pRegister

pSTS :: Parser Instruction
pSTS = do
    cistring "STS" >> space1
    k <- pWord16
    pComma
    STS k <$> pRegister

pSUB :: Parser Instruction
pSUB = do
    cistring "SUB" >> space1
    rd <- pRegister
    pComma
    SUB rd <$> pRegister

pSUBI :: Parser Instruction
pSUBI = do
    cistring "SUBI" >> space1
    rd <- pRegister
    pComma
    SUBI rd <$> pWord8

pSWAP :: Parser Instruction
pSWAP = do
    cistring "SWAP" >> space1
    SWAP <$> pRegister

pTST :: Parser Instruction
pTST = do
    cistring "TST" >> space1
    TST <$> pRegister

-- Main parsers

instructionParser :: Parser (Instruction)
instructionParser = do
    space
    choice
        [
        pADC,
        pADD,
        pADIW,
        pANDI,
        pAND,
        pASR,
        pBCLR,
        pBLD,
        pBRCC,
        pBRCS,
        pBREQ,
        pBRGE,
        pBRHC,
        pBRHS,
        pBRID,
        pBRIE,
        pBRLO,
        pBRLT,
        pBRMI,
        pBRNE,
        pBRPL,
        pBRSH,
        pBRTC,
        pBRTS,
        pBRVC,
        pBRVS,
        pCALL,
        pCBR,
        pCLC,
        pCLH,
        pCLI,
        pCLN,
        pCLR,
        pCLS,
        pCLT,
        pCLV,
        pCLZ,
        pCOM,
        pCPSE,
        pCPC,
        pCPI,
        pCP,
        pDEC,
        pEOR,
        pINC,
        pJMP,
        pLDI,
        pLDS,
        pLD,
        pLSL,
        pLSR,
        pMOVW,
        pMOV,
        pMULS,
        pMUL,
        pNEG,
        pNOP,
        pORI,
        pOR,
        pPOP,
        pPUSH,
        pRET,
        pROL,
        pROR,
        pSBC,
        pSBRC,
        pSBRS,
        pSEC,
        pSEH,
        pSEI,
        pSEN,
        pSER,
        pSES,
        pSET,
        pSEV,
        pSEZ,
        pSTS,
        pST,
        pSUBI,
        pSUB,
        pSWAP,
        pTST
        ]

-- | Parser for comments
commentParser :: Parser (Instruction)
commentParser = do
    _ <- char ';'  -- Skip the comment start
    choice [
        try (manyTill printChar eol),
        manyTill printChar eof]  -- Consume the comment content
    return (Comment)  -- Return the comment instruction type, which will get filtered out

-- | Parser for a line. First tries a label, if that fails, it backtracks and tries either an instruction or a comment
lineParser :: Parser (Instruction)
lineParser = (try pLabel) <|> (instructionParser <?> "instruction") <|> (commentParser <?> "comment")

-- | Parser for a list of instructions
programParser :: Parser [Instruction]
programParser = do
    instructions <- many (space *> lineParser <* space)
    eof
    return (filter isNotComment instructions)
    where
        isNotComment (Comment) = False
        isNotComment _ = True

prettyPrintErrorBundle :: ParseErrorBundle T.Text Void -> String
prettyPrintErrorBundle = errorBundlePretty

-- Run the parser on the input
parseAssembly :: String -> Either (ParseErrorBundle T.Text Void) [Instruction]
parseAssembly input = do
    let textInput = T.pack input
    runParser programParser "" textInput
