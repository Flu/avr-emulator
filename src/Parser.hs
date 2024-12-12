-- Enable the OverloadedStrings language extension so we can use string literals as Text values
{-# LANGUAGE OverloadedStrings #-}


module Parser(parseAssembly) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import Data.Void
import Data.Binary
import Data.Char
import Numeric (readHex, readInt)
import Data.Maybe (catMaybes)

import Emulator

-- Alias for the parser type
type Parser = Parsec Void T.Text

-- Basic components parsers

-- Match the lowercase or uppercase form of 'c'
cichar c = char (toLower c) <|> char (toUpper c)

-- Match the string 's', accepting either lowercase or uppercase form of each character 
cistring s = try (mapM cichar s) <?> "\"" ++ s ++ "\""

pRegister :: Parser Register
pRegister = do
    cichar 'R'
    reg <- some digitChar
    return (read reg)

pRegisterPair :: Parser (Register, Register)
pRegisterPair = do
    cichar 'R'
    reg1 <- some digitChar
    char ':'
    reg2 <- some digitChar
    return (read reg1, read reg2)

pHexDigit :: Parser Char
pHexDigit = oneOf ['0'..'9'] <|> oneOf ['a'..'f'] <|> oneOf ['A'..'F']

pFlagDigit :: Parser Int
pFlagDigit = digitToInt <$> oneOf ['0'..'7']

pWord8 :: Parser Word8
pWord8 = do
    string "0x" <|> string "$"
    hexDigits <- some pHexDigit
    case readHex hexDigits of
        [(value, "")] ->
            if 0 <= value && value <= 0xFF
                then return (fromInteger value)
            else fail "Hexadecimal value out of range for Word8"
        _ -> fail "Invalid hexadecimal format"

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

pDecimal :: Parser Word8
pDecimal = do
    digits <- some digitChar
    return (read digits)

pLabel :: Parser Instruction
pLabel = do
    label <- some (alphaNumChar <|> char '_')
    char ':'
    return (LABEL label)

pComma :: Parser ()
pComma = char ',' >> space

-- Parsers for instructions

pADC :: Parser Instruction
pADC = do
    cistring "ADC" >> space
    rd <- pRegister
    pComma
    ADC rd <$> pRegister

pADD :: Parser Instruction
pADD = do
    cistring "ADD" >> space
    rd <- pRegister
    pComma
    ADD rd <$> pRegister

pADIW :: Parser Instruction
pADIW = do
    cistring "ADIW" >> space
    (reg1, reg2) <- pRegisterPair
    pComma
    ADIW reg1 reg2 <$> pWord8

pAND :: Parser Instruction
pAND = do
    cistring "AND" >> space
    rd <- pRegister
    pComma
    AND rd <$> pRegister

pANDI :: Parser Instruction
pANDI = do
    cistring "ANDI" >> space
    rd <- pRegister
    pComma
    ANDI rd <$> pWord8

pASR :: Parser Instruction
pASR = do
    cistring "ASR" >> space
    ASR <$> pRegister

pBCLR :: Parser Instruction
pBCLR = do
    cistring "BCLR" >> space
    BCLR <$> pFlagDigit

pBLD :: Parser Instruction
pBLD = do
    cistring "BLD" >> space
    rd <- pRegister
    pComma
    BLD rd <$> pFlagDigit

pBRCC :: Parser Instruction
pBRCC = do
    cistring "BRCC" >> space
    label <- some (alphaNumChar <|> char '_')
    return (BRCC label)

pBRCS :: Parser Instruction
pBRCS = do
    cistring "BRCS" >> space
    label <- some (alphaNumChar <|> char '_')
    return (BRCS label)

pBREQ :: Parser Instruction
pBREQ = do
    cistring "BREQ" >> space
    label <- some (alphaNumChar <|> char '_')
    return (BREQ label)

pBRGE :: Parser Instruction
pBRGE = do
    cistring "BRGE" >> space
    label <- some (alphaNumChar <|> char '_')
    return (BRGE label)

pBRHC :: Parser Instruction
pBRHC = do
    cistring "BRHC" >> space
    label <- some (alphaNumChar <|> char '_')
    return (BRHC label)

pBRHS :: Parser Instruction
pBRHS = do
    cistring "BRHS" >> space
    label <- some (alphaNumChar <|> char '_')
    return (BRHS label)

pBRID :: Parser Instruction
pBRID = do
    cistring "BRID" >> space
    label <- some (alphaNumChar <|> char '_')
    return (BRID label)

pBRIE :: Parser Instruction
pBRIE = do
    cistring "BRIE" >> space
    label <- some (alphaNumChar <|> char '_')
    return (BRIE label)

pBRLO :: Parser Instruction
pBRLO = do
    cistring "BRLO" >> space
    label <- some (alphaNumChar <|> char '_')
    return (BRLO label)

pBRLT :: Parser Instruction
pBRLT = do
    cistring "BRLT" >> space
    label <- some (alphaNumChar <|> char '_')
    return (BRLT label)

pBRMI :: Parser Instruction
pBRMI = do
    cistring "BRMI" >> space
    label <- some (alphaNumChar <|> char '_')
    return (BRMI label)

pBRNE :: Parser Instruction
pBRNE = do
    cistring "BRNE" >> space
    label <- some (alphaNumChar <|> char '_')
    return (BRNE label)

pBRPL :: Parser Instruction
pBRPL = do
    cistring "BRPL" >> space
    label <- some (alphaNumChar <|> char '_')
    return (BRPL label)

pBRSH :: Parser Instruction
pBRSH = do
    cistring "BRSH" >> space
    label <- some (alphaNumChar <|> char '_')
    return (BRSH label)

pBRTC :: Parser Instruction
pBRTC = do
    cistring "BRTC" >> space
    label <- some (alphaNumChar <|> char '_')
    return (BRTC label)

pBRTS :: Parser Instruction
pBRTS = do
    cistring "BRTS" >> space
    label <- some (alphaNumChar <|> char '_')
    return (BRTS label)

pBRVC :: Parser Instruction
pBRVC = do
    cistring "BRVC" >> space
    label <- some (alphaNumChar <|> char '_')
    return (BRVC label)

pBRVS :: Parser Instruction
pBRVS = do
    cistring "BRVS" >> space
    label <- some (alphaNumChar <|> char '_')
    return (BRVS label)

pCALL :: Parser Instruction
pCALL = do
    cistring "CALL" >> space
    label <- some (alphaNumChar <|> char '_')
    return (CALL label)

pCBR :: Parser Instruction
pCBR = do
    cistring "CBR" >> space
    rd <- pRegister
    pComma
    CBR rd <$> pWord8

pCLC :: Parser Instruction
pCLC = do
    cistring "CLC" >> space
    return CLC

pCLH :: Parser Instruction
pCLH = do
    cistring "CLH" >> space
    return CLH

pCLI :: Parser Instruction
pCLI = do
    cistring "CLI" >> space
    return CLI

pCLN :: Parser Instruction
pCLN = do
    cistring "CLN" >> space
    return CLN

pCLR :: Parser Instruction
pCLR = do
    cistring "CLR" >> space
    CLR <$> pRegister

pCLS :: Parser Instruction
pCLS = do
    cistring "CLS" >> space
    return CLS

pCLT :: Parser Instruction
pCLT = do
    cistring "CLT" >> space
    return CLT

pCLV :: Parser Instruction
pCLV = do
    cistring "CLV" >> space
    return CLV

pCLZ :: Parser Instruction
pCLZ = do
    cistring "CLZ" >> space
    return CLZ


pCOM :: Parser Instruction
pCOM = do
    cistring "COM" >> space
    COM <$> pRegister

pCP :: Parser Instruction
pCP = do
    cistring "CP" >> space
    rd <- pRegister
    pComma
    CP rd <$> pRegister

pCPC :: Parser Instruction
pCPC = do
    cistring "CPC" >> space
    rd <- pRegister
    pComma
    CPC rd <$> pRegister

pCPI :: Parser Instruction
pCPI = do
    cistring "CPI" >> space
    rd <- pRegister
    pComma
    CPI rd <$> pWord8

pCPSE :: Parser Instruction
pCPSE = do
    cistring "CPSE" >> space
    rd <- pRegister
    pComma
    CPSE rd <$> pRegister

pDEC :: Parser Instruction
pDEC = do
    cistring "DEC" >> space
    DEC <$> pRegister

pEOR :: Parser Instruction
pEOR = do
    cistring "EOR" >> space
    rd <- pRegister
    pComma
    EOR rd <$> pRegister

pINC :: Parser Instruction
pINC = do
    cistring "INC" >> space
    INC <$> pRegister

pJMP :: Parser Instruction
pJMP = do
    cistring "JMP" >> space
    label <- some (alphaNumChar <|> char '_')
    return (JMP label)

pLD :: Parser Instruction
pLD = do
    cistring "LD" >> space
    rd <- pRegister
    pComma
    LD rd <$> pXRegister

pLDI :: Parser Instruction
pLDI = do
    cistring "LDI" >> space
    rd <- pRegister
    pComma
    LDI rd <$> pWord8

pLDS :: Parser Instruction
pLDS = do
    cistring "LDS" >> space
    rd <- pRegister
    pComma
    LDS rd <$> pWord16

pLSL :: Parser Instruction
pLSL = do
    cistring "LSL" >> space
    LSL <$> pRegister

pLSR :: Parser Instruction
pLSR = do
    cistring "LSR" >> space
    LSR <$> pRegister

pMOV :: Parser Instruction
pMOV = do
    cistring "MOV" >> space
    rd <- pRegister
    pComma
    MOV rd <$> pRegister

pMOVW :: Parser Instruction
pMOVW = do
    cistring "MOVW" >> space
    (reg1, reg2) <- pRegisterPair
    pComma
    (reg3, reg4) <- pRegisterPair
    return (MOVW reg1 reg2 reg3 reg4)

pMUL :: Parser Instruction
pMUL = do
    cistring "MUL" >> space
    rd <- pRegister
    pComma
    MUL rd <$> pRegister

pMULS :: Parser Instruction
pMULS = do
    cistring "MULS" >> space
    rd <- pRegister
    pComma
    MULS rd <$> pRegister

pNEG :: Parser Instruction
pNEG = do
    cistring "NEG" >> space
    NEG <$> pRegister

pNOP :: Parser Instruction
pNOP = do
    cistring "NOP" >> space
    return NOP

pOR :: Parser Instruction
pOR = do
    cistring "OR" >> space
    rd <- pRegister
    pComma
    OR rd <$> pRegister

pORI :: Parser Instruction
pORI = do
    cistring "ORI" >> space
    rd <- pRegister
    pComma
    ORI rd <$> pWord8

pPOP :: Parser Instruction
pPOP = do
    cistring "POP" >> space
    POP <$> pRegister

pPUSH :: Parser Instruction
pPUSH = do
    cistring "PUSH" >> space
    PUSH <$> pRegister

pRET :: Parser Instruction
pRET = do
    cistring "RET" >> space
    return RET

pROL :: Parser Instruction
pROL = do
    cistring "ROL" >> space
    ROL <$> pRegister

pROR :: Parser Instruction
pROR = do
    cistring "ROR" >> space
    ROR <$> pRegister

pSBC :: Parser Instruction
pSBC = do
    cistring "SBC" >> space
    rd <- pRegister
    pComma
    SBC rd <$> pRegister

pSBRC :: Parser Instruction
pSBRC = do
    cistring "SBRC" >> space
    rd <- pRegister
    pComma
    SBRC rd <$> pDecimal

pSBRS :: Parser Instruction
pSBRS = do
    cistring "SBRS" >> space
    rd <- pRegister
    pComma
    SBRS rd <$> pDecimal

pSEC :: Parser Instruction
pSEC = do
    cistring "SEC" >> space
    return SEC

pSEH :: Parser Instruction
pSEH = do
    cistring "SEH" >> space
    return SEH

pSEI :: Parser Instruction
pSEI = do
    cistring "SEI" >> space
    return SEI

pSEN :: Parser Instruction
pSEN = do
    cistring "SEN" >> space
    return SEN

pSER :: Parser Instruction
pSER = do
    cistring "SER" >> space
    SER <$> pRegister

pSES :: Parser Instruction
pSES = do
    cistring "SES" >> space
    return SES

pSET :: Parser Instruction
pSET = do
    cistring "SET" >> space
    return SET

pSEV :: Parser Instruction
pSEV = do
    cistring "SEV" >> space
    return SEV

pSEZ :: Parser Instruction
pSEZ = do
    cistring "SEZ" >> space
    return SEZ

pST :: Parser Instruction
pST = do
    cistring "ST" >> space
    x <- pXRegister
    pComma
    ST x <$> pRegister

pSTS :: Parser Instruction
pSTS = do
    cistring "STS" >> space
    k <- pWord16
    pComma
    STS k <$> pRegister

pSUB :: Parser Instruction
pSUB = do
    cistring "SUB" >> space
    rd <- pRegister
    pComma
    SUB rd <$> pRegister

pSUBI :: Parser Instruction
pSUBI = do
    cistring "SUBI" >> space
    rd <- pRegister
    pComma
    SUBI rd <$> pWord8

pSWAP :: Parser Instruction
pSWAP = do
    cistring "SWAP" >> space
    SWAP <$> pRegister

pTST :: Parser Instruction
pTST = do
    cistring "TST" >> space
    TST <$> pRegister

-- Main parsers

instructionParser :: Parser (Maybe Instruction)
instructionParser = do
    space
    choice
        [
        try (Just <$> pADC),
        try (Just <$> pADD),
        try (Just <$> pADIW),
        try (Just <$> pAND),
        try (Just <$> pANDI),
        try (Just <$> pASR),
        try (Just <$> pBCLR),
        try (Just <$> pBLD),
        try (Just <$> pBRCC),
        try (Just <$> pBRCS),
        try (Just <$> pBREQ),
        try (Just <$> pBRGE),
        try (Just <$> pBRHC),
        try (Just <$> pBRHS),
        try (Just <$> pBRID),
        try (Just <$> pBRIE),
        try (Just <$> pBRLO),
        try (Just <$> pBRLT),
        try (Just <$> pBRMI),
        try (Just <$> pBRNE),
        try (Just <$> pBRPL),
        try (Just <$> pBRSH),
        try (Just <$> pBRTC),
        try (Just <$> pBRTS),
        try (Just <$> pBRVC),
        try (Just <$> pBRVS),
        try (Just <$> pCALL),
        try (Just <$> pCBR),
        try (Just <$> pCLC),
        try (Just <$> pCLH),
        try (Just <$> pCLI),
        try (Just <$> pCLN),
        try (Just <$> pCLR),
        try (Just <$> pCLS),
        try (Just <$> pCLT),
        try (Just <$> pCLV),
        try (Just <$> pCLZ),
        try (Just <$> pCOM),
        try (Just <$> pCP),
        try (Just <$> pCPC),
        try (Just <$> pCPI),
        try (Just <$> pCPSE),
        try (Just <$> pDEC),
        try (Just <$> pEOR),
        try (Just <$> pINC),
        try (Just <$> pJMP),
        try (Just <$> pLD),
        try (Just <$> pLabel),
        try (Just <$> pLDI),
        try (Just <$> pLDS),
        try (Just <$> pLSL),
        try (Just <$> pLSR),
        try (Just <$> pMOV),
        try (Just <$> pMOVW),
        try (Just <$> pMUL),
        try (Just <$> pMULS),
        try (Just <$> pNEG),
        try (Just <$> pNOP),
        try (Just <$> pOR),
        try (Just <$> pORI),
        try (Just <$> pPOP),
        try (Just <$> pPUSH),
        try (Just <$> pRET),
        try (Just <$> pROL),
        try (Just <$> pROR),
        try (Just <$> pSBC),
        try (Just <$> pSBRC),
        try (Just <$> pSBRS),
        try (Just <$> pSEC),
        try (Just <$> pSEH),
        try (Just <$> pSEI),
        try (Just <$> pSEN),
        try (Just <$> pSER),
        try (Just <$> pSES),
        try (Just <$> pSET),
        try (Just <$> pSEV),
        try (Just <$> pSEZ),
        try (Just <$> pST),
        try (Just <$> pSTS),
        try (Just <$> pSUB),
        try (Just <$> pSUBI),
        try (Just <$> pSWAP),
        try (Just <$> pTST)
        ]

-- Parser for comments
commentParser :: Parser (Maybe Instruction)
commentParser = do
    _ <- char ';'  -- Skip the comment start
    choice [
        try (manyTill printChar eol),
        manyTill printChar eof]  -- Consume the comment content
    return Nothing  -- Always return Nothing for comments

-- Parser for a line (either an instruction or a comment)
lineParser :: Parser (Maybe Instruction)
lineParser = try instructionParser <|> try commentParser

-- Parser for a list of instructions
programParser :: Parser [Instruction]
programParser = do
    instructions <- many (space *> lineParser <* space)
    eof
    return (catMaybes instructions)

-- Run the parser on the input
parseAssembly :: String -> Either (ParseErrorBundle T.Text Void) [Instruction]
parseAssembly input = do
    let textInput = T.pack input
    runParser programParser "" textInput