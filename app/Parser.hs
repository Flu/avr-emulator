module Parser(parseAssembly) where 

import Text.Parsec
import Text.Parsec.String (Parser)
import Numeric (readHex, readInt)
import Data.Binary
import Data.Char
import Emulator
import Data.Maybe (catMaybes)

-- Parsers for basic components

-- Match the lowercase or uppercase form of 'c'
cichar c = char (toLower c) <|> char (toUpper c)

-- Match the string 's', accepting either lowercase or uppercase form of each character 
cistring s = try (mapM cichar s) <?> "\"" ++ s ++ "\""

pRegister :: Parser Register
pRegister = do
    cichar 'R'
    reg <- many1 digit
    return (read reg)

pRegisterPair :: Parser (Register, Register)
pRegisterPair = do
    cichar 'R'
    reg1 <- many1 digit
    char ':'
    reg2 <- many1 digit
    return (read reg1, read reg2)

pHexDigit :: Parser Char
pHexDigit = oneOf ['0'..'9'] <|> oneOf ['a'..'f'] <|> oneOf ['A'..'F']

pWord8 :: Parser Word8
pWord8 = do
    (string "0x" <|> string "$")
    hexDigits <- many1 pHexDigit
    case readHex hexDigits of
        [(value, "")] -> 
            if 0 <= value && value <= 0xFF 
                then return (fromInteger value)
            else fail "Hexadecimal value out of range for Word8"
        _ -> fail "Invalid hexadecimal format"

pWord16 :: Parser Word16
pWord16 = do
    (string "0x" <|> string "$")
    hexDigits <- many1 pHexDigit
    case readHex hexDigits of
        [(value, "")] -> 
            if 0 <= value && value <= 0xFFFF
                then return (fromInteger value)
            else fail "Hexadecimal value out of range for Word16"
        _ -> fail "Invalid hexadecimal format"

pXRegister :: Parser String
pXRegister = do
    value <- choice [ try (cistring "-X"), try(cistring "X+"), try(cistring "X") ]
    return value

pDecimal :: Parser Word8
pDecimal = do
    digits <- many1 digit
    return (read digits)

pLabel :: Parser Instruction
pLabel = do
    label <- many1 (letter <|> digit <|> char '_')
    char ':'
    return (LABEL label)

pComma :: Parser ()
pComma = char ',' >> spaces

-- Parsers for instructions

pADC :: Parser Instruction
pADC = do
    cistring "ADC" >> spaces
    rd <- pRegister
    pComma
    ADC rd <$> pRegister

pADD :: Parser Instruction
pADD = do
    cistring "ADD" >> spaces
    rd <- pRegister
    pComma
    ADD rd <$> pRegister

pADIW :: Parser Instruction
pADIW = do
    cistring "ADIW" >> spaces
    (reg1, reg2) <- pRegisterPair
    pComma
    ADIW reg1 reg2 <$> pWord8

pAND :: Parser Instruction
pAND = do
    cistring "AND" >> spaces
    rd <- pRegister
    pComma
    AND rd <$> pRegister

pANDI :: Parser Instruction
pANDI = do
    cistring "ANDI" >> spaces
    rd <- pRegister
    pComma
    ANDI rd <$> pWord8

pASR :: Parser Instruction
pASR = do
    cistring "ASR" >> spaces
    ASR <$> pRegister

pBRCC :: Parser Instruction
pBRCC = do
    cistring "BRCC" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRCC label)

pBRCS :: Parser Instruction
pBRCS = do
    cistring "BRCS" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRCS label)

pBREQ :: Parser Instruction
pBREQ = do
    cistring "BREQ" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BREQ label)

pBRGE :: Parser Instruction
pBRGE = do
    cistring "BRGE" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRGE label)

pBRHC :: Parser Instruction
pBRHC = do
    cistring "BRHC" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRHC label)

pBRHS :: Parser Instruction
pBRHS = do
    cistring "BRHS" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRHS label)

pBRID :: Parser Instruction
pBRID = do
    cistring "BRID" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRID label)

pBRIE :: Parser Instruction
pBRIE = do
    cistring "BRIE" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRIE label)

pBRLO :: Parser Instruction
pBRLO = do
    cistring "BRLO" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRLO label)

pBRLT :: Parser Instruction
pBRLT = do
    cistring "BRLT" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRLT label)

pBRMI :: Parser Instruction
pBRMI = do
    cistring "BRMI" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRMI label)

pBRNE :: Parser Instruction
pBRNE = do
    cistring "BRNE" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRNE label)

pBRPL :: Parser Instruction
pBRPL = do
    cistring "BRPL" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRPL label)

pBRSH :: Parser Instruction
pBRSH = do
    cistring "BRSH" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRSH label)

pBRTC :: Parser Instruction
pBRTC = do
    cistring "BRTC" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRTC label)

pBRTS :: Parser Instruction
pBRTS = do
    cistring "BRTS" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRTS label)

pBRVC :: Parser Instruction
pBRVC = do
    cistring "BRVC" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRVC label)

pBRVS :: Parser Instruction
pBRVS = do
    cistring "BRVS" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRVS label)

pCALL :: Parser Instruction
pCALL = do
    cistring "CALL" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (CALL label)

pCLC :: Parser Instruction
pCLC = do
    cistring "CLC" >> spaces
    return (CLC)

pCLH :: Parser Instruction
pCLH = do
    cistring "CLH" >> spaces
    return (CLH)

pCLI :: Parser Instruction
pCLI = do
    cistring "CLI" >> spaces
    return (CLI)

pCLN :: Parser Instruction
pCLN = do
    cistring "CLN" >> spaces
    return (CLN)

pCLR :: Parser Instruction
pCLR = do
    cistring "CLR" >> spaces
    CLR <$> pRegister

pCLS :: Parser Instruction
pCLS = do
    cistring "CLS" >> spaces
    return (CLS)

pCLT :: Parser Instruction
pCLT = do
    cistring "CLT" >> spaces
    return (CLT)

pCLV :: Parser Instruction
pCLV = do
    cistring "CLV" >> spaces
    return (CLV)

pCLZ :: Parser Instruction
pCLZ = do
    cistring "CLZ" >> spaces
    return (CLZ)


pCOM :: Parser Instruction
pCOM = do
    cistring "COM" >> spaces
    COM <$> pRegister

pCP :: Parser Instruction
pCP = do
    cistring "CP" >> spaces
    rd <- pRegister
    pComma
    CP rd <$> pRegister

pCPC :: Parser Instruction
pCPC = do
    cistring "CPC" >> spaces
    rd <- pRegister
    pComma
    CPC rd <$> pRegister

pCPI :: Parser Instruction
pCPI = do
    cistring "CPI" >> spaces
    rd <- pRegister
    pComma
    CPI rd <$> pWord8

pCPSE :: Parser Instruction
pCPSE = do
    cistring "CPSE" >> spaces
    rd <- pRegister
    pComma
    CPSE rd <$> pRegister

pDEC :: Parser Instruction
pDEC = do
    cistring "DEC" >> spaces
    DEC <$> pRegister

pEOR :: Parser Instruction
pEOR = do
    cistring "EOR" >> spaces
    rd <- pRegister
    pComma
    EOR rd <$> pRegister

pINC :: Parser Instruction
pINC = do
    cistring "INC" >> spaces
    INC <$> pRegister

pJMP :: Parser Instruction
pJMP = do
    cistring "JMP" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (JMP label)

pLD :: Parser Instruction
pLD = do
    cistring "LD" >> spaces
    rd <- pRegister
    pComma
    LD rd <$> pXRegister

pLDI :: Parser Instruction
pLDI = do
    cistring "LDI" >> spaces
    rd <- pRegister
    pComma
    LDI rd <$> pWord8

pLDS :: Parser Instruction
pLDS = do
    cistring "LDS" >> spaces
    rd <- pRegister
    pComma
    LDS rd <$> pWord16

pLSL :: Parser Instruction
pLSL = do
    cistring "LSL" >> spaces
    LSL <$> pRegister

pLSR :: Parser Instruction
pLSR = do
    cistring "LSR" >> spaces
    LSR <$> pRegister

pMOV :: Parser Instruction
pMOV = do
    cistring "MOV" >> spaces
    rd <- pRegister
    pComma
    MOV rd <$> pRegister

pMOVW :: Parser Instruction
pMOVW = do
    cistring "MOVW" >> spaces
    (reg1, reg2) <- pRegisterPair
    pComma
    (reg3, reg4) <- pRegisterPair
    return (MOVW reg1 reg2 reg3 reg4) 

pMUL :: Parser Instruction
pMUL = do
    cistring "MUL" >> spaces
    rd <- pRegister
    pComma
    MUL rd <$> pRegister

pMULS :: Parser Instruction
pMULS = do
    cistring "MULS" >> spaces
    rd <- pRegister
    pComma
    MULS rd <$> pRegister

pNEG :: Parser Instruction
pNEG = do
    cistring "NEG" >> spaces
    NEG <$> pRegister

pNOP :: Parser Instruction
pNOP = do
    cistring "NOP" >> spaces
    return NOP

pOR :: Parser Instruction
pOR = do
    cistring "OR" >> spaces
    rd <- pRegister
    pComma
    OR rd <$> pRegister

pORI :: Parser Instruction
pORI = do
    cistring "ORI" >> spaces
    rd <- pRegister
    pComma
    ORI rd <$> pWord8

pPOP :: Parser Instruction
pPOP = do
    cistring "POP" >> spaces
    POP <$> pRegister

pPUSH :: Parser Instruction
pPUSH = do
    cistring "PUSH" >> spaces
    PUSH <$> pRegister

pRET :: Parser Instruction
pRET = do
    cistring "RET" >> spaces
    return (RET)

pROL :: Parser Instruction
pROL = do
    cistring "ROL" >> spaces
    ROL <$> pRegister

pROR :: Parser Instruction
pROR = do
    cistring "ROR" >> spaces
    ROR <$> pRegister

pSBC :: Parser Instruction
pSBC = do
    cistring "SBC" >> spaces
    rd <- pRegister
    pComma
    SBC rd <$> pRegister

pSBRC :: Parser Instruction
pSBRC = do
    cistring "SBRC" >> spaces
    rd <- pRegister
    pComma
    SBRC rd <$> pDecimal

pSBRS :: Parser Instruction
pSBRS = do
    cistring "SBRS" >> spaces
    rd <- pRegister
    pComma
    SBRS rd <$> pDecimal

pSEC :: Parser Instruction
pSEC = do
    cistring "SEC" >> spaces
    return (SEC)

pSEH :: Parser Instruction
pSEH = do
    cistring "SEH" >> spaces
    return (SEH)

pSEI :: Parser Instruction
pSEI = do
    cistring "SEI" >> spaces
    return (SEI)

pSEN :: Parser Instruction
pSEN = do
    cistring "SEN" >> spaces
    return (SEN)

pSER :: Parser Instruction
pSER = do
    cistring "SER" >> spaces
    SER <$> pRegister

pSES :: Parser Instruction
pSES = do
    cistring "SES" >> spaces
    return (SES)

pSET :: Parser Instruction
pSET = do
    cistring "SET" >> spaces
    return (SET)

pSEV :: Parser Instruction
pSEV = do
    cistring "SEV" >> spaces
    return (SEV)

pSEZ :: Parser Instruction
pSEZ = do
    cistring "SEZ" >> spaces
    return (SEZ)

pST :: Parser Instruction
pST = do
    cistring "ST" >> spaces
    x <- pXRegister
    pComma
    ST x <$> pRegister

pSTS :: Parser Instruction
pSTS = do
    cistring "STS" >> spaces
    k <- pWord16
    pComma
    STS k <$> pRegister

pSUB :: Parser Instruction
pSUB = do
    cistring "SUB" >> spaces
    rd <- pRegister
    pComma
    SUB rd <$> pRegister

pSUBI :: Parser Instruction
pSUBI = do
    cistring "SUBI" >> spaces
    rd <- pRegister
    pComma
    SUBI rd <$> pWord8

pSWAP :: Parser Instruction
pSWAP = do
    cistring "SWAP" >> spaces
    SWAP <$> pRegister

pTST :: Parser Instruction
pTST = do
    cistring "TST" >> spaces
    TST <$> pRegister

-- Main parser

pInstruction :: Parser (Maybe Instruction)
pInstruction = do
    spaces
    choice
        [
        try (Just <$> pADC),
        try (Just <$> pADD),
        try (Just <$> pADIW),
        try (Just <$> pAND),
        try (Just <$> pANDI),
        try (Just <$> pASR),
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
        try (Just <$> pTST),
        try (char ';' >> manyTill anyChar newline) >> return Nothing -- Ignore comments
        ]

parseAssembly :: String -> Either ParseError [Instruction]
parseAssembly input = do
    result <- parse (many pInstruction) "" input
    return (catMaybes result)
