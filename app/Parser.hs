module Parser(parseAssembly) where 

import Text.Parsec
import Text.Parsec.String (Parser)
import Numeric (readHex, readInt)
import Data.Binary
import Emulator
import Data.Maybe (catMaybes)

-- Parsers for basic components

pRegister :: Parser Register
pRegister = do
    char 'R'
    reg <- many1 digit
    return (read reg)

pHexDigit :: Parser Char
pHexDigit = oneOf ['0'..'9'] <|> oneOf ['a'..'f'] <|> oneOf ['A'..'F']

pWord8 :: Parser Word8
pWord8 = do
    string "0x"
    hexDigits <- many1 pHexDigit
    case readHex hexDigits of
        [(value, "")] -> 
            if 0 <= value && value <= 0xFF 
                then return (fromInteger value)
            else fail "Hexadecimal value out of range for Word8"
        _ -> fail "Invalid hexadecimal format"

pWord16 :: Parser Word16
pWord16 = do
    string "0x"
    hexDigits <- many1 pHexDigit
    case readHex hexDigits of
        [(value, "")] -> 
            if 0 <= value && value <= 0xFFFF
                then return (fromInteger value)
            else fail "Hexadecimal value out of range for Word8"
        _ -> fail "Invalid hexadecimal format"

pXRegister :: Parser String
pXRegister = do
    value <- choice [ try (string "-X"), try(string "X+"), try(string "X") ]
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
    string "ADC" >> spaces
    rd <- pRegister
    pComma
    ADC rd <$> pRegister

pADD :: Parser Instruction
pADD = do
    string "ADD" >> spaces
    rd <- pRegister
    pComma
    ADD rd <$> pRegister

pADIW :: Parser Instruction
pADIW = do
    string "ADIW" >> spaces
    regPair <- pRegister
    char ':'
    regPair2 <- pDecimal
    pComma
    ADIW regPair regPair2 <$> pWord8

pAND :: Parser Instruction
pAND = do
    string "AND" >> spaces
    rd <- pRegister
    pComma
    AND rd <$> pRegister

pANDI :: Parser Instruction
pANDI = do
    string "ANDI" >> spaces
    rd <- pRegister
    pComma
    ANDI rd <$> pWord8

pBRCC :: Parser Instruction
pBRCC = do
    string "BRCC" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRCC label)

pBRCS :: Parser Instruction
pBRCS = do
    string "BRCS" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRCS label)

pBREQ :: Parser Instruction
pBREQ = do
    string "BREQ" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BREQ label)

pBRGE :: Parser Instruction
pBRGE = do
    string "BRGE" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRGE label)

pBRHC :: Parser Instruction
pBRHC = do
    string "BRHC" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRHC label)

pBRHS :: Parser Instruction
pBRHS = do
    string "BRHS" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRHS label)

pBRID :: Parser Instruction
pBRID = do
    string "BRID" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRID label)

pBRIE :: Parser Instruction
pBRIE = do
    string "BRIE" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRIE label)

pBRLO :: Parser Instruction
pBRLO = do
    string "BRLO" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRLO label)

pBRLT :: Parser Instruction
pBRLT = do
    string "BRLT" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRLT label)

pBRMI :: Parser Instruction
pBRMI = do
    string "BRMI" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRMI label)

pBRNE :: Parser Instruction
pBRNE = do
    string "BRNE" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRNE label)

pBRPL :: Parser Instruction
pBRPL = do
    string "BRPL" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRPL label)

pBRSH :: Parser Instruction
pBRSH = do
    string "BRSH" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRSH label)

pBRTC :: Parser Instruction
pBRTC = do
    string "BRTC" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRTC label)

pBRTS :: Parser Instruction
pBRTS = do
    string "BRTS" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRTS label)

pBRVC :: Parser Instruction
pBRVC = do
    string "BRVC" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRVC label)

pBRVS :: Parser Instruction
pBRVS = do
    string "BRVS" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (BRVS label)

pCALL :: Parser Instruction
pCALL = do
    string "CALL" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (CALL label)

pCP :: Parser Instruction
pCP = do
    string "CP" >> spaces
    rd <- pRegister
    pComma
    CP rd <$> pRegister

pCPC :: Parser Instruction
pCPC = do
    string "CPC" >> spaces
    rd <- pRegister
    pComma
    CPC rd <$> pRegister

pCPI :: Parser Instruction
pCPI = do
    string "CPI" >> spaces
    rd <- pRegister
    pComma
    CPI rd <$> pWord8

pCPSE :: Parser Instruction
pCPSE = do
    string "CPSE" >> spaces
    rd <- pRegister
    pComma
    CPSE rd <$> pRegister

pDEC :: Parser Instruction
pDEC = do
    string "DEC" >> spaces
    DEC <$> pRegister

pEOR :: Parser Instruction
pEOR = do
    string "EOR" >> spaces
    rd <- pRegister
    pComma
    EOR rd <$> pRegister

pINC :: Parser Instruction
pINC = do
    string "INC" >> spaces
    INC <$> pRegister

pJMP :: Parser Instruction
pJMP = do
    string "JMP" >> spaces
    label <- many1 (letter <|> digit <|> char '_')
    return (JMP label)

pLD :: Parser Instruction
pLD = do
    string "LD" >> spaces
    rd <- pRegister
    pComma
    LD rd <$> pXRegister

pLDI :: Parser Instruction
pLDI = do
    string "LDI" >> spaces
    rd <- pRegister
    pComma
    LDI rd <$> pWord8

pLDS :: Parser Instruction
pLDS = do
    string "LDS" >> spaces
    rd <- pRegister
    pComma
    LDS rd <$> pWord16

pLSL :: Parser Instruction
pLSL = do
    string "LSL" >> spaces
    LSL <$> pRegister

pLSR :: Parser Instruction
pLSR = do
    string "LSR" >> spaces
    LSR <$> pRegister

pMOV :: Parser Instruction
pMOV = do
    string "MOV" >> spaces
    rd <- pRegister
    pComma
    MOV rd <$> pRegister

pMUL :: Parser Instruction
pMUL = do
    string "MUL" >> spaces
    rd <- pRegister
    pComma
    MUL rd <$> pRegister

pMULS :: Parser Instruction
pMULS = do
    string "MULS" >> spaces
    rd <- pRegister
    pComma
    MULS rd <$> pRegister

pNOP :: Parser Instruction
pNOP = do
    string "NOP" >> spaces
    return NOP

pOR :: Parser Instruction
pOR = do
    string "OR" >> spaces
    rd <- pRegister
    pComma
    OR rd <$> pRegister

pORI :: Parser Instruction
pORI = do
    string "ORI" >> spaces
    rd <- pRegister
    pComma
    ORI rd <$> pWord8

pPOP :: Parser Instruction
pPOP = do
    string "POP" >> spaces
    POP <$> pRegister

pPUSH :: Parser Instruction
pPUSH = do
    string "PUSH" >> spaces
    PUSH <$> pRegister

pRET :: Parser Instruction
pRET = do
    string "RET" >> spaces
    return (RET)

pSBRC :: Parser Instruction
pSBRC = do
    string "SBRC" >> spaces
    rd <- pRegister
    pComma
    SBRC rd <$> pDecimal

pSBRS :: Parser Instruction
pSBRS = do
    string "SBRS" >> spaces
    rd <- pRegister
    pComma
    SBRS rd <$> pDecimal

pST :: Parser Instruction
pST = do
    string "ST" >> spaces
    x <- pXRegister
    pComma
    ST x <$> pRegister

pSTS :: Parser Instruction
pSTS = do
    string "STS" >> spaces
    k <- pWord16
    pComma
    STS k <$> pRegister

pSUB :: Parser Instruction
pSUB = do
    string "SUB" >> spaces
    rd <- pRegister
    pComma
    SUB rd <$> pRegister

pSUBI :: Parser Instruction
pSUBI = do
    string "SUBI" >> spaces
    rd <- pRegister
    pComma
    SUBI rd <$> pWord8

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
        try (Just <$> pMUL),
        try (Just <$> pMULS),
        try (Just <$> pNOP),
        try (Just <$> pOR),
        try (Just <$> pORI),
        try (Just <$> pPOP),
        try (Just <$> pPUSH),
        try (Just <$> pRET),
        try (Just <$> pSBRC),
        try (Just <$> pSBRS),
        try (Just <$> pST),
        try (Just <$> pSTS),
        try (Just <$> pSUB),
        try (Just <$> pSUBI),
        try (char ';' >> manyTill anyChar newline) >> return Nothing -- Ignore comments
        ]

parseAssembly :: String -> Either ParseError [Instruction]
parseAssembly input = do
    result <- parse (many pInstruction) "" input
    return (catMaybes result)
