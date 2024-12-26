; Compare R21 = 0xAA (10101010) with R22 = 0xAA (10101010)
LDI R21, 0xAA
LDI R22, 0xAA
CPSE R21, R22
LDI R16, 0xFF ; Ensures executed instruction for testing
