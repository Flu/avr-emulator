; Compare R21 = 0x55 (01010101) with R22 = 0xAA (10101010)
LDI R16, 0xFF
LDI R21, 0x55
LDI R22, 0xAA
CPSE R21, R22
LDI R16, 0x00 ; Ensures skipped instruction for testing
