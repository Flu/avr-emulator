    LDI R17,0x7
    LDI R18,0xf0
    LDI R16,0x0
loop:
    SBRS R18, 0
    LDI R16, 0x33