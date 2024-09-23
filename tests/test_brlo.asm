    LDI R16, 0x11
    LDI R17, 0x12

    CP R16, R17
    BRLO not

    LDI R18, 0xff
not: