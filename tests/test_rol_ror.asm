    LDI R16, 0x02
    LDI R17, 0x33

    ; Multiply R17:16 by two
    lsl R16
    rol R17

    LDI R19, 0xF
    LDI R18, 0xAB
    ; Divide r19:r18 by two
    lsr r19
    ror r18