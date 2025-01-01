    LDI ZH, 0x01
    LDI ZL, 0x11
    LDI R20, 0x0E
    ST Z, R20
    CLR R20

    LDI R19, 0x25
    XCH Z, R19

    LDI R20, 0x05
    LDI ZH, 0x01
    LDI ZL, 0x12
    ST Z, R20
    CLR R20
    XCH Z, R25
