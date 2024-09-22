    LDI R17,0x7
    LDI R18,0x3
    LDI R16,0x0
loop:
    SBRC R18, 0
    ADD R16, R17
    LSL R17
    LSR R18
    BRNE loop

    LDI R5, 0x02
    LDI R6, 0x6
    MULS R5, R6