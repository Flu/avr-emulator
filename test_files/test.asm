    LD R16, -X
    LDI R16, 0x10
    LDI R17, 0x20
    LDI R18, 0x30
    LDI R19, 0x40

    ADD R16, R17
    SUB R18, R19
    AND R16, R18

    ADIW R26:27, 0x05
    SUBI R16, 0x0F
    ANDI R17, 0xF0

    CP R16, R17
    BREQ equal
    BRNE not_equal

equal:
    LDI R20, 0x01
    JMP continue

not_equal:
    LDI R20, 0x00

continue:
    CPC R16, R17
    CPSE R18, R19
    CPI R20, 0x01

    BREQ some_label
    BRNE another_label

    JMP end

some_label:
    LDI R16, 0x10
    LDI R17, 0x20
    LDI R18, 0x30
    LDI R19, 0x40

another_label:

    LDI R2, 0xff
loop3:
    LDI R3, 0xff
loop2:
    LDI R6, 0xff
loop1:
    SUBI R6, 0x1
    BRNE loop1

    SUBI R3, 0x1
    BRNE loop2

    SUBI R2, 0x1
    BRNE loop3
end:
  NOP