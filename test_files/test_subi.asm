    LDI R16, 0x8
loop:
    DEC R16
    BRNE loop
    LDI R17, 0x45
end:
    NOP