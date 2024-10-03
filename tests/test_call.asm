    LDI R0, 0x0
    LDI R1, 0x0
    CALL function

    LDI R17, 0x1
    JMP end

function:
    LDI R20, 0x1
    LDI R21, 0x2
    ADD R20, R21
    RET

end:
    NOP