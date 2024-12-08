    LDI R15, 0xe
    LDI R17, 0x0  ; F(n-2)
    LDI R18, 0x1  ; F(n-1)
    STS 0x16, R15
    LDS R0, 0x16

    ; Iterate n times
    MOV R19, R15  ; Copy n to a temporary register
loop:
    ADD R16, R17  ; Calculate F(n-1) + F(n-2)
    MOV R17, R18
    MOV R18, R16
    DEC R19
    BRNE loop