;; (Copyright 2024), Adrian Fluturel
;; Program that tries every combination of all 8-bit values, tries to multiply them using a custom multiplication function
;; and compares the result with the MUL built-in instruction. If all results match, R31 will be set to 0x1. If one of them
;; is different, R31 is 0x00. 
    
    JMP _main

;; Multiplication function
;; First argument in R17, second argument in R18
;; Result in R16
mul_function:
    PUSH R17 ; Save R17, R18 to the stack
    PUSH R18
    LDI R16, 0x0 ; Clear R16 of old values
    loop:
    SBRC R18, 0
    ADD R16, R17
    LSL R17
    LSR R18
    BRNE loop

    POP R18 ; Restore R18 and R17
    POP R17
    RET
;; End of multiplication function

_main:
    ; Go through every combination of all 8-bit values for R17 and R18 and test them
    LDI R17, 0xff
    r17_loop:

    LDI R18, 0xff
    r18_loop:

    ; Test whether the answer of our multiply function is equal to the low byte of the MUL instruction
    CALL mul_function
    MUL R17, R18
    ; The answer for MUL is in the R0 (low byte in R0, high in R1)
    CP R16, R0
    ; Jump to the end if the answers are different, continue otherwise
    BRNE end

    SUBI R18, 0x1
    BRNE r18_loop

    SUBI R17, 0x1
    BRNE r17_loop

    ; Set R31 to 1 if all the answers are correct, else 0
    LDI R31, 0x01
end: