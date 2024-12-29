;; Copyright (2024)
;; Fluturel Adrian
;; MIT License
    
    LDI R16, 0x5 ; The number for the factorial function

    CALL func_factorial
    JMP end_of_program

;; Factorial function
;; Parameters: 
;; R16 - the number to calculate the factorial for
;; Returns:
;; R24:25 - the result of the factorial calculation
func_factorial:
    ; Save registers we are going to modify
    PUSH R16
    PUSH R0
    PUSH R1
    
    ; Set accumulator to 1
    LDI R24, 0x01
    CLR R25

factorial_loop:
    MUL R16, R24 ; Multiply R16 to the accumulator
    MOVW R24, R0 ; Move result to the accumulator
    DEC R16
    BRNE factorial_loop ; If R16 is not 0 yet, we do another loop

    ; Restore changed register
    POP R1
    POP R0
    POP R16
    RET

end_of_program:
