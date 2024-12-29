;; Copyright (2024)
;; Fluturel Adrian
;; MIT License

; Main entry point of the program
_main:

    ; Start by filling the memory with random numbers
    LDI R26, 0x00  ; Start address of the list
    LDI R27, 0x01

    LDI R16, 0xa7 ; Seed for the PRNG
    LDI R17, 0x45 ; Feedback constant for the PRNG

    LDI R25, 0xff
    MOV R8, R25  ; Register pair for how many random values we want
    CLR R9

    LDI R18, 0x01 ; Register pair for subtracting from R9:R8
    LDI R19, 0x00

    CALL func_fill_array

    ; Prepare registers for bubblesort - R12:13 for the start of the list, R26:27 for traversing the list
    ; and R8:9 for the length of the list
    CLR R12
    LDI R25, 0x01
    MOV R13, R25

    LDI R26, 0x00
    LDI R27, 0x00
    
    LDI R25, 0xff
    MOV R8, R25
    CLR R9

    CALL func_bubblesort
    JMP end_of_program


func_fill_array:
    CALL PRNG
    ST X+, R16
    INC R16
    SUB R8, R18 ; decrement loop counter
    SBC R9, R19
    BRNE func_fill_array

    RET

;; Bubble Sort function - sorts a given list of unsigned integers in-place
; R12:13 - start address of the list to sort
; R9:8 - length of the list
; No return value
func_bubblesort:
    ; Save registers we are going to use
    PUSH R19
    PUSH R18
    PUSH R12
    PUSH R13
    PUSH R9
    PUSH R8
    PUSH R27
    PUSH R26
    PUSH R28
    PUSH R29
    PUSH R20
    PUSH R16
    PUSH R17
    PUSH R0

    LDI R28, 0x01  ; Register pair for subtraction from the inner loop counter
    LDI R29, 0x00
OUTER_LOOP:

    MOVW R18, R8   ; Inner loop counter
    MOVW R26, R12 ; Reset X to point at the start of the array

    LDI R20, 0x00  ; Clear swap flag by setting R20 to 0

INNER_LOOP:
    LD R16, X+     ; Load current element into R16
    LD R17, X      ; Load next element into R17

    CP R16, R17    ; Compare current and next element
    BRLO NO_SWAP   ; If current < next, no swap
    BREQ NO_SWAP

    ; Swap the elements
    ST X, R16      ; Store R16 in place of the next element
    ST -X, R17     ; Store R17 in place of the current element

    LDI R0, 0x0    ; Increment R27:R26 with 1
    INC R26        ; Move X forward to point to the next element
    ADC R27, R0
    
    LDI R20, 0x01  ; Set swap flag to 1

NO_SWAP:
    SUB R18, R28    ; Decrement inner loop counter
    SBC R19, R29   
    BRNE INNER_LOOP ; If not zero, continue inner loop

    CPI R20, 0x00   ; Test if any swap occurred
    BRNE OUTER_LOOP ; If swaps occurred, repeat outer loop

    ; Restore registers we saved from the beginning and return
    POP R0
    POP R17
    POP R16
    POP R20
    POP R29
    POP R28
    POP R26
    POP R27
    POP R8
    POP R9
    POP R13
    POP R12
    POP R18
    POP R19

    RET

; PRNG: Pseudo-random number generator
; Input:  R16 (seed), R17 (feedback constant k)
; Output: New random value in R16
; Clobbers: R18, carry flag
; Preserves: All other registers
PRNG:
    PUSH    R17             ; Save k
    PUSH    R18             ; Save temporary register

    MOV     R18, R16        ; Copy seed to R18 for manipulation

    LSR     R16             ; Logical shift right (LSB goes into carry)
    BRCC    PRNG_done       ; If no carry, no need for feedback XOR

    ; XOR feedback based on carry (i.e., if LSB was 1)
    EOR     R16, R17        ; XOR seed with feedback constant k

PRNG_done:
    POP     R18             ; Restore R18
    POP     R17             ; Restore k

    RET                     ; Return to caller with R16 as the new pseudo-random number

end_of_program:
    NOP