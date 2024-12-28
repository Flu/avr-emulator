;; Copyright (2024)
;; Fluturel Adrian
;; MIT License
;; Calculate the number of steps needed in the Collatz sequence with a specific number.
;; The result is in the register pair R30:31

LDI R26, 0x89         ; Initialize low byte of starting value
LDI R27, 0x02         ; Initialize high byte of starting value

MOV R0, R26           ; Move values in R0:1 for testing purposes
MOV R1, R27

LDI R30, 0x00         ; Number of steps for this specific number to get to 1
LDI R31, 0x00

collatz_start:
    ; Check if X (R26:R27) == 1
    LDI R18, 0x01      ; Load low byte of comparison value
    LDI R19, 0x00      ; Load high byte of comparison value
    CP R26, R18        ; Compare low bytes
    CPC R27, R19       ; Compare high bytes with carry
    BREQ collatz_end   ; If equal, we're done

    ; If not, we continue and increment the counter
    ADIW R30, 0x01

    ; Check if X is odd: Test lowest bit of R26
    MOV R16, R26
    ANDI R16, 0x01     ; Mask the lowest bit of R26
    BRNE collatz_odd   ; If result != 0, it’s odd

    ; Even case: X = X / 2
    LSR R27            ; Logical shift right on high byte
    ROR R26            ; Rotate right through carry on low byte
    JMP collatz_start  ; Repeat loop

collatz_odd:
    ; Odd case: X = 3 * X + 1
    MOVW R18:19, R26:27      ; Copy X (R26:R27) to R18:R19 for multiplication
    ADD R26, R26       ; Double low byte
    ADC R27, R27       ; Double high byte with carry
    ADD R26, R18       ; Add original low byte
    ADC R27, R19       ; Add original high byte with carry
    ADIW R26, 0x01     ; Add 1 to the 16-bit value in X
    JMP collatz_start  ; Repeat loop

collatz_end:
    NOP                ; End of program