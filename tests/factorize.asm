    ; Initialize
    LDI R16, 0x44         ; R16 holds the number to factorize (N)
    LDI R18, 0x2         ; Start divisor from 2
    LDI R30, 0x00 ; Initialize X (R26:R27) to point to start of memory
    LDI R31, 0x00
    LDI R19, 0x1         ; Set 1 for comparison (we'll compare N to 1)

FactorLoop:
    ; Check if divisor R18 is greater than current N
    ; If R16 == 1, we're done (factorization complete)
    CP R16, R19        ; Compare R16 (N) with 1
    BREQ Done          ; If R16 == 1, all factors found

    ; Check if N % R18 == 0 (R16 % R18 == 0)
    MOV R20, R16       ; Copy N to R20 for modulus calculation
    CALL Modulus       ; Call Modulus subroutine (remainder in R21)
    TST R21            ; Test R21 (check if remainder is 0)
    BREQ StoreFactor   ; If remainder is 0, store R18 as a factor

    ; If R18 is not a divisor, increment R18 and try the next divisor
    INC R18
    JMP FactorLoop

StoreFactor:
    ; Store the factor in memory (pointed by X)
    ST X+, R18         ; Store current divisor (R18) in memory

    ; Divide N by R18 (N = N / R18)
    MOV R20, R16       ; Copy N to R20
    CALL Divide        ; Call Divide subroutine (result in R16)

    JMP FactorLoop    ; Repeat factorization process

; Modulus subroutine: R20 % R18, result in R21
Modulus:
    MOV R22, R20       ; Copy N (R20) to R22
    MOV R23, R18       ; Copy divisor (R18) to R23
ModLoop:
    CP R22, R23        ; Compare R22 (N) with R23 (divisor)
    BRLO ModDone       ; If N < divisor, we're done (remainder in R22)
    SUB R22, R23       ; N = N - divisor (keep subtracting)
    JMP ModLoop

ModDone:
    MOV R21, R22       ; R21 gets remainder
    RET

; Divide subroutine: R20 / R18, result in R16
Divide:
    LDI R21, 0x00            ; Clear quotient register
DivLoop:
    CP R20, R18        ; Compare N (R20) with divisor (R18)
    BRLO DivDone       ; If N < divisor, division is done
    SUB R20, R18       ; N = N - divisor
    INC R21            ; Increment quotient
    JMP DivLoop

DivDone:
    MOV R16, R21       ; Store quotient in R16
    RET

Done:
    NOP