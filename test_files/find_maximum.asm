;; Copyright (2024)
;; Fluturel Adrian
;; MIT License

    ; First, put some values in the 'array' so we can search for it
    LDI R26, 0xa0
    LDI R27, 0x00

    LDI R16, 0x3c
    ST X+, R16
    LDI R16, 0x49
    ST X+, R16
    LDI R16, 0x8
    ST X+, R16
    LDI R16, 0xaf
    ST X+, R16
    LDI R16, 0x2
    ST X+, R16
    LDI R16, 0x56
    ST X+, R16
    LDI R16, 0x33
    ST X+, R16
    LDI R16, 0x21
    ST X+, R16
    LDI R16, 0x4f
    ST X+, R16

; Find the maximum value in an array
    LDI R26, 0xa0  ; Address of the array
    LDI R27, 0x00
    LDI R16, 0x9 ; Array length
    CALL func_find_max

    JMP end

;; Find maximum in array function - finds the maximum unsigned value in the given array
;; Parameters:
;; R26:27 - the starting address of the array
;; R16 - the length of the array
;; Returns:
;; R18 - the maximum number in that array
func_find_max:
    ; Save registers we are about to modify
    PUSH R26
    PUSH R27
    PUSH R16

    LD R18, X+ ; Load first element into R18 (current max)
find_max_loop:
    LD R19, X+        ; Load next element
    CP R18, R19       ; Compare current max with element
    BRLO update_max   ; If element > max, update max
    JMP continue

update_max:
    MOV R18, R19      ; Update max

continue:
    DEC R16 ; Decrement counter
    BRNE find_max_loop ; Repeat if not zero

    ; Restore registers
    POP R16
    POP R27
    POP R26
    RET

end:
    ; The max is now stored in R18