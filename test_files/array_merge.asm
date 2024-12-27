;; Copyright (2024)
;; Fluturel Adrian
;; MIT License
;; Merge two sorted arrays into a single sorted array

;; Load some values into the first array

LDI R26, 0x00           ; Low byte of first array address
LDI R27, 0x01           ; High byte of first array address

LDI R16, 0x1
ST X+, R16

LDI R16, 0x14
ST X+, R16

LDI R16, 0x20
ST X+, R16

LDI R16, 0x23
ST X+, R16

LDI R16, 0x24
ST X+, R16

LDI R16, 0x25
ST X+, R16

LDI R16, 0x3b
ST X+, R16

LDI R16, 0x44
ST X+, R16

LDI R16, 0x5c
ST X+, R16

LDI R16, 0x7b
ST X+, R16

LDI R16, 0x82
ST X+, R16

LDI R16, 0x84
ST X+, R16

LDI R16, 0xa9
ST X+, R16

LDI R16, 0xaf
ST X+, R16

LDI R16, 0xb1
ST X+, R16

LDI R16, 0xb2
ST X+, R16

LDI R16, 0xb3
ST X+, R16

;; Load some values into the second array

LDI R28, 0x50           ; Low byte of second array address
LDI R29, 0x01           ; High byte of second array address

LDI R16, 0x2
ST Y+, R16

LDI R16, 0xa
ST Y+, R16

LDI R16, 0x2e
ST Y+, R16

LDI R16, 0x2f
ST Y+, R16

LDI R16, 0x31
ST Y+, R16

LDI R16, 0x37
ST Y+, R16

LDI R16, 0x4F
ST Y+, R16

LDI R16, 0x54
ST Y+, R16

LDI R16, 0x65
ST Y+, R16

LDI R16, 0x78
ST Y+, R16

LDI R16, 0x88
ST Y+, R16

LDI R16, 0x89
ST Y+, R16

LDI R16, 0x93
ST Y+, R16

LDI R16, 0x9f
ST Y+, R16

LDI R16, 0xB9
ST Y+, R16

LDI R16, 0xBA
ST Y+, R16

LDI R16, 0xCA
ST Y+, R16

LDI R16, 0xCD
ST Y+, R16

LDI R16, 0xCE
ST Y+, R16

LDI R16, 0xED
ST Y+, R16

LDI R16, 0xFF
ST Y+, R16

; Reset X and Y registers to the start of their respective arrays
LDI R26, 0x00           ; Low byte of first array address
LDI R27, 0x01           ; High byte of first array address

LDI R28, 0x50           ; Low byte of second array address
LDI R29, 0x01           ; High byte of second array address

LDI R30, 0xa0           ; Low byte of output array address
LDI R31, 0x01           ; High byte of output array address

LDI R22, 0x11           ; Length of first array (17 elements)
LDI R23, 0x15           ; Length of second array (21 elements)

CALL merge_arrays       ; Call the merge function

JMP end                 ; Go to the end of the program, it is done

merge_arrays:
    ; R24: Counter for first array
    ; R25: Counter for second array
    LDI R24, 0x00       ; Initialize first array counter
    LDI R25, 0x00       ; Initialize second array counter

merge_loop:
    ; Check if either array is exhausted
    CP R24, R22         ; Compare first array counter with its length
    BRSH merge_y_only   ; If R24 >= R22, process remaining Y array

    CP R25, R23         ; Compare second array counter with its length
    BRSH merge_x_only   ; If R25 >= R23, process remaining X array

    ; Load elements from both arrays
    LD R20, X           ; Load element from first array into R20
    LD R21, Y           ; Load element from second array into R21

    ; Compare and store the smaller element
    CP R20, R21         ; Compare elements
    BRLO store_x        ; If R20 < R21, store R20 and advance X

    ; Store R21 (from second array) in output array and increment Z
    ST Z+, R21          

    INC R25             ; Increment second array counter
    ADIW R28, 0x01      ; Increment Y register
    JMP merge_loop      ; Continue loop

store_x:
    ; Store R20 (from first array) in output array and increment Z
    ST Z+, R20          
    INC R24             ; Increment first array counter
    ADIW R26, 0x01      ; Increment X low byte
    
    JMP merge_loop      ; Continue loop

merge_x_only:
    ; Copy remaining elements from first array
    LD R20, X           ; Load element from first array into R20
    ST Z+, R20          ; Store it in output array and increment Z
    
    INC R24             ; Increment first array counter
    ADIW R26, 0x1       ; Increment X register

    CP R24, R22         ; Check if done with the first array
    BRLO merge_x_only   ; Repeat if not done

merge_y_only:
    ; Copy remaining elements from second array
    LD R21, Y           ; Load element from second array into R21
    ST Z+, R21          ; Store it in output array
    
    INC R25             ; Increment second array counter
    ADIW R28, 0x01      ; Increment Y register

    CP R25, R23         ; Check if done with the second array
    BRLO merge_y_only   ; Repeat if not done

merge_end:
    RET                 ; Return from function

end: