    ;; Copyright (2024)
    ;; Amber van der Graaf
    ;; MIT License

    clr r27
    ldi r26, 0x80               ; X -> 0x0080
    ldi r16, 0x05               ; temp high reg
    mov r0, r16                 ; r0 = 0x05
    st X+, r0                   ; store 0x05 at 0x80

    ldi r16, 0x06
    mov r0, r16                 ; r0 = 0x06
    st X, r0

    clr r0
    ld r0, -X                   ; pre-decrement, load from 0x80 -> r0 = 0x05
    ld r1, X+                   ; r1 = 0x05 (from 0x80 again)
    ld r2, X                    ; r2 = 0x06 (0x81)

    clr r29
    ldi r28, 0x90
    ldi r17, 0x15
    mov r3, r17
    st Y+, r3
    ldi r17, 0x16
    mov r3, r17
    st Y, r3
    clr r3
    ld r4, -Y
    ld r5, Y+
    ld r6, Y

    clr r31
    ldi r30, 0x25
    ldi r18, 0x25
    mov r7, r18
    st Z+, r7
    ldi r18, 0x26
    mov r7, r18
    st Z, r7
    clr r7
    ld r8, -Z
    ld r9, Z+
    ld r10, Z
