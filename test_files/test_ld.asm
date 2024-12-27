    ;; Copyright (2024)
    ;; Amber van der Graaf
    ;; MIT License

    clr r27
    ldi r26, 0x80               ; Point X register to 0x0080
    ldi r0, 0x05                ; Set r0 to 0x27
    st X+, r0                   ; Load 0x27 into memory address 0x0005
    ldi r0, 0x06
    st X, r0
    clr r0                      ; Set r0 to 0x00
    ld r0,-X                    ; Load 0x27 into r0 (pre decrement of the pointer at X)
    ld r1, X+                   ; Load 0x27 into r2 (post increment of the pointer at X)
    ld r2, X                    ; Load 0x27 into r1

    clr r29
    ldi r28, 0x90
    ldi r3, 0x15
    st Y+, r3
    ldi r3, 0x16
    st Y, r3
    clr r3
    ld r4,-Y
    ld r5, Y+
    ld r6, Y

    clr r31
    ldi r30, 0x25
    ldi r7, 0x25
    st Z+, r7
    ldi r7, 0x26
    st Z, r7
    clr r7
    ld r8, -Z
    ld r9, Z+
    ld r10, Z
