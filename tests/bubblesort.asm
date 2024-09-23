; Register usage:
; R16 - Temporary storage for value comparisons
; R17 - Temporary storage for value comparisons
; R18 - Loop counter for outer loop
; R19 - Loop counter for inner loop
; R20 - Flag to check if a swap occurred (0: no swap, 1: swap)
; X   - Memory pointer (R27:R26)
START:
    ; Load 20 integers directly into memory at 0x0000
LDI R26, 0x00              ; Initialize X (R26:R27) with start address
LDI R27, 0x00

LDI R16, 0x82              ; Random unique value 130
ST X+, R16
LDI R16, 0x02              ; Random unique value 2
ST X+, R16
LDI R16, 0x84              ; Random unique value 132
ST X+, R16
LDI R16, 0x05              ; Random unique value 5
ST X+, R16
LDI R16, 0x86              ; Random unique value 134
ST X+, R16
LDI R16, 0x89              ; Random unique value 137
ST X+, R16
LDI R16, 0x0B              ; Random unique value 11
ST X+, R16
LDI R16, 0x0E              ; Random unique value 14
ST X+, R16
LDI R16, 0x8F              ; Random unique value 143
ST X+, R16
LDI R16, 0x90              ; Random unique value 144
ST X+, R16
LDI R16, 0x10              ; Random unique value 16
ST X+, R16
LDI R16, 0x92              ; Random unique value 146
ST X+, R16
LDI R16, 0x13              ; Random unique value 19
ST X+, R16
LDI R16, 0x94              ; Random unique value 148
ST X+, R16
LDI R16, 0x18              ; Random unique value 24
ST X+, R16
LDI R16, 0x19              ; Random unique value 25
ST X+, R16
LDI R16, 0x9B              ; Random unique value 155
ST X+, R16
LDI R16, 0x9D              ; Random unique value 157
ST X+, R16
LDI R16, 0x9E              ; Random unique value 158
ST X+, R16
LDI R16, 0x1F              ; Random unique value 31
ST X+, R16
LDI R16, 0xA0              ; Random unique value 160
ST X+, R16
LDI R16, 0x22              ; Random unique value 34
ST X+, R16
LDI R16, 0x28              ; Random unique value 40
ST X+, R16
LDI R16, 0xA9              ; Random unique value 169
ST X+, R16
LDI R16, 0x2C              ; Random unique value 44
ST X+, R16
LDI R16, 0xAF              ; Random unique value 175
ST X+, R16
LDI R16, 0x32              ; Random unique value 50
ST X+, R16
LDI R16, 0x33              ; Random unique value 51
ST X+, R16
LDI R16, 0xB2              ; Random unique value 178
ST X+, R16
LDI R16, 0xB5              ; Random unique value 181
ST X+, R16
LDI R16, 0x35              ; Random unique value 53
ST X+, R16
LDI R16, 0x37              ; Random unique value 55
ST X+, R16
LDI R16, 0x38              ; Random unique value 56
ST X+, R16
LDI R16, 0x34              ; Random unique value 52
ST X+, R16
LDI R16, 0x3D              ; Random unique value 61
ST X+, R16
LDI R16, 0xC3              ; Random unique value 195
ST X+, R16
LDI R16, 0xC4              ; Random unique value 196
ST X+, R16
LDI R16, 0xC6              ; Random unique value 198
ST X+, R16
LDI R16, 0x4A              ; Random unique value 74
ST X+, R16
LDI R16, 0xCB              ; Random unique value 203
ST X+, R16
LDI R16, 0x50              ; Random unique value 80
ST X+, R16
LDI R16, 0x53              ; Random unique value 83
ST X+, R16
LDI R16, 0xD5              ; Random unique value 213
ST X+, R16
LDI R16, 0x58              ; Random unique value 88
ST X+, R16
LDI R16, 0xDA              ; Random unique value 218
ST X+, R16
LDI R16, 0x5A              ; Random unique value 90
ST X+, R16
LDI R16, 0x5C              ; Random unique value 92
ST X+, R16
LDI R16, 0xDD              ; Random unique value 221
ST X+, R16
LDI R16, 0x5B              ; Random unique value 91
ST X+, R16
LDI R16, 0x63              ; Random unique value 99
ST X+, R16
LDI R16, 0x67              ; Random unique value 103
ST X+, R16
LDI R16, 0xE7              ; Random unique value 231
ST X+, R16
LDI R16, 0xEA              ; Random unique value 234
ST X+, R16
LDI R16, 0xED              ; Random unique value 237
ST X+, R16
LDI R16, 0x76              ; Random unique value 118
ST X+, R16
LDI R16, 0xF6              ; Random unique value 246
ST X+, R16
LDI R16, 0x78              ; Random unique value 120
ST X+, R16
LDI R16, 0x79              ; Random unique value 121
ST X+, R16
LDI R16, 0x7A              ; Random unique value 122
ST X+, R16
LDI R16, 0xFB              ; Random unique value 251
ST X+, R16

; Bubble Sort algorithm
BUBBLE_SORT:
OUTER_LOOP:

    LDI R19, 0x3b              ; Inner loop counter
    LDI R26, 0x00              ; Reset X to point to the start of the array
    LDI R27, 0x00

    LDI R20, 0x00              ; Clear swap flag by setting R20 to 0

INNER_LOOP:
    LD R16, X                  ; Load current element into R16
    INC R26                    ; Increment X low byte
    LD R17, X                  ; Load next element into R17

    CP R16, R17                ; Compare current and next element
    BRLO NO_SWAP               ; If current < next, no swap

    ; Swap the elements
    ST X, R16                  ; Store R16 in place of the next element
    DEC R26                    ; Move X back to current element
    ST X, R17                  ; Store R17 in place of the current element
    INC R26                    ; Move X forward to point to the next element
    LDI R20, 0x01              ; Set swap flag to 1

NO_SWAP:
    DEC R19                    ; Decrement inner loop counter
    BRNE INNER_LOOP            ; If not zero, continue inner loop

    CPI R20,0x00               ; Test if any swap occurred
    BRNE OUTER_LOOP            ; If swaps occurred, repeat outer loop

SORT_DONE: