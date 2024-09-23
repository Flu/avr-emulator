START:
    ; Load 20 integers directly into memory at 0x0000
    LDI R26, 0x00              ; Initialize X (R26:R27) with start address
    LDI R27, 0x00

    ; Load array with specific values
    LDI R16, 0x2B              ; Random value 43
    ST X+, R16
    LDI R16, 0x0C              ; Random value 12
    ST X+, R16
    LDI R16, 0x4E              ; Random value 78
    ST X+, R16
    LDI R16, 0x19              ; Random value 25
    ST X+, R16
    LDI R16, 0x38              ; Random value 56
    ST X+, R16
    LDI R16, 0x22              ; Random value 34
    ST X+, R16
    LDI R16, 0x59              ; Random value 89
    ST X+, R16
    LDI R16, 0x09              ; Random value 9
    ST X+, R16
    LDI R16, 0x40              ; Random value 64
    ST X+, R16
    LDI R16, 0x16              ; Random value 22
    ST X+, R16
    LDI R16, 0x39              ; Random value 57
    ST X+, R16
    LDI R16, 0x5B              ; Random value 91
    ST X+, R16
    LDI R16, 0x1E              ; Random value 30
    ST X+, R16
    LDI R16, 0x28              ; Random value 40
    ST X+, R16
    LDI R16, 0x0B              ; Random value 11
    ST X+, R16
    LDI R16, 0x48              ; Random value 72
    ST X+, R16
    LDI R16, 0x37              ; Random value 55
    ST X+, R16
    LDI R16, 0x24              ; Random value 36
    ST X+, R16
    LDI R16, 0x54              ; Random value 84
    ST X+, R16
    LDI R16, 0x1D              ; Random value 29
    ST X+, R16
    LDI R16, 0x2B              ; Random value 43
ST X+, R16
LDI R16, 0x0C              ; Random value 12
ST X+, R16
LDI R16, 0x4E              ; Random value 78
ST X+, R16
LDI R16, 0x19              ; Random value 25
ST X+, R16
LDI R16, 0x38              ; Random value 56
ST X+, R16
LDI R16, 0x22              ; Random value 34
ST X+, R16
LDI R16, 0x59              ; Random value 89
ST X+, R16
LDI R16, 0x09              ; Random value 9
ST X+, R16
LDI R16, 0x40              ; Random value 64
ST X+, R16
LDI R16, 0x16              ; Random value 22
ST X+, R16