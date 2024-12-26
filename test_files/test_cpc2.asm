; Clear Carry, then compare R19 = 0x80 (10000000) with R20 = 0x7F (01111111)
LDI R19, 0x80
LDI R20, 0x7F
CLC
CPC R19, R20
