; Set Carry, then compare R19 = 0x7F (01111111) with R20 = 0x80 (10000000)
LDI R19, 0x7F
LDI R20, 0x80
SEC
CPC R19, R20
