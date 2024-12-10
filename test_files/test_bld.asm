;-- Setting bits to true which are already true.
ldi r31, 0xFF
set
bld r31, 6
bld r31, 5
bld r31, 4

;-- Setting for no T flag.
bclr 6
ldi r30, 0x0F
bld r30, 1
bld r30, 3
bld r30, 7