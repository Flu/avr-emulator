;-- Setting bits to true which are already true to check if it doesn't flip bits.
ldi r31, 0x0F
set
bld r31, 3
bld r31, 2
bld r31, 1
bld r31, 0

;-- Setting bits to false which are already false to check if it doesn't flip bits.
clt
bld r31, 7
bld r31, 6
bld r31, 5
bld r31, 4
;-- At this point r31 should hold the same value as in the ldi instruction (0x0F).
;-- Now we will switch everybit to test if it works.
bld r31, 3
bld r31, 2
bld r31, 1
bld r31, 0
set
bld r31, 7
bld r31, 6
bld r31, 5
bld r31, 4
;-- The value should now be the reverse (0xF0).