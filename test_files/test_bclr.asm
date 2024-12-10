;-- carry flag
sec
bclr 0

;-- half carry flag
seh
bclr 5

;-- Interrupt flag
sei
bclr 7

;-- Negative flag.
sen
bclr 2

;-- sign flag.
ses
bclr 4

;-- T flag.
set
bclr 6

;-- Overflow flag.
sev
bclr 3

;-- Zero flag.
sez
bclr 1