    eor r19,r19 ; Clear r19
loop:
    inc r19 ; Increase r19
    cpi r19,$10 ; Compare r19 with $10
    brlo loop ; Branch if r19 < $10 (unsigned)
    nop ; Exit from loop (do nothing)