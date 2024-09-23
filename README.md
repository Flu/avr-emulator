# AVR Emulator
[![Haskell CI](https://github.com/Flu/avr-emulator/actions/workflows/haskell.yml/badge.svg)](https://github.com/Flu/avr-emulator/actions/workflows/haskell.yml)

This project is an AVR emulator written in Haskell, utilizing CABAL for building and managing dependencies. The emulator simulates a basic AVR microcontroller environment with a 2KB memory space starting from address `0x0000`. The emulator currently supports a subset of AVR instructions, and you can load and execute assembly programs to test its functionality.

## How to Build and Run

### Prerequisites
You need to have `Cabal` and `GHC` installed to build and run the emulator. We recommend using `ghcup` to install these tools. To install `ghcup`, follow the instructions at [https://www.haskell.org/ghcup/](https://www.haskell.org/ghcup/).

### Build the Emulator

1. Clone the repository and navigate to the project folder.
2. Run the following command to build the project:
   ```bash
   cabal build
   ```

### Run the emulator

```bash
cabal run exes -- /path/to/assembly/file.asm
```

Replace `/path/to/assembly/file.asm` with the path to your assembly file.

### Ending the program

The emulator will automatically end when there are no more instructions to execute.

### Test files

You can find test assembly programs in the `tests/` directory. These files provide examples of how an AVR assembly program is structured and can be used to verify the emulator's functionality.

## Features
- **Memory**: 2KB starting from `0x0000`.
- **Supported Instructions**:
  - `ADC` – Add with carry between two registers.
  - `ADD` – Add two register values and store the result in one of the registers.
  - `ADIW` – Add an immediate word to a register pair.
  - `AND` – Perform a bitwise AND between two registers.
  - `ANDI` – Perform a bitwise AND between a register and an immediate value.
  - `BREQ` – Branch if equal to zero.
  - `BRLO` – Branch if lower (unsigned).
  - `BRNE` – Branch if not equal to zero.
  - `CP` – Compare two registers.
  - `CPC` – Compare with carry between two registers.
  - `CPI` – Compare a register with an immediate value.
  - `CPSE` – Compare and skip if equal.
  - `DEC` – Decrement a register.
  - `EOR` – Perform bitwise exclusive OR between two registers.
  - `INC` – Increment a register.
  - `JMP` – Jump to a label.
  - `LD` – Load indirect from data space using an indirect address.
  - `LDI` – Load an immediate value into a register.
  - `LDS` – Load a direct value from data space.
  - `LSL` – Logical shift left.
  - `LSR` – Logical shift right.
  - `MOV` – Move the value from one register to another.
  - `MUL` – Multiply two registers.
  - `MULS` – Multiply signed values in two registers.
  - `NOP` – No operation.
  - `OR` – Perform a bitwise OR between two registers.
  - `ORI` – Perform a bitwise OR between a register and an immediate value.
  - `SBRC` – Skip if bit in register is cleared.
  - `SBRS` – Skip if bit in register is set.
  - `ST` – Store indirect to data space using an indirect address.
  - `STS` – Store direct to data space.
  - `SUB` – Subtract one register value from another.
  - `SUBI` – Subtract an immediate value from a register.